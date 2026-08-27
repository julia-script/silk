/**
 * Backend and toolchain facts, as data rather than as markup.
 *
 * These phases used to be components that rendered their own cards. The registry now projects
 * every phase into the shared row grammar, so what is left here is the part that was never
 * presentational: running emission without letting a rejected target throw, and planning the
 * three clang invocations. Both are shapes the row projections consume.
 */

import type { Backend } from '@silk-lang/compiler'
import { Analysis, ToolchainPlan } from '@silk-lang/compiler'
import * as Effect from 'effect/Effect'

const clang = '/usr/bin/clang'

const messageOf = (error: unknown): string =>
  error instanceof Error ? error.message : String(error)

const commandText = (planned: ToolchainPlan.PlannedCommand): string =>
  [planned.command, ...planned.arguments].join(' ')

export type Emission =
  | { readonly _tag: 'Emitted'; readonly artifact: Backend.Artifact }
  | { readonly _tag: 'Rejected'; readonly message: string }

/**
 * Emission is an Effect that fails on a target the backend cannot serve — a native backend asked
 * for a wasm target, say. Running it here turns that into data the pane can render, instead of an
 * exception that takes the pane down with it.
 */
export const backendEmission = (
  snapshot: Analysis.Snapshot,
  mode: 'release' | 'debug',
): Emission => {
  try {
    const selection = Analysis.targetOf(snapshot)
    return {
      _tag: 'Emitted',
      artifact:
        selection._tag === 'Resolved' && selection.target.kind === 'WebAssembly'
          ? Effect.runSync(Analysis.codegenWasm(snapshot, { mode }))
          : Effect.runSync(Analysis.codegen(snapshot, { mode })),
    }
  } catch (error) {
    return { _tag: 'Rejected', message: messageOf(error) }
  }
}

/** The outcome of instantiating the emitted module and calling its entry export. */
export type Execution =
  | { readonly _tag: 'Completed'; readonly value: number }
  | { readonly _tag: 'Trapped'; readonly message: string }
  | { readonly _tag: 'Failed'; readonly message: string }

/**
 * Runs the emitted binary in the browser's own WebAssembly engine.
 *
 * This doubles as an independent validator: a module the engine rejects never reaches the call,
 * and the interpreter ran the same MIR the backend emitted from — so a disagreement between the
 * two is a backend bug, and worth surfacing rather than hiding.
 */
export const execute = (bytes: Uint8Array): Execution => {
  let main: () => number
  try {
    // `slice` re-backs the bytes with a plain `ArrayBuffer`, which is what the WebAssembly types
    // accept; the artifact's own array is generic over `ArrayBufferLike`.
    const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes.slice()), {})
    main = instance.exports.silk_main as () => number
  } catch (error) {
    return { _tag: 'Failed', message: messageOf(error) }
  }
  try {
    return { _tag: 'Completed', value: main() }
  } catch (error) {
    return { _tag: 'Trapped', message: messageOf(error) }
  }
}

export type ToolchainCommands =
  | {
      readonly _tag: 'Planned'
      readonly target: string
      readonly commands: ReadonlyArray<readonly [string, string]>
    }
  | { readonly _tag: 'Unavailable'; readonly message: string }

/**
 * The three commands a native build would run.
 *
 * Every planned command is issued *for* a target, so there is nothing to plan until one resolves,
 * and the native toolchain cannot serve a WebAssembly target — saying so beats rendering a clang
 * line that would never be run.
 */
export const toolchainCommands = (
  snapshot: Analysis.Snapshot,
  profile: ToolchainPlan.OptimizationProfile,
): ToolchainCommands => {
  const selection = Analysis.targetOf(snapshot)
  if (selection._tag !== 'Resolved') {
    return { _tag: 'Unavailable', message: selection.error.message }
  }

  const target = selection.target
  if (target.kind !== 'Native') {
    return {
      _tag: 'Unavailable',
      message: `the ${target.id} target has no link driver to orchestrate`,
    }
  }

  return {
    _tag: 'Planned',
    target: target.id,
    commands: [
      [
        'object',
        commandText(
          ToolchainPlan.objectCommand(
            clang,
            target,
            profile,
            '<scope>/program.bc',
            '<scope>/program.o',
          ),
        ),
      ],
      [
        'shim',
        commandText(
          ToolchainPlan.shimCommand(clang, target, '<scope>/silk_shim.c', '<scope>/silk_shim.o'),
        ),
      ],
      [
        'link',
        commandText(
          ToolchainPlan.linkCommand(
            clang,
            target,
            ['<scope>/program.o', '<scope>/silk_shim.o'],
            [],
            '<destination>/program',
          ),
        ),
      ],
    ],
  }
}
