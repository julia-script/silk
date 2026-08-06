import * as Driver from '@silk-effect/compiler/Driver'
import type * as NativeToolchain from '@silk-effect/compiler/NativeToolchain'
import * as Target from '@silk-effect/compiler/Target'
import type * as ToolchainPlan from '@silk-effect/compiler/ToolchainPlan'
import * as Console from 'effect/Console'
import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import type * as FileSystem from 'effect/FileSystem'
import * as Option from 'effect/Option'
import type * as Path from 'effect/Path'
import * as Runtime from 'effect/Runtime'
import { Argument, Command, Flag } from 'effect/unstable/cli'
import * as Report from './Report.js'
import * as SourceEntry from './SourceEntry.js'

/**
 * The `compile` command: read one source file, run the whole driver pipeline over it, and emit a
 * native executable.
 *
 * Scope note — the compiler resolves imports only among the sources handed to it, and has no
 * filesystem module resolution. The command therefore accepts exactly one file and builds a
 * single-entry source map. An `import` inside that file surfaces as an unknown-module diagnostic,
 * which is the honest report; widening to a directory is a compiler change, not a CLI change.
 */

/** The exit status a run reports to the shell. */
export type ExitStatus = 0 | 1

declare const emptyProps: unique symbol

/** An error payload carrying no fields, spelled without the banned bare `{}`. */
interface EmptyProps {
  readonly [emptyProps]?: never
}

/**
 * Signals that a compilation reported failure whose diagnostics are already on stdout. It exists
 * only to drive the process exit status. `errorReported: false` opts out of the runtime's
 * automatic error log, so the exit status is 1 without a redundant stack trace on stderr.
 */
export class CompilationFailed extends Data.TaggedError('CompilationFailed')<EmptyProps> {
  readonly [Runtime.errorReported] = false
}

const targetIds = Target.all.map((candidate) => candidate.id)

const profiles = ['debug', 'release', 'release-with-debug'] as const

const source = Argument.file('source', { mustExist: true }).pipe(
  Argument.withDescription('The single source file to compile.'),
)

const output = Flag.string('output').pipe(
  Flag.withAlias('o'),
  Flag.withDescription('Destination path for the linked executable.'),
  Flag.withDefault('a.out'),
)

const target = Flag.choice('target', targetIds).pipe(
  Flag.withDescription('Compilation target. Defaults to the host target.'),
  Flag.optional,
)

const profile = Flag.choice('profile', profiles).pipe(
  Flag.withDescription('Optimization profile.'),
  Flag.withDefault('debug' as const),
)

const clang = Flag.string('clang').pipe(
  Flag.withDescription('Path to the pinned Clang used for object emission and linking.'),
  Flag.withDefault('clang'),
)

const saveTemps = Flag.boolean('save-temps').pipe(
  Flag.withDescription('Keep the build scope intermediates for inspection.'),
)

const timings = Flag.boolean('timings').pipe(
  Flag.withDescription('Print the per-phase timing and memory report.'),
)

/**
 * Runs one compilation and renders it. Returns the exit status rather than exiting, so the whole
 * pipeline stays testable without spawning a process.
 */
export const run = Effect.fn('CompileCommand.run')(function* (config: {
  readonly source: string
  readonly output: string
  readonly target: string | undefined
  readonly profile: ToolchainPlan.OptimizationProfile
  readonly clang: string
  readonly saveTemps: boolean
  readonly timings: boolean
}): Effect.fn.Return<ExitStatus, SourceEntry.SourceEntryError, FileSystem.FileSystem | Path.Path> {
  const entry = yield* SourceEntry.read(config.source)
  const toolchain: NativeToolchain.Toolchain = Object.freeze({
    _tag: 'Toolchain',
    clang: config.clang,
  })

  const outcome = yield* Driver.compile({
    compilation: {
      rootModule: entry.module,
      sources: new Map([[entry.module, entry.bytes]]),
      ...(config.target === undefined ? {} : { target: config.target }),
    },
    toolchain,
    profile: config.profile,
    destination: config.output,
    scopeName: 'silk-cli',
    saveTemps: config.saveTemps,
  })

  const summary = Report.outcome(outcome, entry)
  if (summary.length > 0) yield* Console.log(summary)
  if (config.timings) {
    yield* Console.log('Phases:')
    yield* Console.log(Report.phases(outcome.report))
  }

  return Report.succeeded(outcome) ? 0 : 1
})

/** The `compile` subcommand wired to the driver. */
export const command = Command.make(
  'compile',
  { source, output, target, profile, clang, saveTemps, timings },
  Effect.fnUntraced(function* (config) {
    const status = yield* run({
      source: config.source,
      output: config.output,
      target: Option.getOrUndefined(config.target),
      profile: config.profile,
      clang: config.clang,
      saveTemps: config.saveTemps,
      timings: config.timings,
    })
    // The diagnostics have already been rendered, so this failure only sets the process exit
    // status; `runMain` maps any failure to 1. It carries no message to avoid a second,
    // redundant error report on stderr.
    if (status !== 0) yield* new CompilationFailed({})
  }),
).pipe(
  Command.withDescription(
    'Compile a single source file into a native executable. Multi-file projects await compiler-side module resolution.',
  ),
)
