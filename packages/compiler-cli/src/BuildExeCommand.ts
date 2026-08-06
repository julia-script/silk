import * as Target from '@silk-effect/compiler/Target'
import type * as ToolchainPlan from '@silk-effect/compiler/ToolchainPlan'
import * as Console from 'effect/Console'
import * as Effect from 'effect/Effect'
import type * as FileSystem from 'effect/FileSystem'
import * as Option from 'effect/Option'
import type * as Path from 'effect/Path'
import * as Result from 'effect/Result'
import { Argument, Command, Flag } from 'effect/unstable/cli'
import * as CommandExit from './CommandExit.js'
import * as SourceEntry from './SourceEntry.js'
import * as Workflow from './Workflow.js'

const targetIds = Target.all.map((candidate) => candidate.id)
const profiles = ['debug', 'release', 'release-with-debug'] as const

const source = Argument.file('source', { mustExist: true }).pipe(
  Argument.withDescription('The root source file to compile.'),
)

const sourceRoot = Flag.string('source-root').pipe(
  Flag.withDescription('Source root used for canonical module resolution.'),
  Flag.optional,
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
  Flag.withDescription('Path to the Clang executable used for object emission and linking.'),
  Flag.withDefault('clang'),
)

const saveTemps = Flag.boolean('save-temps').pipe(
  Flag.withDescription('Keep the build scope intermediates for inspection.'),
)

const timings = Flag.boolean('timings').pipe(
  Flag.withDescription('Print the per-phase timing and memory report.'),
)

export interface Options {
  readonly source: string
  readonly sourceRoot: string | undefined
  readonly output: string
  readonly target: string | undefined
  readonly profile: ToolchainPlan.OptimizationProfile
  readonly clang: string
  readonly saveTemps: boolean
  readonly timings: boolean
}

/** Compiles an explicitly selected root source outside the project manifest workflow. */
export const run = Effect.fn('BuildExeCommand.run')(function* (
  options: Options,
): Effect.fn.Return<Workflow.ExitStatus, never, FileSystem.FileSystem | Path.Path> {
  const loaded = yield* Effect.result(SourceEntry.read(options.source, options.sourceRoot))
  if (Result.isFailure(loaded)) {
    yield* Console.error(loaded.failure.message)
    return 2
  }
  const attempted = yield* Workflow.compile({
    entry: loaded.success,
    ...(options.target === undefined ? {} : { target: options.target }),
    profile: options.profile,
    destination: options.output,
    toolchain: { _tag: 'Toolchain', clang: options.clang },
    scopeName: 'silk-build-exe',
    saveTemps: options.saveTemps,
    timings: options.timings,
  })
  return attempted.status
})

export const command = Command.make(
  'build-exe',
  { source, sourceRoot, output, target, profile, clang, saveTemps, timings },
  Effect.fnUntraced(function* (config) {
    const status = yield* run({
      source: config.source,
      sourceRoot: Option.getOrUndefined(config.sourceRoot),
      output: config.output,
      target: Option.getOrUndefined(config.target),
      profile: config.profile,
      clang: config.clang,
      saveTemps: config.saveTemps,
      timings: config.timings,
    })
    yield* CommandExit.complete(status)
  }),
).pipe(Command.withDescription('Build one rooted Silk source graph into a native executable.'))
