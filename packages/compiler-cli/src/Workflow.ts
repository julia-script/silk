import * as Analysis from '@silk-effect/compiler/Analysis'
import * as Diagnostic from '@silk-effect/compiler/Diagnostic'
import * as Driver from '@silk-effect/compiler/Driver'
import type * as NativeToolchain from '@silk-effect/compiler/NativeToolchain'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import type * as ToolchainPlan from '@silk-effect/compiler/ToolchainPlan'
import * as Console from 'effect/Console'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Path from 'effect/Path'
import * as Result from 'effect/Result'
import type { ChildProcessSpawner } from 'effect/unstable/process'
import * as BuildPlan from './BuildPlan.js'
import * as FileSourceResolver from './FileSourceResolver.js'
import * as Program from './Program.js'
import * as Project from './Project.js'
import type * as ProjectOptions from './ProjectOptions.js'
import * as Report from './Report.js'
import type * as SourceEntry from './SourceEntry.js'

export type ExitStatus = 0 | 1 | 2

export type BuildAttempt =
  | { readonly _tag: 'Built'; readonly status: 0; readonly executable: string }
  | { readonly _tag: 'NotBuilt'; readonly status: 1 | 2 }

export interface ProjectSelection extends ProjectOptions.ProjectOptions {
  readonly workingDirectory?: string
}

export interface CompileOptions {
  readonly entry: SourceEntry.SourceEntry
  readonly target?: string
  readonly profile: ToolchainPlan.OptimizationProfile
  readonly destination: string
  readonly toolchain: NativeToolchain.Toolchain
  readonly scopeName: string
  readonly saveTemps?: boolean
  readonly timings?: boolean
}

const prepare = Effect.fnUntraced(function* (
  options: ProjectSelection,
  purpose: BuildPlan.Purpose,
) {
  const project = yield* Project.load({
    ...(options.workingDirectory === undefined
      ? {}
      : { workingDirectory: options.workingDirectory }),
    ...(options.manifestPath === undefined ? {} : { manifestPath: options.manifestPath }),
  })
  const planned = BuildPlan.make(project, {
    ...(options.target === undefined ? {} : { target: options.target }),
    profile: options.profile,
    purpose,
  })
  if (Result.isFailure(planned)) return yield* planned.failure
  return planned.success
})

const reportPreparationFailure = Effect.fnUntraced(function* (
  error: Project.ProjectError | BuildPlan.BuildPlanError,
) {
  yield* Console.error(error.message)
  return 2 as const
})

const loadedSources = (
  outcome: Driver.Outcome,
  entry: SourceEntry.SourceEntry,
): ReadonlyMap<string, SourceFile.SourceFile> =>
  outcome._tag === 'Rejected'
    ? outcome.sources
    : new Map([[entry.module, SourceFile.make(entry.module, entry.bytes)]])

const outcomeStatus = (outcome: Exclude<Driver.Outcome, { readonly _tag: 'Compiled' }>): 1 | 2 => {
  switch (outcome._tag) {
    case 'NoEntry':
    case 'Rejected':
      return 1
    case 'BackendFailed':
    case 'Failed':
    case 'TargetFailed':
      return 2
  }
}

/** Compiles one already-materialized entry and classifies source versus operational failures. */
export const compile = Effect.fn('Workflow.compile')(function* (
  options: CompileOptions,
): Effect.fn.Return<BuildAttempt, never, FileSystem.FileSystem | Path.Path> {
  const fileSystem = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  const resolver = FileSourceResolver.make(options.entry.sourceRoot)
  const prepared = yield* Effect.result(
    fileSystem.makeDirectory(path.dirname(options.destination), { recursive: true }),
  )
  if (Result.isFailure(prepared)) {
    yield* Console.error(`Cannot prepare build directory for ${options.destination}`)
    return { _tag: 'NotBuilt', status: 2 }
  }

  const attempted = yield* Effect.result(
    Driver.compile({
      compilation: {
        root: SourceFile.make(options.entry.module, options.entry.bytes),
        ...(options.target === undefined ? {} : { target: options.target }),
      },
      toolchain: options.toolchain,
      profile: options.profile,
      destination: options.destination,
      scopeName: options.scopeName,
      saveTemps: options.saveTemps ?? false,
    }).pipe(Effect.provide(FileSourceResolver.layer(resolver))),
  )

  if (Result.isFailure(attempted)) {
    const failure = attempted.failure
    const summary = Report.sourceResolutionFailed(
      failure,
      Report.catalog(resolver, failure.sources, path),
    )
    if (summary.length > 0) yield* Console.error(summary)
    if (options.timings === true) {
      yield* Console.log('Phases:')
      yield* Console.log(Report.phases(failure.report))
    }
    return { _tag: 'NotBuilt', status: 2 }
  }

  const outcome = attempted.success
  const summary = Report.outcome(
    outcome,
    Report.catalog(resolver, loadedSources(outcome, options.entry), path),
    options.entry.path,
  )
  if (summary.length > 0) {
    if (outcome._tag === 'Compiled') yield* Console.log(summary)
    else yield* Console.error(summary)
  }
  if (options.timings === true) {
    yield* Console.log('Phases:')
    yield* Console.log(Report.phases(outcome.report))
  }

  if (outcome._tag === 'Compiled') {
    return { _tag: 'Built', status: 0, executable: outcome.executable }
  }
  const status = outcomeStatus(outcome)
  return { _tag: 'NotBuilt', status }
})

/** Loads a project and performs resilient whole-graph analysis without producing build artifacts. */
export const check = Effect.fn('Workflow.check')(function* (
  options: ProjectSelection,
): Effect.fn.Return<ExitStatus, never, FileSystem.FileSystem | Path.Path> {
  const prepared = yield* Effect.result(prepare(options, 'build'))
  if (Result.isFailure(prepared)) return yield* reportPreparationFailure(prepared.failure)

  const plan = prepared.success
  const path = yield* Path.Path
  const resolver = FileSourceResolver.make(plan.project.entry.sourceRoot)
  const analysis = yield* Analysis.make({
    root: SourceFile.make(plan.project.entry.module, plan.project.entry.bytes),
    target: plan.target.id,
  }).pipe(Effect.provide(FileSourceResolver.layer(resolver)))
  const catalog = Report.catalog(resolver, Analysis.sources(analysis), path)
  const renderedDiagnostics = Report.diagnostics(Analysis.diagnostics(analysis), catalog)
  if (renderedDiagnostics.length > 0) yield* Console.error(renderedDiagnostics)
  const failures = Report.resolutionFailures(Analysis.resolutionFailures(analysis))
  if (failures.length > 0) yield* Console.error(failures.join('\n'))

  if (failures.length > 0) return 2
  return Diagnostic.hasErrors(Analysis.diagnostics(analysis)) ? 1 : 0
})

/** Loads and builds a project into its deterministic target/profile artifact path. */
export const build = Effect.fn('Workflow.build')(function* (
  options: ProjectSelection,
): Effect.fn.Return<ExitStatus, never, FileSystem.FileSystem | Path.Path> {
  const prepared = yield* Effect.result(prepare(options, 'build'))
  if (Result.isFailure(prepared)) return yield* reportPreparationFailure(prepared.failure)
  const plan = prepared.success
  const attempted = yield* compile({
    entry: plan.project.entry,
    target: plan.target.id,
    profile: plan.profile,
    destination: plan.destination,
    toolchain: plan.toolchain,
    scopeName: plan.project.name,
  })
  return attempted.status
})

/** Builds a host project and runs it, forwarding arguments and the program's exact exit status. */
export const run = Effect.fn('Workflow.run')(function* (
  options: ProjectSelection,
  arguments_: ReadonlyArray<string> = [],
): Effect.fn.Return<
  number,
  never,
  FileSystem.FileSystem | Path.Path | ChildProcessSpawner.ChildProcessSpawner
> {
  const prepared = yield* Effect.result(prepare(options, 'run'))
  if (Result.isFailure(prepared)) return yield* reportPreparationFailure(prepared.failure)
  const plan = prepared.success
  const attempted = yield* compile({
    entry: plan.project.entry,
    target: plan.target.id,
    profile: plan.profile,
    destination: plan.destination,
    toolchain: plan.toolchain,
    scopeName: plan.project.name,
  })
  if (attempted._tag === 'NotBuilt') return attempted.status

  const executed = yield* Effect.result(Program.run(attempted.executable, arguments_))
  if (Result.isFailure(executed)) {
    yield* Console.error(executed.failure.message)
    return 2
  }
  return executed.success
})
