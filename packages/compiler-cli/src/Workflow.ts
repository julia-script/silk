import * as Analysis from '@silk-effect/compiler/Analysis'
import type * as Backend from '@silk-effect/compiler/Backend'
import * as Diagnostic from '@silk-effect/compiler/Diagnostic'
import * as Driver from '@silk-effect/compiler/Driver'
import type * as NativeToolchain from '@silk-effect/compiler/NativeToolchain'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import type * as Target from '@silk-effect/compiler/Target'
import type * as ToolchainPlan from '@silk-effect/compiler/ToolchainPlan'
import * as Console from 'effect/Console'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Path from 'effect/Path'
import * as Result from 'effect/Result'
import type { ChildProcessSpawner } from 'effect/unstable/process'
import * as BuildBatch from './BuildBatch.js'
import type * as BuildPlan from './BuildPlan.js'
import * as FileSourceResolver from './FileSourceResolver.js'
import * as Program from './Program.js'
import * as Project from './Project.js'
import type * as ProjectOptions from './ProjectOptions.js'
import * as Report from './Report.js'
import type * as SourceEntry from './SourceEntry.js'
import * as TargetSelector from './TargetSelector.js'

export type ExitStatus = 0 | 1 | 2

export type BuildAttempt =
  | {
      readonly _tag: 'Built'
      readonly status: 0
      readonly artifact: string
      readonly artifactKind: Driver.Compiled['artifactKind']
    }
  | { readonly _tag: 'NotBuilt'; readonly status: 1 | 2 }

export interface ProjectSelection extends ProjectOptions.ProjectOptions {
  readonly workingDirectory?: string
  /** Test/application-edge toolchain injection; ordinary project commands intentionally omit it. */
  readonly clang?: string
}

export interface CompileOptions {
  readonly entry: SourceEntry.SourceEntry
  readonly backend: Backend.Backend
  readonly target?: string
  readonly profile: ToolchainPlan.OptimizationProfile
  readonly destination: string
  readonly toolchain: NativeToolchain.Toolchain
  readonly scopeName: string
  readonly saveTemps?: boolean
  readonly timings?: boolean
}

const loadProject = Effect.fnUntraced(function* (options: ProjectSelection) {
  return yield* Project.load({
    ...(options.workingDirectory === undefined
      ? {}
      : { workingDirectory: options.workingDirectory }),
    ...(options.manifestPath === undefined ? {} : { manifestPath: options.manifestPath }),
  })
})

const planBatch = (
  project: Project.Project,
  options: ProjectSelection,
  purpose: BuildPlan.Purpose = 'build',
) =>
  BuildBatch.make(project, {
    ...(options.backend === undefined ? {} : { backend: options.backend }),
    ...(options.targets === undefined ? {} : { targets: options.targets }),
    profile: options.profile,
    purpose,
    ...(options.clang === undefined ? {} : { clang: options.clang }),
  })

const reportPreparationFailure = Effect.fnUntraced(function* (error: { readonly message: string }) {
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
    case 'BackendFailed':
      return 1
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
      backend: options.backend,
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
    return {
      _tag: 'Built',
      status: 0,
      artifact: outcome.path,
      artifactKind: outcome.artifactKind,
    }
  }
  return { _tag: 'NotBuilt', status: outcomeStatus(outcome) }
})

interface CheckAttempt {
  readonly target: Target.Target
  readonly status: ExitStatus
}

const checkTarget = Effect.fnUntraced(function* (
  project: Project.Project,
  target: Target.Target,
): Effect.fn.Return<CheckAttempt, never, FileSystem.FileSystem | Path.Path> {
  const path = yield* Path.Path
  const resolver = FileSourceResolver.make(project.entry.sourceRoot)
  const analysis = yield* Analysis.makeRealized({
    root: SourceFile.make(project.entry.module, project.entry.bytes),
    target: target.id,
  }).pipe(Effect.provide(FileSourceResolver.layer(resolver)))
  const catalog = Report.catalog(resolver, Analysis.sources(analysis), path)
  const renderedDiagnostics = Report.diagnostics(Analysis.diagnostics(analysis), catalog)
  if (renderedDiagnostics.length > 0) {
    yield* Console.error(`[${target.id}]\n${renderedDiagnostics}`)
  }
  const failures = Report.resolutionFailures(Analysis.resolutionFailures(analysis))
  if (failures.length > 0) yield* Console.error(`[${target.id}]\n${failures.join('\n')}`)
  const status: ExitStatus =
    failures.length > 0 ? 2 : Diagnostic.hasErrors(Analysis.diagnostics(analysis)) ? 1 : 0
  return Object.freeze({ target, status })
})

const aggregateStatus = (statuses: ReadonlyArray<number>): ExitStatus =>
  statuses.some((status) => status === 2) ? 2 : statuses.some((status) => status === 1) ? 1 : 0

/** Loads a project and performs target-qualified analysis once per resolved selected target. */
export const check = Effect.fn('Workflow.check')(function* (
  options: ProjectSelection,
): Effect.fn.Return<ExitStatus, never, FileSystem.FileSystem | Path.Path> {
  const loaded = yield* Effect.result(loadProject(options))
  if (Result.isFailure(loaded)) return yield* reportPreparationFailure(loaded.failure)
  const selectors = options.targets ?? loaded.success.build.targets
  const resolved = TargetSelector.resolveAll(selectors)
  if (Result.isFailure(resolved)) return yield* reportPreparationFailure(resolved.failure)
  const attempts = yield* Effect.forEach(
    resolved.success,
    (target) => checkTarget(loaded.success, target),
    { concurrency: 1 },
  )
  for (const attempt of attempts) {
    yield* Console.log(
      `check ${attempt.target.id}: ${attempt.status === 0 ? 'ok' : `failed (exit ${attempt.status})`}`,
    )
  }
  const succeeded = attempts.filter((attempt) => attempt.status === 0).length
  yield* Console.log(`Check summary: ${succeeded} succeeded, ${attempts.length - succeeded} failed`)
  return aggregateStatus(attempts.map((attempt) => attempt.status))
})

/** Preflights and builds every selected project target sequentially. */
export const build = Effect.fn('Workflow.build')(function* (
  options: ProjectSelection,
): Effect.fn.Return<ExitStatus, never, FileSystem.FileSystem | Path.Path> {
  const loaded = yield* Effect.result(loadProject(options))
  if (Result.isFailure(loaded)) return yield* reportPreparationFailure(loaded.failure)
  const planned = planBatch(loaded.success, options)
  if (Result.isFailure(planned)) return yield* reportPreparationFailure(planned.failure)
  const attempts = yield* Effect.forEach(
    planned.success.plans,
    (plan) =>
      compile({
        entry: plan.project.entry,
        backend: plan.backend,
        target: plan.target.id,
        profile: plan.profile,
        destination: plan.destination,
        toolchain: plan.toolchain,
        scopeName: `${plan.project.name}-${plan.backend.id}-${plan.target.id}`,
      }),
    { concurrency: 1 },
  )
  for (const [index, attempt] of attempts.entries()) {
    const plan = planned.success.plans[index]
    if (plan === undefined) continue
    yield* Console.log(
      attempt._tag === 'Built'
        ? `build ${plan.backend.id}/${plan.target.id}: ok ${attempt.artifact}`
        : `build ${plan.backend.id}/${plan.target.id}: failed (exit ${attempt.status})`,
    )
  }
  const succeeded = attempts.filter((attempt) => attempt.status === 0).length
  yield* Console.log(`Build summary: ${succeeded} succeeded, ${attempts.length - succeeded} failed`)
  return aggregateStatus(attempts.map((attempt) => attempt.status))
})

/** Builds exactly the host target through a runnable backend and preserves program exit status. */
export const run = Effect.fn('Workflow.run')(function* (
  options: ProjectSelection,
  arguments_: ReadonlyArray<string> = [],
): Effect.fn.Return<
  number,
  never,
  FileSystem.FileSystem | Path.Path | ChildProcessSpawner.ChildProcessSpawner
> {
  const loaded = yield* Effect.result(loadProject(options))
  if (Result.isFailure(loaded)) return yield* reportPreparationFailure(loaded.failure)
  const planned = BuildBatch.make(loaded.success, {
    ...(options.backend === undefined ? {} : { backend: options.backend }),
    targets: ['host'],
    profile: options.profile,
    purpose: 'run',
  })
  if (Result.isFailure(planned)) return yield* reportPreparationFailure(planned.failure)
  const plan = planned.success.plans[0]
  const attempted = yield* compile({
    entry: plan.project.entry,
    backend: plan.backend,
    target: plan.target.id,
    profile: plan.profile,
    destination: plan.destination,
    toolchain: plan.toolchain,
    scopeName: plan.project.name,
  })
  if (attempted._tag === 'NotBuilt') return attempted.status
  if (attempted.artifactKind !== 'NativeExecutable') {
    yield* Console.error(`Backend ${plan.backend.id} did not produce a runnable executable`)
    return 2
  }
  const executed = yield* Effect.result(Program.run(attempted.artifact, arguments_))
  if (Result.isFailure(executed)) {
    yield* Console.error(executed.failure.message)
    return 2
  }
  return executed.success
})
