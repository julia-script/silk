import type * as NativeRequirementBinding from '@silklang/compiler/NativeRequirementBinding'
import type * as ArtifactPlan from '@silklang/compiler/ArtifactPlan'
import type * as ModuleClosure from '@silklang/compiler/ModuleClosure'
import * as Analysis from '@silklang/compiler/Analysis'
import type * as ArtifactKind from '@silklang/compiler/ArtifactKind'
import * as Diagnostic from '@silklang/compiler/Diagnostic'
import * as Driver from '@silklang/compiler/Driver'
import * as FileSourceResolver from '@silklang/compiler/FileSourceResolver'
import type * as HeapObservation from '@silklang/compiler/HeapObservation'
import * as NativeToolchain from '@silklang/compiler/NativeToolchain'
import type * as NativeLinkInput from '@silklang/compiler/NativeLinkInput'
import * as Project from '@silklang/compiler/Project'
import type * as SourceEntry from '@silklang/compiler/SourceEntry'
import * as SourceFile from '@silklang/compiler/SourceFile'
import type * as Target from '@silklang/compiler/Target'
import type * as ToolchainPlan from '@silklang/compiler/ToolchainPlan'
import * as Console from 'effect/Console'
import * as Effect from 'effect/Effect'
import * as Fiber from 'effect/Fiber'
import * as FileSystem from 'effect/FileSystem'
import * as Option from 'effect/Option'
import * as Path from 'effect/Path'
import * as Queue from 'effect/Queue'
import * as Ref from 'effect/Ref'
import * as Result from 'effect/Result'
import * as Stream from 'effect/Stream'
import type { ChildProcessSpawner } from 'effect/unstable/process'
import * as BuildBatch from './BuildBatch.js'
import * as BuildPlan from './BuildPlan.js'
import * as Program from './Program.js'
import type * as ProjectOptions from './ProjectOptions.js'
import * as Report from './Report.js'
import * as SourceSettlement from './SourceSettlement.js'

export type ExitStatus = 0 | 1 | 2

export type BuildAttempt =
  | {
      readonly _tag: 'Built'
      readonly status: 0
      readonly artifact: string
      readonly artifactKind: Driver.Compiled['artifactKind']
      readonly libraryInterface?: NativeToolchain.LibraryInterfaceArtifacts
    }
  | { readonly _tag: 'NotBuilt'; readonly status: 1 | 2 }

export interface ProjectSelection extends ProjectOptions.ProjectOptions {
  readonly workingDirectory?: string
  /** Test/application-edge toolchain injection; ordinary project commands intentionally omit it. */
  readonly clang?: string
  readonly llvmAr?: string
}

export interface CompileOptions {
  readonly nativeBindings?: ReadonlyArray<NativeRequirementBinding.NativeRequirementBinding>
  readonly stage?: ArtifactPlan.Stage
  readonly entry: SourceEntry.SourceEntry
  readonly target?: string
  readonly configuration?: ModuleClosure.CompilationRequest['configuration']
  readonly optimization?: ToolchainPlan.OptimizationProfile
  readonly artifactKind: ArtifactKind.ArtifactKind
  readonly packageName: string
  readonly destination: string
  readonly toolchain: NativeToolchain.Toolchain
  readonly nativeLinkInputs?: ReadonlyArray<NativeLinkInput.NativeLinkInput>
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
    ...(options.targets === undefined ? {} : { targets: options.targets }),
    ...(options.optimization === undefined ? {} : { optimization: options.optimization }),
    ...(options.profile === undefined ? {} : { profile: options.profile }),
    ...(options.profileInput === undefined ? {} : { profileInput: options.profileInput }),
    purpose,
    ...(options.clang === undefined ? {} : { clang: options.clang }),
    ...(options.llvmAr === undefined ? {} : { llvmAr: options.llvmAr }),
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
    case 'TargetFailed':
    case 'ToolchainFailed':
      return 2
  }
}

/** Compiles one already-materialized entry and classifies source versus operational failures. */
export const compile = Effect.fn('Workflow.compile')(function* (
  options: CompileOptions,
): Effect.fn.Return<
  BuildAttempt,
  never,
  FileSystem.FileSystem | Path.Path | HeapObservation.HeapObservation
> {
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
        ...(options.configuration === undefined && options.target !== undefined
          ? { target: options.target }
          : {}),
        ...(options.configuration === undefined ? {} : { configuration: options.configuration }),
      },
      toolchain: options.toolchain,
      artifactKind: options.artifactKind,
      ...(options.nativeBindings === undefined ? {} : { nativeBindings: options.nativeBindings }),
      ...(options.stage === undefined ? {} : { stage: options.stage }),
      packageName: options.packageName,
      ...(options.nativeLinkInputs === undefined
        ? {}
        : { nativeLinkInputs: options.nativeLinkInputs }),
      ...(options.configuration !== undefined || options.optimization === undefined
        ? {}
        : { optimization: options.optimization }),
      destination: options.destination,
      scopeName: options.scopeName,
      saveTemps: options.saveTemps ?? false,
    }).pipe(Effect.provide(FileSourceResolver.layer(resolver))),
  )

  if (Result.isFailure(attempted)) {
    const failure = attempted.failure
    if (failure._tag === 'ToolchainError') {
      yield* Console.error(Report.toolchainError(failure))
      return { _tag: 'NotBuilt', status: 2 }
    }
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
    if (outcome.linkPlanPath !== undefined) yield* Console.log(`Link plan: ${outcome.linkPlanPath}`)
    return {
      _tag: 'Built',
      status: 0,
      artifact: outcome.path,
      artifactKind: outcome.artifactKind,
      ...(outcome.libraryInterface === undefined
        ? {}
        : { libraryInterface: outcome.libraryInterface }),
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
  plan: BuildPlan.BuildPlan,
): Effect.fn.Return<CheckAttempt, never, FileSystem.FileSystem | Path.Path> {
  const target = plan.target
  const path = yield* Path.Path
  const resolver = FileSourceResolver.make(project.entry.sourceRoot)
  const analysis = yield* Analysis.makeRealized({
    root: SourceFile.make(project.entry.module, project.entry.bytes),
    configuration: BuildPlan.compilationConfiguration(plan),
  }).pipe(Effect.provide(FileSourceResolver.layer(resolver)))
  const catalog = Report.catalog(resolver, Analysis.sources(analysis), path)
  const renderedDiagnostics = Report.diagnostics(Analysis.diagnostics(analysis), catalog)
  if (renderedDiagnostics.length > 0) {
    yield* Console.error(`[${target.id}]\n${renderedDiagnostics}`)
  }
  const failures = Report.resolutionFailures(Analysis.resolutionFailures(analysis))
  if (failures.length > 0) yield* Console.error(`[${target.id}]\n${failures.join('\n')}`)
  let status: ExitStatus = 0
  if (failures.length > 0) status = 2
  else if (Diagnostic.hasErrors(Analysis.diagnostics(analysis))) status = 1
  return Object.freeze({ target, status })
})

const aggregateStatus = (statuses: ReadonlyArray<number>): ExitStatus => {
  if (statuses.some((status) => status === 2)) return 2
  if (statuses.some((status) => status === 1)) return 1
  return 0
}

/** Performs target-qualified analysis once per resolved selected target. */
export const checkProject = Effect.fn('Workflow.checkProject')(function* (
  project: Project.Project,
  options: ProjectSelection,
): Effect.fn.Return<ExitStatus, never, FileSystem.FileSystem | Path.Path> {
  const planned = yield* Effect.result(planBatch(project, options))
  if (Result.isFailure(planned)) return yield* reportPreparationFailure(planned.failure)
  const attempts = yield* Effect.forEach(
    planned.success.plans,
    (plan) => checkTarget(project, plan),
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

/** Loads a project and performs target-qualified analysis once per resolved selected target. */
export const check = Effect.fn('Workflow.check')(function* (
  options: ProjectSelection,
): Effect.fn.Return<ExitStatus, never, FileSystem.FileSystem | Path.Path> {
  const loaded = yield* Effect.result(loadProject(options))
  if (Result.isFailure(loaded)) return yield* reportPreparationFailure(loaded.failure)
  return yield* checkProject(loaded.success, options)
})

/** Preflights and builds every selected target from one loaded project snapshot. */
export const buildProject = Effect.fn('Workflow.buildProject')(function* (
  project: Project.Project,
  options: ProjectSelection,
): Effect.fn.Return<
  ExitStatus,
  never,
  FileSystem.FileSystem | HeapObservation.HeapObservation | Path.Path
> {
  const planned = yield* Effect.result(planBatch(project, options))
  if (Result.isFailure(planned)) return yield* reportPreparationFailure(planned.failure)
  const attempts = yield* Effect.forEach(
    planned.success.plans,
    (plan) =>
      compile({
        entry: plan.project.entry,
        target: plan.target.id,
        configuration: BuildPlan.compilationConfiguration(plan),
        ...(plan.stage === undefined ? {} : { stage: plan.stage }),
        artifactKind: plan.artifactKind,
        packageName: plan.project.name,
        destination: plan.destination,
        toolchain: plan.toolchain,
        nativeLinkInputs: plan.nativeLinkInputs,
        ...(plan.project.build.nativeBindings === undefined
          ? {}
          : { nativeBindings: plan.project.build.nativeBindings }),
        scopeName: `${plan.project.name}-llvm-${plan.target.id}`,
      }),
    { concurrency: 1 },
  )
  for (const [index, attempt] of attempts.entries()) {
    const plan = planned.success.plans[index]
    if (plan === undefined) continue
    yield* Console.log(
      attempt._tag === 'Built'
        ? `build llvm/${plan.target.id}: ok ${attempt.artifact}`
        : `build llvm/${plan.target.id}: failed (exit ${attempt.status})`,
    )
  }
  const succeeded = attempts.filter((attempt) => attempt.status === 0).length
  yield* Console.log(`Build summary: ${succeeded} succeeded, ${attempts.length - succeeded} failed`)
  return aggregateStatus(attempts.map((attempt) => attempt.status))
})

/** Loads, preflights, and builds every selected project target sequentially. */
export const build = Effect.fn('Workflow.build')(function* (
  options: ProjectSelection,
): Effect.fn.Return<
  ExitStatus,
  never,
  FileSystem.FileSystem | HeapObservation.HeapObservation | Path.Path
> {
  const loaded = yield* Effect.result(loadProject(options))
  if (Result.isFailure(loaded)) return yield* reportPreparationFailure(loaded.failure)
  return yield* buildProject(loaded.success, options)
})

/**
 * Collects the source root and every directory beneath it. `FileSystem.watch` reports only a
 * directory's own entries, so a nested module needs its own watch to be seen at all.
 */
const sourceDirectories = Effect.fnUntraced(function* (
  root: string,
): Effect.fn.Return<ReadonlyArray<string>, never, FileSystem.FileSystem | Path.Path> {
  const fileSystem = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  const found: Array<string> = []
  const pending: Array<string> = [root]
  while (pending.length > 0) {
    const directory = pending.pop()
    if (directory === undefined) continue
    found.push(directory)
    const names = yield* Effect.result(fileSystem.readDirectory(directory))
    if (Result.isFailure(names)) continue
    for (const name of names.success) {
      const candidate = path.resolve(directory, name)
      const info = yield* Effect.result(fileSystem.stat(candidate))
      if (Result.isSuccess(info) && info.success.type === 'Directory') pending.push(candidate)
    }
  }
  return found
})

/**
 * How long the event stream must fall quiet before a burst counts as one finished edit.
 *
 * A save is not one event. `writeFile` is `open(O_TRUNC)`, `write`, `close`, and the watch fires
 * on the truncate, so recompiling per raw event reads a file that is still being written — issue
 * #158 measured 71% of watch-woken reads observing zero bytes. Coalescing sub-millisecond bursts
 * needs only a few milliseconds; 50 ms also absorbs an editor that writes a backup or a swap file
 * beside the source, while staying far enough below the ~100 ms threshold where a feedback loop
 * starts to feel delayed. Two edits further apart than this window remain two compilations.
 */
const quietWindow = '50 millis'

/**
 * Gap between consecutive source-tree samples that decide whether a writer is still mid-flight.
 *
 * Quiet is not the same as finished: one large `write` is a single event that arrives when the
 * write completes, so a slow writer can leave the stream silent while the file on disk is
 * truncated. Comparing the tree against itself across this gap catches that case, and costs one
 * extra sample of the source tree per compilation.
 */
const settleInterval = '25 millis'

/** Bounds how long a stable empty candidate is treated as a possible in-progress truncation. */
const settleSamples = SourceSettlement.maximumObservations

/**
 * Fingerprints every file under the source root by path, size, and modification time.
 *
 * Three equal fingerprints across two `settleInterval` gaps mean no writer is mid-flight; a
 * fingerprint equal to the one already compiled means the burst left the tree byte-identical to
 * what the last pass read. Deliberately not a content hash: rewriting a file with its previous
 * contents is a real edit that must recompile, and a fingerprint records that the file was written
 * at all.
 */
const sourceFingerprint = Effect.fnUntraced(function* (
  root: string,
): Effect.fn.Return<string, never, FileSystem.FileSystem | Path.Path> {
  const fileSystem = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  const entries: Array<string> = []
  const pending: Array<string> = [root]
  while (pending.length > 0) {
    const directory = pending.pop()
    if (directory === undefined) continue
    const names = yield* Effect.result(fileSystem.readDirectory(directory))
    if (Result.isFailure(names)) continue
    for (const name of names.success) {
      const candidate = path.resolve(directory, name)
      const info = yield* Effect.result(fileSystem.stat(candidate))
      if (Result.isFailure(info)) continue
      if (info.success.type === 'Directory') {
        pending.push(candidate)
        continue
      }
      const modified = Option.match(info.success.mtime, {
        onNone: () => 'unknown',
        onSome: (at) => `${at.getTime()}`,
      })
      entries.push(`${candidate}\u0000${info.success.size}\u0000${modified}`)
    }
  }
  return entries.sort().join('\n')
})

interface ProjectSnapshot {
  readonly project: Project.Project
  readonly fingerprint: string
}

/**
 * Loads one project snapshot bracketed by equal source-tree observations. The returned
 * fingerprint therefore describes the same tree from which the materialized entry was read.
 */
const projectSnapshot = Effect.fnUntraced(function* (
  options: ProjectSelection,
  root: string,
): Effect.fn.Return<ProjectSnapshot, Project.ProjectError, FileSystem.FileSystem | Path.Path> {
  while (true) {
    const before = yield* sourceFingerprint(root)
    const project = yield* loadProject(options)
    const after = yield* sourceFingerprint(root)
    if (before === after) return { project, fingerprint: after }
    yield* Effect.sleep(settleInterval)
  }
})

/**
 * Samples a materialized project until its exact fingerprint settles. A transition from a
 * compiled nonempty entry to an empty candidate is held for the existing one-second budget: that
 * is the observable state left by a writer paused after truncation. At the budget boundary the
 * empty candidate is accepted, so an intentional empty edit is delayed but never suppressed.
 */
const settledProject = Effect.fnUntraced(function* (
  compiled: ProjectSnapshot,
  options: ProjectSelection,
  root: string,
): Effect.fn.Return<ProjectSnapshot, Project.ProjectError, FileSystem.FileSystem | Path.Path> {
  let candidate = yield* projectSnapshot(options, root)
  let settlement = SourceSettlement.fromEntryTransition(
    candidate.fingerprint,
    compiled.project.entry.bytes.length,
    candidate.project.entry.bytes.length,
  )
  for (let sample = 0; sample < settleSamples; sample += 1) {
    yield* Effect.sleep(settleInterval)
    const sampled = yield* projectSnapshot(options, root)
    const observed = SourceSettlement.observe(settlement, sampled.fingerprint)
    if (observed._tag === 'Settled') return candidate
    if (observed._tag === 'Changed') {
      candidate = sampled
      settlement = SourceSettlement.fromEntryTransition(
        sampled.fingerprint,
        compiled.project.entry.bytes.length,
        sampled.project.entry.bytes.length,
      )
      continue
    }
    settlement = observed.settlement
  }
  return candidate
})

/**
 * Runs one compilation, then repeats it after every change under the project source root. The
 * status of each pass is reported but never returned: only stopping the watch ends the command,
 * so a pass that reports diagnostics leaves the watch running.
 *
 * A pass reads the source tree only once the tree has settled, so an ordinary non-atomic save is
 * compiled whole rather than at the zero length the truncate left behind, and one edit produces
 * one pass rather than one per raw event.
 */
export const watch = Effect.fn('Workflow.watch')(function* <R>(
  run: (project: Project.Project, options: ProjectSelection) => Effect.Effect<ExitStatus, never, R>,
  options: ProjectSelection,
): Effect.fn.Return<ExitStatus, never, FileSystem.FileSystem | Path.Path | R> {
  const fileSystem = yield* FileSystem.FileSystem
  const loaded = yield* Effect.result(loadProject(options))
  if (Result.isFailure(loaded)) return yield* reportPreparationFailure(loaded.failure)
  const sourceRoot = loaded.success.entry.sourceRoot
  // ponytail: directories are enumerated once; a directory created later needs a restart.
  const directories = yield* sourceDirectories(sourceRoot)
  const changes = yield* Queue.unbounded<void>()
  const watcher = yield* Effect.forkChild(
    Stream.mergeAll(
      directories.map((directory) => fileSystem.watch(directory)),
      { concurrency: 'unbounded' },
    ).pipe(
      Stream.debounce(quietWindow),
      Stream.runForEach(
        Effect.fnUntraced(function* () {
          yield* Queue.offer(changes, undefined)
        }),
      ),
      Effect.catchCause(() => Console.error(`Stopped watching ${sourceRoot}`)),
    ),
    { startImmediately: true },
  )

  const initial = yield* Effect.result(projectSnapshot(options, sourceRoot))
  if (Result.isFailure(initial)) return yield* reportPreparationFailure(initial.failure)
  const compiled = yield* Ref.make(initial.success)
  yield* run(initial.success.project, options)

  const compileChanged = Effect.fnUntraced(function* () {
    const previous = yield* Ref.get(compiled)
    const next = yield* Effect.result(settledProject(previous, options, sourceRoot))
    if (Result.isFailure(next)) {
      yield* reportPreparationFailure(next.failure)
      return
    }
    if (next.success.fingerprint === previous.fingerprint) return
    yield* Ref.set(compiled, next.success)
    yield* run(next.success.project, options)
    yield* Console.log(`Watching ${sourceRoot} for changes.`)
  })

  // Reconcile once before waiting for events. This closes the startup window even on watch
  // backends that do not replay an edit which landed while the initial pass was running.
  yield* compileChanged()
  yield* Console.log(`Watching ${sourceRoot} for changes.`)
  const consume = Effect.forever(Queue.take(changes).pipe(Effect.andThen(compileChanged())))
  yield* Effect.raceFirst(Fiber.join(watcher), consume)
  return 0
})

/**
 * Removes the manifest output directory. Only the build writes there — `Project.load` rejects an
 * `output-dir` that escapes the project — so no source file is reachable from this removal.
 */
export const clean = Effect.fn('Workflow.clean')(function* (
  options: ProjectSelection,
): Effect.fn.Return<ExitStatus, never, FileSystem.FileSystem | Path.Path> {
  const fileSystem = yield* FileSystem.FileSystem
  const loaded = yield* Effect.result(loadProject(options))
  if (Result.isFailure(loaded)) return yield* reportPreparationFailure(loaded.failure)
  const outputDirectory = loaded.success.build.outputDirectory
  const removed = yield* Effect.result(
    fileSystem.remove(outputDirectory, { recursive: true, force: true }),
  )
  if (Result.isFailure(removed)) {
    yield* Console.error(`Cannot remove build directory ${outputDirectory}`)
    return 2
  }
  yield* Console.log(`Removed ${outputDirectory}`)
  return 0
})

/** Builds exactly the host target and preserves program exit status. */
export const run = Effect.fn('Workflow.run')(function* (
  options: ProjectSelection,
  arguments_: ReadonlyArray<string> = [],
): Effect.fn.Return<
  number,
  never,
  | ChildProcessSpawner.ChildProcessSpawner
  | FileSystem.FileSystem
  | HeapObservation.HeapObservation
  | Path.Path
> {
  const loaded = yield* Effect.result(loadProject(options))
  if (Result.isFailure(loaded)) return yield* reportPreparationFailure(loaded.failure)
  const planned = yield* Effect.result(planBatch(loaded.success, options, 'run'))
  if (Result.isFailure(planned)) return yield* reportPreparationFailure(planned.failure)
  const plan = planned.success.plans[0]
  const attempted = yield* compile({
    entry: plan.project.entry,
    target: plan.target.id,
    configuration: BuildPlan.compilationConfiguration(plan),
    ...(plan.stage === undefined ? {} : { stage: plan.stage }),
    artifactKind: plan.artifactKind,
    packageName: plan.project.name,
    destination: plan.destination,
    toolchain: plan.toolchain,
    nativeLinkInputs: plan.nativeLinkInputs,
    ...(plan.project.build.nativeBindings === undefined
      ? {}
      : { nativeBindings: plan.project.build.nativeBindings }),
    scopeName: plan.project.name,
  })
  if (attempted._tag === 'NotBuilt') return attempted.status
  if (attempted.artifactKind !== 'NativeExecutable') {
    yield* Console.error('The compiler did not produce a runnable executable')
    return 2
  }
  const executed = yield* Effect.result(Program.run(attempted.artifact, arguments_))
  if (Result.isFailure(executed)) {
    yield* Console.error(executed.failure.message)
    return 2
  }
  return executed.success
})
