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
import * as Option from 'effect/Option'
import * as Path from 'effect/Path'
import * as Ref from 'effect/Ref'
import * as Result from 'effect/Result'
import * as Stream from 'effect/Stream'
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
 * Gap between the two source-tree samples that decide whether a writer is still mid-flight.
 *
 * Quiet is not the same as finished: one large `write` is a single event that arrives when the
 * write completes, so a slow writer can leave the stream silent while the file on disk is
 * truncated. Comparing the tree against itself across this gap catches that case, and costs one
 * extra sample of the source tree per compilation.
 */
const settleInterval = '25 millis'

/** Caps the settle wait at one second so a continuously rewritten tree still compiles. */
const settleSamples = 40

/**
 * Fingerprints every file under the source root by path, size, and modification time.
 *
 * Two equal fingerprints taken `settleInterval` apart mean no writer is mid-flight; a fingerprint
 * equal to the one already compiled means the burst left the tree byte-identical to what the last
 * pass read. Deliberately not a content hash: rewriting a file with its previous contents is a
 * real edit that must recompile, and a fingerprint records that the file was written at all.
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
      entries.push(`${candidate} ${info.success.size} ${modified}`)
    }
  }
  return entries.sort().join('\n')
})

/** Samples the source tree until two consecutive samples agree, or the settle budget runs out. */
const settledFingerprint = Effect.fnUntraced(function* (
  root: string,
): Effect.fn.Return<string, never, FileSystem.FileSystem | Path.Path> {
  let previous = yield* sourceFingerprint(root)
  for (let sample = 0; sample < settleSamples; sample += 1) {
    yield* Effect.sleep(settleInterval)
    const current = yield* sourceFingerprint(root)
    if (current === previous) return current
    previous = current
  }
  return previous
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
export const watch = Effect.fn('Workflow.watch')(function* (
  run: (
    options: ProjectSelection,
  ) => Effect.Effect<ExitStatus, never, FileSystem.FileSystem | Path.Path>,
  options: ProjectSelection,
): Effect.fn.Return<ExitStatus, never, FileSystem.FileSystem | Path.Path> {
  const fileSystem = yield* FileSystem.FileSystem
  const loaded = yield* Effect.result(loadProject(options))
  if (Result.isFailure(loaded)) return yield* reportPreparationFailure(loaded.failure)
  const sourceRoot = loaded.success.entry.sourceRoot
  yield* run(options)
  // ponytail: directories are enumerated once; a directory created later needs a restart.
  const directories = yield* sourceDirectories(sourceRoot)
  const compiled = yield* Ref.make(yield* sourceFingerprint(sourceRoot))
  yield* Console.log(`Watching ${sourceRoot} for changes.`)
  yield* Stream.mergeAll(
    directories.map((directory) => fileSystem.watch(directory)),
    { concurrency: 'unbounded' },
  ).pipe(
    Stream.debounce(quietWindow),
    Stream.runForEach(
      Effect.fnUntraced(function* () {
        const fingerprint = yield* settledFingerprint(sourceRoot)
        // The burst carried no edit the last pass did not already read: the events were the
        // truncate and the write of one save, or a file was rewritten with the same bytes at the
        // same time. Recompiling would only repeat the result already on screen.
        if (fingerprint === (yield* Ref.get(compiled))) return
        yield* Ref.set(compiled, fingerprint)
        yield* run(options)
        yield* Console.log(`Watching ${sourceRoot} for changes.`)
      }),
    ),
    Effect.catchCause(() => Console.error(`Stopped watching ${sourceRoot}`)),
  )
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
