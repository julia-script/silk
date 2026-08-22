import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as Backend from './Backend.js'
import type * as Diagnostic from './Diagnostic.js'
import * as Frontend from './Frontend.js'
import * as HeapObservation from './HeapObservation.js'
import type * as Instances from './Instances.js'
import * as IntrinsicAvailability from './IntrinsicAvailability.js'
import * as LlvmBackend from './LlvmBackend.js'
import type * as ModuleClosure from './ModuleClosure.js'
import * as NativeToolchain from './NativeToolchain.js'
import * as PhaseReport from './PhaseReport.js'
import * as Realization from './Realization.js'
import * as SourceFile from './SourceFile.js'
import * as SourceResolver from './SourceResolver.js'
import * as Target from './Target.js'
import * as ToolchainIntegrity from './ToolchainIntegrity.js'
import * as ToolchainPlan from './ToolchainPlan.js'
import type * as Type from './Type.js'

/**
 * The end-to-end compiler driver: one orchestration path from a compilation request to a durable
 * native executable or WebAssembly module. The driver invokes backend and finalizer boundaries itself — no external
 * harness performs a stage. Outcomes are closed data naming failing stages with provenance, and
 * every run carries a per-phase report: elapsed time, input and output counts, diagnostic
 * counts, and engine-heap memory totals (the bootstrap approximation of allocator totals).
 */

/** One phase's observability entry. Reports are data, not artifacts — exempt from byte-identity. */
export interface DriverPhaseReport extends Omit<PhaseReport.PhaseReport, 'heapBytes'> {
  readonly heapBytes: number
}

const phaseWithHeap = (entry: PhaseReport.PhaseReport): DriverPhaseReport => {
  if (entry.heapBytes === undefined)
    throw new RangeError(`Driver phase ${entry.phase} lost its engine heap observation`)
  return Object.freeze({ ...entry, heapBytes: entry.heapBytes })
}

/** One driver request. */
export interface CompileRequest {
  readonly compilation: ModuleClosure.CompilationRequest
  readonly toolchain: NativeToolchain.Toolchain
  readonly profile: ToolchainPlan.OptimizationProfile
  readonly destination: string
  readonly backend?: Backend.Backend
  readonly scopeName?: string
  readonly saveTemps?: boolean
  /** Set false to bypass the content-addressed artifact cache for this request. */
  readonly cache?: boolean
  /** Explicit distribution metadata for embeddings and integrity tests; defaults to this build. */
  readonly distribution?: ToolchainIntegrity.Graph
}

/** A completed compilation with its durable artifact identity and report. */
export interface Compiled {
  readonly _tag: 'Compiled'
  readonly backend: Backend.Id
  readonly artifactKind: NativeToolchain.FinalArtifact['kind']
  readonly path: string
  readonly target: Target.Target
  readonly symbols: ReadonlyArray<Backend.SymbolEntry>
  readonly termination: Backend.Termination
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<DriverPhaseReport>
  readonly toolchainIdentity: string
}

/** The request's root module has no valid entry; the toolchain was never invoked. */
export interface NoEntry {
  readonly _tag: 'NoEntry'
  readonly reason: Extract<Instances.Entry, { readonly _tag: 'Unavailable' }>['reason']
  readonly requirements?: ReadonlyArray<Type.Requirement>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<DriverPhaseReport>
}

/** Target selection stopped compilation before MIR lowering. */
export interface TargetFailed {
  readonly _tag: 'TargetFailed'
  readonly error: Target.TargetError
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<DriverPhaseReport>
}

/** Shared MIR validation, compatibility, or backend construction stopped emission. */
export interface BackendFailed {
  readonly _tag: 'BackendFailed'
  readonly error: Backend.BackendError
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<DriverPhaseReport>
}

/** A missing, malformed, or mismatched compiler distribution stopped compilation. */
export interface ToolchainFailed {
  readonly _tag: 'ToolchainFailed'
  readonly expectedIdentity: string
  readonly observedIdentity: string
  readonly failures: ReadonlyArray<ToolchainIntegrity.IntegrityFailure>
  readonly report: ReadonlyArray<DriverPhaseReport>
}

/** Source diagnostics rejected artifact production after the recoverable frontend completed. */
export interface Rejected {
  readonly _tag: 'Rejected'
  readonly sources: ReadonlyMap<string, SourceFile.SourceFile>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<DriverPhaseReport>
}

/** Imported source storage failed operationally after retaining the available frontend facts. */
export class SourceResolutionFailed extends Data.TaggedError('SourceResolutionFailed')<{
  readonly operation: 'Driver.compile'
  readonly message: string
  readonly failures: ReadonlyArray<SourceResolver.SourceResolverError>
  readonly sources: ReadonlyMap<string, SourceFile.SourceFile>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<DriverPhaseReport>
}> {}

/** The closed outcome of one driver run. */
export type Outcome = Compiled | Rejected | NoEntry | TargetFailed | BackendFailed | ToolchainFailed

/** Compiles one request end to end, writing its final artifact to the durable destination. */
export const compile = Effect.fn('Driver.compile')(function* (
  request: CompileRequest,
): Effect.fn.Return<
  Outcome,
  SourceResolutionFailed | NativeToolchain.ToolchainError,
  SourceResolver.SourceResolver | HeapObservation.HeapObservation
> {
  const report: Array<DriverPhaseReport> = []
  const heapObservation = yield* HeapObservation.HeapObservation
  const heapBytes = heapObservation.heapBytes
  const distribution = request.distribution ?? ToolchainIntegrity.installed()

  const frontendIntegrity = yield* PhaseReport.measureEffectInto(
    report,
    'toolchain-integrity',
    distribution.components.length,
    SourceResolver.toolchainSources().pipe(
      Effect.map((sources) =>
        ToolchainIntegrity.validateFrontend(
          distribution,
          new Map([...sources].map(([module, source]) => [module, source.bytes] as const)),
        ),
      ),
      Effect.catchTag('SourceResolverError', (error) =>
        Effect.succeed(
          Object.freeze({
            _tag: 'Invalid' as const,
            failures: Object.freeze([
              ToolchainIntegrity.unreadableSource(error.module, error.message),
            ]),
          }),
        ),
      ),
    ),
    (result) => (result._tag === 'Matched' ? distribution.components.length : 0),
    (result) => (result._tag === 'Invalid' ? result.failures.length : 0),
    { heapBytes },
  )
  if (frontendIntegrity._tag === 'Invalid')
    return Object.freeze({
      _tag: 'ToolchainFailed',
      expectedIdentity: ToolchainIntegrity.installed().digest,
      observedIdentity: distribution.digest,
      failures: frontendIntegrity.failures,
      report: Object.freeze([...report]),
    })

  const frontend = yield* Frontend.frontend(request.compilation, { heapBytes })
  report.push(...frontend.report.map(phaseWithHeap))
  const closure = frontend.closure
  if (closure.resolutionFailures.length > 0) {
    return yield* new SourceResolutionFailed({
      operation: 'Driver.compile',
      message: `Source resolution failed for ${closure.resolutionFailures.length} imported module${closure.resolutionFailures.length === 1 ? '' : 's'}`,
      failures: closure.resolutionFailures,
      sources: closure.sources,
      diagnostics: frontend.diagnostics,
      report: Object.freeze([...report]),
    })
  }
  const backend = request.backend ?? LlvmBackend.LlvmBackend
  const hostSelection =
    request.compilation.target === undefined ? NativeToolchain.hostSelection() : undefined
  if (hostSelection?._tag === 'Unavailable')
    return Object.freeze({
      _tag: 'TargetFailed',
      error: hostSelection.error,
      diagnostics: frontend.diagnostics,
      report: Object.freeze([...report]),
    })
  const targetId =
    request.compilation.target ??
    (hostSelection?._tag === 'Resolved' ? hostSelection.target.id : undefined)
  const preparation = Realization.prepare(frontend, backend, targetId, { heapBytes })
  const integrityReport = report.at(0)
  report.splice(
    0,
    report.length,
    ...(integrityReport === undefined ? [] : [integrityReport]),
    ...preparation.report.map(phaseWithHeap),
  )
  if (preparation._tag === 'Rejected')
    return Object.freeze({
      _tag: 'Rejected',
      sources: closure.sources,
      diagnostics: preparation.diagnostics,
      report: Object.freeze([...report]),
    })
  if (preparation._tag === 'NoEntry')
    return Object.freeze({
      _tag: 'NoEntry',
      reason: preparation.reason,
      ...(preparation.requirements === undefined ? {} : { requirements: preparation.requirements }),
      diagnostics: preparation.diagnostics,
      report: Object.freeze([...report]),
    })
  if (preparation._tag === 'TargetFailed')
    return Object.freeze({
      _tag: 'TargetFailed',
      error: preparation.error,
      diagnostics: preparation.diagnostics,
      report: Object.freeze([...report]),
    })
  if (preparation._tag === 'BackendFailed')
    return Object.freeze({
      _tag: 'BackendFailed',
      error: preparation.error,
      diagnostics: preparation.diagnostics,
      report: Object.freeze([...report]),
    })
  const { diagnostics, program, target } = preparation
  const targetIntegrity = PhaseReport.measureInto(
    report,
    'toolchain-target',
    program.intrinsics.length,
    () =>
      ToolchainIntegrity.validateTarget(
        distribution,
        IntrinsicAvailability.backendTarget(backend.id),
        program.intrinsics,
        closure.sources.keys(),
      ),
    (result) =>
      result._tag === 'Matched' ? result.providers.length + result.runtimeSupport.length : 0,
    (result) => (result._tag === 'Invalid' ? result.failures.length : 0),
    { heapBytes },
  )
  if (targetIntegrity._tag === 'UnsupportedTarget')
    return Object.freeze({
      _tag: 'TargetFailed',
      error: Target.unavailableInventory(target, targetIntegrity.operations),
      diagnostics,
      report: Object.freeze([...report]),
    })
  if (targetIntegrity._tag === 'Invalid')
    return Object.freeze({
      _tag: 'ToolchainFailed',
      expectedIdentity: ToolchainIntegrity.installed().digest,
      observedIdentity: distribution.digest,
      failures: targetIntegrity.failures,
      report: Object.freeze([...report]),
    })
  const emitted = yield* PhaseReport.measureEffectInto(
    report,
    'backend',
    program.functions.length,
    Backend.emit(backend, program, {
      mode: request.profile === 'release' ? 'release' : 'debug',
      sources: new Map(
        [...closure.sources].map(([module, source]) => [module, SourceFile.toUint8Array(source)]),
      ),
    }).pipe(
      Effect.map((artifact) => Object.freeze({ _tag: 'Emitted' as const, artifact })),
      Effect.catchTag('BackendError', (error) =>
        Effect.succeed(Object.freeze({ _tag: 'Rejected' as const, error })),
      ),
    ),
    (result) => (result._tag === 'Emitted' ? result.artifact.symbols.length : 0),
    () => 0,
    { heapBytes },
  )
  if (emitted._tag === 'Rejected') {
    return Object.freeze({
      _tag: 'BackendFailed',
      error: emitted.error,
      diagnostics,
      report: Object.freeze([...report]),
    })
  }
  const artifact = emitted.artifact

  const cacheKind: NativeToolchain.FinalArtifact['kind'] = Target.isNative(target)
    ? 'NativeExecutable'
    : 'WebAssemblyModule'
  const artifactCache =
    request.cache !== false && artifact._tag === 'LlvmBitcodeArtifact'
      ? (request.toolchain.artifactCache ?? NativeToolchain.defaultArtifactCache())
      : undefined
  const cacheKey =
    artifactCache !== undefined && artifact._tag === 'LlvmBitcodeArtifact'
      ? yield* NativeToolchain.artifactCacheKey(
          request.toolchain,
          cacheKind,
          target,
          request.profile,
          artifact.bitcode,
          cacheKind === 'NativeExecutable' ? ToolchainPlan.shimSource(artifact.termination) : '',
        )
      : undefined
  if (
    artifactCache !== undefined &&
    cacheKey !== undefined &&
    artifact._tag === 'LlvmBitcodeArtifact'
  ) {
    const bytes = yield* artifactCache.get(cacheKey)
    if (bytes !== undefined) {
      const committed = yield* PhaseReport.measureEffectInto(
        report,
        'artifact-cache',
        1,
        NativeToolchain.commitCachedArtifact(bytes, cacheKind, target, request.destination),
        () => 1,
        () => 0,
        { heapBytes },
      )
      return Object.freeze({
        _tag: 'Compiled',
        backend: artifact.backend,
        artifactKind: committed.kind,
        path: committed.path,
        target: committed.target,
        symbols: artifact.symbols,
        termination: artifact.termination,
        diagnostics,
        report: Object.freeze([...report]),
        toolchainIdentity: distribution.digest,
      })
    }
  }

  return yield* NativeToolchain.withBuildScope(
    request.scopeName ?? 'driver',
    (scope) =>
      Effect.gen(function* () {
        if (artifact._tag === 'WebAssemblyModuleArtifact') {
          const committed = yield* PhaseReport.measureEffectInto(
            report,
            'artifact-commit',
            1,
            NativeToolchain.commitWasm(artifact, request.destination),
            () => 1,
            () => 0,
            { heapBytes },
          )
          return Object.freeze({
            _tag: 'Compiled',
            backend: artifact.backend,
            artifactKind: committed.kind,
            path: committed.path,
            target: committed.target,
            symbols: artifact.symbols,
            termination: artifact.termination,
            diagnostics,
            report: Object.freeze([...report]),
            toolchainIdentity: distribution.digest,
          })
        }

        if (!Target.isNative(target)) {
          const finalized = yield* PhaseReport.measureEffectInto(
            report,
            'wasm-finalize',
            1,
            NativeToolchain.finalizeWasm(
              request.toolchain,
              scope,
              artifact,
              target,
              request.profile,
              request.destination,
            ),
            () => 1,
            () => 0,
            { heapBytes },
          )
          if (artifactCache !== undefined && cacheKey !== undefined) {
            yield* artifactCache.set(cacheKey, finalized.bytes)
          }
          return Object.freeze({
            _tag: 'Compiled',
            backend: artifact.backend,
            artifactKind: finalized.kind,
            path: finalized.path,
            target: finalized.target,
            symbols: artifact.symbols,
            termination: artifact.termination,
            diagnostics,
            report: Object.freeze([...report]),
            toolchainIdentity: distribution.digest,
          })
        }

        const object = yield* PhaseReport.measureEffectInto(
          report,
          'object',
          1,
          NativeToolchain.emitObject(request.toolchain, scope, artifact, target, request.profile),
          () => 1,
          () => 0,
          { heapBytes },
        )
        const shim = yield* PhaseReport.measureEffectInto(
          report,
          'shim',
          1,
          NativeToolchain.compileShim(
            request.toolchain,
            scope,
            target,
            artifact.termination,
            artifact.nativeRuntimeSymbols,
          ),
          () => 1,
          () => 0,
          { heapBytes },
        )
        const linked = yield* PhaseReport.measureEffectInto(
          report,
          'link',
          2,
          NativeToolchain.ClangLinker.link(
            request.toolchain,
            scope,
            target,
            [object.artifact, shim.artifact],
            [],
            request.destination,
          ),
          () => 1,
          () => 0,
          { heapBytes },
        )
        if (artifactCache !== undefined && cacheKey !== undefined) {
          yield* artifactCache.set(cacheKey, linked.bytes)
        }
        return Object.freeze({
          _tag: 'Compiled',
          backend: artifact.backend,
          artifactKind: 'NativeExecutable',
          path: linked.path,
          target: linked.target,
          symbols: artifact.symbols,
          termination: artifact.termination,
          diagnostics,
          report: Object.freeze([...report]),
          toolchainIdentity: distribution.digest,
        })
      }),
    { saveTemps: request.saveTemps ?? false },
  )
})
