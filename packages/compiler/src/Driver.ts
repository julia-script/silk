import { readFileSync } from 'node:fs'
import { memoryUsage } from 'node:process'
import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as Backend from './Backend.js'
import type * as Diagnostic from './Diagnostic.js'
import type * as Instances from './Instances.js'
import type * as ModuleClosure from './ModuleClosure.js'
import * as NativeToolchain from './NativeToolchain.js'
import * as PhaseObservation from './PhaseReport.js'
import * as Pipeline from './Pipeline.js'
import * as SourceFile from './SourceFile.js'
import type * as SourceResolver from './SourceResolver.js'
import * as Target from './Target.js'
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
export interface PhaseReport extends Omit<PhaseObservation.PhaseReport, 'heapBytes'> {
  readonly heapBytes: number
}

const phaseWithHeap = (entry: PhaseObservation.PhaseReport): PhaseReport => {
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
  readonly report: ReadonlyArray<PhaseReport>
}

/** The request's root module has no valid entry; the toolchain was never invoked. */
export interface NoEntry {
  readonly _tag: 'NoEntry'
  readonly reason: Extract<Instances.Entry, { readonly _tag: 'Unavailable' }>['reason']
  readonly requirements?: ReadonlyArray<Type.Requirement>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<PhaseReport>
}

/** A finalization stage failed; the outcome retains command or storage provenance. */
export interface Failed {
  readonly _tag: 'Failed'
  readonly stage: 'object' | 'shim' | 'link' | 'wasm-finalize' | 'artifact-commit'
  readonly failure: NativeToolchain.FinalizationFailure
  readonly report: ReadonlyArray<PhaseReport>
}

/** Target selection stopped compilation before MIR lowering. */
export interface TargetFailed {
  readonly _tag: 'TargetFailed'
  readonly error: Target.TargetError
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<PhaseReport>
}

/** Shared MIR validation, compatibility, or backend construction stopped emission. */
export interface BackendFailed {
  readonly _tag: 'BackendFailed'
  readonly error: Backend.BackendError
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<PhaseReport>
}

/** Source diagnostics rejected artifact production after the recoverable frontend completed. */
export interface Rejected {
  readonly _tag: 'Rejected'
  readonly sources: ReadonlyMap<string, SourceFile.SourceFile>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<PhaseReport>
}

/** Imported source storage failed operationally after retaining the available frontend facts. */
export class SourceResolutionFailed extends Data.TaggedError('SourceResolutionFailed')<{
  readonly operation: 'Driver.compile'
  readonly message: string
  readonly failures: ReadonlyArray<SourceResolver.SourceResolverError>
  readonly sources: ReadonlyMap<string, SourceFile.SourceFile>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<PhaseReport>
}> {}

/** The closed outcome of one driver run. */
export type Outcome = Compiled | Rejected | NoEntry | TargetFailed | BackendFailed | Failed

/** Compiles one request end to end, writing its final artifact to the durable destination. */
export const compile = Effect.fn('Driver.compile')(function* (
  request: CompileRequest,
): Effect.fn.Return<Outcome, SourceResolutionFailed, SourceResolver.SourceResolver> {
  const report: Array<PhaseReport> = []
  const heapBytes = () => memoryUsage().heapUsed
  const phase = <A>(
    name: string,
    inputs: number,
    run: () => A,
    outputs: (result: A) => number,
    diagnostics: (result: A) => number = () => 0,
  ): A => {
    const measured = PhaseObservation.measure(name, inputs, run, outputs, diagnostics, heapBytes)
    report.push(phaseWithHeap(measured.report))
    return measured.value
  }

  const frontend = yield* Pipeline.frontend(request.compilation, { heapBytes })
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
  const backend = request.backend ?? Backend.LlvmBackend
  const preparation = Pipeline.prepare(frontend, backend, request.compilation.target, { heapBytes })
  report.splice(0, report.length, ...preparation.report.map(phaseWithHeap))
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
  const backendStartedAt = performance.now()
  const emitted = yield* Backend.emit(backend, program, {
    mode: request.profile === 'release' ? 'release' : 'debug',
    sources: new Map(
      [...closure.sources].map(([module, source]) => [module, SourceFile.toUint8Array(source)]),
    ),
  }).pipe(
    Effect.map((artifact) => Object.freeze({ _tag: 'Emitted' as const, artifact })),
    Effect.catchTag('BackendError', (error) =>
      Effect.succeed(Object.freeze({ _tag: 'Rejected' as const, error })),
    ),
  )
  report.push(
    Object.freeze({
      phase: 'backend',
      elapsedMs: performance.now() - backendStartedAt,
      inputs: program.functions.length,
      outputs: emitted._tag === 'Emitted' ? emitted.artifact.symbols.length : 0,
      diagnostics: 0,
      heapBytes: memoryUsage().heapUsed,
    }),
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
      ? NativeToolchain.artifactCacheKey(
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
    const bytes = artifactCache.get(cacheKey)
    if (bytes !== undefined) {
      const committed = phase(
        'artifact-cache',
        1,
        () => NativeToolchain.commitCachedArtifact(bytes, cacheKind, target, request.destination),
        (result) => (result._tag === 'FinalArtifact' ? 1 : 0),
      )
      if (committed._tag === 'StorageFailure') {
        return Object.freeze({
          _tag: 'Failed',
          stage: 'artifact-commit',
          failure: committed,
          report: Object.freeze([...report]),
        })
      }
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
      })
    }
  }

  return NativeToolchain.withBuildScope(
    request.scopeName ?? 'driver',
    (scope) => {
      if (artifact._tag === 'WebAssemblyModuleArtifact') {
        const committed = phase(
          'artifact-commit',
          1,
          () => NativeToolchain.commitWasm(scope, artifact, request.destination),
          (result) => (result._tag === 'FinalArtifact' ? 1 : 0),
        )
        if (committed._tag === 'StorageFailure') {
          return Object.freeze({
            _tag: 'Failed',
            stage: 'artifact-commit',
            failure: committed,
            report: Object.freeze([...report]),
          })
        }
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
        })
      }

      if (!Target.isNative(target)) {
        const finalized = phase(
          'wasm-finalize',
          1,
          () =>
            NativeToolchain.finalizeWasm(
              request.toolchain,
              scope,
              artifact,
              target,
              request.profile,
              request.destination,
            ),
          (result) => (result._tag === 'FinalArtifact' ? 1 : 0),
        )
        if (finalized._tag !== 'FinalArtifact') {
          return Object.freeze({
            _tag: 'Failed',
            stage: 'wasm-finalize',
            failure: finalized,
            report: Object.freeze([...report]),
          })
        }
        if (artifactCache !== undefined && cacheKey !== undefined) {
          artifactCache.set(cacheKey, readFileSync(finalized.path))
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
        })
      }

      const object = phase(
        'object',
        1,
        () =>
          NativeToolchain.emitObject(request.toolchain, scope, artifact, target, request.profile),
        (result) => (result._tag === 'ObjectArtifact' ? 1 : 0),
      )
      if (object._tag === 'ToolchainFailure') {
        return Object.freeze({
          _tag: 'Failed',
          stage: 'object',
          failure: object,
          report: Object.freeze([...report]),
        })
      }
      const shim = phase(
        'shim',
        1,
        () =>
          NativeToolchain.compileShim(
            request.toolchain,
            scope,
            target,
            artifact.termination,
            artifact.nativeRuntimeSymbols,
          ),
        (result) => (result._tag === 'ObjectArtifact' ? 1 : 0),
      )
      if (shim._tag === 'ToolchainFailure') {
        return Object.freeze({
          _tag: 'Failed',
          stage: 'shim',
          failure: shim,
          report: Object.freeze([...report]),
        })
      }
      const linked = phase(
        'link',
        2,
        () =>
          NativeToolchain.ClangLinker.link(
            request.toolchain,
            target,
            [object.artifact, shim.artifact],
            [],
            request.destination,
          ),
        (result) => (result._tag === 'Executable' ? 1 : 0),
      )
      if (linked._tag === 'ToolchainFailure') {
        return Object.freeze({
          _tag: 'Failed',
          stage: 'link',
          failure: linked,
          report: Object.freeze([...report]),
        })
      }
      if (artifactCache !== undefined && cacheKey !== undefined) {
        artifactCache.set(cacheKey, readFileSync(linked.path))
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
      })
    },
    { saveTemps: request.saveTemps ?? false },
  )
})
