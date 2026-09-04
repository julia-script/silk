import * as Config from 'effect/Config'
import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as ArtifactKind from './ArtifactKind.js'
import * as AbiManifest from './AbiManifest.js'
import * as Backend from './Backend.js'
import * as CHeader from './CHeader.js'
import type * as Diagnostic from './Diagnostic.js'
import * as Frontend from './Frontend.js'
import * as HeapObservation from './HeapObservation.js'
import type * as Instances from './Instances.js'
import * as LlvmBackend from './LlvmBackend.js'
import * as LlvmWasmRuntime from './LlvmWasmRuntime.js'
import type * as ModuleClosure from './ModuleClosure.js'
import * as NativeLinkInput from './NativeLinkInput.js'
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

/**
 * Backend emission is deterministic over the source closure, the target, the emission mode, and
 * the compiler build itself (the determinism suites pin byte-identical artifacts across
 * processes), so its output is content-addressable BEFORE it is produced. The executable cache
 * below already skips Clang; this key lets an unchanged compilation skip the LLVM emission pass
 * as well, which measures far above the Clang stage on scheduler-scale programs.
 */
const backendEmissionCacheKey = (
  distributionDigest: string,
  backendId: string,
  targetId: string,
  artifactKind: ArtifactKind.ArtifactKind,
  mode: string,
  sources: ReadonlyMap<string, SourceFile.SourceFile>,
): string => {
  const modules = [...sources]
    .map(
      ([module, source]) =>
        `${module}:${ToolchainIntegrity.contentDigest(SourceFile.toUint8Array(source))}`,
    )
    .sort()
  const digest = ToolchainIntegrity.contentDigest(
    [
      'backend-emission-v2',
      distributionDigest,
      backendId,
      targetId,
      artifactKind,
      mode,
      ...modules,
    ].join('\u0000'),
  )
  return `backend-${digest}.blob`
}

interface CachedEmissionHeader {
  readonly schema: 4
  readonly module: string
  readonly report: Backend.Termination['report']
  readonly symbols: Backend.LlvmBitcodeArtifact['symbols']
  readonly nativeRuntimeSymbols: ReadonlyArray<string>
  readonly runtimeFeatures: Backend.LlvmBitcodeArtifact['runtimeFeatures']
  readonly foreignImports: Backend.LlvmBitcodeArtifact['foreignImports']
  readonly foreignExports: Backend.LlvmBitcodeArtifact['foreignExports']
  readonly foreignStatics: Backend.LlvmBitcodeArtifact['foreignStatics']
}

const encodeCachedEmission = (artifact: Backend.LlvmBitcodeArtifact): Uint8Array | undefined => {
  try {
    const header: CachedEmissionHeader = {
      schema: 4,
      module: artifact.module,
      report: artifact.termination.report,
      symbols: artifact.symbols,
      nativeRuntimeSymbols: artifact.nativeRuntimeSymbols,
      runtimeFeatures: artifact.runtimeFeatures,
      foreignImports: artifact.foreignImports,
      foreignExports: artifact.foreignExports,
      foreignStatics: artifact.foreignStatics,
    }
    const json = new TextEncoder().encode(JSON.stringify(header))
    const bytes = new Uint8Array(4 + json.length + artifact.bitcode.length)
    new DataView(bytes.buffer).setUint32(0, json.length, true)
    bytes.set(json, 4)
    bytes.set(artifact.bitcode, 4 + json.length)
    return bytes
  } catch {
    // Fail open: an unserializable symbol table only means this compilation is not cached.
    return undefined
  }
}

const decodeCachedEmission = (
  bytes: Uint8Array,
  program: Parameters<typeof Backend.terminationOf>[0],
  target: Target.Target,
): Backend.LlvmBitcodeArtifact | undefined => {
  try {
    if (bytes.length < 4) return undefined
    const jsonLength = new DataView(bytes.buffer, bytes.byteOffset).getUint32(0, true)
    if (4 + jsonLength > bytes.length) return undefined
    const header: CachedEmissionHeader = JSON.parse(
      new TextDecoder().decode(bytes.subarray(4, 4 + jsonLength)),
    )
    if (header.schema !== 4) return undefined
    const bitcode = bytes.slice(4 + jsonLength)
    // `control` and `ir` are never read on the driver path; the cast records that this artifact
    // stays internal to the driver rather than flowing back out through Backend.emit.
    return Object.freeze({
      _tag: 'LlvmBitcodeArtifact',
      backend: 'llvm',
      module: header.module,
      target,
      symbols: header.symbols,
      termination: Backend.terminationOf(program, header.report),
      nativeRuntimeSymbols: header.nativeRuntimeSymbols,
      runtimeFeatures: header.runtimeFeatures,
      foreignImports: header.foreignImports,
      foreignExports: header.foreignExports,
      foreignStatics: header.foreignStatics,
      control: Object.freeze([]),
      bitcode,
      ir: '',
    }) as Backend.LlvmBitcodeArtifact
  } catch {
    return undefined
  }
}

/** One driver request. */
export interface CompileRequest {
  readonly compilation: ModuleClosure.CompilationRequest
  readonly toolchain: NativeToolchain.Toolchain
  readonly profile: ToolchainPlan.OptimizationProfile
  readonly artifactKind: ArtifactKind.ArtifactKind
  /** Validated project package name used for durable artifact identities. */
  readonly packageName: string
  readonly destination: string
  /** Ordered, structured native inputs passed after compiler-generated objects. */
  readonly nativeLinkInputs?: ReadonlyArray<NativeLinkInput.NativeLinkInput>
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
  readonly foreignImports: ReadonlyArray<Backend.ForeignImport>
  readonly foreignExports: ReadonlyArray<Backend.ForeignExport>
  readonly foreignStatics: ReadonlyArray<Backend.ForeignStatic>
  readonly libraryInterface?: NativeToolchain.LibraryInterfaceArtifacts
  readonly termination?: Backend.Termination
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

const commitLibraryInterface = Effect.fnUntraced(function* (
  request: CompileRequest,
  artifactPath: string,
  artifact: Backend.Artifact,
  target: Target.Target,
): Effect.fn.Return<NativeToolchain.LibraryInterfaceArtifacts, NativeToolchain.ToolchainError> {
  return yield* NativeToolchain.commitLibraryInterface(
    artifactPath,
    request.destination,
    request.packageName,
    CHeader.encode(
      CHeader.make(request.packageName, artifact.foreignExports, artifact.foreignStatics),
    ),
    AbiManifest.encode(
      AbiManifest.make(
        target,
        artifact.foreignImports,
        artifact.foreignExports,
        artifact.foreignStatics,
      ),
    ),
  )
})

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
  const nativeCacheDirectory = yield* Config.string('SILK_NATIVE_CACHE_DIR').pipe(
    Config.withDefault(''),
    Effect.orDie,
  )

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
  const backend = LlvmBackend.LlvmBackend
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
  const artifactTarget = Target.select(targetId)
  if (
    artifactTarget._tag === 'Resolved' &&
    !ArtifactKind.supports(request.artifactKind, artifactTarget.target)
  )
    return Object.freeze({
      _tag: 'TargetFailed',
      error: Target.unavailableArtifact(artifactTarget.target, request.artifactKind),
      diagnostics: frontend.diagnostics,
      report: Object.freeze([...report]),
    })
  const preparation = Realization.prepare(frontend, targetId, {
    heapBytes,
    artifactKind: request.artifactKind,
  })
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
        target,
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
  const mode = request.profile === 'release' ? 'release' : 'debug'
  const emissionCache =
    request.cache !== false && backend.id === 'llvm'
      ? (request.toolchain.artifactCache ??
        NativeToolchain.defaultArtifactCache(nativeCacheDirectory))
      : undefined
  const emissionCacheKey =
    emissionCache === undefined
      ? undefined
      : backendEmissionCacheKey(
          distribution.digest,
          backend.id,
          target.id,
          request.artifactKind,
          mode,
          closure.sources,
        )
  const cachedEmission =
    emissionCache !== undefined && emissionCacheKey !== undefined
      ? decodeCachedEmission(
          (yield* NativeToolchain.readArtifactCache(emissionCache, emissionCacheKey)) ??
            new Uint8Array(0),
          program,
          target,
        )
      : undefined
  const emitted =
    cachedEmission !== undefined
      ? PhaseReport.measureInto(
          report,
          'backend-cache',
          program.functions.length,
          () => Object.freeze({ _tag: 'Emitted' as const, artifact: cachedEmission }),
          (result) => result.artifact.symbols.length,
          () => 0,
          { heapBytes },
        )
      : yield* PhaseReport.measureEffectInto(
          report,
          'backend',
          program.functions.length,
          Backend.emit(backend, program, {
            mode,
            sources: new Map(
              [...closure.sources].map(([module, source]) => [
                module,
                SourceFile.toUint8Array(source),
              ]),
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
  if (
    cachedEmission === undefined &&
    emissionCache !== undefined &&
    emissionCacheKey !== undefined &&
    artifact._tag === 'LlvmBitcodeArtifact'
  ) {
    const encoded = encodeCachedEmission(artifact)
    if (encoded !== undefined)
      yield* NativeToolchain.writeArtifactCache(emissionCache, emissionCacheKey, encoded)
  }

  const cacheKind = request.artifactKind
  const scopeName = request.scopeName ?? 'driver'
  const requestedNativeInputs = request.nativeLinkInputs ?? Object.freeze([])
  // Float remainder lowers to LLVM `frem`, which becomes an fmod/fmodf libcall on
  // targets whose libm is separate from libc (Linux); macOS folds it into libSystem.
  const nativeLinkInputs =
    cacheKind === 'NativeExecutable' || cacheKind === 'NativeSharedLibrary'
      ? Object.freeze([...requestedNativeInputs, NativeLinkInput.library('m', 'Dynamic')])
      : requestedNativeInputs
  // A missing request input is linker data, not a cache-key storage failure.
  if (cacheKind !== 'WebAssemblyModule' && nativeLinkInputs.length > 0)
    yield* NativeToolchain.requireLinkInputs(
      request.toolchain,
      cacheKind,
      target,
      Object.freeze([]),
      nativeLinkInputs,
      request.destination,
    )
  const artifactCache =
    request.cache !== false && artifact._tag === 'LlvmBitcodeArtifact'
      ? (request.toolchain.artifactCache ??
        NativeToolchain.defaultArtifactCache(nativeCacheDirectory))
      : undefined
  let runtimeSource = ''
  if (cacheKind === 'NativeExecutable')
    runtimeSource = ToolchainPlan.executableSource(
      artifact.termination,
      artifact.nativeRuntimeSymbols,
    )
  else if (cacheKind === 'WebAssemblyModule') runtimeSource = LlvmWasmRuntime.source
  else if (ArtifactKind.isLibrary(cacheKind))
    runtimeSource = ToolchainPlan.runtimeSource(artifact.nativeRuntimeSymbols)
  const cacheKey =
    artifactCache !== undefined && artifact._tag === 'LlvmBitcodeArtifact'
      ? yield* NativeToolchain.artifactCacheKey(
          request.toolchain,
          cacheKind,
          target,
          request.profile,
          artifact.bitcode,
          runtimeSource,
          request.destination,
          nativeLinkInputs,
        )
      : undefined
  if (
    artifactCache !== undefined &&
    cacheKey !== undefined &&
    artifact._tag === 'LlvmBitcodeArtifact'
  ) {
    const bytes = yield* NativeToolchain.readArtifactCache(artifactCache, cacheKey)
    if (bytes !== undefined && NativeToolchain.isCachedArtifact(bytes, cacheKind, target)) {
      const committed = yield* PhaseReport.measureEffectInto(
        report,
        'artifact-cache',
        1,
        NativeToolchain.commitCachedArtifact(bytes, cacheKind, target, request.destination),
        () => 1,
        () => 0,
        { heapBytes },
      )
      const libraryInterface = ArtifactKind.isLibrary(cacheKind)
        ? yield* PhaseReport.measureEffectInto(
            report,
            'library-interface',
            artifact.foreignImports.length +
              artifact.foreignExports.length +
              artifact.foreignStatics.length,
            commitLibraryInterface(request, committed.path, artifact, target),
            () => 2,
            () => 0,
            { heapBytes },
          )
        : undefined
      return Object.freeze({
        _tag: 'Compiled',
        backend: artifact.backend,
        artifactKind: committed.kind,
        path: committed.path,
        target: committed.target,
        symbols: artifact.symbols,
        foreignImports: artifact.foreignImports,
        foreignExports: artifact.foreignExports,
        foreignStatics: artifact.foreignStatics,
        ...(libraryInterface === undefined ? {} : { libraryInterface }),
        ...(ArtifactKind.isLibrary(cacheKind) ? {} : { termination: artifact.termination }),
        diagnostics,
        report: Object.freeze([...report]),
        toolchainIdentity: distribution.digest,
      })
    }
  }

  return yield* NativeToolchain.withBuildScope(
    scopeName,
    (scope) =>
      Effect.gen(function* () {
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
            yield* NativeToolchain.writeArtifactCache(artifactCache, cacheKey, finalized.bytes)
          }
          return Object.freeze({
            _tag: 'Compiled',
            backend: artifact.backend,
            artifactKind: finalized.kind,
            path: finalized.path,
            target: finalized.target,
            symbols: artifact.symbols,
            foreignImports: artifact.foreignImports,
            foreignExports: artifact.foreignExports,
            foreignStatics: artifact.foreignStatics,
            termination: artifact.termination,
            diagnostics,
            report: Object.freeze([...report]),
            toolchainIdentity: distribution.digest,
          })
        }

        if (cacheKind === 'WebAssemblyModule')
          return Object.freeze({
            _tag: 'TargetFailed',
            error: Target.unavailableArtifact(target, cacheKind),
            diagnostics,
            report: Object.freeze([...report]),
          })

        const object = yield* PhaseReport.measureEffectInto(
          report,
          'object',
          1,
          NativeToolchain.emitObject(request.toolchain, scope, artifact, target, request.profile),
          () => 1,
          () => 0,
          { heapBytes },
        )
        const runtime = yield* PhaseReport.measureEffectInto(
          report,
          'runtime',
          1,
          cacheKind === 'NativeExecutable'
            ? NativeToolchain.compileExecutableRuntime(
                request.toolchain,
                scope,
                target,
                artifact.termination,
                artifact.nativeRuntimeSymbols,
              )
            : NativeToolchain.compileRuntime(
                request.toolchain,
                scope,
                target,
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
          NativeToolchain.NativeFinalizer.finalize(
            request.toolchain,
            scope,
            cacheKind,
            target,
            [object.artifact, runtime.artifact],
            nativeLinkInputs,
            request.destination,
          ),
          () => 1,
          () => 0,
          { heapBytes },
        )
        if (artifactCache !== undefined && cacheKey !== undefined) {
          yield* NativeToolchain.writeArtifactCache(artifactCache, cacheKey, linked.bytes)
        }
        const libraryInterface = ArtifactKind.isLibrary(cacheKind)
          ? yield* PhaseReport.measureEffectInto(
              report,
              'library-interface',
              artifact.foreignImports.length +
                artifact.foreignExports.length +
                artifact.foreignStatics.length,
              commitLibraryInterface(request, linked.path, artifact, target),
              () => 2,
              () => 0,
              { heapBytes },
            )
          : undefined
        return Object.freeze({
          _tag: 'Compiled',
          backend: artifact.backend,
          artifactKind: linked.kind,
          path: linked.path,
          target: linked.target,
          symbols: artifact.symbols,
          foreignImports: artifact.foreignImports,
          foreignExports: artifact.foreignExports,
          foreignStatics: artifact.foreignStatics,
          ...(libraryInterface === undefined ? {} : { libraryInterface }),
          ...(ArtifactKind.isLibrary(cacheKind) ? {} : { termination: artifact.termination }),
          diagnostics,
          report: Object.freeze([...report]),
          toolchainIdentity: distribution.digest,
        })
      }),
    { saveTemps: request.saveTemps ?? false },
  )
})
