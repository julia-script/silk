import * as NativeRequirementBinding from './NativeRequirementBinding.js'
import * as ArtifactPlan from './ArtifactPlan.js'
import * as Config from 'effect/Config'
import * as Result from 'effect/Result'
import * as ForeignContract from './ForeignContract.js'
import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as ArtifactKind from './ArtifactKind.js'
import * as AbiManifest from './AbiManifest.js'
import * as Backend from './Backend.js'
import * as CHeader from './CHeader.js'
import * as Diagnostic from './Diagnostic.js'
import * as Frontend from './Frontend.js'
import * as HeapObservation from './HeapObservation.js'
import type * as Instances from './Instances.js'
import * as LlvmBackend from './LlvmBackend.js'
import type * as ModuleClosure from './ModuleClosure.js'
import * as NativeLinkInput from './NativeLinkInput.js'
import * as NativeToolchain from './NativeToolchain.js'
import type * as NativeLinkPlan from './NativeLinkPlan.js'
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
 * processes), so its output is content-addressable BEFORE it is produced. This key lets an
 * unchanged compilation skip LLVM emission independently of final-artifact cache eligibility.
 */
const backendEmissionCacheKey = (
  distributionDigest: string,
  backendId: string,
  profileIdentity: string,
  planIdentity: string,
  artifactKind: ArtifactKind.ArtifactKind,
  mode: string,
  sources: ReadonlyMap<string, SourceFile.SourceFile>,
  interfaces: ReadonlyArray<SourceFile.SourceFile>,
): string => {
  const modules = [...sources]
    .map(
      ([module, source]) =>
        `${module}:${ToolchainIntegrity.contentDigest(SourceFile.toUint8Array(source))}`,
    )
    .sort()
  const digest = ToolchainIntegrity.contentDigest(
    [
      'backend-emission-v5',
      distributionDigest,
      backendId,
      profileIdentity,
      planIdentity,
      artifactKind,
      mode,
      ...modules,
      ...interfaces
        .map(
          (source) =>
            `interface:${source.id}:${ToolchainIntegrity.contentDigest(SourceFile.toUint8Array(source))}`,
        )
        .sort(),
    ].join('\u0000'),
  )
  return `backend-${digest}.blob`
}

interface CachedEmissionHeader {
  readonly schema: 6
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
      schema: 6,
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
    if (header.schema !== 6) return undefined
    if (
      ![...header.foreignImports, ...header.foreignExports].every(
        (entry) =>
          typeof entry.variadic === 'boolean' &&
          (!entry.variadic || entry.parameters.length > 0) &&
          ForeignContract.inspect(entry.contract, entry.parameters, entry.result) !== undefined,
      )
    )
      return undefined
    if (header.foreignExports.some((entry) => entry.variadic)) return undefined
    const bitcode = bytes.slice(4 + jsonLength)
    // The driver cache does not expose IR or control-flow inspection to callers.
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
    })
  } catch {
    return undefined
  }
}

/** One driver request. */
export interface CompileRequest {
  readonly nativeBindings?: ReadonlyArray<NativeRequirementBinding.NativeRequirementBinding>
  readonly stage?: ArtifactPlan.Stage
  readonly compilation: ModuleClosure.CompilationRequest
  readonly toolchain: NativeToolchain.Toolchain
  readonly optimization?: ToolchainPlan.OptimizationProfile
  readonly artifactKind: ArtifactKind.ArtifactKind
  /** Validated project package name used for durable artifact identities. */
  readonly packageName: string
  readonly destination: string
  /** Supplied behavioral ABI JSON snapshots, validated against visible contracts before cache reuse. */
  readonly foreignInterfaces?: ReadonlyArray<SourceFile.SourceFile>
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
  readonly stage?: ArtifactPlan.Stage
  readonly artifactPlan?: ArtifactPlan.ArtifactPlan
  readonly nativeBindings?: NativeRequirementBinding.Resolved
  readonly _tag: 'Compiled'
  readonly linkPlan?: NativeLinkPlan.NativeLinkPlan
  readonly linkPlanPath?: string
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

  const hostSelection =
    request.compilation.target === undefined && request.compilation.configuration === undefined
      ? NativeToolchain.hostSelection()
      : undefined
  if (hostSelection?._tag === 'Unavailable')
    return Object.freeze({
      _tag: 'TargetFailed',
      error: hostSelection.error,
      diagnostics: [],
      report: Object.freeze([...report]),
    })
  const targetId =
    request.compilation.configuration?.profile.target ??
    request.compilation.target ??
    (hostSelection?._tag === 'Resolved' ? hostSelection.target.id : undefined)
  const compilation: ModuleClosure.CompilationRequest =
    request.compilation.configuration !== undefined || targetId === undefined
      ? request.compilation
      : {
          root: request.compilation.root,
          configuration: {
            package: `${request.packageName}@0.0.0`,
            profile: {
              target: targetId,
              artifact: ArtifactKind.profileArtifact(request.artifactKind),
              optimization:
                request.optimization === undefined || request.optimization === 'debug'
                  ? 'none'
                  : 'speed',
              debug: request.optimization !== 'release',
            },
          },
        }
  const frontend = yield* Frontend.frontend(compilation, { heapBytes })
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
  const preparation = yield* Realization.prepare(frontend, targetId, {
    heapBytes,
    artifactKind: request.artifactKind,
    ...(request.compilation.configuration === undefined || request.optimization === undefined
      ? {}
      : { optimization: request.optimization }),
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
  const { diagnostics, program, target } = preparation
  const stage = request.stage ?? 'final'
  const plannedArtifact = yield* Effect.result(
    ArtifactPlan.make(
      frontend,
      preparation.profile,
      preparation.composition,
      program,
      stage,
      distribution.digest,
    ),
  )
  if (Result.isFailure(plannedArtifact)) {
    const span = closure.modules.find((module) => module.name === closure.rootModule)?.syntax.root
      .span
    if (span === undefined) throw new RangeError('Artifact plan lost application source')
    return Object.freeze({
      _tag: 'Rejected',
      sources: closure.sources,
      diagnostics: Diagnostic.merge(diagnostics, [
        Diagnostic.invalidConfiguration(plannedArtifact.failure, span),
      ]),
      report: Object.freeze([...report]),
    })
  }
  const artifactPlan = plannedArtifact.success

  const importedInterfaces: Array<AbiManifest.Imported> = []
  const interfaceDiagnostics: Array<Diagnostic.Diagnostic> = []
  for (const source of request.foreignInterfaces ?? []) {
    const decoded = yield* AbiManifest.decode(source).pipe(Effect.result)
    if (Result.isFailure(decoded)) interfaceDiagnostics.push(decoded.failure)
    else importedInterfaces.push(decoded.success)
  }
  interfaceDiagnostics.push(...AbiManifest.check(importedInterfaces, program))
  if (interfaceDiagnostics.length > 0)
    return Object.freeze({
      _tag: 'Rejected',
      sources: new Map([
        ...closure.sources,
        ...(request.foreignInterfaces ?? []).map((source) => [source.id, source] as const),
      ]),
      diagnostics: Object.freeze([...diagnostics, ...interfaceDiagnostics]),
      report: Object.freeze([...report]),
    })
  const targetIntegrity = PhaseReport.measureInto(
    report,
    'toolchain-target',
    program.intrinsics.length,
    () => ToolchainIntegrity.validateTarget(distribution, target, program.intrinsics),
    (result) => (result._tag === 'Matched' ? result.runtimeSupport.length : 0),
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
  const mode = preparation.profile.debug ? 'debug' : 'release'
  const emissionCache =
    request.cache !== false
      ? (request.toolchain.artifactCache ??
        NativeToolchain.defaultArtifactCache(nativeCacheDirectory))
      : undefined
  const emissionCacheKey =
    emissionCache === undefined
      ? undefined
      : backendEmissionCacheKey(
          distribution.digest,
          backend.id,
          preparation.profile.identity,
          artifactPlan.identity,
          request.artifactKind,
          mode,
          closure.sources,
          request.foreignInterfaces ?? [],
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

  if (stage !== 'final') {
    const path = yield* NativeToolchain.withBuildScope(
      request.scopeName ?? 'representation',
      Effect.fnUntraced(function* (scope: NativeToolchain.BuildScope) {
        return yield* NativeToolchain.emitRepresentation(
          request.toolchain,
          scope,
          artifact,
          preparation.profile,
          stage,
          request.destination,
        )
      }),
      { saveTemps: request.saveTemps ?? false },
    )
    return Object.freeze({
      _tag: 'Compiled',
      backend: artifact.backend,
      artifactKind: request.artifactKind,
      stage,
      artifactPlan,
      path,
      target,
      symbols: artifact.symbols,
      foreignImports: artifact.foreignImports,
      foreignExports: artifact.foreignExports,
      foreignStatics: artifact.foreignStatics,
      diagnostics,
      report: Object.freeze([...report]),
      toolchainIdentity: distribution.digest,
    })
  }
  const bound = yield* Effect.result(
    NativeRequirementBinding.resolve(
      artifactPlan.requirements,
      request.nativeBindings ?? [],
      artifactPlan.form,
    ),
  )
  if (Result.isFailure(bound)) {
    const span = closure.modules.find((module) => module.name === closure.rootModule)?.syntax.root
      .span
    if (span === undefined)
      throw new RangeError('Native requirement binding lost application source')
    return Object.freeze({
      _tag: 'Rejected',
      sources: closure.sources,
      diagnostics: Diagnostic.merge(diagnostics, [
        Diagnostic.invalidConfiguration(bound.failure, span),
      ]),
      report: Object.freeze([...report]),
    })
  }
  const cacheKind = request.artifactKind
  const scopeName = request.scopeName ?? 'driver'
  const requestedNativeInputs = Object.freeze([
    ...(request.nativeLinkInputs ?? []),
    ...bound.success.inputs,
  ])
  // Float remainder lowers to LLVM `frem`, which becomes an fmod/fmodf libcall on
  // targets whose libm is separate from libc (Linux); macOS folds it into libSystem.
  const nativeLinkInputs =
    (cacheKind === 'NativeExecutable' || cacheKind === 'NativeSharedLibrary') &&
    preparation.profile.libc !== 'none'
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
  const cacheAdmission = NativeToolchain.finalArtifactCacheAdmission(cacheKind)
  const artifactCache =
    cacheAdmission._tag !== 'Ineligible' &&
    request.cache !== false &&
    artifact._tag === 'LlvmBitcodeArtifact'
      ? (request.toolchain.artifactCache ??
        NativeToolchain.defaultArtifactCache(nativeCacheDirectory))
      : undefined
  const runtimeSource = NativeToolchain.artifactRuntimeSource(
    cacheKind,
    artifact.termination,
    artifact.nativeRuntimeSymbols,
    program.entry._tag !== 'NoInvocation',
  )
  const cacheKey =
    artifactCache !== undefined && artifact._tag === 'LlvmBitcodeArtifact'
      ? yield* NativeToolchain.wasmArtifactCacheKey(
          request.toolchain,
          preparation.profile,
          artifact.bitcode,
          runtimeSource,
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
      const libraryInterface =
        ArtifactKind.isLibrary(cacheKind) || cacheKind === 'NativeObject'
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
        stage,
        artifactPlan,
        nativeBindings: bound.success,
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
              preparation.profile,
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
            stage,
            artifactPlan,
            nativeBindings: bound.success,
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

        const toolchain = yield* NativeToolchain.resolveToolchain(
          request.toolchain,
          preparation.profile,
        )
        const object = yield* PhaseReport.measureEffectInto(
          report,
          'object',
          1,
          NativeToolchain.emitObject(toolchain, scope, artifact, preparation.profile),
          () => 1,
          () => 0,
          { heapBytes },
        )
        const generatedObjects: Array<NativeToolchain.PathArtifact> = [object.artifact]
        if (program.entry._tag !== 'NoInvocation' || artifact.nativeRuntimeSymbols.length > 0) {
          const runtime = yield* PhaseReport.measureEffectInto(
            report,
            'runtime',
            1,
            program.entry._tag !== 'NoInvocation'
              ? NativeToolchain.compileExecutableRuntime(
                  toolchain,
                  scope,
                  target,
                  artifact.termination,
                  artifact.nativeRuntimeSymbols,
                )
              : NativeToolchain.compileRuntime(
                  toolchain,
                  scope,
                  target,
                  artifact.nativeRuntimeSymbols,
                ),
            () => 1,
            () => 0,
            { heapBytes },
          )
          generatedObjects.push(runtime.artifact)
        }
        if (
          cacheKind === 'NativeObject' &&
          generatedObjects.length === 1 &&
          nativeLinkInputs.length === 0
        ) {
          const path = yield* NativeToolchain.commitPathRepresentation(
            object.artifact,
            request.destination,
          )
          const libraryInterface = yield* commitLibraryInterface(request, path, artifact, target)
          return Object.freeze({
            _tag: 'Compiled',
            backend: artifact.backend,
            artifactKind: cacheKind,
            stage,
            artifactPlan,
            nativeBindings: bound.success,
            path,
            target,
            symbols: artifact.symbols,
            foreignImports: artifact.foreignImports,
            foreignExports: artifact.foreignExports,
            foreignStatics: artifact.foreignStatics,
            libraryInterface,
            diagnostics,
            report: Object.freeze([...report]),
            toolchainIdentity: distribution.digest,
          })
        }
        const linkPlan = yield* NativeToolchain.planNativeLink(
          toolchain,
          scope,
          cacheKind,
          preparation.profile,
          generatedObjects,
          nativeLinkInputs,
          request.destination,
          artifactPlan.composition.loader,
        )
        const nativeAdmission = NativeToolchain.finalArtifactCacheAdmission(cacheKind, linkPlan)
        const nativeCache =
          request.cache === false || nativeAdmission._tag !== 'CompleteNativePlan'
            ? undefined
            : (toolchain.artifactCache ??
              NativeToolchain.defaultArtifactCache(nativeCacheDirectory))
        const nativeKey = `native-${linkPlan.identity}.blob`
        yield* NativeToolchain.validateLinkPlan(linkPlan)
        const cached =
          nativeCache === undefined
            ? undefined
            : yield* NativeToolchain.readArtifactCache(nativeCache, nativeKey)
        const reusable =
          cached !== undefined && NativeToolchain.isCachedArtifact(cached, cacheKind, target)
        const linked = yield* PhaseReport.measureEffectInto(
          report,
          reusable ? 'artifact-cache' : 'link',
          2,
          reusable
            ? NativeToolchain.commitCachedArtifact(cached, cacheKind, target, request.destination)
            : NativeToolchain.NativeFinalizer.finalize(linkPlan, cacheKind, request.destination),
          () => 1,
          () => 0,
          { heapBytes },
        )
        if (!reusable && nativeCache !== undefined)
          yield* NativeToolchain.writeArtifactCache(nativeCache, nativeKey, linked.bytes)
        const linkPlanPath = yield* NativeToolchain.commitLinkPlan(
          linkPlan,
          `${request.destination}.link.json`,
        )
        const libraryInterface =
          ArtifactKind.isLibrary(cacheKind) || cacheKind === 'NativeObject'
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
          linkPlan,
          linkPlanPath,
          stage,
          artifactPlan,
          nativeBindings: bound.success,
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
