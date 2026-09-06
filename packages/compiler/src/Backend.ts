import type * as ForeignContract from './ForeignContract.js'
import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as Diagnostic from './Diagnostic.js'
import * as Instances from './Instances.js'
import * as ForeignAvailability from './ForeignAvailability.js'
import * as ForeignPlanning from './ForeignPlanning.js'
import * as IntrinsicAvailability from './IntrinsicAvailability.js'
import * as Mir from './Mir.js'
import * as MirVerification from './MirVerification.js'
import type * as SourceSpan from './SourceSpan.js'
import * as StaticValue from './StaticValue.js'
import type * as Target from './Target.js'
import * as TerminationModel from './Termination.js'
import * as Type from './Type.js'
import type * as CAbi from './CAbi.js'

export interface CodegenRequest {
  readonly mode: 'debug' | 'release'
  readonly sources?: ReadonlyMap<string, Uint8Array>
  readonly privateExecutionStackPages?: number
}

export interface SymbolEntry {
  readonly declaration: DeclarationFacts.CanonicalId
  readonly instance: Mir.MirFunction['instance']
  readonly symbol: string
}

export interface ControlProvenance {
  readonly _tag: 'BackendControlProvenance'
  readonly backend: 'LLVM'
  readonly function: DeclarationFacts.CanonicalId
  readonly instance: Mir.MirFunction['instance']
  readonly region: Mir.RegionId
  readonly construct: 'LlvmJump' | 'LlvmBranch' | 'LlvmReturn' | 'LlvmTrap'
  readonly targets: ReadonlyArray<Mir.RegionId>
  readonly loop?: Mir.LoopId
  readonly span: SourceSpan.SourceSpan
}

export type Id = 'llvm'
export type Termination = TerminationModel.Contract

/** Canonical target-runtime capabilities that the backend actually emitted. */
export type RuntimeFeature =
  | 'DormantContinuation'
  | 'ExecutionDrive'
  | 'ExecutionPackage'
  | 'ExternalWakeCell'
  | 'NestedSuspensionRuntime'
  | 'ReadinessNotification'

/**
 * One reachable foreign (`extern "C"`) symbol the artifact expects the link step to resolve, with
 * its classified C signature spelled as C class names (`i32`, `u64`, `f64`, `void`, ...).
 */
export interface ForeignImport {
  readonly variadic: boolean
  readonly contract: ForeignContract.ForeignContract
  readonly symbol: string
  readonly parameters: ReadonlyArray<CAbi.TypeText>
  readonly result: CAbi.TypeText
}

/** One exported C-callable symbol and the C class spellings of its thunk signature. */
export interface ForeignExport {
  readonly variadic: boolean
  readonly contract: ForeignContract.ForeignContract
  readonly symbol: string
  readonly parameters: ReadonlyArray<CAbi.TypeText>
  readonly result: CAbi.TypeText
}

/** One native C data symbol retained by an artifact, with its scalar C class. */
export interface ForeignStatic {
  readonly symbol: string
  readonly type: CAbi.TypeText
  readonly direction: 'Import' | 'Export'
}

interface ArtifactBase {
  readonly module: string
  readonly backend: Id
  readonly target: Target.Target
  readonly symbols: ReadonlyArray<SymbolEntry>
  readonly termination: Termination
  readonly nativeRuntimeSymbols: ReadonlyArray<string>
  readonly runtimeFeatures: ReadonlyArray<RuntimeFeature>
  /** Reachable foreign symbols sorted by symbol. */
  readonly foreignImports: ReadonlyArray<ForeignImport>
  /** Exported C-callable symbols with their classified signatures, sorted by symbol. */
  readonly foreignExports: ReadonlyArray<ForeignExport>
  /** Imported and exported C data symbols, sorted by symbol and direction. */
  readonly foreignStatics: ReadonlyArray<ForeignStatic>
  readonly control: ReadonlyArray<ControlProvenance>
}

export interface LlvmBitcodeArtifact extends ArtifactBase {
  readonly _tag: 'LlvmBitcodeArtifact'
  readonly backend: 'llvm'
  readonly bitcode: Uint8Array
  readonly ir: string
}

export type Artifact = LlvmBitcodeArtifact

export interface ModuleViolation {
  readonly function: string
  readonly message: string
  readonly detail: ReadonlyArray<string>
}

export const formatModuleViolations = (
  violations: ReadonlyArray<ModuleViolation>,
  limit = 5,
): string => {
  const shown = violations
    .slice(0, limit)
    .map((violation) =>
      [`${violation.message} (in ${violation.function})`, ...violation.detail].join('\n'),
    )
  const remaining = violations.length - shown.length
  return [...shown, ...(remaining > 0 ? [`... and ${remaining} more violation(s)`] : [])].join('\n')
}

export class BackendError extends Data.TaggedError('BackendError')<{
  readonly operation: 'Backend.emit'
  readonly backend: string
  readonly message: string
  readonly reason:
    | { readonly _tag: 'InvalidMir'; readonly violations: ReadonlyArray<Mir.Violation> }
    | { readonly _tag: 'InvalidModule'; readonly violations: ReadonlyArray<ModuleViolation> }
    | { readonly _tag: 'UnsupportedMir'; readonly detail: string }
    | { readonly _tag: 'UnsupportedTarget'; readonly target: Target.Id }
    | {
        readonly _tag: 'UnsupportedIntrinsic'
        readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
      }
    | {
        readonly _tag: 'UnsupportedForeignFunction'
        readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
      }
    | { readonly _tag: 'ForeignSymbolConflict'; readonly symbol: string }
    | { readonly _tag: 'WrappedFailure'; readonly cause: unknown }
}> {}

export interface Backend<A extends Artifact = Artifact> {
  readonly _tag: 'Backend'
  readonly id: Id
  readonly name: string
  readonly targets: ReadonlyArray<Target.Id>
  readonly emit: (program: Mir.Module, request: CodegenRequest) => Effect.Effect<A, BackendError>
}

/** Validates shared MIR/target invariants before dispatching to one backend implementation. */
export const emit = Effect.fn('Backend.emit')(function* <A extends Artifact>(
  self: Backend<A>,
  program: Mir.Module,
  request: CodegenRequest,
): Effect.fn.Return<A, BackendError> {
  const violations = MirVerification.verify(program)
  if (violations.length > 0) {
    return yield* new BackendError({
      operation: 'Backend.emit',
      backend: self.name,
      message: `${self.name} cannot emit invalid MIR`,
      reason: { _tag: 'InvalidMir', violations },
    })
  }
  const availability = IntrinsicAvailability.select(program.intrinsics, program.layout.target)
  if (availability._tag === 'Unavailable') {
    return yield* new BackendError({
      operation: 'Backend.emit',
      backend: self.name,
      message: `${self.name} cannot emit a program with unavailable intrinsics`,
      reason: { _tag: 'UnsupportedIntrinsic', diagnostics: availability.diagnostics },
    })
  }
  const foreign = ForeignAvailability.select(
    program.foreignCalls,
    program.layout.target,
    program.foreignStatics,
    ForeignAvailability.callbackAddresses(program),
    ForeignAvailability.staticLoads(program),
  )
  const planning = ForeignPlanning.check(program, program.layout.target)
  if (foreign.length > 0 || planning.length > 0) {
    return yield* new BackendError({
      operation: 'Backend.emit',
      backend: self.name,
      message: `${self.name} cannot emit a program with unavailable foreign functions`,
      reason: { _tag: 'UnsupportedForeignFunction', diagnostics: [...foreign, ...planning] },
    })
  }
  if (!self.targets.includes(program.layout.target.id)) {
    return yield* new BackendError({
      operation: 'Backend.emit',
      backend: self.name,
      message: `${self.name} does not support target ${program.layout.target.id}`,
      reason: { _tag: 'UnsupportedTarget', target: program.layout.target.id },
    })
  }
  return yield* self.emit(program, request)
})

/** Every source-level function that can appear in a logical failure path, in stable ordinal order. */
export const logicalFrameEntries = (
  program: Mir.Module,
): ReadonlyArray<{ readonly fn: Mir.MirFunction; readonly frame: TerminationModel.LogicalFrame }> =>
  Object.freeze(
    program.functions.flatMap((fn) => {
      if (fn.id.name === '$effect-entry' || fn.id.name === '$unit-entry') return []
      const region = fn.regions.find((candidate) => candidate.id.ordinal === fn.entry.ordinal)
      let provenance: SourceSpan.SourceSpan | undefined
      if (region?._tag === 'OperationRegion') {
        provenance = region.operations.at(0)?.provenance.span ?? region.outcome.provenance.span
      } else if (region?._tag === 'CleanupRegion') {
        provenance = region.releases.at(0)?.provenance.span ?? region.outcome.provenance.span
      } else {
        provenance = region?.provenance.span
      }
      return provenance === undefined
        ? []
        : [Object.freeze({ fn, frame: Object.freeze({ function: fn.id, provenance }) })]
    }),
  )

export const terminationOf = (
  program: Mir.Module,
  report: TerminationModel.Report = TerminationModel.emptyReport,
): Termination => {
  if (program.entry._tag === 'UnavailableEntry') {
    throw new RangeError(`Cannot emit unavailable entry: ${program.entry.reason}`)
  }
  const logicalFrames = Object.freeze(logicalFrameEntries(program).map((entry) => entry.frame))
  return Object.freeze({
    _tag: 'EntryTermination',
    success:
      program.entry._tag !== 'NoInvocation' &&
      program.entry._tag === 'OrdinaryEntry' &&
      program.entry.machine.declaration.name !== '$unit-entry'
        ? 'ReturnedStatus'
        : 'Zero',
    failures:
      program.entry._tag === 'EffectEntry'
        ? Object.freeze(
            program.entry.failures.map(({ tag, identity }) => Object.freeze({ tag, identity })),
          )
        : Object.freeze([]),
    logicalFrames,
    report,
  })
}

export const sanitize = (name: string): string => name.replace(/[^A-Za-z0-9_]/g, '_')

const injectivePart = (value: string): string => {
  const bytes = new TextEncoder().encode(value)
  return `${bytes.length}_${Array.from(bytes, (byte) => byte.toString(16).padStart(2, '0')).join(
    '',
  )}`
}

export const symbolFor = (fn: Mir.MirFunction, entry: Instances.InstanceKey | undefined): string =>
  entry !== undefined && Mir.matchesInstanceKey(fn, entry)
    ? 'silk_main'
    : `silk_${sanitize(fn.id.module)}_${sanitize(fn.id.name)}__${[
        fn.instance.declaration.module,
        fn.instance.declaration.name,
        ...Type.runtimeArgumentKeys(fn.instance.typeArguments),
        ...fn.instance.staticArguments.map(StaticValue.key),
        ...fn.instance.contractRow,
      ]
        .map(injectivePart)
        .join('_')}`

export const suspensionPointKey = (point: Mir.SuspensionPointId): string =>
  `${Instances.keyText(point.owner)}\u0000${point.sourceId}\u0000${point.spanStart}\u0000${point.spanEnd}\u0000${point.ordinal}`

export interface LineTable {
  readonly lineStarts: ReadonlyArray<number>
}

export const lineTable = (bytes: Uint8Array | undefined): LineTable => {
  const lineStarts = [0]
  if (bytes !== undefined) {
    for (let index = 0; index < bytes.length; index += 1) {
      if (bytes[index] === 0x0a) lineStarts.push(index + 1)
    }
  }
  return { lineStarts }
}

export const positionOf = (
  table: LineTable,
  offset: number,
): { readonly line: number; readonly column: number } => {
  let line = 0
  while (line + 1 < table.lineStarts.length && (table.lineStarts[line + 1] ?? 0) <= offset) {
    line += 1
  }
  return { line: line + 1, column: offset - (table.lineStarts[line] ?? 0) + 1 }
}
