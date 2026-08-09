import * as Alignment from '@silk-effect/llvm/Alignment'
import * as Bitcode from '@silk-effect/llvm/Bitcode'
import * as LlvmBlock from '@silk-effect/llvm/Block'
import * as Builder from '@silk-effect/llvm/Builder'
import * as Constant from '@silk-effect/llvm/Constant'
import * as DISPFlags from '@silk-effect/llvm/DISPFlags'
import * as FunctionActor from '@silk-effect/llvm/Function'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import * as Intrinsic from '@silk-effect/llvm/Intrinsic'
import * as IrText from '@silk-effect/llvm/IrText'
import type * as LlvmError from '@silk-effect/llvm/LlvmError'
import * as LlvmMetadata from '@silk-effect/llvm/Metadata'
import * as LlvmType from '@silk-effect/llvm/Type'
import * as Value from '@silk-effect/llvm/Value'
import * as Variable from '@silk-effect/llvm/Variable'
import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as FloatingPoint from './FloatingPoint.js'
import type * as Instances from './Instances.js'
import * as Layout from './Layout.js'
import type * as Match from './Match.js'
import * as Mir from './Mir.js'
import type * as Ownership from './Ownership.js'
import * as Scalar from './Scalar.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Target from './Target.js'
import * as Transcendental from './Transcendental.js'
import * as SilkType from './Type.js'

/**
 * Field paths to every reclaim context inside one plan, or undefined when the plan holds
 * shapes (hooks, arrays, nested unions, callables) that conditional union cleanup cannot lower.
 */
const reclaimContextPaths = (
  plan: Ownership.CleanupPlan,
  prefix: ReadonlyArray<DeclarationIndex.FieldId> = [],
): ReadonlyArray<ReadonlyArray<DeclarationIndex.FieldId>> | undefined => {
  switch (plan._tag) {
    case 'NoCleanup':
    case 'ParameterCleanup':
      return []
    case 'AllocationCleanup':
    case 'RawBufferCleanup':
      return [prefix]
    case 'StructCleanup': {
      const collected: Array<ReadonlyArray<DeclarationIndex.FieldId>> = []
      for (const field of plan.fields) {
        const nested = reclaimContextPaths(field.cleanup, [...prefix, field.field])
        if (nested === undefined) return undefined
        collected.push(...nested)
      }
      return collected
    }
    default:
      return undefined
  }
}

/** Tests whether one cleanup plan consumes a reclaim ticket at any nesting depth. */
const planReclaims = (plan: Ownership.CleanupPlan): boolean =>
  plan._tag === 'AllocationCleanup' ||
  plan._tag === 'RawBufferCleanup' ||
  (plan._tag === 'HookCleanup' && planReclaims(plan.inner)) ||
  (plan._tag === 'StructCleanup' && plan.fields.some((field) => planReclaims(field.cleanup))) ||
  (plan._tag === 'ArrayCleanup' && planReclaims(plan.element)) ||
  (plan._tag === 'UnionCleanup' && plan.cases.some((entry) => planReclaims(entry.cleanup))) ||
  (plan._tag === 'CallableCleanup' && plan.slots.some((slot) => planReclaims(slot.cleanup)))

/**
 * Code generation as a nominal `Backend` service: one operation consuming the whole
 * monomorphized MIR program plus its compiler-owned target/layout plan and codegen request,
 * producing one program artifact. The bootstrap `LlvmBackend` lowers MIR through the Silk LLVM builder and
 * emits deterministic bitcode directly — no `libLLVM`, no LLVM C API, no compiler-private
 * native FFI. Textual LLVM IR is an implementation-specific inspection artifact.
 */

/** One codegen request. Debug builds emit native LLVM debug metadata. */
export interface CodegenRequest {
  readonly mode: 'debug' | 'release'
  readonly sources?: ReadonlyMap<string, Uint8Array>
}

/** One instance's deterministic native symbol. */
export interface SymbolEntry {
  readonly declaration: DeclarationIndex.CanonicalId
  readonly instance: Mir.MirFunction['instance']
  readonly symbol: string
}

/** One backend-local control construct traced back to its canonical MIR region and source span. */
export interface ControlProvenance {
  readonly _tag: 'BackendControlProvenance'
  readonly backend: 'LLVM' | 'WebAssembly'
  readonly function: DeclarationIndex.CanonicalId
  readonly instance: Mir.MirFunction['instance']
  readonly region: Mir.RegionId
  readonly construct:
    | 'LlvmJump'
    | 'LlvmBranch'
    | 'LlvmReturn'
    | 'LlvmTrap'
    | 'WasmIf'
    | 'WasmLoop'
    | 'WasmBr'
    | 'WasmReturn'
    | 'WasmTrap'
  readonly targets: ReadonlyArray<Mir.RegionId>
  readonly loop?: Mir.LoopId
  readonly span: SourceSpan.SourceSpan
}

/** Stable public backend identifiers used by manifests and command-line selection. */
export type Id = 'llvm' | 'wasm'

/** Process-facing interpretation of the scalar `silk_main` result. */
export type Termination =
  | { readonly _tag: 'PassThrough' }
  | { readonly _tag: 'EffectReports'; readonly reports: ReadonlyArray<string> }

interface ArtifactBase {
  readonly module: string
  readonly backend: Id
  readonly target: Target.Target
  readonly symbols: ReadonlyArray<SymbolEntry>
  readonly termination: Termination
  readonly control: ReadonlyArray<ControlProvenance>
}

/** Retains the ordered report identities paired with normalized one-based failure tags. */
export const terminationOf = (program: Mir.Module): Termination =>
  program.entry._tag === 'UnavailableEntry'
    ? (() => {
        throw new RangeError(`Cannot emit unavailable entry: ${program.entry.reason}`)
      })()
    : program.entry._tag === 'OrdinaryEntry'
      ? Object.freeze({ _tag: 'PassThrough' })
      : Object.freeze({
          _tag: 'EffectReports',
          reports: Object.freeze(program.entry.failures.map((failure) => failure.report)),
        })

/** Deterministic LLVM bitcode requiring target-specific durable finalization. */
export interface LlvmBitcodeArtifact extends ArtifactBase {
  readonly _tag: 'LlvmBitcodeArtifact'
  readonly backend: 'llvm'
  readonly bitcode: Uint8Array
  readonly ir: string
}

/** A validated final WebAssembly module that can be committed without an external linker. */
export interface WebAssemblyModuleArtifact extends ArtifactBase {
  readonly _tag: 'WebAssemblyModuleArtifact'
  readonly backend: 'wasm'
  readonly bytes: Uint8Array
  readonly wat: string
  readonly hostImports: ReadonlyArray<{
    readonly module: string
    readonly name: string
  }>
}

/** The closed family of backend program artifacts. */
export type Artifact = LlvmBitcodeArtifact | WebAssemblyModuleArtifact

/** An expected failure at the backend boundary. */
export class BackendError extends Data.TaggedError('BackendError')<{
  readonly operation: 'Backend.emit'
  readonly backend: string
  readonly message: string
  readonly reason:
    | { readonly _tag: 'InvalidMir'; readonly violations: ReadonlyArray<Mir.Violation> }
    | { readonly _tag: 'UnsupportedMir'; readonly detail: string }
    | { readonly _tag: 'UnsupportedTarget'; readonly target: Target.Id }
    | { readonly _tag: 'WrappedFailure'; readonly cause: unknown }
}> {}

/** The nominal backend service contract. */
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
  const violations = Mir.verify(program)
  if (violations.length > 0) {
    return yield* new BackendError({
      operation: 'Backend.emit',
      backend: self.name,
      message: `${self.name} cannot emit invalid MIR`,
      reason: { _tag: 'InvalidMir', violations },
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

const sanitize = (name: string): string => name.replace(/[^A-Za-z0-9_]/g, '_')

const injectivePart = (value: string): string => {
  const bytes = new TextEncoder().encode(value)
  return `${bytes.length}_${Array.from(bytes, (byte) => byte.toString(16).padStart(2, '0')).join('')}`
}

/** The explicit machine entry is stable; every other symbol encodes its concrete instance key. */
export const symbolFor = (fn: Mir.MirFunction, entry: Instances.InstanceKey): string =>
  Mir.matchesInstanceKey(fn, entry)
    ? 'silk_main'
    : `silk_${sanitize(fn.id.module)}_${sanitize(fn.id.name)}__${[
        fn.instance.declaration.module,
        fn.instance.declaration.name,
        ...fn.instance.typeArguments.map(SilkType.key),
        ...fn.instance.contractRow,
      ]
        .map(injectivePart)
        .join('_')}`

interface LineTable {
  readonly lineStarts: ReadonlyArray<number>
}

const lineTable = (bytes: Uint8Array | undefined): LineTable => {
  const lineStarts = [0]
  if (bytes !== undefined) {
    for (let index = 0; index < bytes.length; index += 1) {
      if (bytes[index] === 0x0a) lineStarts.push(index + 1)
    }
  }
  return { lineStarts }
}

const positionOf = (table: LineTable, offset: number): { line: number; column: number } => {
  let line = 0
  while (line + 1 < table.lineStarts.length && (table.lineStarts[line + 1] ?? 0) <= offset) {
    line += 1
  }
  return { line: line + 1, column: offset - (table.lineStarts[line] ?? 0) + 1 }
}

type LinearTerminator =
  | { readonly _tag: 'Return'; readonly value: Mir.LocalId; readonly provenance: Mir.Provenance }
  | { readonly _tag: 'Jump'; readonly target: Mir.RegionId; readonly provenance: Mir.Provenance }
  | {
      readonly _tag: 'Branch'
      readonly condition: Mir.LocalId
      readonly taken: Mir.RegionId
      readonly otherwise: Mir.RegionId
      readonly provenance: Mir.Provenance
    }
  | {
      readonly _tag: 'MatchBranch'
      readonly scrutinee: Mir.LocalId
      readonly memberOrdinal: number
      readonly taken: Mir.RegionId
      readonly otherwise: Mir.RegionId
      readonly provenance: Mir.Provenance
    }
  | { readonly _tag: 'Trap'; readonly reason: string; readonly provenance: Mir.Provenance }

type LinearOperation =
  | Exclude<Mir.Operation, { readonly _tag: 'Match' }>
  | {
      readonly _tag: 'BindMatch'
      readonly scrutinee: Mir.LocalId
      readonly shape: Layout.CallingShape
      readonly member: SilkType.Nominal
      readonly binding: Mir.MatchBinding
      readonly provenance: Mir.Provenance
    }

interface LinearBlock {
  readonly id: Mir.RegionId
  readonly origin: Mir.RegionId
  readonly kind: 'Normal' | 'Cleanup'
  readonly operations: ReadonlyArray<LinearOperation>
  readonly terminator: LinearTerminator
}

interface StructuredBlock {
  readonly id: Mir.RegionId
  readonly origin: Mir.RegionId
  readonly kind: 'Normal' | 'Cleanup'
  readonly operations: ReadonlyArray<Mir.Operation>
  readonly terminator: LinearTerminator
}

const destinationOf = (operation: LinearOperation): Mir.LocalId | undefined => {
  switch (operation._tag) {
    case 'Literal':
    case 'Binary':
    case 'ConvertInteger':
    case 'CheckedInteger':
    case 'Move':
    case 'BeginLoan':
    case 'SliceLength':
    case 'ConvertUnion':
    case 'Call':
    case 'MakeEffect':
    case 'MakeCallable':
    case 'ApplyCallable':
    case 'PackEffectOutcome':
    case 'UnpackEffectSuccess':
    case 'CatchEffect':
    case 'RetryEffect':
    case 'RunEffect':
    case 'RunEffectValue':
    case 'CloseEffectEntry':
    case 'Construct':
    case 'ConstructArray':
    case 'Project':
    case 'ReadPlace':
    case 'ValidateLayout':
    case 'RepeatLayout':
    case 'Allocate':
    case 'StandardStreamWrite':
    case 'RawBufferFrom':
    case 'RawBufferCount':
    case 'RawBufferSlot':
    case 'SlotWrite':
    case 'SlotTake':
    case 'SlotCopy':
    case 'SlotDrop':
      return operation.destination
    case 'BindMatch':
      return operation.binding.destination
    case 'CheckPlace':
    case 'WritePlace':
    case 'EndLoan':
    case 'Drop':
      return undefined
  }
}

const opensRuntimeContinuation = (operation: LinearOperation): boolean =>
  operation._tag === 'Allocate' ||
  operation._tag === 'StandardStreamWrite' ||
  operation._tag === 'RawBufferFrom' ||
  operation._tag === 'RawBufferSlot' ||
  operation._tag === 'CatchEffect' ||
  operation._tag === 'RetryEffect' ||
  operation._tag === 'RunEffect' ||
  operation._tag === 'RunEffectValue' ||
  operation._tag === 'CloseEffectEntry' ||
  (operation._tag === 'Binary' &&
    operation.operator !== 'Equals' &&
    operation.operator !== 'NotEquals' &&
    operation.operator !== 'LessThan' &&
    operation.operator !== 'LessOrEqual' &&
    operation.operator !== 'GreaterThan' &&
    operation.operator !== 'GreaterOrEqual') ||
  ((operation._tag === 'ReadPlace' ||
    operation._tag === 'CheckPlace' ||
    operation._tag === 'WritePlace') &&
    operation.selectors.some(
      (selector) =>
        selector._tag === 'SliceElementSelector' ||
        (selector._tag === 'ElementSelector' && selector.index._tag === 'Runtime'),
    ))

const expandMatches = (
  fn: Mir.MirFunction,
  input: ReadonlyArray<StructuredBlock>,
): ReadonlyArray<LinearBlock> => {
  let nextRegion = Math.max(-1, ...fn.regions.map((region) => region.id.ordinal)) + 1
  const reserve = (): Mir.RegionId => Object.freeze({ _tag: 'Region', ordinal: nextRegion++ })
  const blocks: Array<LinearBlock> = []
  const jump = (target: Mir.RegionId, provenance: Mir.Provenance): LinearTerminator =>
    Object.freeze({ _tag: 'Jump', target, provenance })

  const lowerSequence = (
    id: Mir.RegionId,
    origin: Mir.RegionId,
    kind: LinearBlock['kind'],
    operations: ReadonlyArray<Mir.Operation | LinearOperation>,
    terminator: LinearTerminator,
  ): void => {
    const matchIndex = operations.findIndex((operation) => operation._tag === 'Match')
    if (matchIndex < 0) {
      blocks.push(
        Object.freeze({
          id,
          origin,
          kind,
          operations: Object.freeze(operations as ReadonlyArray<LinearOperation>),
          terminator,
        }),
      )
      return
    }
    const match = operations.at(matchIndex)
    if (match?._tag !== 'Match') throw new RangeError('LLVM match expansion lost its operation')
    const dispatch = reserve()
    const following = reserve()
    blocks.push(
      Object.freeze({
        id,
        origin,
        kind,
        operations: Object.freeze(
          operations.slice(0, matchIndex) as ReadonlyArray<LinearOperation>,
        ),
        terminator: jump(dispatch, match.provenance),
      }),
    )
    lowerSequence(following, origin, kind, operations.slice(matchIndex + 1), terminator)

    const trap = reserve()
    blocks.push(
      Object.freeze({
        id: trap,
        origin,
        kind: 'Normal',
        operations: Object.freeze([]),
        terminator: Object.freeze({
          _tag: 'Trap',
          reason: 'exhaustive match rejected every candidate',
          provenance: match.provenance,
        }),
      }),
    )

    const candidateEntry = (
      member: SilkType.Nominal,
      candidates: ReadonlyArray<Match.ArmId>,
      ordinal: number,
    ): Mir.RegionId => {
      const entry = reserve()
      const candidate = candidates.at(ordinal)
      if (candidate === undefined) {
        blocks.push(
          Object.freeze({
            id: entry,
            origin,
            kind: 'Normal',
            operations: Object.freeze([]),
            terminator: jump(trap, match.provenance),
          }),
        )
        return entry
      }
      const arm = match.arms.find((item) => item.id.ordinal === candidate.ordinal)
      if (arm === undefined) throw new RangeError('LLVM match expansion lost a candidate arm')
      const bindings: ReadonlyArray<LinearOperation> = Object.freeze(
        arm.bindings.map((binding) =>
          Object.freeze({
            _tag: 'BindMatch' as const,
            scrutinee: match.scrutinee,
            shape: match.scrutineeShape,
            member,
            binding,
            provenance: binding.provenance,
          }),
        ),
      )
      const selected = reserve()
      lowerSequence(
        selected,
        origin,
        'Normal',
        [
          ...arm.selected.operations,
          Object.freeze({
            _tag: 'Move' as const,
            destination: match.destination,
            source: arm.selected.result,
            provenance: arm.provenance,
          }),
        ],
        jump(following, arm.provenance),
      )
      if (arm.guard === undefined) {
        lowerSequence(entry, origin, 'Normal', bindings, jump(selected, arm.provenance))
      } else {
        const fallback = candidateEntry(member, candidates, ordinal + 1)
        lowerSequence(
          entry,
          origin,
          'Normal',
          [...bindings, ...arm.guard.operations],
          Object.freeze({
            _tag: 'Branch',
            condition: arm.guard.result,
            taken: selected,
            otherwise: fallback,
            provenance: arm.provenance,
          }),
        )
      }
      return entry
    }

    const decisionEntries = match.decisions.map((decision) =>
      candidateEntry(decision.member, decision.candidates, 0),
    )
    if (match.scrutineeType._tag === 'Nominal') {
      const selected = decisionEntries.at(0) ?? trap
      blocks.push(
        Object.freeze({
          id: dispatch,
          origin,
          kind: 'Normal',
          operations: Object.freeze([]),
          terminator: jump(selected, match.provenance),
        }),
      )
      return
    }
    const dispatchIds = match.decisions.map((_, ordinal) => (ordinal === 0 ? dispatch : reserve()))
    match.decisions.forEach((_, ordinal) => {
      blocks.push(
        Object.freeze({
          id: dispatchIds.at(ordinal) ?? dispatch,
          origin,
          kind: 'Normal',
          operations: Object.freeze([]),
          terminator: Object.freeze({
            _tag: 'MatchBranch',
            scrutinee: match.scrutinee,
            memberOrdinal: ordinal,
            taken: decisionEntries.at(ordinal) ?? trap,
            otherwise: dispatchIds.at(ordinal + 1) ?? trap,
            provenance: match.provenance,
          }),
        }),
      )
    })
  }

  for (const block of input) {
    lowerSequence(block.id, block.origin, block.kind, block.operations, block.terminator)
  }
  const byId = new Map(blocks.map((block) => [block.id.ordinal, block] as const))
  const visited = new Set<number>()
  const ordered: Array<LinearBlock> = []
  const visit = (id: Mir.RegionId): void => {
    if (visited.has(id.ordinal)) return
    visited.add(id.ordinal)
    const block = byId.get(id.ordinal)
    if (block === undefined) return
    ordered.push(block)
    const terminator = block.terminator
    if (terminator._tag === 'Jump') visit(terminator.target)
    if (terminator._tag === 'Branch' || terminator._tag === 'MatchBranch') {
      visit(terminator.taken)
      visit(terminator.otherwise)
    }
  }
  visit(fn.entry)
  for (const block of [...blocks].sort((left, right) => left.id.ordinal - right.id.ordinal)) {
    visit(block.id)
  }
  return Object.freeze(ordered)
}

/** LLVM-private flattening of the compiler-owned DAG. Repeat is the only source of a back-edge. */
const linearize = (fn: Mir.MirFunction): ReadonlyArray<LinearBlock> => {
  const loops = new Map(
    fn.regions.flatMap((region) =>
      region._tag === 'LoopRegion' ? [[region.loop.ordinal, region] as const] : [],
    ),
  )
  const conditionOwners = new Map(
    fn.regions.flatMap((region) =>
      region._tag === 'LoopRegion' ? [[region.condition.ordinal, region] as const] : [],
    ),
  )
  const outcome = (region: Mir.OperationRegion | Mir.CleanupRegion): LinearTerminator => {
    const value = region.outcome
    switch (value._tag) {
      case 'Forward':
      case 'Return':
      case 'Trap':
        return value._tag === 'Forward'
          ? Object.freeze({ _tag: 'Jump', target: value.target, provenance: value.provenance })
          : value
      case 'Repeat': {
        const loop = loops.get(value.loop.ordinal)
        if (loop === undefined) throw new RangeError('LLVM linearizer lost repeat loop')
        return Object.freeze({ _tag: 'Jump', target: loop.id, provenance: value.provenance })
      }
      case 'Exit': {
        const loop = loops.get(value.loop.ordinal)
        if (loop === undefined) throw new RangeError('LLVM linearizer lost exit loop')
        return Object.freeze({ _tag: 'Jump', target: loop.following, provenance: value.provenance })
      }
      case 'Yield': {
        const loop = conditionOwners.get(region.id.ordinal)
        if (loop === undefined) throw new RangeError('LLVM linearizer found unowned yield')
        return Object.freeze({
          _tag: 'Branch',
          condition: loop.conditionValue,
          taken: loop.body,
          otherwise: loop.following,
          provenance: value.provenance,
        })
      }
    }
  }
  const raw = Mir.topologicalRegions(fn).map((region): StructuredBlock => {
    if (region._tag === 'ConditionalRegion') {
      return Object.freeze({
        id: region.id,
        origin: region.id,
        kind: 'Normal',
        operations: Object.freeze([]),
        terminator: Object.freeze({
          _tag: 'Branch',
          condition: region.condition,
          taken: region.taken,
          otherwise: region.otherwise,
          provenance: region.provenance,
        }),
      })
    }
    if (region._tag === 'LoopRegion') {
      return Object.freeze({
        id: region.id,
        origin: region.id,
        kind: 'Normal',
        operations: Object.freeze([]),
        terminator: Object.freeze({
          _tag: 'Jump',
          target: region.condition,
          provenance: region.provenance,
        }),
      })
    }
    return Object.freeze({
      id: region.id,
      origin: region.id,
      kind: region._tag === 'CleanupRegion' ? 'Cleanup' : 'Normal',
      operations: region._tag === 'CleanupRegion' ? region.releases : region.operations,
      terminator: outcome(region),
    })
  })
  const incoming = new Map<number, number>()
  for (const edge of Mir.controlEdges(fn)) {
    incoming.set(edge.to.ordinal, (incoming.get(edge.to.ordinal) ?? 0) + 1)
  }
  const byId = new Map(raw.map((block) => [block.id.ordinal, block] as const))
  const regionsById = new Map(fn.regions.map((region) => [region.id.ordinal, region] as const))
  const inlined = new Set<number>()
  const blocks = raw.map((block): StructuredBlock => {
    let operations = [...block.operations]
    let terminator = block.terminator
    const seen = new Set<number>()
    while (terminator._tag === 'Jump' && !seen.has(terminator.target.ordinal)) {
      const target = byId.get(terminator.target.ordinal)
      const targetRegion = regionsById.get(terminator.target.ordinal)
      const inlineable =
        target !== undefined &&
        incoming.get(target.id.ordinal) === 1 &&
        (target.kind === 'Cleanup' || targetRegion?._tag === 'ConditionalRegion')
      if (!inlineable) break
      seen.add(target.id.ordinal)
      inlined.add(target.id.ordinal)
      operations = [...operations, ...target.operations]
      terminator = target.terminator
    }
    return Object.freeze({ ...block, operations: Object.freeze(operations), terminator })
  })
  const referenced = new Set<number>([fn.entry.ordinal])
  // Structural incoming counts do not include every edge introduced by outcome lowering. Keep an
  // inline candidate when the rewritten graph still names it; otherwise a valid branch can target
  // a block removed by this private optimization.
  for (const block of blocks) {
    const terminator = block.terminator
    if (terminator._tag === 'Jump') referenced.add(terminator.target.ordinal)
    if (terminator._tag === 'Branch' || terminator._tag === 'MatchBranch') {
      referenced.add(terminator.taken.ordinal)
      referenced.add(terminator.otherwise.ordinal)
    }
  }
  return expandMatches(
    fn,
    Object.freeze(
      blocks.filter((block) => !inlined.has(block.id.ordinal) || referenced.has(block.id.ordinal)),
    ),
  )
}

const llvmControl = (program: Mir.Module): ReadonlyArray<ControlProvenance> =>
  Object.freeze(
    program.functions.flatMap((fn) =>
      (() => {
        const linear = linearize(fn)
        const originOf = (target: Mir.RegionId): Mir.RegionId | undefined =>
          linear.find((candidate) => candidate.id.ordinal === target.ordinal)?.origin
        return linear.map((block): ControlProvenance => {
          const terminator = block.terminator
          const targets =
            terminator._tag === 'Jump'
              ? [originOf(terminator.target)]
              : terminator._tag === 'Branch' || terminator._tag === 'MatchBranch'
                ? [originOf(terminator.taken), originOf(terminator.otherwise)]
                : []
          const canonicalTargets = Object.freeze(
            targets.flatMap((target) =>
              target === undefined || target.ordinal === block.origin.ordinal ? [] : [target],
            ),
          )
          return Object.freeze({
            _tag: 'BackendControlProvenance',
            backend: 'LLVM',
            function: fn.id,
            instance: fn.instance,
            region: block.origin,
            construct:
              terminator._tag === 'Jump'
                ? 'LlvmJump'
                : terminator._tag === 'Branch' || terminator._tag === 'MatchBranch'
                  ? 'LlvmBranch'
                  : terminator._tag === 'Return'
                    ? 'LlvmReturn'
                    : 'LlvmTrap',
            targets: canonicalTargets,
            span: terminator.provenance.span,
          })
        })
      })(),
    ),
  )

const functionStart = (fn: Mir.MirFunction): number => {
  const region = fn.regions.find((candidate) => candidate.id.ordinal === fn.entry.ordinal)
  if (region === undefined) return 0
  if (region._tag === 'ConditionalRegion' || region._tag === 'LoopRegion') {
    return region.provenance.span.start
  }
  return (
    (region._tag === 'OperationRegion' ? region.operations : region.releases).at(0)?.provenance.span
      .start ?? region.outcome.provenance.span.start
  )
}

const emitProgram = (program: Mir.Module, request: CodegenRequest) =>
  Effect.gen(function* () {
    const i32Layout = Layout.entry(program.layout, 'i32')
    if (i32Layout === undefined || i32Layout.representation._tag !== 'SignedInteger') {
      return yield* new BackendError({
        operation: 'Backend.emit',
        backend: 'LLVM',
        message: 'LLVM requires the planned i32 representation',
        reason: { _tag: 'InvalidMir', violations: Mir.verify(program) },
      })
    }
    const scalarBits = i32Layout.representation.bits
    const builder = yield* Builder.make({
      sourceFilename: program.module,
      targetTriple: program.layout.target.triple,
      strip: request.mode !== 'debug',
    })
    const i32 = yield* LlvmType.integer(builder, scalarBits)
    const usesScalar = (spelling: Scalar.Spelling): boolean =>
      program.layout.callingShapes.some((shape) =>
        shape.lanes.some((lane) => lane.type === spelling),
      )
    // LLVM assigns type-table identities in creation order, so preserve byte-for-byte output for
    // programs that do not use floating-point values by creating these types only when required.
    const f32 = usesScalar('f32') ? yield* LlvmType.float(builder) : i32
    const f64 = usesScalar('f64') ? yield* LlvmType.double(builder) : i32
    const usizeLayout = Layout.entry(program.layout, 'usize')
    const usizeType =
      usizeLayout?.representation._tag === 'UnsignedInteger'
        ? yield* LlvmType.integer(builder, usizeLayout.representation.bits)
        : undefined
    const integerTypes = new Map<number, LlvmType.Type>([[32, i32]])
    if (usizeLayout?.representation._tag === 'UnsignedInteger' && usizeType !== undefined) {
      integerTypes.set(usizeLayout.representation.bits, usizeType)
    }
    for (const bits of [8, 16, 64] as const) {
      if (!integerTypes.has(bits)) integerTypes.set(bits, yield* LlvmType.integer(builder, bits))
    }
    const hasAddressLane =
      (program.staticData?.length ?? 0) > 0 ||
      program.functions.some((fn) =>
        Mir.operations(fn).some(
          (operation) =>
            operation._tag === 'Allocate' ||
            operation._tag === 'RawBufferFrom' ||
            operation._tag === 'RawBufferCount' ||
            operation._tag === 'RawBufferSlot' ||
            operation._tag === 'SlotWrite' ||
            operation._tag === 'SlotTake' ||
            operation._tag === 'SlotCopy' ||
            operation._tag === 'SlotDrop',
        ),
      ) ||
      program.layout.callingShapes.some((shape) =>
        shape.lanes.some((lane) => typeof lane.type !== 'string'),
      ) ||
      program.layout.effectEnvironments.some(
        (environment) =>
          environment._tag === 'EffectEnvironment' &&
          environment.fields.some((field) => field.representation === 'Borrow'),
      ) ||
      program.layout.callableEnvironments.some(
        (environment) =>
          environment._tag === 'CallableEnvironment' &&
          environment.fields.some((field) => field.representation === 'Borrow'),
      )
    const i8 = hasAddressLane ? yield* LlvmType.integer(builder, 8) : i32
    const pointer = hasAddressLane ? yield* LlvmType.pointer(builder) : i32
    const staticPointers = new Map<string, Constant.Constant>()
    for (const [ordinal, data] of (program.staticData ?? []).entries()) {
      const storageType = yield* LlvmType.array(builder, i8, data.bytes.length)
      const initializer = yield* Constant.string(builder, Uint8Array.from(data.bytes))
      const variable = yield* Variable.make(builder, `silk.static.${ordinal}`, storageType, {
        initializer,
        constant: true,
        linkage: 'internal',
        unnamedAddress: 'unnamed_addr',
      })
      staticPointers.set(
        data.id,
        yield* Constant.fromGlobal(builder, yield* Variable.global(builder, variable)),
      )
    }
    let voidType: LlvmType.Type | undefined
    const lanesFor = (type: Mir.Type): ReadonlyArray<Layout.CallingLane> => {
      if (type._tag === 'EffectBorrow')
        return Object.freeze([
          Object.freeze({
            _tag: 'CallingLane' as const,
            path: Object.freeze([]),
            type: Object.freeze({
              _tag: 'Address' as const,
              element: type.type,
              bits: program.layout.target.pointerSize === 4 ? 32 : 64,
            }),
          }),
        ])
      if (type._tag === 'EffectValue')
        return Layout.effectEnvironmentLanes(program.layout, type.environment)
      if (type._tag === 'CallableValue')
        return type.environment === undefined
          ? Object.freeze([])
          : Layout.callableEnvironmentLanes(program.layout, type.environment)
      const shape = Layout.callingShape(program.layout, Mir.semanticType(type))
      if (shape === undefined) {
        throw new RangeError(`LLVM backend lost calling shape for ${Mir.semanticType(type)}`)
      }
      return shape.lanes
    }
    const valueLanesFor = (type: Mir.Type): ReadonlyArray<Layout.CallingLane> => {
      if (type._tag !== 'EffectBorrow') return lanesFor(type)
      const shape = Layout.callingShape(program.layout, type.type)
      if (shape === undefined) {
        throw new RangeError(
          `LLVM backend lost borrowed calling shape for ${SilkType.encode(type.type)}`,
        )
      }
      return shape.lanes
    }
    const laneType = (lane: Layout.CallingLane): LlvmType.Type => {
      if (typeof lane.type !== 'string') return pointer
      const scalar = Scalar.find(lane.type)
      if (scalar === undefined) return i32
      if (scalar.category === 'Floating') return scalar.spelling === 'f32' ? f32 : f64
      const bits = Scalar.bits(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
      return integerTypes.get(bits) ?? i32
    }
    type OverflowSignature = {
      readonly returnType: LlvmType.Type
      readonly parameters: ReadonlyArray<LlvmType.Type>
    }
    const signedOverflowSignatures = new Map<number, OverflowSignature>()
    const unsignedOverflowSignatures = new Map<number, OverflowSignature>()
    const needsAllocation = program.functions.some((fn) =>
      Mir.operations(fn).some(
        (operation) =>
          operation._tag === 'Allocate' ||
          operation._tag === 'RawBufferFrom' ||
          operation._tag === 'RawBufferCount' ||
          operation._tag === 'RawBufferSlot' ||
          operation._tag === 'SlotWrite' ||
          operation._tag === 'SlotTake' ||
          operation._tag === 'SlotCopy' ||
          operation._tag === 'SlotDrop' ||
          (operation._tag === 'CloseEffectEntry' &&
            operation.failures.some((failure) => planReclaims(failure.cleanup))) ||
          // A witness-dispatched allocation can arrive without any local Allocate operation,
          // and its cleanup still calls the release shim — at any nesting depth of the plan.
          (operation._tag === 'Drop' && planReclaims(operation.cleanup)),
      ),
    )
    const malloc =
      needsAllocation && usizeType !== undefined
        ? yield* FunctionActor.declare(
            builder,
            'malloc',
            yield* LlvmType.functionType(builder, pointer, [usizeType]),
          )
        : undefined
    const free = needsAllocation
      ? yield* FunctionActor.declare(
          builder,
          'free',
          yield* LlvmType.functionType(builder, voidType ?? (yield* LlvmType.voidType(builder)), [
            pointer,
          ]),
        )
      : undefined
    const needsStandardStreams = program.functions.some((fn) =>
      Mir.operations(fn).some((operation) => operation._tag === 'StandardStreamWrite'),
    )
    const standardWrite =
      needsStandardStreams && usizeType !== undefined
        ? yield* FunctionActor.declare(
            builder,
            'silk_standard_stream_write_v1',
            yield* LlvmType.functionType(builder, i32, [i32, pointer, usizeType]),
          )
        : undefined

    const declared: Array<{
      readonly fn: Mir.MirFunction
      readonly symbol: string
      readonly handle: FunctionActor.Function
      readonly resultType: LlvmType.Type
      readonly resultLaneCount: number
      readonly linear: ReadonlyArray<LinearBlock>
    }> = []
    for (const fn of program.functions) {
      const resultLaneCount = lanesFor(fn.result).length
      let resultType: LlvmType.Type
      if (resultLaneCount === 0) {
        const selected = voidType ?? (yield* LlvmType.voidType(builder))
        voidType = selected
        resultType = selected
      } else if (resultLaneCount === 1) {
        const lane = lanesFor(fn.result).at(0)
        if (lane === undefined) throw new RangeError('LLVM result lost its scalar lane')
        resultType = laneType(lane)
      } else {
        resultType = yield* LlvmType.structure(builder, lanesFor(fn.result).map(laneType))
      }
      const parameters =
        fn.regions.length === 0
          ? []
          : fn.localTypes
              .slice(0, fn.parameterCount)
              .flatMap((type) => lanesFor(type).map(laneType))
      const signature = yield* LlvmType.functionType(builder, resultType, parameters)
      declared.push({
        fn,
        symbol: symbolFor(fn, Mir.machineEntry(program)),
        handle: yield* FunctionActor.declare(
          builder,
          symbolFor(fn, Mir.machineEntry(program)),
          signature,
        ),
        resultType,
        resultLaneCount,
        linear: linearize(fn),
      })
    }

    const debug = request.mode === 'debug'
    let compileUnit: LlvmMetadata.Optional
    let file: LlvmMetadata.Optional
    const table = lineTable(request.sources?.get(program.module))
    if (debug) {
      const fileName = yield* LlvmMetadata.string(builder, program.module)
      file = yield* LlvmMetadata.file(builder, fileName)
      const producer = yield* LlvmMetadata.string(builder, 'silk-effect bootstrap')
      compileUnit = yield* LlvmMetadata.compileUnit(builder, file, producer, {})
      if (compileUnit !== undefined) {
        yield* LlvmMetadata.named(builder, 'llvm.dbg.cu', [compileUnit])
      }
    }

    for (const entry of declared) {
      let subprogram: LlvmMetadata.Optional
      if (debug && file !== undefined && compileUnit !== undefined) {
        const startLine = positionOf(table, functionStart(entry.fn)).line
        const symbolName = yield* LlvmMetadata.string(builder, entry.symbol)
        subprogram = yield* LlvmMetadata.subprogram(builder, file, symbolName, {
          line: startLine,
          scopeLine: startLine,
          spFlags: DISPFlags.make({ definition: true }),
          compileUnit,
        })
        if (subprogram !== undefined) {
          yield* FunctionActor.setSubprogram(builder, entry.handle, subprogram)
        }
      }
      const scope = subprogram

      yield* FunctionActor.buildBody(
        builder,
        entry.handle,
        Effect.fnUntraced(function* (body) {
          const blocks = new Map<number, LlvmBlock.Block>()
          for (const block of entry.linear) {
            blocks.set(
              block.id.ordinal,
              yield* LlvmBlock.make(
                body,
                `bb${block.id.ordinal}${block.kind === 'Cleanup' ? '_cleanup' : ''}`,
              ),
            )
          }
          let trapBlock: LlvmBlock.Block | undefined
          let checkOrdinal = 0
          const locals = new Map<number, ReadonlyArray<Value.Input>>()
          const assignments = new Map<number, number>()
          for (const operation of entry.linear.flatMap((block) => block.operations)) {
            const destination = destinationOf(operation)
            if (destination !== undefined) {
              assignments.set(destination.ordinal, (assignments.get(destination.ordinal) ?? 0) + 1)
            }
          }
          const continuationLocals = entry.linear.flatMap((block) => {
            if (block.terminator._tag === 'Return' || block.terminator._tag === 'Trap') return []
            let afterRuntimeContinuation = false
            const ordinals: Array<number> = []
            for (const operation of block.operations) {
              if (opensRuntimeContinuation(operation)) afterRuntimeContinuation = true
              if (!afterRuntimeContinuation) continue
              const destination = destinationOf(operation)
              if (destination !== undefined) ordinals.push(destination.ordinal)
            }
            return ordinals
          })
          const runtimeContinuationDestinations = entry.linear.flatMap((block) =>
            block.operations.flatMap((operation) => {
              if (!opensRuntimeContinuation(operation) || operation._tag === 'Binary') return []
              const destination = destinationOf(operation)
              return destination === undefined ? [] : [destination.ordinal]
            }),
          )
          const mutableRoots = new Set([
            ...entry.linear.flatMap((block) =>
              block.operations.flatMap((operation) =>
                operation._tag === 'WritePlace' &&
                entry.fn.localTypes.at(operation.root.ordinal)?._tag !== 'Slice'
                  ? [operation.root.ordinal]
                  : [],
              ),
            ),
            ...[...assignments].flatMap(([ordinal, count]) => (count > 1 ? [ordinal] : [])),
            ...continuationLocals,
            ...runtimeContinuationDestinations,
          ])
          const borrowedCaptureRoots = new Set(
            entry.linear.flatMap((block) =>
              block.operations.flatMap((operation) =>
                operation._tag === 'MakeEffect' || operation._tag === 'MakeCallable'
                  ? operation.captures.flatMap((capture) =>
                      capture.access === 'Shared' || capture.access === 'Exclusive'
                        ? [capture.source.ordinal]
                        : [],
                    )
                  : [],
              ),
            ),
          )
          for (const root of borrowedCaptureRoots) mutableRoots.add(root)
          const addressRoots = new Set([
            ...entry.linear.flatMap((block) =>
              block.operations.flatMap((operation) =>
                operation._tag === 'BeginLoan' && operation.sourceType._tag !== 'Slice'
                  ? [operation.root.ordinal]
                  : [],
              ),
            ),
            ...borrowedCaptureRoots,
          ])
          // Address roots reload after any call that could mutate them; those reloads must not
          // leak SSA values across blocks, so they persist through the mutable-root storage.
          for (const root of addressRoots) mutableRoots.add(root)
          const mutableStorage = new Map<number, ReadonlyArray<Value.Input>>()
          for (const root of [...mutableRoots].sort((left, right) => left - right)) {
            const logicalType = entry.fn.localTypes.at(root)
            if (logicalType === undefined)
              throw new RangeError(`Backend lost mutable root %${root}`)
            if (logicalType._tag === 'EffectBorrow') continue
            const storage: Array<Value.Input> = []
            for (const [lane, callingLane] of valueLanesFor(logicalType).entries()) {
              storage.push(
                yield* FunctionBody.alloca(body, laneType(callingLane), `mut${root}_${lane}`),
              )
            }
            mutableStorage.set(root, Object.freeze(storage))
          }
          const addressStorage = new Map<number, Value.Input>()
          for (const root of [...addressRoots].sort((left, right) => left - right)) {
            const logicalType = entry.fn.localTypes.at(root)
            const layout =
              logicalType === undefined
                ? undefined
                : Layout.entry(program.layout, Mir.semanticType(logicalType))
            if (logicalType === undefined || layout === undefined) {
              throw new RangeError(`Backend lost address-taken value %${root}`)
            }
            const count = yield* Constant.integerUnsigned(builder, i32, BigInt(layout.size))
            addressStorage.set(
              root,
              yield* FunctionBody.alloca(body, i8, `addr${root}`, {
                count,
                alignment: yield* Alignment.fromByteUnits(layout.alignment),
              }),
            )
          }
          const storeAddressRootValues = Effect.fnUntraced(function* (
            root: number,
            values: ReadonlyArray<Value.Input>,
            name: string,
          ) {
            const base = addressStorage.get(root)
            const logicalType = entry.fn.localTypes.at(root)
            if (base === undefined || logicalType === undefined) {
              throw new RangeError(`Backend lost address storage for %${root}`)
            }
            for (const [ordinal, lane] of valueLanesFor(logicalType).entries()) {
              const offset = Layout.laneOffset(
                program.layout,
                Mir.semanticType(logicalType),
                lane.path,
              )
              const stored = values.at(ordinal)
              if (offset === undefined || stored === undefined) {
                throw new RangeError(`Backend lost address lane ${ordinal} for %${root}`)
              }
              yield* FunctionBody.store(
                body,
                stored,
                yield* FunctionBody.getElementPtr(
                  body,
                  i8,
                  base,
                  [yield* Constant.integerUnsigned(builder, i32, BigInt(offset))],
                  `${name}_${ordinal}_ptr`,
                ),
              )
            }
          })
          for (const root of [...addressRoots].sort((left, right) => left - right)) {
            const logicalType = entry.fn.localTypes.at(root)
            if (logicalType === undefined)
              throw new RangeError(`Backend lost address root %${root}`)
            yield* storeAddressRootValues(
              root,
              Object.freeze(
                yield* Effect.forEach(valueLanesFor(logicalType), (lane) =>
                  Constant.nullValue(builder, laneType(lane)),
                ),
              ),
              `addr${root}_zero`,
            )
          }
          let physicalParameter = 0
          for (let ordinal = 0; ordinal < entry.fn.parameterCount; ordinal += 1) {
            const logicalType = entry.fn.localTypes.at(ordinal)
            if (logicalType === undefined) {
              throw new RangeError(`Backend lost parameter type %${ordinal}`)
            }
            const values: Array<Value.Input> = []
            for (let lane = 0; lane < lanesFor(logicalType).length; lane += 1) {
              values.push(yield* Value.argument(body, physicalParameter))
              physicalParameter += 1
            }
            if (logicalType._tag === 'EffectBorrow') {
              const base = values.at(0)
              if (base === undefined) throw new RangeError(`Backend lost Effect borrow %${ordinal}`)
              const storage: Array<Value.Input> = []
              const loaded: Array<Value.Input> = []
              for (const [lane, callingLane] of valueLanesFor(logicalType).entries()) {
                const offset = Layout.laneOffset(program.layout, logicalType.type, callingLane.path)
                if (offset === undefined)
                  throw new RangeError(`Backend lost Effect borrow lane ${lane}`)
                const pointer = yield* FunctionBody.getElementPtr(
                  body,
                  i8,
                  base,
                  [yield* Constant.integerUnsigned(builder, i32, BigInt(offset))],
                  `borrow${ordinal}_${lane}_ptr`,
                )
                storage.push(pointer)
                loaded.push(
                  yield* FunctionBody.load(
                    body,
                    laneType(callingLane),
                    pointer,
                    `borrow${ordinal}_${lane}`,
                  ),
                )
              }
              mutableStorage.set(ordinal, Object.freeze(storage))
              locals.set(ordinal, Object.freeze(loaded))
              continue
            }
            locals.set(ordinal, Object.freeze(values))
            const storage = mutableStorage.get(ordinal)
            if (storage !== undefined) {
              for (const [lane, pointer] of storage.entries()) {
                const stored = values.at(lane)
                if (stored !== undefined) yield* FunctionBody.store(body, stored, pointer)
              }
            }
            if (addressRoots.has(ordinal)) {
              yield* storeAddressRootValues(ordinal, Object.freeze(values), `addr${ordinal}_param`)
            }
          }
          const readLocal = (local: Mir.LocalId): ReadonlyArray<Value.Input> => {
            const found = locals.get(local.ordinal)
            if (found === undefined) {
              throw new RangeError(`Backend read undefined local %${local.ordinal}`)
            }
            return found
          }
          const readScalar = (local: Mir.LocalId): Value.Input => {
            const values = readLocal(local)
            const first = values.at(0)
            if (values.length !== 1 || first === undefined) {
              throw new RangeError(`Backend expected scalar local %${local.ordinal}`)
            }
            return first
          }
          const locate = (span: SourceSpan.SourceSpan, instruction: unknown) =>
            Effect.gen(function* () {
              if (!debug || scope === undefined || instruction === undefined) return
              const position = positionOf(table, span.start)
              const location = yield* LlvmMetadata.location(
                builder,
                position.line,
                position.column,
                scope,
              )
              yield* FunctionBody.setDebugLocation(
                body,
                instruction as Parameters<typeof FunctionBody.setDebugLocation>[1],
                location,
              )
            })

          const coerceLane = Effect.fnUntraced(function* (
            input: Value.Input,
            source: Layout.CallingLane,
            target: Layout.CallingLane,
            name: string,
          ) {
            const pointerBits = program.layout.target.pointerSize === 4 ? 32 : 64
            const scalarBits = (lane: Layout.CallingLane): number =>
              typeof lane.type === 'string'
                ? Scalar.bits(Scalar.find(lane.type) ?? Scalar.defaultInteger, pointerBits)
                : pointerBits
            const sourceBits = scalarBits(source)
            const targetBits = scalarBits(target)
            if (sourceBits === targetBits) return input
            const sourceScalar =
              typeof source.type === 'string' ? Scalar.find(source.type) : undefined
            return yield* FunctionBody.cast(
              body,
              targetBits > sourceBits
                ? sourceScalar?.signedness === 'Signed'
                  ? 'sext'
                  : 'zext'
                : 'trunc',
              input,
              laneType(target),
              name,
            )
          })

          const callValues = Effect.fnUntraced(function* (
            target: (typeof declared)[number],
            arguments_: ReadonlyArray<Value.Input>,
            name: string,
          ) {
            const result = yield* FunctionBody.callDirect(body, target.handle, arguments_, name)
            for (const root of [...addressRoots].sort((left, right) => left - right)) {
              yield* reloadAddressRoot(root)
            }
            if (target.resultLaneCount === 0) return Object.freeze([])
            if (result === undefined) throw new RangeError('Backend call produced no value')
            if (target.resultLaneCount === 1) return Object.freeze([result])
            const values: Array<Value.Input> = []
            for (let lane = 0; lane < target.resultLaneCount; lane += 1) {
              values.push(yield* FunctionBody.extractValue(body, result, [lane], `${name}_${lane}`))
            }
            return Object.freeze(values)
          })

          const emitCallableBinary = Effect.fnUntraced(function* (
            operator: Mir.BinaryOperator,
            left: Value.Input,
            right: Value.Input,
            operandMirType: Mir.Type,
            span: SourceSpan.SourceSpan,
            nameOrdinal: number,
          ) {
            const leftLane = valueLanesFor(operandMirType).at(0)
            if (leftLane === undefined)
              throw new RangeError('LLVM callable binary operation lost its operand type')
            const semanticOperand = Mir.semanticType(operandMirType)
            const scalar =
              typeof semanticOperand === 'string' ? Scalar.find(semanticOperand) : undefined
            const unsigned = scalar?.signedness === 'Unsigned'
            const operandType = laneType(leftLane)
            if (scalar?.category === 'Floating') {
              const predicate: FunctionBody.FloatingPredicate | undefined =
                operator === 'Equals'
                  ? 'oeq'
                  : operator === 'NotEquals'
                    ? 'une'
                    : operator === 'LessThan'
                      ? 'olt'
                      : operator === 'LessOrEqual'
                        ? 'ole'
                        : operator === 'GreaterThan'
                          ? 'ogt'
                          : operator === 'GreaterOrEqual'
                            ? 'oge'
                            : undefined
              if (predicate !== undefined) {
                const flag = yield* FunctionBody.floatingCompare(
                  body,
                  predicate,
                  left,
                  right,
                  `callable_fcmp${nameOrdinal}_flag`,
                )
                return yield* FunctionBody.cast(
                  body,
                  'zext',
                  flag,
                  i32,
                  `callable_fcmp${nameOrdinal}`,
                )
              }
              const mnemonic: FunctionBody.FloatingBinaryKind | undefined =
                operator === 'Add'
                  ? 'fadd'
                  : operator === 'Subtract'
                    ? 'fsub'
                    : operator === 'Multiply'
                      ? 'fmul'
                      : operator === 'Divide'
                        ? 'fdiv'
                        : operator === 'Remainder'
                          ? 'frem'
                          : undefined
              if (mnemonic === undefined)
                throw new RangeError(`LLVM callable float ${operator} is unavailable`)
              const result = yield* FunctionBody.binary(
                body,
                mnemonic,
                left,
                right,
                `callable_float${nameOrdinal}`,
              )
              yield* locate(span, yield* Value.instruction(body, result))
              return result
            }
            const comparisonPredicates: Readonly<
              Partial<
                Record<
                  Mir.BinaryOperator,
                  'eq' | 'ne' | 'slt' | 'sle' | 'sgt' | 'sge' | 'ult' | 'ule' | 'ugt' | 'uge'
                >
              >
            > = {
              Equals: 'eq',
              NotEquals: 'ne',
              LessThan: unsigned ? 'ult' : 'slt',
              LessOrEqual: unsigned ? 'ule' : 'sle',
              GreaterThan: unsigned ? 'ugt' : 'sgt',
              GreaterOrEqual: unsigned ? 'uge' : 'sge',
            }
            const predicate = comparisonPredicates[operator]
            if (predicate !== undefined) {
              const flag = yield* FunctionBody.integerCompare(
                body,
                predicate,
                left,
                right,
                `callable_cmp${nameOrdinal}_flag`,
              )
              const widened = yield* FunctionBody.cast(
                body,
                'zext',
                flag,
                i32,
                `callable_cmp${nameOrdinal}`,
              )
              yield* locate(span, yield* Value.instruction(body, flag))
              return widened
            }
            if (
              operator === 'BitAnd' ||
              operator === 'BitOr' ||
              operator === 'BitXor' ||
              operator === 'WrappingAdd' ||
              operator === 'WrappingSubtract' ||
              operator === 'WrappingMultiply'
            ) {
              const result = yield* FunctionBody.binary(
                body,
                operator === 'BitAnd'
                  ? 'and'
                  : operator === 'BitOr'
                    ? 'or'
                    : operator === 'BitXor'
                      ? 'xor'
                      : operator === 'WrappingAdd'
                        ? 'add'
                        : operator === 'WrappingSubtract'
                          ? 'sub'
                          : 'mul',
                left,
                right,
                `callable_integer${nameOrdinal}`,
              )
              yield* locate(span, yield* Value.instruction(body, result))
              return result
            }
            if (operator === 'ShiftLeft' || operator === 'ShiftRight') {
              if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'arith_trap')
              const width =
                scalar === undefined
                  ? 32
                  : Scalar.bits(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
              const limit = yield* Constant.integerUnsigned(builder, operandType, BigInt(width))
              const invalid = yield* FunctionBody.integerCompare(
                body,
                'uge',
                right,
                limit,
                `callable_shift${nameOrdinal}_invalid`,
              )
              const continueBlock = yield* LlvmBlock.make(body, `callable_shift${nameOrdinal}_ok`)
              yield* FunctionBody.conditionalBranch(body, invalid, trapBlock, continueBlock)
              yield* LlvmBlock.setInsertionPoint(body, continueBlock)
              const result = yield* FunctionBody.binary(
                body,
                operator === 'ShiftLeft' ? 'shl' : unsigned ? 'lshr' : 'ashr',
                left,
                right,
                `callable_shift${nameOrdinal}`,
              )
              yield* locate(span, yield* Value.instruction(body, result))
              return result
            }
            if (operator === 'RotateLeft' || operator === 'RotateRight') {
              const signature = Object.freeze({
                returnType: operandType,
                parameters: Object.freeze([operandType, operandType, operandType]),
              })
              const result = yield* Intrinsic.call(
                body,
                operator === 'RotateLeft' ? 'fshl' : 'fshr',
                [operandType],
                [left, left, right],
                `callable_rotate${nameOrdinal}`,
                { signature },
              )
              if (result === undefined)
                throw new RangeError('LLVM callable rotate produced no value')
              yield* locate(span, yield* Value.instruction(body, result))
              return result
            }
            if (operator === 'SaturatingAdd' || operator === 'SaturatingSubtract') {
              const signature = Object.freeze({
                returnType: operandType,
                parameters: Object.freeze([operandType, operandType]),
              })
              const intrinsic =
                operator === 'SaturatingAdd'
                  ? unsigned
                    ? 'uadd.sat'
                    : 'sadd.sat'
                  : unsigned
                    ? 'usub.sat'
                    : 'ssub.sat'
              const result = yield* Intrinsic.call(
                body,
                intrinsic,
                [operandType],
                [left, right],
                `callable_saturating${nameOrdinal}`,
                { signature },
              )
              if (result === undefined)
                throw new RangeError('LLVM callable saturating arithmetic produced no value')
              yield* locate(span, yield* Value.instruction(body, result))
              return result
            }
            if (operator === 'SaturatingMultiply') {
              const bits =
                scalar === undefined
                  ? 32
                  : Scalar.bits(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
              const signatures = unsigned ? unsignedOverflowSignatures : signedOverflowSignatures
              let signature = signatures.get(bits)
              if (signature === undefined) {
                const i1 = yield* LlvmType.integer(builder, 1)
                signature = Object.freeze({
                  returnType: yield* LlvmType.structure(builder, [operandType, i1]),
                  parameters: Object.freeze([operandType, operandType]),
                })
                signatures.set(bits, signature)
              }
              const pair = yield* Intrinsic.call(
                body,
                unsigned ? 'umul.with.overflow' : 'smul.with.overflow',
                [operandType],
                [left, right],
                `callable_saturating${nameOrdinal}_pair`,
                { signature },
              )
              if (pair === undefined)
                throw new RangeError('LLVM callable saturating multiply produced no value')
              const wrapped = yield* FunctionBody.extractValue(
                body,
                pair,
                [0],
                `callable_saturating${nameOrdinal}_wrapped`,
              )
              const overflowed = yield* FunctionBody.extractValue(
                body,
                pair,
                [1],
                `callable_saturating${nameOrdinal}_overflow`,
              )
              const range =
                scalar?.category === 'Integer'
                  ? Scalar.range(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
                  : { minimum: -2147483648n, maximum: 2147483647n }
              const maximum = unsigned
                ? yield* Constant.integerUnsigned(builder, operandType, range.maximum)
                : yield* Constant.integerSigned(builder, operandType, range.maximum)
              let boundary: Value.Input = maximum
              if (!unsigned) {
                const zero = yield* Constant.integerSigned(builder, operandType, 0n)
                const minimum = yield* Constant.integerSigned(builder, operandType, range.minimum)
                const signs = yield* FunctionBody.binary(
                  body,
                  'xor',
                  left,
                  right,
                  `callable_saturating${nameOrdinal}_signs`,
                )
                const negative = yield* FunctionBody.integerCompare(
                  body,
                  'slt',
                  signs,
                  zero,
                  `callable_saturating${nameOrdinal}_negative`,
                )
                boundary = yield* FunctionBody.select(
                  body,
                  negative,
                  minimum,
                  maximum,
                  `callable_saturating${nameOrdinal}_boundary`,
                )
              }
              const result = yield* FunctionBody.select(
                body,
                overflowed,
                boundary,
                wrapped,
                `callable_saturating${nameOrdinal}`,
              )
              yield* locate(span, yield* Value.instruction(body, result))
              return result
            }
            if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'arith_trap')
            let result: Value.Value
            if (operator === 'Add' || operator === 'Subtract' || operator === 'Multiply') {
              const intrinsicId =
                operator === 'Add'
                  ? unsigned
                    ? ('uadd.with.overflow' as const)
                    : ('sadd.with.overflow' as const)
                  : operator === 'Subtract'
                    ? unsigned
                      ? ('usub.with.overflow' as const)
                      : ('ssub.with.overflow' as const)
                    : unsigned
                      ? ('umul.with.overflow' as const)
                      : ('smul.with.overflow' as const)
              const bits =
                scalar === undefined
                  ? 32
                  : Scalar.bits(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
              const signatures = unsigned ? unsignedOverflowSignatures : signedOverflowSignatures
              let overflowSignature = signatures.get(bits)
              if (overflowSignature === undefined) {
                const i1 = yield* LlvmType.integer(builder, 1)
                overflowSignature = Object.freeze({
                  returnType: yield* LlvmType.structure(builder, [operandType, i1]),
                  parameters: Object.freeze([operandType, operandType]),
                })
                signatures.set(bits, overflowSignature)
              }
              const pair = yield* Intrinsic.call(
                body,
                intrinsicId,
                [operandType],
                [left, right],
                `callable_arith${nameOrdinal}_pair`,
                { signature: overflowSignature },
              )
              if (pair === undefined)
                throw new RangeError('Backend callable overflow intrinsic produced no value')
              result = yield* FunctionBody.extractValue(
                body,
                pair,
                [0],
                `callable_arith${nameOrdinal}`,
              )
              const overflowed = yield* FunctionBody.extractValue(
                body,
                pair,
                [1],
                `callable_arith${nameOrdinal}_flag`,
              )
              const continueBlock = yield* LlvmBlock.make(body, `callable_arith${nameOrdinal}_ok`)
              yield* FunctionBody.conditionalBranch(body, overflowed, trapBlock, continueBlock)
              yield* LlvmBlock.setInsertionPoint(body, continueBlock)
            } else {
              const zero = yield* Constant.integerUnsigned(builder, operandType, 0n)
              const zeroDivisor = yield* FunctionBody.integerCompare(
                body,
                'eq',
                right,
                zero,
                `callable_div${nameOrdinal}_zero`,
              )
              let trapping: Value.Value = zeroDivisor
              if (!unsigned) {
                const minimum = yield* Constant.integerSigned(
                  builder,
                  operandType,
                  scalar?.category === 'Integer'
                    ? Scalar.range(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
                        .minimum
                    : -2147483648n,
                )
                const negativeOne = yield* Constant.integerSigned(builder, operandType, -1n)
                const minimumDividend = yield* FunctionBody.integerCompare(
                  body,
                  'eq',
                  left,
                  minimum,
                  `callable_div${nameOrdinal}_min`,
                )
                const negativeOneDivisor = yield* FunctionBody.integerCompare(
                  body,
                  'eq',
                  right,
                  negativeOne,
                  `callable_div${nameOrdinal}_negone`,
                )
                const overflowCase = yield* FunctionBody.binary(
                  body,
                  'and',
                  minimumDividend,
                  negativeOneDivisor,
                  `callable_div${nameOrdinal}_overflow`,
                )
                trapping = yield* FunctionBody.binary(
                  body,
                  'or',
                  zeroDivisor,
                  overflowCase,
                  `callable_div${nameOrdinal}_trapping`,
                )
              }
              const continueBlock = yield* LlvmBlock.make(body, `callable_div${nameOrdinal}_ok`)
              yield* FunctionBody.conditionalBranch(body, trapping, trapBlock, continueBlock)
              yield* LlvmBlock.setInsertionPoint(body, continueBlock)
              result = yield* FunctionBody.binary(
                body,
                operator === 'Divide' ? (unsigned ? 'udiv' : 'sdiv') : unsigned ? 'urem' : 'srem',
                left,
                right,
                `callable_arith${nameOrdinal}`,
              )
            }
            yield* locate(span, yield* Value.instruction(body, result))
            return result
          })

          const emitIntegerConversion = Effect.fnUntraced(function* (
            input: Value.Input,
            sourceType: Mir.ScalarType,
            targetType: Mir.ScalarType,
            name: string,
          ) {
            const source = Scalar.find(sourceType._tag)
            const target = Scalar.find(targetType._tag)
            if (
              source === undefined ||
              source.category !== 'Integer' ||
              target === undefined ||
              target.category !== 'Integer'
            )
              throw new RangeError('LLVM integer conversion lost its scalar types')
            const pointerBits = program.layout.target.pointerSize === 4 ? 32 : 64
            const sourceBits = Scalar.bits(source, pointerBits)
            const targetBits = Scalar.bits(target, pointerBits)
            const sourceRange = Scalar.range(source, pointerBits)
            const targetRange = Scalar.range(target, pointerBits)
            const physicalSource = integerTypes.get(sourceBits) ?? i32
            const physicalTarget = integerTypes.get(targetBits) ?? i32
            const checks: Array<Value.Input> = []
            if (targetRange.minimum > sourceRange.minimum) {
              checks.push(
                yield* FunctionBody.integerCompare(
                  body,
                  source.signedness === 'Signed' ? 'slt' : 'ult',
                  input,
                  source.signedness === 'Signed'
                    ? yield* Constant.integerSigned(builder, physicalSource, targetRange.minimum)
                    : yield* Constant.integerUnsigned(builder, physicalSource, targetRange.minimum),
                  `${name}_below`,
                ),
              )
            }
            if (targetRange.maximum < sourceRange.maximum) {
              checks.push(
                yield* FunctionBody.integerCompare(
                  body,
                  source.signedness === 'Signed' ? 'sgt' : 'ugt',
                  input,
                  source.signedness === 'Signed'
                    ? yield* Constant.integerSigned(builder, physicalSource, targetRange.maximum)
                    : yield* Constant.integerUnsigned(builder, physicalSource, targetRange.maximum),
                  `${name}_above`,
                ),
              )
            }
            let invalid = checks.at(0)
            for (const [ordinal, check] of checks.slice(1).entries())
              invalid = yield* FunctionBody.binary(
                body,
                'or',
                invalid ?? check,
                check,
                `${name}_invalid${ordinal}`,
              )
            if (invalid !== undefined) {
              if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'arith_trap')
              const following = yield* LlvmBlock.make(body, `${name}_ok`)
              yield* FunctionBody.conditionalBranch(body, invalid, trapBlock, following)
              yield* LlvmBlock.setInsertionPoint(body, following)
            }
            if (sourceBits === targetBits) return input
            return yield* FunctionBody.cast(
              body,
              sourceBits < targetBits
                ? source.signedness === 'Signed'
                  ? 'sext'
                  : 'zext'
                : 'trunc',
              input,
              physicalTarget,
              name,
            )
          })

          const storeMutable = Effect.fnUntraced(function* (
            root: Mir.LocalId,
            values: ReadonlyArray<Value.Input>,
          ) {
            const storage = mutableStorage.get(root.ordinal)
            if (storage === undefined) return
            for (const [lane, pointer] of storage.entries()) {
              const stored = values.at(lane)
              if (stored === undefined) throw new RangeError('Mutable root lost a physical lane')
              yield* FunctionBody.store(body, stored, pointer)
            }
          })

          const bytePointer = Effect.fnUntraced(function* (
            base: Value.Input,
            offset: Value.Input,
            name: string,
          ) {
            return yield* FunctionBody.getElementPtr(body, i8, base, [offset], name)
          })
          const constantBytePointer = Effect.fnUntraced(function* (
            base: Value.Input,
            offset: number,
            name: string,
          ) {
            return yield* bytePointer(
              base,
              yield* Constant.integerUnsigned(builder, i32, BigInt(offset)),
              name,
            )
          })
          const aggregateFieldOffset = (type: SilkType.Type, name: string): number => {
            const planned = Layout.entry(program.layout, type)
            if (planned?.representation._tag !== 'Aggregate') {
              throw new RangeError(`LLVM raw storage lost aggregate ${SilkType.encode(type)}`)
            }
            const field = planned.representation.fields.find((candidate) => candidate.name === name)
            if (field === undefined) throw new RangeError(`LLVM raw storage lost field ${name}`)
            return field.offset
          }
          const emitHostFailure = Effect.fnUntraced(function* (
            operation: Extract<
              Mir.Operation,
              { readonly _tag: 'Allocate' | 'StandardStreamWrite' }
            >,
          ) {
            const lanes = lanesFor(operation.propagationType)
            const values: Array<Value.Input> = []
            for (const [ordinal, lane] of lanes.entries()) {
              values.push(
                yield* Constant.integerUnsigned(
                  builder,
                  laneType(lane),
                  ordinal === 0 ? BigInt(operation.failureTag) : 0n,
                ),
              )
            }
            if (values.length === 0) {
              yield* FunctionBody.returnVoid(body)
              return
            }
            const single = values.at(0)
            if (values.length === 1 && single !== undefined) {
              yield* FunctionBody.returnValue(body, single)
              return
            }
            yield* FunctionBody.returnValue(
              body,
              yield* FunctionBody.buildAggregate(
                body,
                entry.resultType,
                Object.freeze(values),
                `host_failure${operation.destination.ordinal}`,
              ),
            )
          })
          let materializeSequence = 0
          const materializeAddressRoot = Effect.fnUntraced(function* (root: Mir.LocalId) {
            const materializeId = materializeSequence++
            const base = addressStorage.get(root.ordinal)
            const logicalType = entry.fn.localTypes.at(root.ordinal)
            if (base === undefined || logicalType === undefined) {
              throw new RangeError(`Backend lost address storage for %${root.ordinal}`)
            }
            yield* storeAddressRootValues(
              root.ordinal,
              readLocal(root),
              `addr${root.ordinal}_${materializeId}`,
            )
          })
          const ensureAddressRoot = Effect.fnUntraced(function* (root: Mir.LocalId) {
            if (!addressStorage.has(root.ordinal)) {
              const logicalType = entry.fn.localTypes.at(root.ordinal)
              const layout =
                logicalType === undefined
                  ? undefined
                  : Layout.entry(program.layout, Mir.semanticType(logicalType))
              if (logicalType === undefined || layout === undefined)
                throw new RangeError(`Backend cannot materialize callable capture %${root.ordinal}`)
              addressStorage.set(
                root.ordinal,
                yield* FunctionBody.alloca(body, i8, `callable_addr${root.ordinal}`, {
                  count: yield* Constant.integerUnsigned(builder, i32, BigInt(layout.size)),
                  alignment: yield* Alignment.fromByteUnits(layout.alignment),
                }),
              )
            }
            yield* materializeAddressRoot(root)
          })
          let reloadSequence = 0
          const reloadAddressRoot = Effect.fnUntraced(function* (root: number) {
            const reloadId = reloadSequence++
            const base = addressStorage.get(root)
            const logicalType = entry.fn.localTypes.at(root)
            if (base === undefined || logicalType === undefined) {
              throw new RangeError(`Backend lost address storage for %${root}`)
            }
            const values: Array<Value.Input> = []
            for (const [ordinal, lane] of valueLanesFor(logicalType).entries()) {
              const offset = Layout.laneOffset(
                program.layout,
                Mir.semanticType(logicalType),
                lane.path,
              )
              if (offset === undefined) throw new RangeError(`Backend lost address lane ${ordinal}`)
              values.push(
                yield* FunctionBody.load(
                  body,
                  laneType(lane),
                  yield* constantBytePointer(
                    base,
                    offset,
                    `reload${root}_${ordinal}_${reloadId}_ptr`,
                  ),
                  `reload${root}_${ordinal}_${reloadId}`,
                ),
              )
            }
            const frozen = Object.freeze(values)
            locals.set(root, frozen)
            yield* storeMutable(Object.freeze({ _tag: 'Local', ordinal: root }), frozen)
          })

          const semanticLanesOf = (type: SilkType.Type): ReadonlyArray<Layout.CallingLane> => {
            const shape = Layout.callingShape(program.layout, type)
            if (shape === undefined)
              throw new RangeError(`LLVM cleanup lost calling shape for ${SilkType.encode(type)}`)
            return shape.lanes
          }

          /**
           * Releases one owned value's lanes through its complete cleanup plan: hooks run
           * against a stack materialization before their inner cleanup sees the (possibly
           * mutated) lanes, struct fields release in declaration order, and every ticket-backed
           * lane calls the release shim exactly once.
           */
          const dropThroughPlan: (
            plan: Ownership.CleanupPlan,
            values: ReadonlyArray<Value.Input>,
            tag: string,
          ) => Effect.Effect<void, LlvmError.LlvmError> = Effect.fnUntraced(function* (
            plan: Ownership.CleanupPlan,
            values: ReadonlyArray<Value.Input>,
            tag: string,
          ) {
            switch (plan._tag) {
              case 'NoCleanup':
              case 'ParameterCleanup':
              case 'CallableCleanup':
                return
              case 'AllocationCleanup':
              case 'RawBufferCleanup': {
                const context = values.at(4)
                if (context === undefined || free === undefined)
                  throw new RangeError('LLVM allocation cleanup lost its reclaim context')
                yield* FunctionBody.callDirect(body, free, [
                  yield* FunctionBody.cast(body, 'inttoptr', context, pointer, `${tag}_context`),
                ])
                return
              }
              case 'HookCleanup': {
                const target = declared.find((candidate) =>
                  Mir.matchesInstance(candidate.fn, plan.hook, plan.typeArguments),
                )
                if (target === undefined)
                  throw new RangeError('LLVM cleanup cannot resolve its Drop hook instance')
                const layoutEntry = Layout.entry(program.layout, plan.type)
                if (layoutEntry === undefined || usizeType === undefined)
                  throw new RangeError('LLVM hook cleanup lost its layout')
                const base = yield* FunctionBody.alloca(body, i8, `${tag}_hook_storage`, {
                  count: yield* Constant.integerUnsigned(builder, i32, BigInt(layoutEntry.size)),
                  alignment: yield* Alignment.fromByteUnits(layoutEntry.alignment),
                })
                const lanes = semanticLanesOf(plan.type)
                for (const [ordinal, lane] of lanes.entries()) {
                  const offset = Layout.laneOffset(program.layout, plan.type, lane.path)
                  const stored = values.at(ordinal)
                  if (offset === undefined || stored === undefined)
                    throw new RangeError('LLVM hook cleanup lost a lane')
                  yield* FunctionBody.store(
                    body,
                    stored,
                    yield* constantBytePointer(base, offset, `${tag}_store${ordinal}`),
                  )
                }
                yield* callValues(target, [base], `${tag}_hook`)
                const reloaded: Array<Value.Input> = []
                for (const [ordinal, lane] of lanes.entries()) {
                  const offset = Layout.laneOffset(program.layout, plan.type, lane.path)
                  if (offset === undefined)
                    throw new RangeError('LLVM hook cleanup lost a lane offset')
                  reloaded.push(
                    yield* FunctionBody.load(
                      body,
                      laneType(lane),
                      yield* constantBytePointer(base, offset, `${tag}_reload${ordinal}_ptr`),
                      `${tag}_reload${ordinal}`,
                    ),
                  )
                }
                yield* dropThroughPlan(plan.inner, Object.freeze(reloaded), `${tag}_inner`)
                return
              }
              case 'StructCleanup': {
                const lanes = semanticLanesOf(plan.type)
                for (const [fieldOrdinal, field] of plan.fields.entries()) {
                  if (!planReclaims(field.cleanup) && field.cleanup._tag !== 'HookCleanup') continue
                  const fieldValues = lanes.flatMap((lane, index) => {
                    const first = lane.path.at(0)
                    const value = values.at(index)
                    return first !== undefined &&
                      first._tag === 'FieldId' &&
                      value !== undefined &&
                      first.ordinal === field.field.ordinal &&
                      first.struct.ordinal === field.field.struct.ordinal &&
                      first.struct.sourceId === field.field.struct.sourceId
                      ? [value]
                      : []
                  })
                  yield* dropThroughPlan(
                    field.cleanup,
                    Object.freeze(fieldValues),
                    `${tag}_f${fieldOrdinal}`,
                  )
                }
                return
              }
              case 'ArrayCleanup': {
                if (!planReclaims(plan.element) && plan.element._tag !== 'HookCleanup') return
                const lanes = semanticLanesOf(plan.type)
                for (let index = 0; index < plan.length; index += 1) {
                  const elementValues = lanes.flatMap((lane, ordinal) => {
                    const first = lane.path.at(0)
                    const value = values.at(ordinal)
                    return first !== undefined &&
                      first._tag === 'ElementSelector' &&
                      first.index === index &&
                      value !== undefined
                      ? [value]
                      : []
                  })
                  yield* dropThroughPlan(
                    plan.element,
                    Object.freeze(elementValues),
                    `${tag}_e${index}`,
                  )
                }
                return
              }
              case 'UnionCleanup': {
                if (plan.cases.every((entry) => !planReclaims(entry.cleanup))) return
                // Conditional release without new blocks: every reclaim context selects to null
                // unless the live tag matches its member, and libc free ignores null.
                const shape = Layout.callingShape(program.layout, plan.type)
                const tagValue = values.at(0)
                if (
                  shape === undefined ||
                  tagValue === undefined ||
                  free === undefined ||
                  usizeType === undefined
                ) {
                  throw new RangeError('LLVM union cleanup lost its shape')
                }
                const zero = yield* Constant.integerUnsigned(builder, usizeType, 0n)
                for (const entry of plan.cases) {
                  const paths = reclaimContextPaths(entry.cleanup)
                  if (paths === undefined) {
                    throw new RangeError('LLVM cleanup does not yet lower this union case shape')
                  }
                  for (const [pathOrdinal, path] of paths.entries()) {
                    const slots = Layout.memberFieldSlots(shape, entry.member, path)
                    const contextSlot = slots?.at(4)
                    const context = contextSlot === undefined ? undefined : values.at(contextSlot)
                    if (context === undefined) {
                      throw new RangeError('LLVM union cleanup lost a reclaim lane')
                    }
                    const matches = yield* FunctionBody.integerCompare(
                      body,
                      'eq',
                      tagValue,
                      yield* Constant.integerSigned(builder, i32, BigInt(entry.ordinal)),
                      `${tag}_u${entry.ordinal}_${pathOrdinal}_is`,
                    )
                    const guarded = yield* FunctionBody.select(
                      body,
                      matches,
                      context,
                      zero,
                      `${tag}_u${entry.ordinal}_${pathOrdinal}_context`,
                    )
                    yield* FunctionBody.callDirect(body, free, [
                      yield* FunctionBody.cast(
                        body,
                        'inttoptr',
                        guarded,
                        pointer,
                        `${tag}_u${entry.ordinal}_${pathOrdinal}_pointer`,
                      ),
                    ])
                  }
                }
                return
              }
            }
          })

          for (const [blockOrdinal, block] of entry.linear.entries()) {
            const blockHandle = blocks.get(block.id.ordinal)
            if (blockHandle === undefined) continue
            if (blockOrdinal > 0) yield* LlvmBlock.setInsertionPoint(body, blockHandle)
            if (blockOrdinal > 0) {
              for (const root of [...mutableRoots].sort((left, right) => left - right)) {
                const storage = mutableStorage.get(root)
                if (storage === undefined) continue
                const loaded: Array<Value.Input> = []
                const logicalType = entry.fn.localTypes.at(root)
                if (logicalType === undefined) throw new RangeError('Mutable root lost its type')
                for (const [lane, pointer] of storage.entries()) {
                  const callingLane = valueLanesFor(logicalType).at(lane)
                  if (callingLane === undefined) throw new RangeError('Mutable root lost a lane')
                  loaded.push(
                    yield* FunctionBody.load(
                      body,
                      laneType(callingLane),
                      pointer,
                      `mut${root}_${lane}_load_b${block.id.ordinal}`,
                    ),
                  )
                }
                locals.set(root, Object.freeze(loaded))
              }
            }
            for (const operation of block.operations) {
              switch (operation._tag) {
                case 'BindMatch': {
                  const physical = Layout.memberFieldSlots(
                    operation.shape,
                    operation.member,
                    operation.binding.path,
                  )
                  if (physical === undefined) {
                    throw new RangeError('LLVM match lost a pattern payload path')
                  }
                  const source = readLocal(operation.scrutinee)
                  const sourceLanes = operation.shape.lanes
                  const targetLanes = lanesFor(operation.binding.type)
                  const selected: Array<Value.Input> = []
                  for (const [targetOrdinal, ordinal] of physical.entries()) {
                    const value = source.at(ordinal)
                    const sourceLane = sourceLanes.at(ordinal)
                    const targetLane = targetLanes.at(targetOrdinal)
                    if (
                      value === undefined ||
                      sourceLane === undefined ||
                      targetLane === undefined
                    ) {
                      continue
                    }
                    selected.push(
                      yield* coerceLane(
                        value,
                        sourceLane,
                        targetLane,
                        `match${operation.binding.destination.ordinal}_${targetOrdinal}_lane`,
                      ),
                    )
                  }
                  if (selected.length !== targetLanes.length) {
                    throw new RangeError('LLVM match binding disagrees with its payload lanes')
                  }
                  locals.set(operation.binding.destination.ordinal, Object.freeze(selected))
                  break
                }
                case 'Literal': {
                  const lane = lanesFor(operation.type).at(0)
                  if (lane === undefined) throw new RangeError('LLVM literal lost its lane')
                  const physicalType = laneType(lane)
                  const semantic = Mir.semanticType(operation.type)
                  const floating = typeof semantic === 'string' ? Scalar.find(semantic) : undefined
                  if (floating?.category === 'Floating') {
                    locals.set(
                      operation.destination.ordinal,
                      Object.freeze([
                        yield* Constant.floatingRaw(
                          builder,
                          physicalType,
                          floating.spelling === 'f32' ? 'float' : 'double',
                          FloatingPoint.littleEndianBytes({
                            width: floating.spelling === 'f32' ? 32 : 64,
                            bits: BigInt(operation.value),
                          }),
                        ),
                      ]),
                    )
                    break
                  }
                  const unsigned =
                    typeof semantic === 'string' && Scalar.find(semantic)?.signedness === 'Unsigned'
                  locals.set(
                    operation.destination.ordinal,
                    Object.freeze([
                      unsigned
                        ? yield* Constant.integerUnsigned(
                            builder,
                            physicalType,
                            BigInt(operation.value),
                          )
                        : yield* Constant.integerSigned(
                            builder,
                            physicalType,
                            BigInt(operation.value),
                          ),
                    ]),
                  )
                  break
                }
                case 'StaticView': {
                  const address = staticPointers.get(operation.data)
                  if (address === undefined || usizeType === undefined) {
                    throw new RangeError('LLVM static view lost its data placement or usize type')
                  }
                  locals.set(
                    operation.destination.ordinal,
                    Object.freeze([
                      address,
                      yield* Constant.integerUnsigned(builder, usizeType, operation.length),
                    ]),
                  )
                  break
                }
                case 'Allocate': {
                  const [bytes, alignment] = readLocal(operation.layout)
                  if (
                    bytes === undefined ||
                    alignment === undefined ||
                    usizeType === undefined ||
                    malloc === undefined
                  ) {
                    throw new RangeError('LLVM allocation lost its platform boundary')
                  }
                  const one = yield* Constant.integerUnsigned(builder, usizeType, 1n)
                  const zero = yield* Constant.integerUnsigned(builder, usizeType, 0n)
                  const padding = yield* FunctionBody.binary(
                    body,
                    'sub',
                    alignment,
                    one,
                    `allocation${operation.destination.ordinal}_padding`,
                  )
                  const usizeBits = program.layout.target.pointerSize * 8
                  let unsignedOverflowSignature = unsignedOverflowSignatures.get(usizeBits)
                  if (unsignedOverflowSignature === undefined) {
                    const i1 = yield* LlvmType.integer(builder, 1)
                    unsignedOverflowSignature = Object.freeze({
                      returnType: yield* LlvmType.structure(builder, [usizeType, i1]),
                      parameters: Object.freeze([usizeType, usizeType]),
                    })
                    unsignedOverflowSignatures.set(usizeBits, unsignedOverflowSignature)
                  }
                  const requestPair = yield* Intrinsic.call(
                    body,
                    'uadd.with.overflow',
                    [usizeType],
                    [bytes, padding],
                    `allocation${operation.destination.ordinal}_request_pair`,
                    { signature: unsignedOverflowSignature },
                  )
                  if (requestPair === undefined) {
                    throw new RangeError('LLVM allocation size calculation produced no value')
                  }
                  const requested = yield* FunctionBody.extractValue(
                    body,
                    requestPair,
                    [0],
                    `allocation${operation.destination.ordinal}_requested`,
                  )
                  const overflowed = yield* FunctionBody.extractValue(
                    body,
                    requestPair,
                    [1],
                    `allocation${operation.destination.ordinal}_overflowed`,
                  )
                  const empty = yield* FunctionBody.integerCompare(
                    body,
                    'eq',
                    requested,
                    zero,
                    `allocation${operation.destination.ordinal}_empty`,
                  )
                  const physicalSize = yield* FunctionBody.select(
                    body,
                    empty,
                    one,
                    requested,
                    `allocation${operation.destination.ordinal}_physical_size`,
                  )
                  const raw = yield* FunctionBody.callDirect(
                    body,
                    malloc,
                    [physicalSize],
                    `allocation${operation.destination.ordinal}_raw`,
                  )
                  if (raw === undefined) throw new RangeError('LLVM malloc returned no value')
                  const rawAddress = yield* FunctionBody.cast(
                    body,
                    'ptrtoint',
                    raw,
                    usizeType,
                    `allocation${operation.destination.ordinal}_context`,
                  )
                  const missing = yield* FunctionBody.integerCompare(
                    body,
                    'eq',
                    rawAddress,
                    zero,
                    `allocation${operation.destination.ordinal}_missing`,
                  )
                  const rejected = yield* FunctionBody.binary(
                    body,
                    'or',
                    overflowed,
                    missing,
                    `allocation${operation.destination.ordinal}_rejected`,
                  )
                  const failed = yield* LlvmBlock.make(
                    body,
                    `allocation${operation.destination.ordinal}_failure`,
                  )
                  const acquired = yield* LlvmBlock.make(
                    body,
                    `allocation${operation.destination.ordinal}_success`,
                  )
                  yield* FunctionBody.conditionalBranch(body, rejected, failed, acquired)
                  yield* LlvmBlock.setInsertionPoint(body, failed)
                  if (free === undefined) throw new RangeError('LLVM allocation lost release shim')
                  yield* FunctionBody.callDirect(body, free, [raw])
                  yield* emitHostFailure(operation)
                  yield* LlvmBlock.setInsertionPoint(body, acquired)
                  const advanced = yield* FunctionBody.binary(
                    body,
                    'add',
                    rawAddress,
                    padding,
                    `allocation${operation.destination.ordinal}_advanced`,
                  )
                  const mask = yield* FunctionBody.binary(
                    body,
                    'sub',
                    zero,
                    alignment,
                    `allocation${operation.destination.ordinal}_mask`,
                  )
                  const base = yield* FunctionBody.binary(
                    body,
                    'and',
                    advanced,
                    mask,
                    `allocation${operation.destination.ordinal}_base`,
                  )
                  locals.set(
                    operation.destination.ordinal,
                    Object.freeze([base, bytes, alignment, one, rawAddress, one]),
                  )
                  break
                }
                case 'StandardStreamWrite': {
                  const stream = readLocal(operation.stream).at(0)
                  const [address, length] = readLocal(operation.bytes)
                  if (
                    stream === undefined ||
                    address === undefined ||
                    length === undefined ||
                    standardWrite === undefined
                  ) {
                    throw new RangeError('LLVM standard-stream write lost its host boundary lanes')
                  }
                  const status = yield* FunctionBody.callDirect(
                    body,
                    standardWrite,
                    [stream, address, length],
                    `standard_stream${operation.destination.ordinal}_status`,
                  )
                  if (status === undefined) {
                    throw new RangeError('LLVM standard-stream host returned no status')
                  }
                  const failedStatus = yield* FunctionBody.integerCompare(
                    body,
                    'ne',
                    status,
                    yield* Constant.integerUnsigned(builder, i32, 0n),
                    `standard_stream${operation.destination.ordinal}_failed`,
                  )
                  const failed = yield* LlvmBlock.make(
                    body,
                    `standard_stream${operation.destination.ordinal}_failure`,
                  )
                  const written = yield* LlvmBlock.make(
                    body,
                    `standard_stream${operation.destination.ordinal}_success`,
                  )
                  yield* FunctionBody.conditionalBranch(body, failedStatus, failed, written)
                  yield* LlvmBlock.setInsertionPoint(body, failed)
                  yield* emitHostFailure(operation)
                  yield* LlvmBlock.setInsertionPoint(body, written)
                  locals.set(operation.destination.ordinal, Object.freeze([]))
                  break
                }
                case 'RawBufferFrom': {
                  const allocation = readLocal(operation.allocation)
                  const count = readLocal(operation.count).at(0)
                  const bytes = allocation.at(1)
                  const alignment = allocation.at(2)
                  if (
                    count === undefined ||
                    bytes === undefined ||
                    alignment === undefined ||
                    usizeType === undefined
                  ) {
                    throw new RangeError('LLVM RawBuffer construction lost its lanes')
                  }
                  const expected = yield* FunctionBody.binary(
                    body,
                    'mul',
                    count,
                    yield* Constant.integerUnsigned(builder, usizeType, BigInt(operation.stride)),
                    `raw_buffer${operation.destination.ordinal}_bytes`,
                  )
                  const bytesMismatch = yield* FunctionBody.integerCompare(
                    body,
                    'ne',
                    expected,
                    bytes,
                    `raw_buffer${operation.destination.ordinal}_bytes_mismatch`,
                  )
                  const alignmentMismatch = yield* FunctionBody.integerCompare(
                    body,
                    'ne',
                    alignment,
                    yield* Constant.integerUnsigned(
                      builder,
                      usizeType,
                      BigInt(operation.elementAlignment),
                    ),
                    `raw_buffer${operation.destination.ordinal}_alignment_mismatch`,
                  )
                  const invalid = yield* FunctionBody.binary(
                    body,
                    'or',
                    bytesMismatch,
                    alignmentMismatch,
                    `raw_buffer${operation.destination.ordinal}_invalid`,
                  )
                  if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'raw_trap')
                  const accepted = yield* LlvmBlock.make(
                    body,
                    `raw_buffer${operation.destination.ordinal}_accepted`,
                  )
                  yield* FunctionBody.conditionalBranch(body, invalid, trapBlock, accepted)
                  yield* LlvmBlock.setInsertionPoint(body, accepted)
                  locals.set(operation.destination.ordinal, Object.freeze([...allocation, count]))
                  break
                }
                case 'RawBufferCount': {
                  const address = readLocal(operation.buffer).at(0)
                  const referenceType = entry.fn.localTypes.at(operation.buffer.ordinal)
                  if (
                    address === undefined ||
                    referenceType?._tag !== 'Reference' ||
                    !SilkType.isRawBuffer(referenceType.type.target) ||
                    usizeType === undefined
                  ) {
                    throw new RangeError('LLVM RawBuffer.count lost its referenced buffer')
                  }
                  const value = yield* FunctionBody.load(
                    body,
                    usizeType,
                    yield* constantBytePointer(
                      address,
                      aggregateFieldOffset(referenceType.type.target, 'count'),
                      `raw_buffer_count${operation.destination.ordinal}_ptr`,
                    ),
                    `raw_buffer_count${operation.destination.ordinal}`,
                  )
                  locals.set(operation.destination.ordinal, Object.freeze([value]))
                  break
                }
                case 'RawBufferSlot': {
                  const address = readLocal(operation.buffer).at(0)
                  const index = readLocal(operation.index).at(0)
                  const element = Layout.entry(program.layout, operation.element)
                  if (
                    address === undefined ||
                    index === undefined ||
                    element === undefined ||
                    usizeType === undefined
                  ) {
                    throw new RangeError('LLVM RawBuffer.slot lost its storage provenance')
                  }
                  const bufferType = SilkType.rawBuffer(operation.element)
                  const count = yield* FunctionBody.load(
                    body,
                    usizeType,
                    yield* constantBytePointer(
                      address,
                      aggregateFieldOffset(bufferType, 'count'),
                      `raw_slot${operation.destination.ordinal}_count_ptr`,
                    ),
                    `raw_slot${operation.destination.ordinal}_count`,
                  )
                  const outOfBounds = yield* FunctionBody.integerCompare(
                    body,
                    'uge',
                    index,
                    count,
                    `raw_slot${operation.destination.ordinal}_bounds`,
                  )
                  if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'raw_trap')
                  const accepted = yield* LlvmBlock.make(
                    body,
                    `raw_slot${operation.destination.ordinal}_accepted`,
                  )
                  yield* FunctionBody.conditionalBranch(body, outOfBounds, trapBlock, accepted)
                  yield* LlvmBlock.setInsertionPoint(body, accepted)
                  const allocationOffset = aggregateFieldOffset(bufferType, '$allocation')
                  const baseAddress = yield* FunctionBody.load(
                    body,
                    usizeType,
                    yield* constantBytePointer(
                      address,
                      allocationOffset + aggregateFieldOffset(SilkType.allocation, '$base'),
                      `raw_slot${operation.destination.ordinal}_base_ptr`,
                    ),
                    `raw_slot${operation.destination.ordinal}_base`,
                  )
                  const stride = Math.ceil(element.size / element.alignment) * element.alignment
                  const offset = yield* FunctionBody.binary(
                    body,
                    'mul',
                    index,
                    yield* Constant.integerUnsigned(builder, usizeType, BigInt(stride)),
                    `raw_slot${operation.destination.ordinal}_offset`,
                  )
                  const selected = yield* FunctionBody.binary(
                    body,
                    'add',
                    baseAddress,
                    offset,
                    `raw_slot${operation.destination.ordinal}_address`,
                  )
                  locals.set(operation.destination.ordinal, Object.freeze([selected]))
                  break
                }
                case 'SlotWrite': {
                  const address = readLocal(operation.slot).at(0)
                  if (address === undefined || usizeType === undefined) {
                    throw new RangeError('LLVM Slot.write lost its address')
                  }
                  const base = yield* FunctionBody.cast(
                    body,
                    'inttoptr',
                    address,
                    pointer,
                    `slot_write${operation.destination.ordinal}_base`,
                  )
                  const values = readLocal(operation.value)
                  const lanes = Layout.callingShape(program.layout, operation.element)?.lanes
                  if (lanes === undefined) throw new RangeError('LLVM Slot.write lost its shape')
                  for (const [ordinal, lane] of lanes.entries()) {
                    const value = values.at(ordinal)
                    const offset = Layout.laneOffset(program.layout, operation.element, lane.path)
                    if (value === undefined || offset === undefined) {
                      throw new RangeError('LLVM Slot.write lost an element lane')
                    }
                    yield* FunctionBody.store(
                      body,
                      value,
                      yield* constantBytePointer(
                        base,
                        offset,
                        `slot_write${operation.destination.ordinal}_${ordinal}_ptr`,
                      ),
                    )
                  }
                  locals.set(operation.destination.ordinal, Object.freeze([]))
                  break
                }
                case 'ValidateLayout': {
                  const bytes = readLocal(operation.bytes).at(0)
                  const alignment = readLocal(operation.alignment).at(0)
                  if (bytes === undefined || alignment === undefined || usizeType === undefined) {
                    throw new RangeError('LLVM layout validation lost its operands')
                  }
                  const name = `validate${operation.destination.ordinal}`
                  const zero = yield* Constant.integerUnsigned(builder, usizeType, 0n)
                  const one = yield* Constant.integerUnsigned(builder, usizeType, 1n)
                  const nonZero = yield* FunctionBody.integerCompare(
                    body,
                    'ne',
                    alignment,
                    zero,
                    `${name}_nonzero`,
                  )
                  const decremented = yield* FunctionBody.binary(
                    body,
                    'sub',
                    alignment,
                    one,
                    `${name}_decrement`,
                  )
                  const masked = yield* FunctionBody.binary(
                    body,
                    'and',
                    alignment,
                    decremented,
                    `${name}_mask`,
                  )
                  const powerOfTwo = yield* FunctionBody.integerCompare(
                    body,
                    'eq',
                    masked,
                    zero,
                    `${name}_pow2`,
                  )
                  const valid = yield* FunctionBody.binary(
                    body,
                    'and',
                    nonZero,
                    powerOfTwo,
                    `${name}_valid`,
                  )
                  const members = operation.type.type.members
                  const layoutOrdinal = members.findIndex((member) =>
                    SilkType.equals(member, SilkType.layout),
                  )
                  const invalidOrdinal = members.findIndex((member) =>
                    SilkType.equals(member, SilkType.invalidAlignment),
                  )
                  if (layoutOrdinal < 0 || invalidOrdinal < 0) {
                    throw new RangeError('LLVM layout validation lost its union members')
                  }
                  const tag = yield* FunctionBody.select(
                    body,
                    valid,
                    yield* Constant.integerSigned(builder, i32, BigInt(layoutOrdinal)),
                    yield* Constant.integerSigned(builder, i32, BigInt(invalidOrdinal)),
                    `${name}_tag`,
                  )
                  // Layout packs {bytes, alignment}; InvalidAlignment packs {alignment} at slot 0.
                  const first = yield* FunctionBody.select(
                    body,
                    valid,
                    bytes,
                    alignment,
                    `${name}_slot0`,
                  )
                  const second = yield* FunctionBody.select(
                    body,
                    valid,
                    alignment,
                    zero,
                    `${name}_slot1`,
                  )
                  const lanes = lanesFor(operation.type)
                  const values: Array<Value.Input> = [tag, first, second]
                  while (values.length < lanes.length) {
                    const lane = lanes.at(values.length)
                    if (lane === undefined) break
                    values.push(yield* Constant.integerUnsigned(builder, laneType(lane), 0n))
                  }
                  locals.set(operation.destination.ordinal, Object.freeze(values))
                  break
                }
                case 'RepeatLayout': {
                  const layoutValues = readLocal(operation.layout)
                  const bytes = layoutValues.at(0)
                  const alignment = layoutValues.at(1)
                  const count = readLocal(operation.count).at(0)
                  if (
                    bytes === undefined ||
                    alignment === undefined ||
                    count === undefined ||
                    usizeType === undefined
                  ) {
                    throw new RangeError('LLVM repeated layout lost its operands')
                  }
                  const name = `repeat${operation.destination.ordinal}`
                  const zero = yield* Constant.integerUnsigned(builder, usizeType, 0n)
                  const one = yield* Constant.integerUnsigned(builder, usizeType, 1n)
                  const maximum = yield* Constant.integerUnsigned(
                    builder,
                    usizeType,
                    program.layout.target.pointerSize === 4 ? 4294967295n : 18446744073709551615n,
                  )
                  const alignmentZero = yield* FunctionBody.integerCompare(
                    body,
                    'eq',
                    alignment,
                    zero,
                    `${name}_alignment_zero`,
                  )
                  const safeAlignment = yield* FunctionBody.select(
                    body,
                    alignmentZero,
                    one,
                    alignment,
                    `${name}_safe_alignment`,
                  )
                  const summed = yield* FunctionBody.binary(
                    body,
                    'add',
                    bytes,
                    yield* FunctionBody.binary(body, 'sub', safeAlignment, one, `${name}_pad`),
                    `${name}_summed`,
                  )
                  const quotient = yield* FunctionBody.binary(
                    body,
                    'udiv',
                    summed,
                    safeAlignment,
                    `${name}_quotient`,
                  )
                  const rounded = yield* FunctionBody.binary(
                    body,
                    'mul',
                    quotient,
                    safeAlignment,
                    `${name}_rounded`,
                  )
                  const stride = yield* FunctionBody.select(
                    body,
                    alignmentZero,
                    zero,
                    rounded,
                    `${name}_stride`,
                  )
                  const countZero = yield* FunctionBody.integerCompare(
                    body,
                    'eq',
                    count,
                    zero,
                    `${name}_count_zero`,
                  )
                  const safeCount = yield* FunctionBody.select(
                    body,
                    countZero,
                    one,
                    count,
                    `${name}_safe_count`,
                  )
                  const budget = yield* FunctionBody.binary(
                    body,
                    'udiv',
                    maximum,
                    safeCount,
                    `${name}_budget`,
                  )
                  const exceeds = yield* FunctionBody.integerCompare(
                    body,
                    'ugt',
                    stride,
                    budget,
                    `${name}_exceeds`,
                  )
                  const countPositive = yield* FunctionBody.integerCompare(
                    body,
                    'ne',
                    count,
                    zero,
                    `${name}_count_positive`,
                  )
                  // Rounding up can wrap the integer itself; classify that as overflow directly.
                  const headroom = yield* FunctionBody.binary(
                    body,
                    'sub',
                    maximum,
                    yield* FunctionBody.binary(body, 'sub', safeAlignment, one, `${name}_pad2`),
                    `${name}_headroom`,
                  )
                  const huge = yield* FunctionBody.integerCompare(
                    body,
                    'ugt',
                    bytes,
                    headroom,
                    `${name}_huge`,
                  )
                  const exceedsOrHuge = yield* FunctionBody.binary(
                    body,
                    'or',
                    exceeds,
                    huge,
                    `${name}_exceeds_or_huge`,
                  )
                  const overflow = yield* FunctionBody.binary(
                    body,
                    'and',
                    countPositive,
                    exceedsOrHuge,
                    `${name}_overflow`,
                  )
                  const total = yield* FunctionBody.binary(
                    body,
                    'mul',
                    stride,
                    count,
                    `${name}_total`,
                  )
                  const members = operation.type.type.members
                  const layoutOrdinal = members.findIndex((member) =>
                    SilkType.equals(member, SilkType.layout),
                  )
                  const overflowOrdinal = members.findIndex((member) =>
                    SilkType.equals(member, SilkType.layoutOverflow),
                  )
                  if (layoutOrdinal < 0 || overflowOrdinal < 0) {
                    throw new RangeError('LLVM repeated layout lost its union members')
                  }
                  const tag = yield* FunctionBody.select(
                    body,
                    overflow,
                    yield* Constant.integerSigned(builder, i32, BigInt(overflowOrdinal)),
                    yield* Constant.integerSigned(builder, i32, BigInt(layoutOrdinal)),
                    `${name}_tag`,
                  )
                  const totalOut = yield* FunctionBody.select(
                    body,
                    overflow,
                    zero,
                    total,
                    `${name}_bytes`,
                  )
                  const lanes = lanesFor(operation.type)
                  const values: Array<Value.Input> = [tag, totalOut, alignment]
                  while (values.length < lanes.length) {
                    const lane = lanes.at(values.length)
                    if (lane === undefined) break
                    values.push(yield* Constant.integerUnsigned(builder, laneType(lane), 0n))
                  }
                  locals.set(operation.destination.ordinal, Object.freeze(values))
                  break
                }
                case 'SlotTake':
                case 'SlotCopy': {
                  const address = readLocal(operation.slot).at(0)
                  if (address === undefined || usizeType === undefined) {
                    throw new RangeError('LLVM Slot.take lost its address')
                  }
                  const base = yield* FunctionBody.cast(
                    body,
                    'inttoptr',
                    address,
                    pointer,
                    `slot_take${operation.destination.ordinal}_base`,
                  )
                  const lanes = Layout.callingShape(program.layout, operation.element)?.lanes
                  if (lanes === undefined) throw new RangeError('LLVM Slot.take lost its shape')
                  const values: Array<Value.Input> = []
                  for (const [ordinal, lane] of lanes.entries()) {
                    const offset = Layout.laneOffset(program.layout, operation.element, lane.path)
                    if (offset === undefined) throw new RangeError('LLVM Slot.take lost a lane')
                    values.push(
                      yield* FunctionBody.load(
                        body,
                        laneType(lane),
                        yield* constantBytePointer(
                          base,
                          offset,
                          `slot_take${operation.destination.ordinal}_${ordinal}_ptr`,
                        ),
                        `slot_take${operation.destination.ordinal}_${ordinal}`,
                      ),
                    )
                  }
                  locals.set(operation.destination.ordinal, Object.freeze(values))
                  break
                }
                case 'SlotDrop': {
                  const address = readLocal(operation.slot).at(0)
                  if (address === undefined || usizeType === undefined) {
                    throw new RangeError('LLVM Slot.drop lost its address')
                  }
                  const base = yield* FunctionBody.cast(
                    body,
                    'inttoptr',
                    address,
                    pointer,
                    `slot_drop${operation.destination.ordinal}_base`,
                  )
                  const lanes = Layout.callingShape(program.layout, operation.element)?.lanes
                  if (lanes === undefined) throw new RangeError('LLVM Slot.drop lost its shape')
                  const values: Array<Value.Input> = []
                  for (const [ordinal, lane] of lanes.entries()) {
                    const offset = Layout.laneOffset(program.layout, operation.element, lane.path)
                    if (offset === undefined) throw new RangeError('LLVM Slot.drop lost a lane')
                    values.push(
                      yield* FunctionBody.load(
                        body,
                        laneType(lane),
                        yield* constantBytePointer(
                          base,
                          offset,
                          `slot_drop${operation.destination.ordinal}_${ordinal}_ptr`,
                        ),
                        `slot_drop${operation.destination.ordinal}_${ordinal}`,
                      ),
                    )
                  }
                  yield* dropThroughPlan(
                    operation.cleanup,
                    Object.freeze(values),
                    `slot_drop${operation.destination.ordinal}`,
                  )
                  locals.set(operation.destination.ordinal, Object.freeze([]))
                  break
                }
                case 'Move':
                  locals.set(operation.destination.ordinal, readLocal(operation.source))
                  break
                case 'BeginLoan': {
                  if (operation.sourceType._tag === 'Slice') {
                    locals.set(operation.destination.ordinal, readLocal(operation.root))
                    break
                  }
                  yield* materializeAddressRoot(operation.root)
                  const base = addressStorage.get(operation.root.ordinal)
                  if (base === undefined)
                    throw new RangeError('LLVM borrow formation lost its root')
                  if (operation.type._tag === 'Reference') {
                    locals.set(operation.destination.ordinal, Object.freeze([base]))
                    break
                  }
                  if (operation.sourceType._tag !== 'FixedArray') {
                    throw new RangeError('LLVM slice formation requires an array root')
                  }
                  locals.set(
                    operation.destination.ordinal,
                    Object.freeze([
                      base,
                      yield* Constant.integerUnsigned(
                        builder,
                        usizeType ?? i32,
                        BigInt(operation.sourceType.type.length),
                      ),
                    ]),
                  )
                  break
                }
                case 'EndLoan':
                  break
                case 'SliceLength': {
                  const length = readLocal(operation.slice).at(1)
                  if (length === undefined) throw new RangeError('LLVM slice lost its length lane')
                  locals.set(operation.destination.ordinal, Object.freeze([length]))
                  break
                }
                case 'ConvertUnion': {
                  const source = readLocal(operation.source)
                  const targetWidth = operation.targetShape.laneCount
                  const zero = yield* Constant.integerSigned(builder, i32, 0n)
                  const sourceLanes = operation.sourceShape.lanes
                  const targetLanes = operation.targetShape.lanes
                  if (operation.conversion === 'Inject') {
                    const mapping = operation.mappings.at(0)
                    if (mapping === undefined) {
                      throw new RangeError('LLVM union injection has no member map')
                    }
                    const tag = yield* Constant.integerSigned(
                      builder,
                      i32,
                      BigInt(mapping.targetOrdinal),
                    )
                    const payload: Array<Value.Input> = []
                    for (let ordinal = 0; ordinal < Math.max(0, targetWidth - 1); ordinal += 1) {
                      const targetLane = targetLanes.at(ordinal + 1)
                      if (targetLane === undefined) {
                        throw new RangeError('LLVM union injection lost a target payload lane')
                      }
                      const input = source.at(ordinal)
                      const sourceLane = sourceLanes.at(ordinal)
                      payload.push(
                        input === undefined || sourceLane === undefined
                          ? yield* Constant.integerUnsigned(builder, laneType(targetLane), 0n)
                          : yield* coerceLane(
                              input,
                              sourceLane,
                              targetLane,
                              `union${operation.destination.ordinal}_${ordinal}_inject`,
                            ),
                      )
                    }
                    locals.set(operation.destination.ordinal, Object.freeze([tag, ...payload]))
                    break
                  }
                  const sourceTag = source.at(0)
                  if (sourceTag === undefined) {
                    throw new RangeError('LLVM union widening has no source tag')
                  }
                  let tag: Value.Input = zero
                  for (const [ordinal, mapping] of operation.mappings.entries()) {
                    const sourceOrdinal = yield* Constant.integerSigned(
                      builder,
                      i32,
                      BigInt(mapping.sourceOrdinal),
                    )
                    const matches = yield* FunctionBody.integerCompare(
                      body,
                      'eq',
                      sourceTag,
                      sourceOrdinal,
                      `union${operation.destination.ordinal}_${ordinal}_matches`,
                    )
                    const targetOrdinal = yield* Constant.integerSigned(
                      builder,
                      i32,
                      BigInt(mapping.targetOrdinal),
                    )
                    tag = yield* FunctionBody.select(
                      body,
                      matches,
                      targetOrdinal,
                      tag,
                      `union${operation.destination.ordinal}_${ordinal}_tag`,
                    )
                  }
                  const payload: Array<Value.Input> = []
                  for (let ordinal = 0; ordinal < Math.max(0, targetWidth - 1); ordinal += 1) {
                    const targetLane = targetLanes.at(ordinal + 1)
                    if (targetLane === undefined) {
                      throw new RangeError('LLVM union widening lost a target payload lane')
                    }
                    const input = source.at(ordinal + 1)
                    const sourceLane = sourceLanes.at(ordinal + 1)
                    payload.push(
                      input === undefined || sourceLane === undefined
                        ? yield* Constant.integerUnsigned(builder, laneType(targetLane), 0n)
                        : yield* coerceLane(
                            input,
                            sourceLane,
                            targetLane,
                            `union${operation.destination.ordinal}_${ordinal}_widen`,
                          ),
                    )
                  }
                  locals.set(operation.destination.ordinal, Object.freeze([tag, ...payload]))
                  break
                }
                case 'Construct':
                  locals.set(
                    operation.destination.ordinal,
                    Object.freeze(operation.fields.flatMap((field) => [...readLocal(field.value)])),
                  )
                  break
                case 'ConstructArray':
                  locals.set(
                    operation.destination.ordinal,
                    Object.freeze(operation.elements.flatMap((element) => [...readLocal(element)])),
                  )
                  break
                case 'Project': {
                  const sourceType = entry.fn.localTypes.at(operation.source.ordinal)
                  if (sourceType === undefined) {
                    throw new RangeError('Backend projection lost its source type')
                  }
                  const sourceLanes = lanesFor(sourceType)
                  const sourceValues = readLocal(operation.source)
                  const projected = sourceLanes.flatMap((lane, index) => {
                    const first = lane.path.at(0)
                    const selected = sourceValues.at(index)
                    return first !== undefined &&
                      first._tag === 'FieldId' &&
                      selected !== undefined &&
                      first.ordinal === operation.field.ordinal &&
                      first.struct.sourceId === operation.field.struct.sourceId &&
                      first.struct.ordinal === operation.field.struct.ordinal
                      ? [selected]
                      : []
                  })
                  locals.set(operation.destination.ordinal, Object.freeze(projected))
                  break
                }
                case 'ReadPlace': {
                  const sourceType = entry.fn.localTypes.at(operation.root.ordinal)
                  if (sourceType === undefined) {
                    throw new RangeError('Backend place read lost its root type')
                  }
                  const sourceSemantic = Mir.semanticType(sourceType)
                  if (SilkType.isReference(sourceSemantic)) {
                    // The place lives on the referenced target: static field offsets off the
                    // borrow's address, one load per lane of the projected value.
                    const address = readLocal(operation.root).at(0)
                    if (address === undefined)
                      throw new RangeError('LLVM reference read lost its address')
                    const base = yield* FunctionBody.cast(
                      body,
                      'inttoptr',
                      address,
                      pointer,
                      `reference_read${operation.destination.ordinal}_base`,
                    )
                    const staticSelectors: Array<Layout.Selector> = []
                    for (const candidate of operation.selectors) {
                      if (candidate._tag !== 'FieldSelector')
                        throw new RangeError('LLVM reference place supports only field selectors')
                      staticSelectors.push(candidate.field)
                    }
                    const target = sourceSemantic.target
                    const values: Array<Value.Input> = []
                    for (const [ordinal, lane] of lanesFor(operation.type).entries()) {
                      const offset = Layout.laneOffset(program.layout, target, [
                        ...staticSelectors,
                        ...lane.path,
                      ])
                      if (offset === undefined)
                        throw new RangeError('LLVM reference read lost a lane offset')
                      values.push(
                        yield* FunctionBody.load(
                          body,
                          laneType(lane),
                          yield* constantBytePointer(
                            base,
                            offset,
                            `reference_read${operation.destination.ordinal}_${ordinal}_ptr`,
                          ),
                          `reference_read${operation.destination.ordinal}_${ordinal}`,
                        ),
                      )
                    }
                    locals.set(operation.destination.ordinal, Object.freeze(values))
                    break
                  }
                  if (SilkType.isSlice(sourceSemantic)) {
                    const [selector, ...suffixSelectors] = operation.selectors
                    if (selector?._tag !== 'SliceElementSelector') {
                      throw new RangeError('LLVM slice read lost its runtime element selector')
                    }
                    const [base, length] = readLocal(operation.root)
                    if (base === undefined || length === undefined) {
                      throw new RangeError('LLVM slice read lost its address or length lane')
                    }
                    if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'trap')
                    const index = readScalar(selector.index)
                    const inBounds = yield* FunctionBody.integerCompare(
                      body,
                      'ult',
                      index,
                      length,
                      `slice${checkOrdinal}_in_bounds`,
                    )
                    yield* locate(
                      selector.provenance.span,
                      yield* Value.instruction(body, inBounds),
                    )
                    const continueBlock = yield* LlvmBlock.make(body, `slice${checkOrdinal}_ok`)
                    yield* FunctionBody.conditionalBranch(body, inBounds, continueBlock, trapBlock)
                    yield* LlvmBlock.setInsertionPoint(body, continueBlock)
                    const sliceLayout = Layout.entry(program.layout, sourceSemantic)
                    if (sliceLayout?.representation._tag !== 'Slice') {
                      throw new RangeError('LLVM slice read lost its compiler layout')
                    }
                    const stride = yield* Constant.integerUnsigned(
                      builder,
                      usizeType ?? i32,
                      BigInt(sliceLayout.representation.stride),
                    )
                    const elementOffset = yield* FunctionBody.binary(
                      body,
                      'mul',
                      index,
                      stride,
                      `slice${checkOrdinal}_element_offset`,
                    )
                    const staticSelectors: Array<Layout.Selector> = []
                    for (const candidate of suffixSelectors) {
                      if (candidate._tag === 'FieldSelector') {
                        staticSelectors.push(candidate.field)
                      } else if (
                        candidate._tag === 'ElementSelector' &&
                        candidate.index._tag === 'Proven'
                      ) {
                        staticSelectors.push(
                          Object.freeze({
                            _tag: 'ElementSelector',
                            index: candidate.index.value,
                          }),
                        )
                      } else {
                        throw new RangeError('LLVM nested runtime slice place is not canonical')
                      }
                    }
                    const selectedValues: Array<Value.Input> = []
                    for (const [laneOrdinal, lane] of lanesFor(operation.type).entries()) {
                      const staticOffset = Layout.laneOffset(
                        program.layout,
                        sourceSemantic.element,
                        Object.freeze([...staticSelectors, ...lane.path]),
                      )
                      if (staticOffset === undefined) {
                        throw new RangeError(`LLVM slice read lost lane ${laneOrdinal}`)
                      }
                      const offset =
                        staticOffset === 0
                          ? elementOffset
                          : yield* FunctionBody.binary(
                              body,
                              'add',
                              elementOffset,
                              yield* Constant.integerUnsigned(
                                builder,
                                usizeType ?? i32,
                                BigInt(staticOffset),
                              ),
                              `slice${checkOrdinal}_${laneOrdinal}_offset`,
                            )
                      const address = yield* bytePointer(
                        base,
                        offset,
                        `slice${checkOrdinal}_${laneOrdinal}_ptr`,
                      )
                      selectedValues.push(
                        yield* FunctionBody.load(
                          body,
                          laneType(lane),
                          address,
                          `slice${checkOrdinal}_${laneOrdinal}`,
                        ),
                      )
                    }
                    locals.set(operation.destination.ordinal, Object.freeze(selectedValues))
                    checkOrdinal += 1
                    break
                  }
                  const sourceLanes = lanesFor(sourceType)
                  const sourceValues = readLocal(operation.root)
                  const destinationLanes = lanesFor(operation.type)
                  const runtimeSelectors = operation.selectors.flatMap((selector, ordinal) =>
                    selector._tag === 'ElementSelector' && selector.index._tag === 'Runtime'
                      ? [
                          Object.freeze({
                            local: selector.index.local,
                            length: selector.length,
                            span: selector.provenance.span,
                            ordinal,
                          }),
                        ]
                      : [],
                  )
                  for (const [runtimeOrdinal, selector] of runtimeSelectors.entries()) {
                    if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'trap')
                    const limit = yield* Constant.integerUnsigned(
                      builder,
                      usizeType ?? i32,
                      BigInt(selector.length),
                    )
                    const inBounds = yield* FunctionBody.integerCompare(
                      body,
                      'ult',
                      readScalar(selector.local),
                      limit,
                      `index${checkOrdinal}_${runtimeOrdinal}_in_bounds`,
                    )
                    const instruction = yield* Value.instruction(body, inBounds)
                    yield* locate(selector.span, instruction)
                    const continueBlock = yield* LlvmBlock.make(
                      body,
                      `index${checkOrdinal}_${runtimeOrdinal}_ok`,
                    )
                    yield* FunctionBody.conditionalBranch(body, inBounds, continueBlock, trapBlock)
                    yield* LlvmBlock.setInsertionPoint(body, continueBlock)
                  }

                  const selectedValues: Array<Value.Input> = []
                  for (const [destinationOrdinal, destinationLane] of destinationLanes.entries()) {
                    const candidates = sourceLanes.flatMap((sourceLane, sourceOrdinal) => {
                      if (
                        sourceLane.path.length !==
                        operation.selectors.length + destinationLane.path.length
                      ) {
                        return []
                      }
                      const runtimeElements: Array<{
                        readonly local: Mir.LocalId
                        readonly element: number
                      }> = []
                      for (const [selectorOrdinal, selector] of operation.selectors.entries()) {
                        const physical = sourceLane.path.at(selectorOrdinal)
                        if (physical === undefined) return []
                        if (selector._tag === 'FieldSelector') {
                          if (
                            physical._tag !== 'FieldId' ||
                            physical.ordinal !== selector.field.ordinal ||
                            physical.struct.sourceId !== selector.field.struct.sourceId ||
                            physical.struct.ordinal !== selector.field.struct.ordinal
                          ) {
                            return []
                          }
                        } else {
                          if (physical._tag !== 'ElementSelector') return []
                          if (
                            selector.index._tag === 'Proven' &&
                            physical.index !== selector.index.value
                          ) {
                            return []
                          }
                          if (selector.index._tag === 'Runtime') {
                            runtimeElements.push(
                              Object.freeze({
                                local: selector.index.local,
                                element: physical.index,
                              }),
                            )
                          }
                        }
                      }
                      const suffix = sourceLane.path.slice(operation.selectors.length)
                      const sameSuffix = suffix.every((physical, ordinal) => {
                        const expected = destinationLane.path.at(ordinal)
                        return expected !== undefined && Layout.selectorEquals(physical, expected)
                      })
                      const selected = sourceValues.at(sourceOrdinal)
                      return sameSuffix && selected !== undefined
                        ? [Object.freeze({ value: selected, runtimeElements })]
                        : []
                    })
                    const first = candidates.at(0)
                    if (
                      first === undefined &&
                      operation.selectors.some(
                        (selector) => selector._tag === 'ElementSelector' && selector.length === 0,
                      )
                    ) {
                      selectedValues.push(yield* Constant.integerSigned(builder, i32, 0n))
                      continue
                    }
                    if (first === undefined) {
                      throw new RangeError(
                        `Backend could not realize place-read lane ${destinationOrdinal}`,
                      )
                    }
                    let selected = first.value
                    for (const [candidateOrdinal, candidate] of candidates.slice(1).entries()) {
                      let condition: Value.Input | undefined
                      for (const [elementOrdinal, element] of candidate.runtimeElements.entries()) {
                        const expected = yield* Constant.integerUnsigned(
                          builder,
                          usizeType ?? i32,
                          BigInt(element.element),
                        )
                        const equal = yield* FunctionBody.integerCompare(
                          body,
                          'eq',
                          readScalar(element.local),
                          expected,
                          `index${checkOrdinal}_${destinationOrdinal}_${candidateOrdinal}_${elementOrdinal}`,
                        )
                        condition =
                          condition === undefined
                            ? equal
                            : yield* FunctionBody.binary(
                                body,
                                'and',
                                condition,
                                equal,
                                `index${checkOrdinal}_${destinationOrdinal}_${candidateOrdinal}_${elementOrdinal}_all`,
                              )
                      }
                      if (condition !== undefined) {
                        selected = yield* FunctionBody.select(
                          body,
                          condition,
                          candidate.value,
                          selected,
                          `index${checkOrdinal}_${destinationOrdinal}_${candidateOrdinal}_value`,
                        )
                      }
                    }
                    selectedValues.push(selected)
                  }
                  checkOrdinal += 1
                  locals.set(operation.destination.ordinal, Object.freeze(selectedValues))
                  break
                }
                case 'CheckPlace': {
                  const rootType = entry.fn.localTypes.at(operation.root.ordinal)
                  if (rootType?._tag === 'Slice') {
                    const selector = operation.selectors.at(0)
                    const length = readLocal(operation.root).at(1)
                    if (selector?._tag !== 'SliceElementSelector' || length === undefined) {
                      throw new RangeError('LLVM slice write check lost its canonical lanes')
                    }
                    if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'trap')
                    const inBounds = yield* FunctionBody.integerCompare(
                      body,
                      'ult',
                      readScalar(selector.index),
                      length,
                      `write_slice${checkOrdinal}_in_bounds`,
                    )
                    yield* locate(
                      selector.provenance.span,
                      yield* Value.instruction(body, inBounds),
                    )
                    const continueBlock = yield* LlvmBlock.make(
                      body,
                      `write_slice${checkOrdinal}_ok`,
                    )
                    yield* FunctionBody.conditionalBranch(body, inBounds, continueBlock, trapBlock)
                    yield* LlvmBlock.setInsertionPoint(body, continueBlock)
                    checkOrdinal += 1
                    break
                  }
                  const runtimeSelectors = operation.selectors.flatMap((selector, ordinal) =>
                    selector._tag === 'ElementSelector' && selector.index._tag === 'Runtime'
                      ? [
                          Object.freeze({
                            local: selector.index.local,
                            length: selector.length,
                            span: selector.provenance.span,
                            ordinal,
                          }),
                        ]
                      : [],
                  )
                  for (const [runtimeOrdinal, selector] of runtimeSelectors.entries()) {
                    if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'trap')
                    const limit = yield* Constant.integerUnsigned(
                      builder,
                      usizeType ?? i32,
                      BigInt(selector.length),
                    )
                    const inBounds = yield* FunctionBody.integerCompare(
                      body,
                      'ult',
                      readScalar(selector.local),
                      limit,
                      `write_index${checkOrdinal}_${runtimeOrdinal}_in_bounds`,
                    )
                    const instruction = yield* Value.instruction(body, inBounds)
                    yield* locate(selector.span, instruction)
                    const continueBlock = yield* LlvmBlock.make(
                      body,
                      `write_index${checkOrdinal}_${runtimeOrdinal}_ok`,
                    )
                    yield* FunctionBody.conditionalBranch(body, inBounds, continueBlock, trapBlock)
                    yield* LlvmBlock.setInsertionPoint(body, continueBlock)
                  }
                  checkOrdinal += 1
                  break
                }
                case 'WritePlace': {
                  if (operation.rootType._tag === 'Reference') {
                    // Writing through the borrow stores each value lane at its target offset.
                    const address = readLocal(operation.root).at(0)
                    if (address === undefined)
                      throw new RangeError('LLVM reference write lost its address')
                    const base = yield* FunctionBody.cast(
                      body,
                      'inttoptr',
                      address,
                      pointer,
                      `reference_write${operation.source.ordinal}_base`,
                    )
                    const staticSelectors: Array<Layout.Selector> = []
                    for (const candidate of operation.selectors) {
                      if (candidate._tag !== 'FieldSelector')
                        throw new RangeError('LLVM reference place supports only field selectors')
                      staticSelectors.push(candidate.field)
                    }
                    const target = operation.rootType.type.target
                    const values = readLocal(operation.source)
                    for (const [ordinal, lane] of lanesFor(operation.type).entries()) {
                      const value = values.at(ordinal)
                      const offset = Layout.laneOffset(program.layout, target, [
                        ...staticSelectors,
                        ...lane.path,
                      ])
                      if (value === undefined || offset === undefined)
                        throw new RangeError('LLVM reference write lost a lane offset')
                      yield* FunctionBody.store(
                        body,
                        value,
                        yield* constantBytePointer(
                          base,
                          offset,
                          `reference_write${operation.source.ordinal}_${ordinal}_ptr`,
                        ),
                      )
                    }
                    break
                  }
                  if (operation.rootType._tag === 'Slice') {
                    const [selector, ...suffixSelectors] = operation.selectors
                    const [base] = readLocal(operation.root)
                    if (selector?._tag !== 'SliceElementSelector' || base === undefined) {
                      throw new RangeError('LLVM slice write lost its canonical address lane')
                    }
                    const sliceLayout = Layout.entry(program.layout, operation.rootType.type)
                    if (sliceLayout?.representation._tag !== 'Slice') {
                      throw new RangeError('LLVM slice write lost its compiler layout')
                    }
                    const stride = yield* Constant.integerUnsigned(
                      builder,
                      usizeType ?? i32,
                      BigInt(sliceLayout.representation.stride),
                    )
                    const elementOffset = yield* FunctionBody.binary(
                      body,
                      'mul',
                      readScalar(selector.index),
                      stride,
                      `write_slice${checkOrdinal}_element_offset`,
                    )
                    const staticSelectors: Array<Layout.Selector> = []
                    for (const candidate of suffixSelectors) {
                      if (candidate._tag === 'FieldSelector') {
                        staticSelectors.push(candidate.field)
                      } else if (
                        candidate._tag === 'ElementSelector' &&
                        candidate.index._tag === 'Proven'
                      ) {
                        staticSelectors.push(
                          Object.freeze({
                            _tag: 'ElementSelector',
                            index: candidate.index.value,
                          }),
                        )
                      } else {
                        throw new RangeError('LLVM nested runtime slice write is not canonical')
                      }
                    }
                    const sourceValues = readLocal(operation.source)
                    for (const [laneOrdinal, lane] of lanesFor(operation.type).entries()) {
                      const staticOffset = Layout.laneOffset(
                        program.layout,
                        operation.rootType.type.element,
                        Object.freeze([...staticSelectors, ...lane.path]),
                      )
                      const stored = sourceValues.at(laneOrdinal)
                      if (staticOffset === undefined || stored === undefined) {
                        throw new RangeError(`LLVM slice write lost lane ${laneOrdinal}`)
                      }
                      const offset =
                        staticOffset === 0
                          ? elementOffset
                          : yield* FunctionBody.binary(
                              body,
                              'add',
                              elementOffset,
                              yield* Constant.integerUnsigned(
                                builder,
                                usizeType ?? i32,
                                BigInt(staticOffset),
                              ),
                              `write_slice${checkOrdinal}_${laneOrdinal}_offset`,
                            )
                      yield* FunctionBody.store(
                        body,
                        stored,
                        yield* bytePointer(
                          base,
                          offset,
                          `write_slice${checkOrdinal}_${laneOrdinal}_ptr`,
                        ),
                      )
                    }
                    checkOrdinal += 1
                    break
                  }
                  const rootLanes = lanesFor(operation.rootType)
                  const rootValues = readLocal(operation.root)
                  const sourceLanes = lanesFor(operation.type)
                  const sourceValues = readLocal(operation.source)
                  if (operation.selectors.length === 0) {
                    locals.set(operation.root.ordinal, sourceValues)
                    yield* storeMutable(operation.root, sourceValues)
                    break
                  }
                  const updated: Array<Value.Input> = []
                  for (const [rootOrdinal, rootLane] of rootLanes.entries()) {
                    const previous = rootValues.at(rootOrdinal)
                    if (previous === undefined) throw new RangeError('Mutable root lost a lane')
                    const runtimeElements: Array<{
                      readonly local: Mir.LocalId
                      readonly element: number
                    }> = []
                    let matches = true
                    for (const [selectorOrdinal, selector] of operation.selectors.entries()) {
                      const physical = rootLane.path.at(selectorOrdinal)
                      if (physical === undefined) {
                        matches = false
                        break
                      }
                      if (selector._tag === 'FieldSelector') {
                        if (
                          physical._tag !== 'FieldId' ||
                          physical.ordinal !== selector.field.ordinal ||
                          physical.struct.sourceId !== selector.field.struct.sourceId ||
                          physical.struct.ordinal !== selector.field.struct.ordinal
                        ) {
                          matches = false
                          break
                        }
                      } else if (selector._tag === 'SliceElementSelector') {
                        matches = false
                        break
                      } else if (physical._tag !== 'ElementSelector') {
                        matches = false
                        break
                      } else if (selector.index._tag === 'Proven') {
                        if (physical.index !== selector.index.value) {
                          matches = false
                          break
                        }
                      } else {
                        runtimeElements.push(
                          Object.freeze({
                            local: selector.index.local,
                            element: physical.index,
                          }),
                        )
                      }
                    }
                    if (!matches) {
                      updated.push(previous)
                      continue
                    }
                    const suffix = rootLane.path.slice(operation.selectors.length)
                    const sourceOrdinal = sourceLanes.findIndex(
                      (lane) =>
                        lane.path.length === suffix.length &&
                        lane.path.every((physical, ordinal) => {
                          const expected = suffix.at(ordinal)
                          return expected !== undefined && Layout.selectorEquals(physical, expected)
                        }),
                    )
                    const replacement = sourceValues.at(sourceOrdinal)
                    if (replacement === undefined) {
                      throw new RangeError(
                        `Backend could not realize place-write lane ${rootOrdinal}`,
                      )
                    }
                    let condition: Value.Input | undefined
                    for (const [elementOrdinal, element] of runtimeElements.entries()) {
                      const expected = yield* Constant.integerUnsigned(
                        builder,
                        usizeType ?? i32,
                        BigInt(element.element),
                      )
                      const equal = yield* FunctionBody.integerCompare(
                        body,
                        'eq',
                        readScalar(element.local),
                        expected,
                        `write_index${checkOrdinal}_${rootOrdinal}_${elementOrdinal}`,
                      )
                      condition =
                        condition === undefined
                          ? equal
                          : yield* FunctionBody.binary(
                              body,
                              'and',
                              condition,
                              equal,
                              `write_index${checkOrdinal}_${rootOrdinal}_${elementOrdinal}_all`,
                            )
                    }
                    updated.push(
                      condition === undefined
                        ? replacement
                        : yield* FunctionBody.select(
                            body,
                            condition,
                            replacement,
                            previous,
                            `write_index${checkOrdinal}_${rootOrdinal}_value`,
                          ),
                    )
                  }
                  checkOrdinal += 1
                  const frozen = Object.freeze(updated)
                  locals.set(operation.root.ordinal, frozen)
                  yield* storeMutable(operation.root, frozen)
                  break
                }
                case 'ConvertInteger': {
                  const result = yield* emitIntegerConversion(
                    readScalar(operation.source),
                    operation.sourceType,
                    operation.type,
                    `convert${operation.destination.ordinal}`,
                  )
                  locals.set(operation.destination.ordinal, Object.freeze([result]))
                  break
                }
                case 'ConvertScalar': {
                  const source = Scalar.find(operation.sourceType._tag)
                  const target = Scalar.find(operation.type._tag)
                  if (
                    source === undefined ||
                    target === undefined ||
                    source.category === 'Boolean' ||
                    target.category === 'Boolean'
                  )
                    throw new RangeError('LLVM scalar conversion lost its types')
                  const sourceValue = readScalar(operation.source)
                  const destinationType =
                    target.category === 'Floating'
                      ? target.spelling === 'f32'
                        ? f32
                        : f64
                      : (integerTypes.get(
                          Scalar.bits(target, program.layout.target.pointerSize === 4 ? 32 : 64),
                        ) ?? i32)
                  const kind: FunctionBody.CastKind =
                    source.category === 'Floating' && target.category === 'Floating'
                      ? source.spelling === 'f64'
                        ? 'fptrunc'
                        : 'fpext'
                      : source.category === 'Floating' && target.category === 'Integer'
                        ? target.signedness === 'Signed'
                          ? 'fptosi'
                          : 'fptoui'
                        : source.category === 'Integer' && target.category === 'Floating'
                          ? source.signedness === 'Signed'
                            ? 'sitofp'
                            : 'uitofp'
                          : (() => {
                              throw new RangeError('LLVM scalar conversion was not numeric')
                            })()
                  const result = yield* FunctionBody.cast(
                    body,
                    kind,
                    sourceValue,
                    destinationType,
                    `convert${operation.destination.ordinal}`,
                  )
                  locals.set(operation.destination.ordinal, Object.freeze([result]))
                  break
                }
                case 'ReinterpretScalar': {
                  const targetLane = lanesFor(operation.type).at(0)
                  if (targetLane === undefined)
                    throw new RangeError('LLVM reinterpretation lost its lane')
                  const result = yield* FunctionBody.cast(
                    body,
                    'bitcast',
                    readScalar(operation.source),
                    laneType(targetLane),
                    `reinterpret${operation.destination.ordinal}`,
                  )
                  locals.set(operation.destination.ordinal, Object.freeze([result]))
                  break
                }
                case 'FloatUnary': {
                  const source = Scalar.find(operation.sourceType._tag)
                  if (source?.category !== 'Floating')
                    throw new RangeError('LLVM float unary lost its source type')
                  const subject = readScalar(operation.source)
                  if (operation.operation === 'Negate') {
                    const result = yield* FunctionBody.unary(
                      body,
                      'fneg',
                      subject,
                      `fneg${operation.destination.ordinal}`,
                    )
                    locals.set(operation.destination.ordinal, Object.freeze([result]))
                    break
                  }
                  const width = source.spelling === 'f32' ? 32 : 64
                  const integerType = integerTypes.get(width) ?? i32
                  const raw = yield* FunctionBody.cast(
                    body,
                    'bitcast',
                    subject,
                    integerType,
                    `floatbits${operation.destination.ordinal}`,
                  )
                  const fractionBits = source.spelling === 'f32' ? 23 : 52
                  const exponentBits = source.spelling === 'f32' ? 8 : 11
                  const exponentMask = ((1n << BigInt(exponentBits)) - 1n) << BigInt(fractionBits)
                  const fractionMask = (1n << BigInt(fractionBits)) - 1n
                  const zero = yield* Constant.integerUnsigned(builder, integerType, 0n)
                  const exponentMaskValue = yield* Constant.integerUnsigned(
                    builder,
                    integerType,
                    exponentMask,
                  )
                  const fractionMaskValue = yield* Constant.integerUnsigned(
                    builder,
                    integerType,
                    fractionMask,
                  )
                  const exponent = yield* FunctionBody.binary(
                    body,
                    'and',
                    raw,
                    exponentMaskValue,
                    `fclass_exp${operation.destination.ordinal}`,
                  )
                  const fraction = yield* FunctionBody.binary(
                    body,
                    'and',
                    raw,
                    fractionMaskValue,
                    `fclass_frac${operation.destination.ordinal}`,
                  )
                  const exponentZero = yield* FunctionBody.integerCompare(
                    body,
                    'eq',
                    exponent,
                    zero,
                    `fclass_exp_zero${operation.destination.ordinal}`,
                  )
                  const exponentAll = yield* FunctionBody.integerCompare(
                    body,
                    'eq',
                    exponent,
                    exponentMaskValue,
                    `fclass_exp_all${operation.destination.ordinal}`,
                  )
                  const fractionZero = yield* FunctionBody.integerCompare(
                    body,
                    'eq',
                    fraction,
                    zero,
                    `fclass_frac_zero${operation.destination.ordinal}`,
                  )
                  let flag: Value.Input
                  if (operation.operation === 'IsSignNegative') {
                    flag = yield* FunctionBody.integerCompare(
                      body,
                      'slt',
                      raw,
                      zero,
                      `fclass_sign${operation.destination.ordinal}`,
                    )
                  } else if (operation.operation === 'IsNaN') {
                    const fractionNonzero = yield* FunctionBody.integerCompare(
                      body,
                      'ne',
                      fraction,
                      zero,
                      `fclass_frac_nonzero${operation.destination.ordinal}`,
                    )
                    flag = yield* FunctionBody.binary(
                      body,
                      'and',
                      exponentAll,
                      fractionNonzero,
                      `fclass_nan${operation.destination.ordinal}`,
                    )
                  } else if (operation.operation === 'IsInfinite') {
                    flag = yield* FunctionBody.binary(
                      body,
                      'and',
                      exponentAll,
                      fractionZero,
                      `fclass_inf${operation.destination.ordinal}`,
                    )
                  } else if (operation.operation === 'IsFinite') {
                    flag = yield* FunctionBody.integerCompare(
                      body,
                      'ne',
                      exponent,
                      exponentMaskValue,
                      `fclass_finite${operation.destination.ordinal}`,
                    )
                  } else if (operation.operation === 'IsNormal') {
                    const nonzero = yield* FunctionBody.integerCompare(
                      body,
                      'ne',
                      exponent,
                      zero,
                      `fclass_nonzero${operation.destination.ordinal}`,
                    )
                    const finite = yield* FunctionBody.integerCompare(
                      body,
                      'ne',
                      exponent,
                      exponentMaskValue,
                      `fclass_notall${operation.destination.ordinal}`,
                    )
                    flag = yield* FunctionBody.binary(
                      body,
                      'and',
                      nonzero,
                      finite,
                      `fclass_normal${operation.destination.ordinal}`,
                    )
                  } else {
                    const fractionNonzero = yield* FunctionBody.integerCompare(
                      body,
                      'ne',
                      fraction,
                      zero,
                      `fclass_sub_frac${operation.destination.ordinal}`,
                    )
                    flag = yield* FunctionBody.binary(
                      body,
                      'and',
                      exponentZero,
                      fractionNonzero,
                      `fclass_sub${operation.destination.ordinal}`,
                    )
                  }
                  const result = yield* FunctionBody.cast(
                    body,
                    'zext',
                    flag,
                    i32,
                    `fclass${operation.destination.ordinal}`,
                  )
                  locals.set(operation.destination.ordinal, Object.freeze([result]))
                  break
                }
                case 'FloatTranscendental': {
                  const source = Scalar.find(operation.sourceType._tag)
                  if (source?.category !== 'Floating')
                    throw new RangeError('LLVM transcendental lost its source type')
                  const width = source.spelling === 'f32' ? 32 : 64
                  const floatType = source.spelling === 'f32' ? f32 : f64
                  const format = source.spelling === 'f32' ? 'float' : 'double'
                  const self = Transcendental.plan(width)
                  const constant = Effect.fnUntraced(function* (bits: bigint) {
                    return yield* Constant.floatingRaw(
                      builder,
                      floatType,
                      format,
                      FloatingPoint.littleEndianBytes({ width, bits }),
                    )
                  })
                  const binary = Effect.fnUntraced(function* (
                    kind: FunctionBody.FloatingBinaryKind,
                    left: Value.Input,
                    right: Value.Input,
                    name: string,
                  ) {
                    return yield* FunctionBody.binary(body, kind, left, right, name)
                  })
                  const subject = readScalar(operation.source)
                  const zero = yield* constant(0n)
                  const half = yield* constant(self.half)
                  const negativeHalf = yield* FunctionBody.unary(
                    body,
                    'fneg',
                    half,
                    `trans_half_neg${operation.destination.ordinal}`,
                  )
                  const negative = yield* FunctionBody.floatingCompare(
                    body,
                    'olt',
                    subject,
                    zero,
                    `trans_negative${operation.destination.ordinal}`,
                  )
                  const offset = yield* FunctionBody.select(
                    body,
                    negative,
                    negativeHalf,
                    half,
                    `trans_offset${operation.destination.ordinal}`,
                  )
                  const scaled = yield* binary(
                    'fmul',
                    subject,
                    yield* constant(self.inverseHalfPi),
                    `trans_scaled${operation.destination.ordinal}`,
                  )
                  const shifted = yield* binary(
                    'fadd',
                    scaled,
                    offset,
                    `trans_shifted${operation.destination.ordinal}`,
                  )
                  const i64Type = integerTypes.get(64) ?? i32
                  const quadrantInteger = yield* FunctionBody.cast(
                    body,
                    'fptosi',
                    shifted,
                    i64Type,
                    `trans_quadrant_integer${operation.destination.ordinal}`,
                  )
                  const quadrantFloat = yield* FunctionBody.cast(
                    body,
                    'sitofp',
                    quadrantInteger,
                    floatType,
                    `trans_quadrant_float${operation.destination.ordinal}`,
                  )
                  let residual: Value.Input = subject
                  for (const [index, part] of self.halfPi.entries()) {
                    const product = yield* binary(
                      'fmul',
                      quadrantFloat,
                      yield* constant(part),
                      `trans_reduce_product${operation.destination.ordinal}_${index}`,
                    )
                    residual = yield* binary(
                      'fsub',
                      residual,
                      product,
                      `trans_reduce${operation.destination.ordinal}_${index}`,
                    )
                  }
                  const squared = yield* binary(
                    'fmul',
                    residual,
                    residual,
                    `trans_squared${operation.destination.ordinal}`,
                  )
                  const polynomial = Effect.fnUntraced(function* (
                    coefficients: ReadonlyArray<bigint>,
                    name: string,
                  ) {
                    let result: Value.Input = yield* constant(coefficients.at(-1) ?? 0n)
                    for (let index = coefficients.length - 2; index >= 0; index -= 1) {
                      const product: Value.Input = yield* binary(
                        'fmul',
                        squared,
                        result,
                        `${name}_mul${index}`,
                      )
                      result = yield* binary(
                        'fadd',
                        yield* constant(coefficients[index] ?? 0n),
                        product,
                        `${name}_add${index}`,
                      )
                    }
                    return result
                  })
                  const sineTail = yield* polynomial(
                    self.sine,
                    `trans_sine_tail${operation.destination.ordinal}`,
                  )
                  const residualSquared = yield* binary(
                    'fmul',
                    residual,
                    squared,
                    `trans_residual_squared${operation.destination.ordinal}`,
                  )
                  const sine = yield* binary(
                    'fadd',
                    residual,
                    yield* binary(
                      'fmul',
                      residualSquared,
                      sineTail,
                      `trans_sine_product${operation.destination.ordinal}`,
                    ),
                    `trans_sine${operation.destination.ordinal}`,
                  )
                  const cosineTail = yield* polynomial(
                    self.cosine,
                    `trans_cosine_tail${operation.destination.ordinal}`,
                  )
                  const cosineBase = yield* binary(
                    'fsub',
                    yield* constant(self.one),
                    yield* binary(
                      'fmul',
                      half,
                      squared,
                      `trans_cosine_half${operation.destination.ordinal}`,
                    ),
                    `trans_cosine_base${operation.destination.ordinal}`,
                  )
                  const cosine = yield* binary(
                    'fadd',
                    cosineBase,
                    yield* binary(
                      'fmul',
                      yield* binary(
                        'fmul',
                        squared,
                        squared,
                        `trans_fourth${operation.destination.ordinal}`,
                      ),
                      cosineTail,
                      `trans_cosine_product${operation.destination.ordinal}`,
                    ),
                    `trans_cosine${operation.destination.ordinal}`,
                  )
                  const mask = yield* Constant.integerUnsigned(builder, i64Type, 3n)
                  const quadrant = yield* FunctionBody.binary(
                    body,
                    'and',
                    quadrantInteger,
                    mask,
                    `trans_quadrant${operation.destination.ordinal}`,
                  )
                  const quadrantConstant = (value: bigint) =>
                    Constant.integerUnsigned(builder, i64Type, value)
                  const isQuadrant = Effect.fnUntraced(function* (value: bigint) {
                    return yield* FunctionBody.integerCompare(
                      body,
                      'eq',
                      quadrant,
                      yield* quadrantConstant(value),
                      `trans_quadrant_${value.toString()}_${operation.destination.ordinal}`,
                    )
                  })
                  const negativeSine = yield* FunctionBody.unary(
                    body,
                    'fneg',
                    sine,
                    `trans_sine_neg${operation.destination.ordinal}`,
                  )
                  const negativeCosine = yield* FunctionBody.unary(
                    body,
                    'fneg',
                    cosine,
                    `trans_cosine_neg${operation.destination.ordinal}`,
                  )
                  const q2 = yield* FunctionBody.select(
                    body,
                    yield* isQuadrant(2n),
                    operation.operation === 'Sin' ? negativeSine : negativeCosine,
                    operation.operation === 'Sin' ? negativeCosine : sine,
                    `trans_q2${operation.destination.ordinal}`,
                  )
                  const q1 = yield* FunctionBody.select(
                    body,
                    yield* isQuadrant(1n),
                    operation.operation === 'Sin' ? cosine : negativeSine,
                    q2,
                    `trans_q1${operation.destination.ordinal}`,
                  )
                  const finite = yield* FunctionBody.select(
                    body,
                    yield* isQuadrant(0n),
                    operation.operation === 'Sin' ? sine : cosine,
                    q1,
                    `trans_finite${operation.destination.ordinal}`,
                  )
                  const unordered = yield* FunctionBody.floatingCompare(
                    body,
                    'uno',
                    subject,
                    subject,
                    `trans_nan${operation.destination.ordinal}`,
                  )
                  const positiveInfinity = yield* constant(
                    width === 32 ? 0x7f800000n : 0x7ff0000000000000n,
                  )
                  const negativeInfinity = yield* constant(
                    width === 32 ? 0xff800000n : 0xfff0000000000000n,
                  )
                  const positiveInfinite = yield* FunctionBody.floatingCompare(
                    body,
                    'oeq',
                    subject,
                    positiveInfinity,
                    `trans_positive_infinite${operation.destination.ordinal}`,
                  )
                  const negativeInfinite = yield* FunctionBody.floatingCompare(
                    body,
                    'oeq',
                    subject,
                    negativeInfinity,
                    `trans_negative_infinite${operation.destination.ordinal}`,
                  )
                  const infinite = yield* FunctionBody.binary(
                    body,
                    'or',
                    positiveInfinite,
                    negativeInfinite,
                    `trans_infinite${operation.destination.ordinal}`,
                  )
                  const nonFinite = yield* FunctionBody.binary(
                    body,
                    'or',
                    unordered,
                    infinite,
                    `trans_nonfinite${operation.destination.ordinal}`,
                  )
                  const isZero = yield* FunctionBody.floatingCompare(
                    body,
                    'oeq',
                    subject,
                    zero,
                    `trans_zero${operation.destination.ordinal}`,
                  )
                  const finiteWithZero = yield* FunctionBody.select(
                    body,
                    isZero,
                    operation.operation === 'Sin' ? subject : yield* constant(self.one),
                    finite,
                    `trans_finite_zero${operation.destination.ordinal}`,
                  )
                  const result = yield* FunctionBody.select(
                    body,
                    nonFinite,
                    yield* constant(self.canonicalNaN),
                    finiteWithZero,
                    `transcendental${operation.destination.ordinal}`,
                  )
                  yield* locate(operation.provenance.span, yield* Value.instruction(body, result))
                  locals.set(operation.destination.ordinal, Object.freeze([result]))
                  break
                }
                case 'CheckedInteger': {
                  const leftLocal = operation.operands.at(0)
                  const rightLocal = operation.operands.at(1)
                  const source = Scalar.find(operation.sourceType._tag)
                  const target = Scalar.find(operation.valueType._tag)
                  if (
                    leftLocal === undefined ||
                    source?.category !== 'Integer' ||
                    target?.category !== 'Integer'
                  )
                    throw new RangeError('LLVM checked integer operation lost its scalar types')
                  const left = readScalar(leftLocal)
                  const right = rightLocal === undefined ? undefined : readScalar(rightLocal)
                  const pointerBits = program.layout.target.pointerSize === 4 ? 32 : 64
                  const sourceBits = Scalar.bits(source, pointerBits)
                  const targetBits = Scalar.bits(target, pointerBits)
                  const sourcePhysical = integerTypes.get(sourceBits) ?? i32
                  const targetPhysical = integerTypes.get(targetBits) ?? i32
                  const name = `checked${operation.destination.ordinal}`
                  let result: Value.Input
                  let invalid: Value.Input
                  if (operation.operation.startsWith('CheckedConvertTo')) {
                    const sourceRange = Scalar.range(source, pointerBits)
                    const targetRange = Scalar.range(target, pointerBits)
                    const checks: Array<Value.Input> = []
                    if (targetRange.minimum > sourceRange.minimum)
                      checks.push(
                        yield* FunctionBody.integerCompare(
                          body,
                          source.signedness === 'Signed' ? 'slt' : 'ult',
                          left,
                          source.signedness === 'Signed'
                            ? yield* Constant.integerSigned(
                                builder,
                                sourcePhysical,
                                targetRange.minimum,
                              )
                            : yield* Constant.integerUnsigned(
                                builder,
                                sourcePhysical,
                                targetRange.minimum,
                              ),
                          `${name}_below`,
                        ),
                      )
                    if (targetRange.maximum < sourceRange.maximum)
                      checks.push(
                        yield* FunctionBody.integerCompare(
                          body,
                          source.signedness === 'Signed' ? 'sgt' : 'ugt',
                          left,
                          source.signedness === 'Signed'
                            ? yield* Constant.integerSigned(
                                builder,
                                sourcePhysical,
                                targetRange.maximum,
                              )
                            : yield* Constant.integerUnsigned(
                                builder,
                                sourcePhysical,
                                targetRange.maximum,
                              ),
                          `${name}_above`,
                        ),
                      )
                    invalid =
                      checks.at(0) ??
                      (yield* Constant.integerUnsigned(
                        builder,
                        yield* LlvmType.integer(builder, 1),
                        0n,
                      ))
                    for (const [ordinal, check] of checks.slice(1).entries())
                      invalid = yield* FunctionBody.binary(
                        body,
                        'or',
                        invalid,
                        check,
                        `${name}_invalid${ordinal}`,
                      )
                    result =
                      sourceBits === targetBits
                        ? left
                        : yield* FunctionBody.cast(
                            body,
                            sourceBits < targetBits
                              ? source.signedness === 'Signed'
                                ? 'sext'
                                : 'zext'
                              : 'trunc',
                            left,
                            targetPhysical,
                            `${name}_value`,
                          )
                  } else if (
                    operation.operation === 'CheckedAdd' ||
                    operation.operation === 'CheckedSubtract' ||
                    operation.operation === 'CheckedMultiply'
                  ) {
                    if (right === undefined)
                      throw new RangeError('LLVM checked arithmetic lost its right operand')
                    const signatures =
                      target.signedness === 'Unsigned'
                        ? unsignedOverflowSignatures
                        : signedOverflowSignatures
                    let signature = signatures.get(targetBits)
                    if (signature === undefined) {
                      const i1 = yield* LlvmType.integer(builder, 1)
                      signature = Object.freeze({
                        returnType: yield* LlvmType.structure(builder, [targetPhysical, i1]),
                        parameters: Object.freeze([targetPhysical, targetPhysical]),
                      })
                      signatures.set(targetBits, signature)
                    }
                    const stem =
                      operation.operation === 'CheckedAdd'
                        ? 'add'
                        : operation.operation === 'CheckedSubtract'
                          ? 'sub'
                          : 'mul'
                    const pair = yield* Intrinsic.call(
                      body,
                      `${target.signedness === 'Unsigned' ? 'u' : 's'}${stem}.with.overflow`,
                      [targetPhysical],
                      [left, right],
                      `${name}_pair`,
                      { signature },
                    )
                    if (pair === undefined)
                      throw new RangeError('LLVM checked arithmetic produced no outcome')
                    result = yield* FunctionBody.extractValue(body, pair, [0], `${name}_value`)
                    invalid = yield* FunctionBody.extractValue(body, pair, [1], `${name}_invalid`)
                  } else {
                    if (right === undefined)
                      throw new RangeError('LLVM checked division lost its right operand')
                    const zero = yield* Constant.integerUnsigned(builder, targetPhysical, 0n)
                    invalid = yield* FunctionBody.integerCompare(
                      body,
                      'eq',
                      right,
                      zero,
                      `${name}_zero`,
                    )
                    if (target.signedness === 'Signed' && operation.operation === 'CheckedDivide') {
                      const range = Scalar.range(target, pointerBits)
                      const minimum = yield* Constant.integerSigned(
                        builder,
                        targetPhysical,
                        range.minimum,
                      )
                      const negativeOne = yield* Constant.integerSigned(
                        builder,
                        targetPhysical,
                        -1n,
                      )
                      const minimumDividend = yield* FunctionBody.integerCompare(
                        body,
                        'eq',
                        left,
                        minimum,
                        `${name}_minimum`,
                      )
                      const negativeDivisor = yield* FunctionBody.integerCompare(
                        body,
                        'eq',
                        right,
                        negativeOne,
                        `${name}_negative_one`,
                      )
                      const overflow = yield* FunctionBody.binary(
                        body,
                        'and',
                        minimumDividend,
                        negativeDivisor,
                        `${name}_overflow`,
                      )
                      invalid = yield* FunctionBody.binary(
                        body,
                        'or',
                        invalid,
                        overflow,
                        `${name}_invalid`,
                      )
                    }
                    const one = yield* Constant.integerUnsigned(builder, targetPhysical, 1n)
                    const safeRight = yield* FunctionBody.select(
                      body,
                      invalid,
                      one,
                      right,
                      `${name}_divisor`,
                    )
                    result = yield* FunctionBody.binary(
                      body,
                      operation.operation === 'CheckedDivide'
                        ? target.signedness === 'Unsigned'
                          ? 'udiv'
                          : 'sdiv'
                        : target.signedness === 'Unsigned'
                          ? 'urem'
                          : 'srem',
                      left,
                      safeRight,
                      `${name}_value`,
                    )
                  }
                  const successOrdinal = operation.type.type.members.findIndex((member) =>
                    SilkType.equals(member, operation.success),
                  )
                  const failureOrdinal = operation.type.type.members.findIndex((member) =>
                    SilkType.equals(member, operation.failure),
                  )
                  if (successOrdinal < 0 || failureOrdinal < 0)
                    throw new RangeError('LLVM checked integer operation lost its Option members')
                  const successTag = yield* Constant.integerSigned(
                    builder,
                    i32,
                    BigInt(successOrdinal),
                  )
                  const failureTag = yield* Constant.integerSigned(
                    builder,
                    i32,
                    BigInt(failureOrdinal),
                  )
                  const tag = yield* FunctionBody.select(
                    body,
                    invalid,
                    failureTag,
                    successTag,
                    `${name}_tag`,
                  )
                  const valueLane = lanesFor(operation.valueType).at(0)
                  const payloadLane = lanesFor(operation.type).at(1)
                  if (valueLane === undefined || payloadLane === undefined)
                    throw new RangeError('LLVM checked integer operation lost its payload lane')
                  const payload = yield* coerceLane(
                    result,
                    valueLane,
                    payloadLane,
                    `${name}_payload`,
                  )
                  locals.set(operation.destination.ordinal, Object.freeze([tag, payload]))
                  break
                }
                case 'Binary': {
                  const left = readScalar(operation.left)
                  const right = readScalar(operation.right)
                  const leftType = entry.fn.localTypes.at(operation.left.ordinal)
                  const leftLane =
                    leftType === undefined ? undefined : valueLanesFor(leftType).at(0)
                  if (leftType === undefined || leftLane === undefined) {
                    throw new RangeError('LLVM binary operation lost its operand type')
                  }
                  const semanticOperand = Mir.semanticType(leftType)
                  const scalar =
                    typeof semanticOperand === 'string' ? Scalar.find(semanticOperand) : undefined
                  const unsigned = scalar?.signedness === 'Unsigned'
                  const operandType = laneType(leftLane)
                  const ordinal = checkOrdinal
                  checkOrdinal += 1
                  if (scalar?.category === 'Floating') {
                    if (operation.operator === 'TotalOrder') {
                      const width = scalar.spelling === 'f32' ? 32 : 64
                      const integerType = integerTypes.get(width) ?? i32
                      const leftBits = yield* FunctionBody.cast(
                        body,
                        'bitcast',
                        left,
                        integerType,
                        `total${ordinal}_left_bits`,
                      )
                      const rightBits = yield* FunctionBody.cast(
                        body,
                        'bitcast',
                        right,
                        integerType,
                        `total${ordinal}_right_bits`,
                      )
                      const zero = yield* Constant.integerUnsigned(builder, integerType, 0n)
                      const all = yield* Constant.integerUnsigned(
                        builder,
                        integerType,
                        (1n << BigInt(width)) - 1n,
                      )
                      const sign = yield* Constant.integerUnsigned(
                        builder,
                        integerType,
                        1n << BigInt(width - 1),
                      )
                      const key = Effect.fnUntraced(function* (bits: Value.Input, side: string) {
                        const negative = yield* FunctionBody.integerCompare(
                          body,
                          'slt',
                          bits,
                          zero,
                          `total${ordinal}_${side}_negative`,
                        )
                        const mask = yield* FunctionBody.select(
                          body,
                          negative,
                          all,
                          sign,
                          `total${ordinal}_${side}_mask`,
                        )
                        return yield* FunctionBody.binary(
                          body,
                          'xor',
                          bits,
                          mask,
                          `total${ordinal}_${side}_key`,
                        )
                      })
                      const leftKey = yield* key(leftBits, 'left')
                      const rightKey = yield* key(rightBits, 'right')
                      const flag = yield* FunctionBody.integerCompare(
                        body,
                        'ule',
                        leftKey,
                        rightKey,
                        `total${ordinal}_flag`,
                      )
                      const result = yield* FunctionBody.cast(
                        body,
                        'zext',
                        flag,
                        i32,
                        `total${ordinal}`,
                      )
                      locals.set(operation.destination.ordinal, Object.freeze([result]))
                      break
                    }
                    const predicate: FunctionBody.FloatingPredicate | undefined =
                      operation.operator === 'Equals'
                        ? 'oeq'
                        : operation.operator === 'NotEquals'
                          ? 'une'
                          : operation.operator === 'LessThan'
                            ? 'olt'
                            : operation.operator === 'LessOrEqual'
                              ? 'ole'
                              : operation.operator === 'GreaterThan'
                                ? 'ogt'
                                : operation.operator === 'GreaterOrEqual'
                                  ? 'oge'
                                  : undefined
                    if (predicate !== undefined) {
                      const flag = yield* FunctionBody.floatingCompare(
                        body,
                        predicate,
                        left,
                        right,
                        `fcmp${ordinal}_flag`,
                      )
                      const result = yield* FunctionBody.cast(
                        body,
                        'zext',
                        flag,
                        i32,
                        `fcmp${ordinal}`,
                      )
                      locals.set(operation.destination.ordinal, Object.freeze([result]))
                      break
                    }
                    const mnemonic: FunctionBody.FloatingBinaryKind | undefined =
                      operation.operator === 'Add'
                        ? 'fadd'
                        : operation.operator === 'Subtract'
                          ? 'fsub'
                          : operation.operator === 'Multiply'
                            ? 'fmul'
                            : operation.operator === 'Divide'
                              ? 'fdiv'
                              : operation.operator === 'Remainder'
                                ? 'frem'
                                : undefined
                    if (mnemonic === undefined)
                      throw new RangeError(
                        `LLVM float operation ${operation.operator} is unavailable`,
                      )
                    const result = yield* FunctionBody.binary(
                      body,
                      mnemonic,
                      left,
                      right,
                      `float${ordinal}`,
                    )
                    yield* locate(operation.provenance.span, yield* Value.instruction(body, result))
                    locals.set(operation.destination.ordinal, Object.freeze([result]))
                    break
                  }
                  const comparisonPredicates: Readonly<
                    Partial<
                      Record<
                        Mir.BinaryOperator,
                        'eq' | 'ne' | 'slt' | 'sle' | 'sgt' | 'sge' | 'ult' | 'ule' | 'ugt' | 'uge'
                      >
                    >
                  > = {
                    Equals: 'eq',
                    NotEquals: 'ne',
                    LessThan: unsigned ? 'ult' : 'slt',
                    LessOrEqual: unsigned ? 'ule' : 'sle',
                    GreaterThan: unsigned ? 'ugt' : 'sgt',
                    GreaterOrEqual: unsigned ? 'uge' : 'sge',
                  }
                  const predicate = comparisonPredicates[operation.operator]
                  if (predicate !== undefined) {
                    const flag = yield* FunctionBody.integerCompare(
                      body,
                      predicate,
                      left,
                      right,
                      `cmp${ordinal}_flag`,
                    )
                    const widened = yield* FunctionBody.cast(
                      body,
                      'zext',
                      flag,
                      i32,
                      `cmp${ordinal}`,
                    )
                    const instruction = yield* Value.instruction(body, flag)
                    yield* locate(operation.provenance.span, instruction)
                    locals.set(operation.destination.ordinal, Object.freeze([widened]))
                    break
                  }
                  if (
                    operation.operator === 'BitAnd' ||
                    operation.operator === 'BitOr' ||
                    operation.operator === 'BitXor' ||
                    operation.operator === 'WrappingAdd' ||
                    operation.operator === 'WrappingSubtract' ||
                    operation.operator === 'WrappingMultiply'
                  ) {
                    const mnemonic =
                      operation.operator === 'BitAnd'
                        ? 'and'
                        : operation.operator === 'BitOr'
                          ? 'or'
                          : operation.operator === 'BitXor'
                            ? 'xor'
                            : operation.operator === 'WrappingAdd'
                              ? 'add'
                              : operation.operator === 'WrappingSubtract'
                                ? 'sub'
                                : 'mul'
                    const result = yield* FunctionBody.binary(
                      body,
                      mnemonic,
                      left,
                      right,
                      `integer${ordinal}`,
                    )
                    yield* locate(operation.provenance.span, yield* Value.instruction(body, result))
                    locals.set(operation.destination.ordinal, Object.freeze([result]))
                    break
                  }
                  if (operation.operator === 'ShiftLeft' || operation.operator === 'ShiftRight') {
                    if (trapBlock === undefined)
                      trapBlock = yield* LlvmBlock.make(body, 'arith_trap')
                    const width =
                      scalar === undefined
                        ? 32
                        : Scalar.bits(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
                    const limit = yield* Constant.integerUnsigned(
                      builder,
                      operandType,
                      BigInt(width),
                    )
                    const invalid = yield* FunctionBody.integerCompare(
                      body,
                      'uge',
                      right,
                      limit,
                      `shift${ordinal}_invalid`,
                    )
                    const continueBlock = yield* LlvmBlock.make(body, `shift${ordinal}_ok`)
                    yield* FunctionBody.conditionalBranch(body, invalid, trapBlock, continueBlock)
                    yield* LlvmBlock.setInsertionPoint(body, continueBlock)
                    const result = yield* FunctionBody.binary(
                      body,
                      operation.operator === 'ShiftLeft' ? 'shl' : unsigned ? 'lshr' : 'ashr',
                      left,
                      right,
                      `shift${ordinal}`,
                    )
                    yield* locate(operation.provenance.span, yield* Value.instruction(body, result))
                    locals.set(operation.destination.ordinal, Object.freeze([result]))
                    break
                  }
                  if (operation.operator === 'RotateLeft' || operation.operator === 'RotateRight') {
                    const signature = Object.freeze({
                      returnType: operandType,
                      parameters: Object.freeze([operandType, operandType, operandType]),
                    })
                    const result = yield* Intrinsic.call(
                      body,
                      operation.operator === 'RotateLeft' ? 'fshl' : 'fshr',
                      [operandType],
                      [left, left, right],
                      `rotate${ordinal}`,
                      { signature },
                    )
                    if (result === undefined) throw new RangeError('LLVM rotate produced no value')
                    yield* locate(operation.provenance.span, yield* Value.instruction(body, result))
                    locals.set(operation.destination.ordinal, Object.freeze([result]))
                    break
                  }
                  if (
                    operation.operator === 'SaturatingAdd' ||
                    operation.operator === 'SaturatingSubtract'
                  ) {
                    const signature = Object.freeze({
                      returnType: operandType,
                      parameters: Object.freeze([operandType, operandType]),
                    })
                    const intrinsic =
                      operation.operator === 'SaturatingAdd'
                        ? unsigned
                          ? 'uadd.sat'
                          : 'sadd.sat'
                        : unsigned
                          ? 'usub.sat'
                          : 'ssub.sat'
                    const result = yield* Intrinsic.call(
                      body,
                      intrinsic,
                      [operandType],
                      [left, right],
                      `saturating${ordinal}`,
                      { signature },
                    )
                    if (result === undefined)
                      throw new RangeError('LLVM saturating arithmetic produced no value')
                    yield* locate(operation.provenance.span, yield* Value.instruction(body, result))
                    locals.set(operation.destination.ordinal, Object.freeze([result]))
                    break
                  }
                  if (operation.operator === 'SaturatingMultiply') {
                    const bits =
                      scalar === undefined
                        ? 32
                        : Scalar.bits(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
                    const signatures = unsigned
                      ? unsignedOverflowSignatures
                      : signedOverflowSignatures
                    let signature = signatures.get(bits)
                    if (signature === undefined) {
                      const i1 = yield* LlvmType.integer(builder, 1)
                      signature = Object.freeze({
                        returnType: yield* LlvmType.structure(builder, [operandType, i1]),
                        parameters: Object.freeze([operandType, operandType]),
                      })
                      signatures.set(bits, signature)
                    }
                    const pair = yield* Intrinsic.call(
                      body,
                      unsigned ? 'umul.with.overflow' : 'smul.with.overflow',
                      [operandType],
                      [left, right],
                      `saturating${ordinal}_pair`,
                      { signature },
                    )
                    if (pair === undefined)
                      throw new RangeError('LLVM saturating multiply produced no value')
                    const wrapped = yield* FunctionBody.extractValue(
                      body,
                      pair,
                      [0],
                      `saturating${ordinal}_wrapped`,
                    )
                    const overflowed = yield* FunctionBody.extractValue(
                      body,
                      pair,
                      [1],
                      `saturating${ordinal}_overflow`,
                    )
                    const range =
                      scalar?.category === 'Integer'
                        ? Scalar.range(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
                        : { minimum: -2147483648n, maximum: 2147483647n }
                    const maximum = unsigned
                      ? yield* Constant.integerUnsigned(builder, operandType, range.maximum)
                      : yield* Constant.integerSigned(builder, operandType, range.maximum)
                    let boundary: Value.Input = maximum
                    if (!unsigned) {
                      const zero = yield* Constant.integerSigned(builder, operandType, 0n)
                      const minimum = yield* Constant.integerSigned(
                        builder,
                        operandType,
                        range.minimum,
                      )
                      const signs = yield* FunctionBody.binary(
                        body,
                        'xor',
                        left,
                        right,
                        `saturating${ordinal}_signs`,
                      )
                      const negative = yield* FunctionBody.integerCompare(
                        body,
                        'slt',
                        signs,
                        zero,
                        `saturating${ordinal}_negative`,
                      )
                      boundary = yield* FunctionBody.select(
                        body,
                        negative,
                        minimum,
                        maximum,
                        `saturating${ordinal}_boundary`,
                      )
                    }
                    const result = yield* FunctionBody.select(
                      body,
                      overflowed,
                      boundary,
                      wrapped,
                      `saturating${ordinal}`,
                    )
                    yield* locate(operation.provenance.span, yield* Value.instruction(body, result))
                    locals.set(operation.destination.ordinal, Object.freeze([result]))
                    break
                  }
                  let result: Value.Value
                  if (trapBlock === undefined) {
                    trapBlock = yield* LlvmBlock.make(body, 'arith_trap')
                  }
                  if (
                    operation.operator === 'Add' ||
                    operation.operator === 'Subtract' ||
                    operation.operator === 'Multiply'
                  ) {
                    const intrinsicId =
                      operation.operator === 'Add'
                        ? unsigned
                          ? ('uadd.with.overflow' as const)
                          : ('sadd.with.overflow' as const)
                        : operation.operator === 'Subtract'
                          ? unsigned
                            ? ('usub.with.overflow' as const)
                            : ('ssub.with.overflow' as const)
                          : unsigned
                            ? ('umul.with.overflow' as const)
                            : ('smul.with.overflow' as const)
                    const bits =
                      scalar === undefined
                        ? 32
                        : Scalar.bits(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
                    const signatures = unsigned
                      ? unsignedOverflowSignatures
                      : signedOverflowSignatures
                    let overflowSignature = signatures.get(bits)
                    if (overflowSignature === undefined) {
                      const i1 = yield* LlvmType.integer(builder, 1)
                      overflowSignature = Object.freeze({
                        returnType: yield* LlvmType.structure(builder, [operandType, i1]),
                        parameters: Object.freeze([operandType, operandType]),
                      })
                      signatures.set(bits, overflowSignature)
                    }
                    const pair = yield* Intrinsic.call(
                      body,
                      intrinsicId,
                      [operandType],
                      [left, right],
                      `arith${ordinal}_pair`,
                      { signature: overflowSignature },
                    )
                    if (pair === undefined) {
                      throw new RangeError('Backend overflow intrinsic produced no value')
                    }
                    const valuePart = yield* FunctionBody.extractValue(
                      body,
                      pair,
                      [0],
                      `arith${ordinal}`,
                    )
                    const overflowed = yield* FunctionBody.extractValue(
                      body,
                      pair,
                      [1],
                      `arith${ordinal}_flag`,
                    )
                    const continueBlock = yield* LlvmBlock.make(body, `arith${ordinal}_ok`)
                    yield* FunctionBody.conditionalBranch(
                      body,
                      overflowed,
                      trapBlock,
                      continueBlock,
                    )
                    yield* LlvmBlock.setInsertionPoint(body, continueBlock)
                    result = valuePart
                  } else {
                    const zero = yield* Constant.integerUnsigned(builder, operandType, 0n)
                    const zeroDivisor = yield* FunctionBody.integerCompare(
                      body,
                      'eq',
                      right,
                      zero,
                      `div${ordinal}_zero`,
                    )
                    let trapping: Value.Value = zeroDivisor
                    if (!unsigned) {
                      const minimum = yield* Constant.integerSigned(
                        builder,
                        operandType,
                        scalar?.category === 'Integer'
                          ? Scalar.range(scalar, program.layout.target.pointerSize === 4 ? 32 : 64)
                              .minimum
                          : -2147483648n,
                      )
                      const negativeOne = yield* Constant.integerSigned(builder, operandType, -1n)
                      const minimumDividend = yield* FunctionBody.integerCompare(
                        body,
                        'eq',
                        left,
                        minimum,
                        `div${ordinal}_min`,
                      )
                      const negativeOneDivisor = yield* FunctionBody.integerCompare(
                        body,
                        'eq',
                        right,
                        negativeOne,
                        `div${ordinal}_negone`,
                      )
                      const overflowCase = yield* FunctionBody.binary(
                        body,
                        'and',
                        minimumDividend,
                        negativeOneDivisor,
                        `div${ordinal}_overflow`,
                      )
                      trapping = yield* FunctionBody.binary(
                        body,
                        'or',
                        zeroDivisor,
                        overflowCase,
                        `div${ordinal}_trapping`,
                      )
                    }
                    const continueBlock = yield* LlvmBlock.make(body, `div${ordinal}_ok`)
                    yield* FunctionBody.conditionalBranch(body, trapping, trapBlock, continueBlock)
                    yield* LlvmBlock.setInsertionPoint(body, continueBlock)
                    result = yield* FunctionBody.binary(
                      body,
                      operation.operator === 'Divide'
                        ? unsigned
                          ? 'udiv'
                          : 'sdiv'
                        : unsigned
                          ? 'urem'
                          : 'srem',
                      left,
                      right,
                      `arith${ordinal}`,
                    )
                  }
                  const instruction = yield* Value.instruction(body, result)
                  yield* locate(operation.provenance.span, instruction)
                  locals.set(operation.destination.ordinal, Object.freeze([result]))
                  break
                }
                case 'Drop': {
                  if (planReclaims(operation.cleanup) || operation.cleanup._tag === 'HookCleanup') {
                    yield* dropThroughPlan(
                      operation.cleanup,
                      readLocal(operation.local),
                      `drop${operation.local.ordinal}`,
                    )
                  }
                  break
                }
                case 'MakeEffect':
                case 'MakeCallable': {
                  const captured: Array<Value.Input> = []
                  const fields =
                    operation._tag === 'MakeEffect'
                      ? operation.type.environment.fields
                      : (operation.type.environment?.fields ?? Object.freeze([]))
                  for (const [ordinal, capture] of operation.captures.entries()) {
                    const field = fields.at(ordinal)
                    if (field === undefined)
                      throw new RangeError('Effect capture lost its environment field')
                    if (field.representation === 'Value') {
                      captured.push(...readLocal(capture.source))
                      continue
                    }
                    yield* ensureAddressRoot(capture.source)
                    const base = addressStorage.get(capture.source.ordinal)
                    if (base === undefined)
                      throw new RangeError('Effect borrowed capture lost its storage')
                    captured.push(base)
                  }
                  if (captured.length !== lanesFor(operation.type).length)
                    throw new RangeError('Effect environment capture lanes do not match its plan')
                  locals.set(operation.destination.ordinal, Object.freeze(captured))
                  break
                }
                case 'PackEffectOutcome': {
                  const source = [...readLocal(operation.source)]
                  const lanes = lanesFor(operation.type)
                  const values: Array<Value.Input> = [
                    yield* Constant.integerSigned(builder, i32, BigInt(operation.tag)),
                    ...source,
                  ]
                  while (values.length < lanes.length) {
                    const lane = lanes.at(values.length)
                    if (lane === undefined) break
                    values.push(yield* Constant.integerUnsigned(builder, laneType(lane), 0n))
                  }
                  locals.set(operation.destination.ordinal, Object.freeze(values))
                  break
                }
                case 'UnpackEffectSuccess': {
                  const count = lanesFor(operation.type).length
                  locals.set(
                    operation.destination.ordinal,
                    Object.freeze(readLocal(operation.source).slice(1, 1 + count)),
                  )
                  break
                }
                case 'RunEffect': {
                  const target = declared.find((candidate) =>
                    Mir.matchesInstance(candidate.fn, operation.target, operation.typeArguments),
                  )
                  if (target === undefined)
                    throw new RangeError('Backend cannot resolve propagated effect target')
                  const outcomeValues = yield* callValues(
                    target,
                    operation.arguments.flatMap((argument) => [...readLocal(argument)]),
                    `effect_run${operation.destination.ordinal}`,
                  )
                  locals.set(operation.outcome.ordinal, outcomeValues)
                  const tag = outcomeValues.at(0)
                  if (tag === undefined) throw new RangeError('Effect outcome lost its tag')
                  const zero = yield* Constant.integerSigned(builder, i32, 0n)
                  const succeeded = yield* FunctionBody.integerCompare(
                    body,
                    'eq',
                    tag,
                    zero,
                    `effect_run_success${operation.destination.ordinal}`,
                  )
                  const successBlock = yield* LlvmBlock.make(
                    body,
                    `effect_run${operation.destination.ordinal}_success`,
                  )
                  const failureBlock = yield* LlvmBlock.make(
                    body,
                    `effect_run${operation.destination.ordinal}_failure`,
                  )
                  const followingBlock = yield* LlvmBlock.make(
                    body,
                    `effect_run${operation.destination.ordinal}_following`,
                  )
                  yield* FunctionBody.conditionalBranch(body, succeeded, successBlock, failureBlock)
                  const resultLaneCount = lanesFor(operation.type).length
                  yield* LlvmBlock.setInsertionPoint(body, successBlock)
                  yield* storeMutable(
                    operation.destination,
                    Object.freeze(outcomeValues.slice(1, 1 + resultLaneCount)),
                  )
                  yield* FunctionBody.branch(body, followingBlock)
                  yield* LlvmBlock.setInsertionPoint(body, failureBlock)
                  let mappedTag: Value.Input = yield* Constant.integerSigned(builder, i32, -1n)
                  for (const [ordinal, mapping] of operation.tagMappings.entries()) {
                    const source = yield* Constant.integerSigned(
                      builder,
                      i32,
                      BigInt(mapping.source),
                    )
                    const matches = yield* FunctionBody.integerCompare(
                      body,
                      'eq',
                      tag,
                      source,
                      `effect_tag${operation.destination.ordinal}_${ordinal}`,
                    )
                    mappedTag = yield* FunctionBody.select(
                      body,
                      matches,
                      yield* Constant.integerSigned(builder, i32, BigInt(mapping.target)),
                      mappedTag,
                      `effect_mapped_tag${operation.destination.ordinal}_${ordinal}`,
                    )
                  }
                  const propagationLanes = lanesFor(operation.propagationType)
                  // The two outcome rows can carry differently shaped payload unions, so only
                  // positionally type-identical lanes survive the copy; past the shared failure
                  // member the payload is dead on this path, and zeros keep the aggregate typed.
                  const outcomeLanes = lanesFor(operation.outcomeType)
                  // Owners still live at this site release before the failure leaves the function
                  // through their complete cleanup plans, matching the Drop lowering.
                  for (const release of operation.releases ?? []) {
                    if (!planReclaims(release.cleanup) && release.cleanup._tag !== 'HookCleanup')
                      continue
                    yield* dropThroughPlan(
                      release.cleanup,
                      readLocal(release.local),
                      `propagation_release${release.local.ordinal}`,
                    )
                  }
                  const returned: Array<Value.Input> = [mappedTag]
                  while (returned.length < operation.propagationLaneCount) {
                    const lane = propagationLanes.at(returned.length)
                    if (lane === undefined) break
                    const sourceLane = outcomeLanes.at(returned.length)
                    const sourceValue = outcomeValues.at(returned.length)
                    returned.push(
                      sourceValue !== undefined &&
                        sourceLane !== undefined &&
                        laneType(sourceLane) === laneType(lane)
                        ? sourceValue
                        : yield* Constant.integerUnsigned(builder, laneType(lane), 0n),
                    )
                  }
                  if (returned.length === 1) {
                    const single = returned.at(0)
                    if (single === undefined)
                      throw new RangeError('Effect propagation lost its tag')
                    yield* FunctionBody.returnValue(body, single)
                  } else {
                    yield* FunctionBody.returnValue(
                      body,
                      yield* FunctionBody.buildAggregate(
                        body,
                        entry.resultType,
                        Object.freeze(returned.slice(0, operation.propagationLaneCount)),
                        'propagated_effect',
                      ),
                    )
                  }
                  yield* LlvmBlock.setInsertionPoint(body, followingBlock)
                  const storage = mutableStorage.get(operation.destination.ordinal)
                  if (storage === undefined)
                    throw new RangeError('Effect run destination is not materialized')
                  const loaded: Array<Value.Input> = []
                  for (const [lane, pointer] of storage.entries()) {
                    const callingLane = lanesFor(operation.type).at(lane)
                    if (callingLane === undefined)
                      throw new RangeError('Effect run destination lost a lane')
                    loaded.push(
                      yield* FunctionBody.load(
                        body,
                        laneType(callingLane),
                        pointer,
                        `effect_run${operation.destination.ordinal}_${lane}`,
                      ),
                    )
                  }
                  locals.set(operation.destination.ordinal, Object.freeze(loaded))
                  break
                }
                case 'RunEffectValue': {
                  const target = declared.find((candidate) =>
                    Mir.matchesInstance(
                      candidate.fn,
                      operation.runner,
                      operation.runnerTypeArguments,
                    ),
                  )
                  if (target === undefined)
                    throw new RangeError('Backend cannot resolve Effect value runner')
                  const outcomeValues = yield* callValues(
                    target,
                    [
                      ...readLocal(operation.effect),
                      ...operation.arguments.flatMap((argument) => [...readLocal(argument)]),
                    ],
                    `effect_value_run${operation.destination.ordinal}`,
                  )
                  locals.set(operation.outcome.ordinal, outcomeValues)
                  const resultLaneCount = lanesFor(operation.type).length
                  if (operation.propagationType === undefined) {
                    locals.set(
                      operation.destination.ordinal,
                      Object.freeze(outcomeValues.slice(1, 1 + resultLaneCount)),
                    )
                    break
                  }
                  const tag = outcomeValues.at(0)
                  if (tag === undefined) throw new RangeError('Effect outcome lost its tag')
                  const zero = yield* Constant.integerSigned(builder, i32, 0n)
                  const succeeded = yield* FunctionBody.integerCompare(
                    body,
                    'eq',
                    tag,
                    zero,
                    `effect_value_success${operation.destination.ordinal}`,
                  )
                  const successBlock = yield* LlvmBlock.make(
                    body,
                    `effect_value${operation.destination.ordinal}_success`,
                  )
                  const failureBlock = yield* LlvmBlock.make(
                    body,
                    `effect_value${operation.destination.ordinal}_failure`,
                  )
                  const followingBlock = yield* LlvmBlock.make(
                    body,
                    `effect_value${operation.destination.ordinal}_following`,
                  )
                  yield* FunctionBody.conditionalBranch(body, succeeded, successBlock, failureBlock)
                  yield* LlvmBlock.setInsertionPoint(body, successBlock)
                  yield* storeMutable(
                    operation.destination,
                    Object.freeze(outcomeValues.slice(1, 1 + resultLaneCount)),
                  )
                  yield* FunctionBody.branch(body, followingBlock)
                  yield* LlvmBlock.setInsertionPoint(body, failureBlock)
                  let mappedTag: Value.Input = yield* Constant.integerSigned(builder, i32, -1n)
                  for (const [ordinal, mapping] of operation.tagMappings.entries()) {
                    const source = yield* Constant.integerSigned(
                      builder,
                      i32,
                      BigInt(mapping.source),
                    )
                    const matches = yield* FunctionBody.integerCompare(
                      body,
                      'eq',
                      tag,
                      source,
                      `effect_value_tag${operation.destination.ordinal}_${ordinal}`,
                    )
                    mappedTag = yield* FunctionBody.select(
                      body,
                      matches,
                      yield* Constant.integerSigned(builder, i32, BigInt(mapping.target)),
                      mappedTag,
                      `effect_value_mapped_tag${operation.destination.ordinal}_${ordinal}`,
                    )
                  }
                  const propagationLanes = lanesFor(operation.propagationType)
                  // See RunEffect: only positionally type-identical payload lanes are copied.
                  const outcomeLanes = lanesFor(operation.outcomeType)
                  // Owners still live at this site release before the failure leaves the function
                  // through their complete cleanup plans, matching the Drop lowering.
                  for (const release of operation.releases ?? []) {
                    if (!planReclaims(release.cleanup) && release.cleanup._tag !== 'HookCleanup')
                      continue
                    yield* dropThroughPlan(
                      release.cleanup,
                      readLocal(release.local),
                      `propagation_release${release.local.ordinal}`,
                    )
                  }
                  const returned: Array<Value.Input> = [mappedTag]
                  while (returned.length < operation.propagationLaneCount) {
                    const lane = propagationLanes.at(returned.length)
                    if (lane === undefined) break
                    const sourceLane = outcomeLanes.at(returned.length)
                    const sourceValue = outcomeValues.at(returned.length)
                    returned.push(
                      sourceValue !== undefined &&
                        sourceLane !== undefined &&
                        laneType(sourceLane) === laneType(lane)
                        ? sourceValue
                        : yield* Constant.integerUnsigned(builder, laneType(lane), 0n),
                    )
                  }
                  yield* FunctionBody.returnValue(
                    body,
                    returned.length === 1
                      ? (returned.at(0) ?? mappedTag)
                      : yield* FunctionBody.buildAggregate(
                          body,
                          entry.resultType,
                          Object.freeze(returned.slice(0, operation.propagationLaneCount)),
                          'propagated_effect_value',
                        ),
                  )
                  yield* LlvmBlock.setInsertionPoint(body, followingBlock)
                  const storage = mutableStorage.get(operation.destination.ordinal)
                  if (storage === undefined)
                    throw new RangeError('Effect value run destination is not materialized')
                  const loaded: Array<Value.Input> = []
                  for (const [lane, pointer] of storage.entries()) {
                    const callingLane = lanesFor(operation.type).at(lane)
                    if (callingLane === undefined)
                      throw new RangeError('Effect value run destination lost a lane')
                    loaded.push(
                      yield* FunctionBody.load(
                        body,
                        laneType(callingLane),
                        pointer,
                        `effect_value${operation.destination.ordinal}_${lane}`,
                      ),
                    )
                  }
                  locals.set(operation.destination.ordinal, Object.freeze(loaded))
                  break
                }
                case 'CloseEffectEntry': {
                  const target = declared.find((candidate) =>
                    Mir.matchesInstance(candidate.fn, operation.target, operation.typeArguments),
                  )
                  const runner = declared.find((candidate) =>
                    Mir.matchesInstance(candidate.fn, operation.runner, operation.typeArguments),
                  )
                  if (target === undefined || runner === undefined)
                    throw new RangeError(
                      'Backend cannot resolve effect entry constructor or runner',
                    )
                  const effectValues = yield* callValues(target, [], 'effect_entry_make')
                  locals.set(operation.effect.ordinal, effectValues)
                  const outcomeValues = yield* callValues(runner, effectValues, 'effect_entry_run')
                  locals.set(operation.outcome.ordinal, outcomeValues)
                  const tag = outcomeValues.at(0)
                  if (tag === undefined) throw new RangeError('Effect entry outcome lost its tag')
                  const following = yield* LlvmBlock.make(body, 'effect_entry_following')
                  const success = yield* LlvmBlock.make(body, 'effect_entry_success')
                  const failureDispatch = yield* LlvmBlock.make(body, 'effect_entry_failure')
                  yield* FunctionBody.conditionalBranch(
                    body,
                    yield* FunctionBody.integerCompare(
                      body,
                      'eq',
                      tag,
                      yield* Constant.integerSigned(builder, i32, 0n),
                      'effect_entry_succeeded',
                    ),
                    success,
                    failureDispatch,
                  )
                  yield* LlvmBlock.setInsertionPoint(body, success)
                  yield* storeMutable(
                    operation.destination,
                    Object.freeze([yield* Constant.integerSigned(builder, i32, 0n)]),
                  )
                  yield* FunctionBody.branch(body, following)
                  yield* LlvmBlock.setInsertionPoint(body, failureDispatch)
                  for (const [ordinal, failure] of operation.failures.entries()) {
                    const selected = yield* LlvmBlock.make(body, `effect_entry_tag${failure.tag}`)
                    const otherwise = yield* LlvmBlock.make(
                      body,
                      `effect_entry_tag${failure.tag}_otherwise`,
                    )
                    yield* FunctionBody.conditionalBranch(
                      body,
                      yield* FunctionBody.integerCompare(
                        body,
                        'eq',
                        tag,
                        yield* Constant.integerSigned(builder, i32, BigInt(failure.tag)),
                        `effect_entry_is_tag${failure.tag}`,
                      ),
                      selected,
                      otherwise,
                    )
                    yield* LlvmBlock.setInsertionPoint(body, selected)
                    const payloadLaneCount = lanesFor({
                      _tag: 'Nominal',
                      type: failure.type,
                    }).length
                    const payload = outcomeValues.slice(1, 1 + payloadLaneCount)
                    if (payload.length !== payloadLaneCount) {
                      throw new RangeError('Effect entry failure lost its typed payload lanes')
                    }
                    locals.set(failure.payload.ordinal, Object.freeze(payload))
                    if (planReclaims(failure.cleanup) || failure.cleanup._tag === 'HookCleanup') {
                      yield* dropThroughPlan(
                        failure.cleanup,
                        Object.freeze(payload),
                        `effect_entry_cleanup${failure.tag}`,
                      )
                    }
                    yield* storeMutable(
                      operation.destination,
                      Object.freeze([
                        yield* Constant.integerSigned(builder, i32, BigInt(failure.tag)),
                      ]),
                    )
                    yield* FunctionBody.branch(body, following)
                    yield* LlvmBlock.setInsertionPoint(body, otherwise)
                    if (ordinal === operation.failures.length - 1) {
                      if (trapBlock === undefined)
                        trapBlock = yield* LlvmBlock.make(body, 'effect_entry_invalid_tag')
                      yield* FunctionBody.branch(body, trapBlock)
                    }
                  }
                  if (operation.failures.length === 0) {
                    if (trapBlock === undefined)
                      trapBlock = yield* LlvmBlock.make(body, 'effect_entry_invalid_tag')
                    yield* FunctionBody.branch(body, trapBlock)
                  }
                  yield* LlvmBlock.setInsertionPoint(body, following)
                  const storage = mutableStorage.get(operation.destination.ordinal)
                  const pointer = storage?.at(0)
                  if (pointer === undefined)
                    throw new RangeError('Effect entry status is not materialized')
                  locals.set(
                    operation.destination.ordinal,
                    Object.freeze([
                      yield* FunctionBody.load(body, i32, pointer, 'effect_entry_status'),
                    ]),
                  )
                  break
                }
                case 'RetryEffect': {
                  const protectedTargetId = operation.protectedTarget
                  const protectedTarget =
                    protectedTargetId === undefined
                      ? undefined
                      : declared.find((candidate) =>
                          Mir.matchesInstance(
                            candidate.fn,
                            protectedTargetId,
                            operation.protectedTypeArguments,
                          ),
                        )
                  const protectedRunner = declared.find((candidate) =>
                    Mir.matchesInstance(
                      candidate.fn,
                      operation.protectedRunner,
                      operation.protectedTypeArguments,
                    ),
                  )
                  if (
                    (operation.protectedTarget !== undefined && protectedTarget === undefined) ||
                    protectedRunner === undefined
                  )
                    throw new RangeError('Backend cannot resolve Effect retry target or runner')
                  const environment =
                    protectedTarget === undefined
                      ? readLocal(operation.protectedValue)
                      : yield* callValues(
                          protectedTarget,
                          operation.protectedArguments.flatMap((argument) => [
                            ...readLocal(argument),
                          ]),
                          `effect_retry_factory${operation.destination.ordinal}`,
                        )
                  locals.set(operation.protectedValue.ordinal, environment)
                  const counter = yield* FunctionBody.alloca(
                    body,
                    i32,
                    `effect_retry${operation.destination.ordinal}_remaining`,
                  )
                  yield* FunctionBody.store(body, readScalar(operation.retries), counter)
                  const attemptBlock = yield* LlvmBlock.make(
                    body,
                    `effect_retry${operation.destination.ordinal}_attempt`,
                  )
                  const failedBlock = yield* LlvmBlock.make(
                    body,
                    `effect_retry${operation.destination.ordinal}_failed`,
                  )
                  const repeatBlock = yield* LlvmBlock.make(
                    body,
                    `effect_retry${operation.destination.ordinal}_repeat`,
                  )
                  const exhaustedBlock = yield* LlvmBlock.make(
                    body,
                    `effect_retry${operation.destination.ordinal}_exhausted`,
                  )
                  const successBlock = yield* LlvmBlock.make(
                    body,
                    `effect_retry${operation.destination.ordinal}_success`,
                  )
                  const followingBlock = yield* LlvmBlock.make(
                    body,
                    `effect_retry${operation.destination.ordinal}_following`,
                  )
                  yield* FunctionBody.branch(body, attemptBlock)
                  yield* LlvmBlock.setInsertionPoint(body, attemptBlock)
                  const outcomeValues = yield* callValues(
                    protectedRunner,
                    environment,
                    `effect_retry_run${operation.destination.ordinal}`,
                  )
                  locals.set(operation.protectedOutcome.ordinal, outcomeValues)
                  const tag = outcomeValues.at(0)
                  if (tag === undefined) throw new RangeError('Effect retry outcome lost its tag')
                  const zero = yield* Constant.integerSigned(builder, i32, 0n)
                  const succeeded = yield* FunctionBody.integerCompare(
                    body,
                    'eq',
                    tag,
                    zero,
                    `effect_retry${operation.destination.ordinal}_succeeded`,
                  )
                  yield* FunctionBody.conditionalBranch(body, succeeded, successBlock, failedBlock)
                  yield* LlvmBlock.setInsertionPoint(body, successBlock)
                  const resultLaneCount = lanesFor(operation.type).length
                  yield* storeMutable(
                    operation.destination,
                    Object.freeze(outcomeValues.slice(1, 1 + resultLaneCount)),
                  )
                  yield* FunctionBody.branch(body, followingBlock)
                  yield* LlvmBlock.setInsertionPoint(body, failedBlock)
                  const remaining = yield* FunctionBody.load(
                    body,
                    i32,
                    counter,
                    `effect_retry${operation.destination.ordinal}_remaining_value`,
                  )
                  const canRetry = yield* FunctionBody.integerCompare(
                    body,
                    'sgt',
                    remaining,
                    zero,
                    `effect_retry${operation.destination.ordinal}_can_retry`,
                  )
                  yield* FunctionBody.conditionalBranch(body, canRetry, repeatBlock, exhaustedBlock)
                  yield* LlvmBlock.setInsertionPoint(body, repeatBlock)
                  const one = yield* Constant.integerSigned(builder, i32, 1n)
                  const decremented = yield* FunctionBody.binary(
                    body,
                    'sub',
                    remaining,
                    one,
                    `effect_retry${operation.destination.ordinal}_decremented`,
                  )
                  yield* FunctionBody.store(body, decremented, counter)
                  yield* FunctionBody.branch(body, attemptBlock)
                  yield* LlvmBlock.setInsertionPoint(body, exhaustedBlock)
                  if (operation.propagationType === undefined) {
                    if (trapBlock === undefined) trapBlock = yield* LlvmBlock.make(body, 'trap')
                    yield* FunctionBody.branch(body, trapBlock)
                  } else {
                    let mappedTag: Value.Input = yield* Constant.integerSigned(builder, i32, -1n)
                    for (const [ordinal, mapping] of operation.tagMappings.entries()) {
                      const source = yield* Constant.integerSigned(
                        builder,
                        i32,
                        BigInt(mapping.source),
                      )
                      const matches = yield* FunctionBody.integerCompare(
                        body,
                        'eq',
                        tag,
                        source,
                        `effect_retry${operation.destination.ordinal}_tag${ordinal}`,
                      )
                      mappedTag = yield* FunctionBody.select(
                        body,
                        matches,
                        yield* Constant.integerSigned(builder, i32, BigInt(mapping.target)),
                        mappedTag,
                        `effect_retry${operation.destination.ordinal}_mapped${ordinal}`,
                      )
                    }
                    const propagationLanes = lanesFor(operation.propagationType)
                    // See RunEffect: only positionally type-identical payload lanes are copied.
                    const outcomeLanes = lanesFor(operation.protectedType)
                    // Owners still live at this site release before the failure leaves the function
                    // through their complete cleanup plans, matching the Drop lowering.
                    for (const release of operation.releases ?? []) {
                      if (!planReclaims(release.cleanup) && release.cleanup._tag !== 'HookCleanup')
                        continue
                      yield* dropThroughPlan(
                        release.cleanup,
                        readLocal(release.local),
                        `propagation_release${release.local.ordinal}`,
                      )
                    }
                    const returned: Array<Value.Input> = [mappedTag]
                    while (returned.length < operation.propagationLaneCount) {
                      const lane = propagationLanes.at(returned.length)
                      if (lane === undefined) break
                      const sourceLane = outcomeLanes.at(returned.length)
                      const sourceValue = outcomeValues.at(returned.length)
                      returned.push(
                        sourceValue !== undefined &&
                          sourceLane !== undefined &&
                          laneType(sourceLane) === laneType(lane)
                          ? sourceValue
                          : yield* Constant.integerUnsigned(builder, laneType(lane), 0n),
                      )
                    }
                    yield* FunctionBody.returnValue(
                      body,
                      returned.length === 1
                        ? (returned.at(0) ?? mappedTag)
                        : yield* FunctionBody.buildAggregate(
                            body,
                            entry.resultType,
                            Object.freeze(returned.slice(0, operation.propagationLaneCount)),
                            'propagated_effect_retry',
                          ),
                    )
                  }
                  yield* LlvmBlock.setInsertionPoint(body, followingBlock)
                  const storage = mutableStorage.get(operation.destination.ordinal)
                  if (storage === undefined)
                    throw new RangeError('Effect retry destination is not materialized')
                  const loaded: Array<Value.Input> = []
                  for (const [lane, storagePointer] of storage.entries()) {
                    const callingLane = lanesFor(operation.type).at(lane)
                    if (callingLane === undefined) throw new RangeError('Effect retry lost a lane')
                    loaded.push(
                      yield* FunctionBody.load(
                        body,
                        laneType(callingLane),
                        storagePointer,
                        `effect_retry${operation.destination.ordinal}_${lane}`,
                      ),
                    )
                  }
                  locals.set(operation.destination.ordinal, Object.freeze(loaded))
                  break
                }
                case 'CatchEffect': {
                  const protectedTargetId = operation.protectedTarget
                  const protectedTarget =
                    protectedTargetId === undefined
                      ? undefined
                      : declared.find((candidate) =>
                          Mir.matchesInstance(
                            candidate.fn,
                            protectedTargetId,
                            operation.protectedTypeArguments,
                          ),
                        )
                  const protectedRunner = declared.find((candidate) =>
                    Mir.matchesInstance(
                      candidate.fn,
                      operation.protectedRunner,
                      operation.protectedTypeArguments,
                    ),
                  )
                  const handlerTarget = declared.find((candidate) =>
                    Mir.matchesInstance(
                      candidate.fn,
                      operation.handlerTarget,
                      operation.handlerTypeArguments,
                    ),
                  )
                  const handlerRunner = declared.find((candidate) =>
                    Mir.matchesInstance(
                      candidate.fn,
                      operation.handlerRunner,
                      operation.handlerTypeArguments,
                    ),
                  )
                  if (
                    (protectedTargetId !== undefined && protectedTarget === undefined) ||
                    protectedRunner === undefined ||
                    handlerTarget === undefined ||
                    handlerRunner === undefined
                  )
                    throw new RangeError('Backend cannot resolve effect catch target')
                  const protectedEnvironment =
                    protectedTarget === undefined
                      ? readLocal(operation.protectedValue)
                      : yield* callValues(
                          protectedTarget,
                          operation.protectedArguments.flatMap((argument) => [
                            ...readLocal(argument),
                          ]),
                          `effect_protected_factory${operation.destination.ordinal}`,
                        )
                  locals.set(operation.protectedValue.ordinal, protectedEnvironment)
                  const protectedValues = yield* callValues(
                    protectedRunner,
                    Object.freeze([
                      ...protectedEnvironment,
                      ...operation.protectedRunnerArguments.flatMap((argument) => [
                        ...readLocal(argument),
                      ]),
                    ]),
                    `effect_protected${operation.destination.ordinal}`,
                  )
                  locals.set(operation.protectedOutcome.ordinal, protectedValues)
                  const tag = protectedValues.at(0)
                  if (tag === undefined) throw new RangeError('Effect outcome lost its tag')
                  const zero = yield* Constant.integerSigned(builder, i32, 0n)
                  const succeeded = yield* FunctionBody.integerCompare(
                    body,
                    'eq',
                    tag,
                    zero,
                    `effect_success${operation.destination.ordinal}`,
                  )
                  const successBlock = yield* LlvmBlock.make(
                    body,
                    `effect${operation.destination.ordinal}_success`,
                  )
                  const failureBlock = yield* LlvmBlock.make(
                    body,
                    `effect${operation.destination.ordinal}_failure`,
                  )
                  const followingBlock = yield* LlvmBlock.make(
                    body,
                    `effect${operation.destination.ordinal}_following`,
                  )
                  yield* FunctionBody.conditionalBranch(body, succeeded, successBlock, failureBlock)
                  const resultLaneCount = lanesFor(operation.type).length
                  yield* LlvmBlock.setInsertionPoint(body, successBlock)
                  yield* storeMutable(
                    operation.destination,
                    Object.freeze(protectedValues.slice(1, 1 + resultLaneCount)),
                  )
                  yield* FunctionBody.branch(body, followingBlock)
                  yield* LlvmBlock.setInsertionPoint(body, failureBlock)
                  const handlerEnvironment = yield* callValues(
                    handlerTarget,
                    Object.freeze([
                      ...protectedValues.slice(1, 1 + operation.handledLaneCount),
                      ...readLocal(operation.handlerCallable),
                    ]),
                    `effect_handler_factory${operation.destination.ordinal}`,
                  )
                  locals.set(operation.handlerValue.ordinal, handlerEnvironment)
                  const handlerValues = yield* callValues(
                    handlerRunner,
                    handlerEnvironment,
                    `effect_handler${operation.destination.ordinal}`,
                  )
                  locals.set(operation.handlerOutcome.ordinal, handlerValues)
                  yield* storeMutable(
                    operation.destination,
                    Object.freeze(handlerValues.slice(1, 1 + resultLaneCount)),
                  )
                  yield* FunctionBody.branch(body, followingBlock)
                  yield* LlvmBlock.setInsertionPoint(body, followingBlock)
                  const storage = mutableStorage.get(operation.destination.ordinal)
                  if (storage === undefined)
                    throw new RangeError('Effect catch destination is not materialized')
                  const loaded: Array<Value.Input> = []
                  for (const [lane, pointer] of storage.entries()) {
                    const callingLane = lanesFor(operation.type).at(lane)
                    if (callingLane === undefined)
                      throw new RangeError('Effect catch destination lost a lane')
                    loaded.push(
                      yield* FunctionBody.load(
                        body,
                        laneType(callingLane),
                        pointer,
                        `effect${operation.destination.ordinal}_${lane}`,
                      ),
                    )
                  }
                  locals.set(operation.destination.ordinal, Object.freeze(loaded))
                  break
                }
                case 'ApplyCallable': {
                  const sourceType =
                    operation.callable === undefined
                      ? undefined
                      : entry.fn.localTypes.at(operation.callable.ordinal)
                  const target =
                    operation.target ??
                    (sourceType?._tag === 'CallableValue' ? sourceType.target : undefined)
                  if (target === undefined)
                    throw new RangeError('Backend callable application lost its hidden identity')
                  const captureValues: Array<Value.Input> = []
                  if (operation.callable !== undefined) {
                    if (
                      sourceType?._tag !== 'CallableValue' ||
                      sourceType.environment === undefined
                    )
                      throw new RangeError('Stored callable application lost its environment')
                    const environmentValues = readLocal(operation.callable)
                    let cursor = 0
                    for (const field of sourceType.environment.fields) {
                      const shape = Layout.callingShape(program.layout, field.type)
                      if (shape === undefined)
                        throw new RangeError('Callable capture lost its semantic calling shape')
                      if (field.representation === 'Value') {
                        captureValues.push(
                          ...environmentValues.slice(cursor, cursor + shape.laneCount),
                        )
                        cursor += shape.laneCount
                        continue
                      }
                      const base = environmentValues.at(cursor)
                      if (base === undefined)
                        throw new RangeError('Callable borrowed environment lost its pointer')
                      cursor += 1
                      for (const [laneOrdinal, lane] of shape.lanes.entries()) {
                        const offset = Layout.laneOffset(program.layout, field.type, lane.path)
                        if (offset === undefined)
                          throw new RangeError('Callable borrowed capture lost its lane offset')
                        captureValues.push(
                          yield* FunctionBody.load(
                            body,
                            laneType(lane),
                            yield* constantBytePointer(
                              base,
                              offset,
                              `callable${operation.destination.ordinal}_capture${field.ordinal}_${laneOrdinal}_ptr`,
                            ),
                            `callable${operation.destination.ordinal}_capture${field.ordinal}_${laneOrdinal}`,
                          ),
                        )
                      }
                    }
                  } else {
                    for (const capture of operation.captures) {
                      if (capture.access === 'Copy' || capture.access === 'Take') {
                        captureValues.push(...readLocal(capture.source))
                        continue
                      }
                      captureValues.push(...readLocal(capture.source))
                    }
                  }
                  if (target._tag === 'BuiltinCallableTarget') {
                    const supplied = Object.freeze([
                      ...operation.arguments.flatMap((argument) => [...readLocal(argument)]),
                      ...captureValues,
                    ])
                    const first = supplied.at(0)
                    const firstLocal = operation.arguments.at(0)
                    const firstType =
                      firstLocal === undefined
                        ? undefined
                        : entry.fn.localTypes.at(firstLocal.ordinal)
                    if (first === undefined || firstType === undefined)
                      throw new RangeError('LLVM callable builtin lost its first operand')
                    const conversionTarget = Scalar.conversionTarget(target.operation)
                    if (conversionTarget !== undefined) {
                      const sourceScalar = Scalar.find(firstType._tag)
                      if (sourceScalar?.category === 'Floating') {
                        const destination =
                          integerTypes.get(
                            Scalar.bits(
                              conversionTarget,
                              program.layout.target.pointerSize === 4 ? 32 : 64,
                            ),
                          ) ?? i32
                        const result = yield* FunctionBody.cast(
                          body,
                          conversionTarget.signedness === 'Signed' ? 'fptosi' : 'fptoui',
                          first,
                          destination,
                          `callable_convert${operation.destination.ordinal}`,
                        )
                        locals.set(operation.destination.ordinal, Object.freeze([result]))
                        break
                      }
                      if (sourceScalar?.category !== 'Integer')
                        throw new RangeError('LLVM callable conversion lost its source type')
                      const result = yield* emitIntegerConversion(
                        first,
                        Object.freeze({ _tag: sourceScalar.spelling }),
                        Object.freeze({ _tag: conversionTarget.spelling }),
                        `callable_convert${operation.destination.ordinal}`,
                      )
                      locals.set(operation.destination.ordinal, Object.freeze([result]))
                      break
                    }
                    const floatTarget = Scalar.floatConversionTarget(target.operation)
                    if (floatTarget !== undefined) {
                      const source = Scalar.find(firstType._tag)
                      if (source === undefined || source.category === 'Boolean')
                        throw new RangeError('LLVM callable float conversion lost its source type')
                      const destination = floatTarget.spelling === 'f32' ? f32 : f64
                      const result =
                        source.category === 'Floating'
                          ? source.spelling === floatTarget.spelling
                            ? first
                            : yield* FunctionBody.cast(
                                body,
                                source.spelling === 'f64' ? 'fptrunc' : 'fpext',
                                first,
                                destination,
                                `callable_convert${operation.destination.ordinal}`,
                              )
                          : yield* FunctionBody.cast(
                              body,
                              source.signedness === 'Signed' ? 'sitofp' : 'uitofp',
                              first,
                              destination,
                              `callable_convert${operation.destination.ordinal}`,
                            )
                      locals.set(operation.destination.ordinal, Object.freeze([result]))
                      break
                    }
                    if (
                      target.operation === 'Negate' &&
                      Scalar.find(firstType._tag)?.category === 'Floating'
                    ) {
                      const result = yield* FunctionBody.unary(
                        body,
                        'fneg',
                        first,
                        `callable_fneg${operation.destination.ordinal}`,
                      )
                      locals.set(operation.destination.ordinal, Object.freeze([result]))
                      break
                    }
                    if (
                      target.operation === 'Not' ||
                      target.operation === 'Negate' ||
                      target.operation === 'WrappingNegate' ||
                      target.operation === 'SaturatingNegate' ||
                      target.operation === 'BitNot'
                    ) {
                      const firstLane = valueLanesFor(firstType).at(0)
                      if (firstLane === undefined)
                        throw new RangeError('LLVM callable unary operation lost its lane')
                      const operandType = laneType(firstLane)
                      const zero = yield* Constant.integerSigned(builder, operandType, 0n)
                      if (target.operation !== 'Not') {
                        const unaryOperator =
                          target.operation === 'Negate'
                            ? 'Subtract'
                            : target.operation === 'WrappingNegate'
                              ? 'WrappingSubtract'
                              : target.operation === 'SaturatingNegate'
                                ? 'SaturatingSubtract'
                                : 'BitXor'
                        const right =
                          target.operation === 'BitNot'
                            ? yield* Constant.integerSigned(builder, operandType, -1n)
                            : first
                        const values = Object.freeze([
                          yield* emitCallableBinary(
                            unaryOperator,
                            target.operation === 'BitNot' ? first : zero,
                            right,
                            firstType,
                            operation.provenance.span,
                            operation.destination.ordinal,
                          ),
                        ])
                        locals.set(operation.destination.ordinal, values)
                        break
                      }
                      const boolZero = yield* Constant.integerSigned(builder, i32, 0n)
                      const flag = yield* FunctionBody.integerCompare(
                        body,
                        'eq',
                        first,
                        boolZero,
                        `callable_not${operation.destination.ordinal}_flag`,
                      )
                      const values = Object.freeze([
                        yield* FunctionBody.cast(
                          body,
                          'zext',
                          flag,
                          i32,
                          `callable_not${operation.destination.ordinal}`,
                        ),
                      ])
                      locals.set(operation.destination.ordinal, values)
                      break
                    }
                    const second = supplied.at(1)
                    if (
                      second === undefined ||
                      target.operation === 'LayoutMake' ||
                      target.operation === 'LayoutRepeat' ||
                      target.operation === 'SystemAllocatorMake' ||
                      target.operation === 'AllocatorAllocate' ||
                      !Mir.isBinaryOperator(target.operation)
                    ) {
                      throw new RangeError(
                        `LLVM callable builtin ${target.actor}.${target.operation} is unavailable`,
                      )
                    }
                    const values = Object.freeze([
                      yield* emitCallableBinary(
                        target.operation,
                        first,
                        second,
                        firstType,
                        operation.provenance.span,
                        operation.destination.ordinal,
                      ),
                    ])
                    locals.set(operation.destination.ordinal, values)
                    break
                  }
                  const callableTarget = declared.find((candidate) =>
                    Mir.matchesInstance(candidate.fn, target.declaration, operation.typeArguments),
                  )
                  if (callableTarget === undefined)
                    throw new RangeError('Backend cannot resolve callable target')
                  const result = yield* callValues(
                    callableTarget,
                    Object.freeze([
                      ...operation.arguments.flatMap((argument) => [...readLocal(argument)]),
                      ...captureValues,
                    ]),
                    `callable${operation.destination.ordinal}`,
                  )
                  locals.set(operation.destination.ordinal, result)
                  break
                }
                case 'Call': {
                  const target = declared.find((candidate) =>
                    Mir.matchesInstance(candidate.fn, operation.target, operation.typeArguments),
                  )
                  if (target === undefined) {
                    throw new RangeError(
                      `Backend cannot resolve call target ${operation.target.name}`,
                    )
                  }
                  const result = yield* FunctionBody.callDirect(
                    body,
                    target.handle,
                    operation.arguments.flatMap((argument) => [...readLocal(argument)]),
                    `t${operation.destination.ordinal}`,
                  )
                  for (const root of [...addressRoots].sort((left, right) => left - right)) {
                    yield* reloadAddressRoot(root)
                  }
                  if (target.resultLaneCount === 0) {
                    locals.set(operation.destination.ordinal, Object.freeze([]))
                    break
                  }
                  if (result === undefined) {
                    throw new RangeError('Backend call produced no value')
                  }
                  const instruction = yield* Value.instruction(body, result)
                  yield* locate(operation.provenance.span, instruction)
                  if (target.resultLaneCount === 1) {
                    locals.set(operation.destination.ordinal, Object.freeze([result]))
                    break
                  }
                  const values: Array<Value.Input> = []
                  for (let lane = 0; lane < target.resultLaneCount; lane += 1) {
                    values.push(
                      yield* FunctionBody.extractValue(
                        body,
                        result,
                        [lane],
                        `t${operation.destination.ordinal}_${lane}`,
                      ),
                    )
                  }
                  locals.set(operation.destination.ordinal, Object.freeze(values))
                  break
                }
              }
              const destination = destinationOf(operation)
              if (destination !== undefined && mutableRoots.has(destination.ordinal)) {
                yield* storeMutable(destination, readLocal(destination))
              }
              if (destination !== undefined && addressRoots.has(destination.ordinal)) {
                yield* storeAddressRootValues(
                  destination.ordinal,
                  readLocal(destination),
                  `addr${destination.ordinal}_defined`,
                )
              }
            }
            const terminator = block.terminator
            switch (terminator._tag) {
              case 'Return': {
                const returned = readLocal(terminator.value)
                const instruction =
                  returned.length === 0
                    ? yield* FunctionBody.returnVoid(body)
                    : returned.length === 1
                      ? yield* FunctionBody.returnValue(body, readScalar(terminator.value))
                      : yield* FunctionBody.returnValue(
                          body,
                          yield* FunctionBody.buildAggregate(
                            body,
                            entry.resultType,
                            returned,
                            `return_value_b${block.id.ordinal}`,
                          ),
                        )
                yield* locate(terminator.provenance.span, instruction)
                break
              }
              case 'Jump': {
                const target = blocks.get(terminator.target.ordinal)
                if (target === undefined) throw new RangeError('Backend jump to missing block')
                yield* FunctionBody.branch(body, target)
                break
              }
              case 'Branch': {
                const zero = yield* Constant.integerSigned(builder, i32, 0n)
                const condition = yield* FunctionBody.integerCompare(
                  body,
                  'ne',
                  readScalar(terminator.condition),
                  zero,
                  `c${blockOrdinal}`,
                )
                const taken = blocks.get(terminator.taken.ordinal)
                const otherwise = blocks.get(terminator.otherwise.ordinal)
                if (taken === undefined || otherwise === undefined) {
                  throw new RangeError('Backend branch to missing block')
                }
                yield* FunctionBody.conditionalBranch(body, condition, taken, otherwise)
                break
              }
              case 'MatchBranch': {
                const tag = readLocal(terminator.scrutinee).at(0)
                if (tag === undefined) throw new RangeError('LLVM union match has no tag lane')
                const expected = yield* Constant.integerSigned(
                  builder,
                  i32,
                  BigInt(terminator.memberOrdinal),
                )
                const condition = yield* FunctionBody.integerCompare(
                  body,
                  'eq',
                  tag,
                  expected,
                  `match${block.id.ordinal}_member`,
                )
                const taken = blocks.get(terminator.taken.ordinal)
                const otherwise = blocks.get(terminator.otherwise.ordinal)
                if (taken === undefined || otherwise === undefined) {
                  throw new RangeError('LLVM match branch targets a missing block')
                }
                yield* FunctionBody.conditionalBranch(body, condition, taken, otherwise)
                break
              }
              case 'Trap': {
                yield* Intrinsic.call(body, 'trap', [], [])
                const instruction = yield* FunctionBody.unreachable(body)
                yield* locate(terminator.provenance.span, instruction)
                break
              }
            }
          }

          if (trapBlock !== undefined) {
            yield* LlvmBlock.setInsertionPoint(body, trapBlock)
            yield* Intrinsic.call(body, 'trap', [], [])
            yield* FunctionBody.unreachable(body)
          }
        }),
      )
    }

    return {
      symbols: declared.map((entry) =>
        Object.freeze({
          declaration: entry.fn.id,
          instance: entry.fn.instance,
          symbol: entry.symbol,
        }),
      ),
      ir: yield* IrText.render(builder),
      bitcode: yield* Bitcode.encode(builder),
    }
  })

/** The bootstrap LLVM backend over the Silk LLVM builder. */
export const LlvmBackend: Backend<LlvmBitcodeArtifact> = Object.freeze({
  _tag: 'Backend',
  id: 'llvm',
  name: 'LLVM',
  targets: Object.freeze([
    ...Target.native.map((target) => target.id),
    Target.wasm32UnknownUnknown.id,
  ]),
  emit: Effect.fn('Backend.LLVM.emit')(function* (
    program: Mir.Module,
    request: CodegenRequest,
  ): Effect.fn.Return<LlvmBitcodeArtifact, BackendError> {
    const output = yield* emitProgram(program, request).pipe(
      Effect.mapError((cause) =>
        cause._tag === 'BackendError'
          ? cause
          : new BackendError({
              operation: 'Backend.emit',
              backend: 'LLVM',
              message: `LLVM emission failed for ${program.module}`,
              reason: { _tag: 'WrappedFailure', cause },
            }),
      ),
    )
    return Object.freeze({
      _tag: 'LlvmBitcodeArtifact',
      backend: 'llvm',
      module: program.module,
      target: program.layout.target,
      symbols: Object.freeze(output.symbols),
      termination: terminationOf(program),
      control: llvmControl(program),
      bitcode: output.bitcode,
      ir: output.ir,
    })
  }),
})
