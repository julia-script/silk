import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Hir from './Hir.js'
import type * as Instances from './Instances.js'
import type * as Match from './Match.js'
import * as Mir from './Mir.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'

/**
 * The closed bootstrap interpreter, executing the lowered MIR program from the entry instance
 * discovery resolved. It is the semantics oracle for the coming native differential checks and
 * the second consumer keeping MIR's meaning in MIR. A severable leaf: nothing in the pipeline
 * depends on it.
 */

/** The exact value produced by the closed bootstrap interpreter. */
export interface I32Value {
  readonly _tag: 'I32Value'
  readonly value: number
}

/** An exact target-sized unsigned integer, independent of host number precision. */
export interface UsizeValue {
  readonly _tag: 'UsizeValue'
  readonly value: bigint
}

export interface AggregateValue {
  readonly _tag: 'AggregateValue'
  readonly type: Type.Nominal
  readonly fields: ReadonlyArray<{
    readonly field: DeclarationIndex.FieldId
    readonly value: Value
  }>
}

export interface ArrayValue {
  readonly _tag: 'ArrayValue'
  readonly type: Type.FixedArray
  readonly elements: ReadonlyArray<Value>
}

/** A logical borrowed view. Permission and loan identity remain compiler facts, not values. */
export interface SliceValue {
  readonly _tag: 'SliceValue'
  readonly frame: number
  readonly cell: number
  readonly base: number
  readonly length: number
}

export interface UnionValue {
  readonly _tag: 'UnionValue'
  readonly type: Type.StructuralUnion
  readonly member: Type.Nominal
  readonly payload: AggregateValue
}

export interface EffectOutcomeValue {
  readonly _tag: 'EffectOutcomeValue'
  readonly type: Type.Effect
  readonly tag: number
  readonly payload: Value
}

export interface EffectBorrowValue {
  readonly _tag: 'EffectBorrowValue'
  readonly frame: number
  readonly cell: number
  readonly access: 'Shared' | 'Exclusive'
}

export interface EffectValue {
  readonly _tag: 'EffectValue'
  readonly type: Type.Effect
  readonly site: Hir.EffectSiteId
  readonly runner: DeclarationIndex.CanonicalId
  readonly runnerTypeArguments: ReadonlyArray<Type.Type>
  readonly captures: ReadonlyArray<Value>
}

/** One logical heap block; identity and liveness live in evaluator state, not JS identity. */
export interface AllocationValue {
  readonly _tag: 'AllocationValue'
  readonly type: Type.Nominal
  readonly ticket: number
  readonly bytes: bigint
  readonly alignment: bigint
}

/** One immutable logical evaluator value, independent of backend lane realization. */
export type Value =
  | I32Value
  | UsizeValue
  | AggregateValue
  | ArrayValue
  | SliceValue
  | UnionValue
  | EffectBorrowValue
  | EffectValue
  | EffectOutcomeValue
  | AllocationValue

/** Entered the resolved entry instance. */
export interface EntryTraceEvent {
  readonly _tag: 'Entry'
  readonly function: DeclarationIndex.CanonicalId
  readonly instance: Instances.InstanceKey
  readonly span: SourceSpan.SourceSpan
}

/** Executed one call operation after its argument locals were computed. */
export interface CallTraceEvent {
  readonly _tag: 'Call'
  readonly caller: DeclarationIndex.CanonicalId
  readonly target: DeclarationIndex.CanonicalId
  readonly callerInstance: Instances.InstanceKey
  readonly targetInstance: Instances.InstanceKey
  readonly span: SourceSpan.SourceSpan
}

/** Bound one computed argument value to its positional parameter local. */
export interface BindingTraceEvent {
  readonly _tag: 'Binding'
  readonly target: DeclarationIndex.CanonicalId
  readonly targetInstance: Instances.InstanceKey
  readonly callSpan: SourceSpan.SourceSpan
  readonly argumentOrdinal: number
  readonly parameterOrdinal: number
  readonly value: Value
  readonly fromCall: boolean
  readonly span: SourceSpan.SourceSpan
}

/** Returned one evaluated value from a lowered function. */
export interface ReturnTraceEvent {
  readonly _tag: 'Return'
  readonly function: DeclarationIndex.CanonicalId
  readonly instance: Instances.InstanceKey
  readonly value: Value
  readonly span: SourceSpan.SourceSpan
}

/** One deterministic event emitted while replaying lowered operations. */
export interface ConstructTraceEvent {
  readonly _tag: 'Construct'
  readonly function: DeclarationIndex.CanonicalId
  readonly type: Type.Nominal
  readonly fieldCount: number
  readonly span: SourceSpan.SourceSpan
}

export interface ProjectTraceEvent {
  readonly _tag: 'Project'
  readonly function: DeclarationIndex.CanonicalId
  readonly type: Type.Nominal
  readonly field: DeclarationIndex.FieldId
  readonly span: SourceSpan.SourceSpan
}

export interface ArrayConstructTraceEvent {
  readonly _tag: 'ArrayConstruct'
  readonly function: DeclarationIndex.CanonicalId
  readonly type: Type.FixedArray
  readonly elementCount: number
  readonly span: SourceSpan.SourceSpan
}

export interface UnionConversionTraceEvent {
  readonly _tag: 'UnionConversion'
  readonly function: DeclarationIndex.CanonicalId
  readonly conversion: 'Inject' | 'Widen'
  readonly source: Type.Nominal | Type.StructuralUnion
  readonly target: Type.StructuralUnion
  readonly member: Type.Nominal
  readonly span: SourceSpan.SourceSpan
}

export interface PlaceReadTraceEvent {
  readonly _tag: 'PlaceRead'
  readonly function: DeclarationIndex.CanonicalId
  readonly selectors: ReadonlyArray<
    | { readonly _tag: 'Field'; readonly field: DeclarationIndex.FieldId }
    | {
        readonly _tag: 'Element'
        readonly array: Type.FixedArray
        readonly index: number
        readonly bounds: 'Proven' | 'Checked'
        readonly span: SourceSpan.SourceSpan
      }
  >
  readonly value: Value
  readonly span: SourceSpan.SourceSpan
}

export interface CleanupTraceEvent {
  readonly _tag: 'Cleanup'
  readonly function: DeclarationIndex.CanonicalId
  readonly local: number
  readonly members?: ReadonlyArray<Type.Nominal>
  readonly span: SourceSpan.SourceSpan
}

export interface MatchTraceEvent {
  readonly _tag:
    | 'MatchDispatch'
    | 'MatchCandidate'
    | 'MatchSelected'
    | 'MatchCleanup'
    | 'MatchBorrowEnd'
  readonly function: DeclarationIndex.CanonicalId
  readonly match: number
  readonly arm?: number
  readonly member: Type.Nominal
  readonly access: Match.Access
  readonly binding?: number
  readonly path?: ReadonlyArray<DeclarationIndex.FieldId>
  readonly value?: Value
  readonly members?: ReadonlyArray<Type.Nominal>
  readonly span: SourceSpan.SourceSpan
}

export interface ControlTraceEvent {
  readonly _tag:
    | 'RegionEntry'
    | 'Condition'
    | 'Iteration'
    | 'WriteCheck'
    | 'ReplacementCleanup'
    | 'Replacement'
    | 'Repeat'
    | 'Exit'
    | 'Transfer'
  readonly function: DeclarationIndex.CanonicalId
  readonly region: number
  readonly loop?: number
  readonly members?: ReadonlyArray<Type.Nominal>
  readonly span: SourceSpan.SourceSpan
}

export interface EffectTraceEvent {
  readonly _tag: 'EffectSuccess' | 'EffectFailure'
  readonly function: DeclarationIndex.CanonicalId
  readonly tag: number
  readonly span: SourceSpan.SourceSpan
}

export type TraceEvent =
  | EntryTraceEvent
  | CallTraceEvent
  | BindingTraceEvent
  | ReturnTraceEvent
  | ConstructTraceEvent
  | ProjectTraceEvent
  | ArrayConstructTraceEvent
  | UnionConversionTraceEvent
  | PlaceReadTraceEvent
  | CleanupTraceEvent
  | MatchTraceEvent
  | ControlTraceEvent
  | EffectTraceEvent

/** Every expected reason the closed bootstrap interpreter can stop. */
export type BlockedReason =
  | {
      readonly _tag: 'InvalidMir'
      readonly violations: ReadonlyArray<Mir.Violation>
    }
  | {
      readonly _tag: 'UnavailableEntry'
      readonly reason:
        | 'MissingEntry'
        | 'AmbiguousEntry'
        | 'GenericEntry'
        | 'ParameterizedEntry'
        | 'UntypedEntry'
        | 'InvalidSource'
    }
  | {
      readonly _tag: 'Trap'
      readonly function: DeclarationIndex.CanonicalId
      readonly reason: string
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'MissingFunction'
      readonly target: DeclarationIndex.CanonicalId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'RecursiveCycle'
      readonly cycle: ReadonlyArray<Instances.InstanceKey>
      readonly closingCallSpan: SourceSpan.SourceSpan
    }

/** A completed exact bootstrap result. */
export interface Completed {
  readonly _tag: 'Completed'
  readonly entry: DeclarationIndex.CanonicalId
  readonly result: I32Value
  readonly trace: ReadonlyArray<TraceEvent>
}

/** A normal, inspectable reason bootstrap evaluation could not complete. */
export interface Blocked {
  readonly _tag: 'Blocked'
  readonly entry: DeclarationIndex.CanonicalId | undefined
  readonly reason: BlockedReason
  readonly trace: ReadonlyArray<TraceEvent>
}

/** The closed outcome of executing one lowered program. */
export type Outcome = Completed | Blocked

type Step =
  | { readonly _tag: 'Value'; readonly value: Value }
  | { readonly _tag: 'Blocked'; readonly reason: BlockedReason }

const value = (input: number): I32Value => Object.freeze({ _tag: 'I32Value', value: input })
const usizeValue = (input: bigint): UsizeValue =>
  Object.freeze({ _tag: 'UsizeValue', value: input })

const blockedStep = (reason: BlockedReason): Step =>
  Object.freeze({ _tag: 'Blocked', reason: Object.freeze(reason) })

const sameInstance = (left: Instances.InstanceKey, right: Instances.InstanceKey): boolean =>
  left.declaration.module === right.declaration.module &&
  left.declaration.name === right.declaration.name &&
  left.typeArguments.length === right.typeArguments.length &&
  left.typeArguments.every((argument, ordinal) => {
    const candidate = right.typeArguments.at(ordinal)
    return candidate !== undefined && Type.equals(argument, candidate)
  })

const functionFor = (
  program: Mir.Module,
  id: DeclarationIndex.CanonicalId,
  typeArguments: ReadonlyArray<Type.Type>,
): Mir.MirFunction | undefined =>
  program.functions.find((fn) => Mir.matchesInstance(fn, id, typeArguments))

interface LocalState {
  readonly value: Value
  readonly fromCall: boolean
}

interface EvaluationState {
  nextFrame: number
  nextAllocation: number
  readonly cells: Map<string, LocalState>
  readonly allocations: Map<number, { active: boolean }>
}

const cellKey = (frame: number, cell: number): string => `${frame}:${cell}`

function executeFunction(
  program: Mir.Module,
  fn: Mir.MirFunction,
  argumentValues: ReadonlyArray<Value>,
  active: ReadonlyArray<Instances.InstanceKey>,
  trace: Array<TraceEvent>,
  state: EvaluationState,
): Step {
  const frame = state.nextFrame
  state.nextFrame += 1
  const locals = new Map<number, LocalState>()
  argumentValues.forEach((argument, ordinal) => {
    locals.set(ordinal, { value: argument, fromCall: false })
  })

  const read = (local: Mir.LocalId): LocalState => {
    const direct = state.cells.get(cellKey(frame, local.ordinal)) ??
      locals.get(local.ordinal) ?? { value: value(0), fromCall: false }
    if (direct.value._tag !== 'EffectBorrowValue') return direct
    return (
      state.cells.get(cellKey(direct.value.frame, direct.value.cell)) ?? {
        value: value(0),
        fromCall: false,
      }
    )
  }

  const write = (local: Mir.LocalId, next: LocalState): void => {
    const alias = locals.get(local.ordinal)?.value
    if (alias?._tag === 'EffectBorrowValue') {
      state.cells.set(cellKey(alias.frame, alias.cell), next)
      return
    }
    locals.set(local.ordinal, next)
    const key = cellKey(frame, local.ordinal)
    if (state.cells.has(key)) state.cells.set(key, next)
  }

  const cell = (slice: SliceValue): LocalState => {
    const found = state.cells.get(cellKey(slice.frame, slice.cell))
    if (found === undefined) throw new RangeError('MIR slice references a missing evaluator cell')
    return found
  }

  const readI32 = (local: Mir.LocalId): I32Value => {
    const found = read(local).value
    if (found._tag !== 'I32Value') {
      throw new RangeError(`MIR verifier allowed aggregate local %${local.ordinal} as a scalar`)
    }
    return found
  }

  const readInteger = (local: Mir.LocalId): I32Value | UsizeValue => {
    const found = read(local).value
    if (found._tag !== 'I32Value' && found._tag !== 'UsizeValue') {
      throw new RangeError(`MIR verifier allowed aggregate local %${local.ordinal} as an integer`)
    }
    return found
  }

  const regions = new Map(fn.regions.map((region) => [region.id.ordinal, region] as const))
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
  const checkedPlaces = new Map<ReadonlyArray<Mir.PlaceSelector>, ReadonlyArray<number>>()

  const cleanupMembers = (
    cleanup: Extract<Mir.Operation, { readonly _tag: 'Drop' }>['cleanup'],
    owner: Value,
  ): ReadonlyArray<Type.Nominal> => {
    if (cleanup._tag === 'NoCleanup' || cleanup._tag === 'ParameterCleanup') {
      return Object.freeze([])
    }
    if (cleanup._tag === 'AllocationCleanup') return Object.freeze([Type.allocation])
    if (cleanup._tag === 'UnionCleanup') {
      if (owner._tag !== 'UnionValue') return Object.freeze([])
      const active = cleanup.cases.find((candidate) => Type.equals(candidate.member, owner.member))
      return Object.freeze([
        owner.member,
        ...(active === undefined ? [] : cleanupMembers(active.cleanup, owner.payload)),
      ])
    }
    if (cleanup._tag === 'ArrayCleanup') {
      return owner._tag === 'ArrayValue'
        ? Object.freeze(
            owner.elements.flatMap((element) => cleanupMembers(cleanup.element, element)),
          )
        : Object.freeze([])
    }
    if (owner._tag !== 'AggregateValue') return Object.freeze([])
    return Object.freeze(
      cleanup.fields.flatMap((field) => {
        const value = owner.fields.find(
          (candidate) => candidate.field.ordinal === field.field.ordinal,
        )
        return value === undefined ? [] : cleanupMembers(field.cleanup, value.value)
      }),
    )
  }

  const selectFieldPath = (
    root: AggregateValue,
    path: ReadonlyArray<DeclarationIndex.FieldId>,
  ): Value => {
    let selected: Value = root
    for (const selector of path) {
      if (selected._tag !== 'AggregateValue') {
        throw new RangeError('MIR verifier allowed a match field below a non-struct value')
      }
      const field: AggregateValue['fields'][number] | undefined = selected.fields.find(
        (candidate) =>
          candidate.field.ordinal === selector.ordinal &&
          candidate.field.struct.sourceId === selector.struct.sourceId &&
          candidate.field.struct.ordinal === selector.struct.ordinal,
      )
      if (field === undefined) {
        throw new RangeError('MIR verifier allowed a missing match field')
      }
      selected = field.value
    }
    return selected
  }

  const resolvePlace = (
    root: Mir.LocalId,
    selectors: ReadonlyArray<Mir.PlaceSelector>,
  ):
    | {
        readonly _tag: 'Resolved'
        readonly selected: Value
        readonly indexes: ReadonlyArray<number>
      }
    | { readonly _tag: 'Blocked'; readonly step: Step } => {
    let selected = read(root).value
    const indexes: Array<number> = []
    for (const selector of selectors) {
      if (selector._tag === 'FieldSelector') {
        if (selected._tag !== 'AggregateValue') {
          throw new RangeError('MIR verifier allowed a field selector on a non-struct value')
        }
        const field = selected.fields.find(
          (candidate) =>
            candidate.field.ordinal === selector.field.ordinal &&
            candidate.field.struct.sourceId === selector.field.struct.sourceId &&
            candidate.field.struct.ordinal === selector.field.struct.ordinal,
        )
        if (field === undefined)
          throw new RangeError('MIR verifier allowed a missing field selector')
        selected = field.value
        indexes.push(selector.field.ordinal)
        continue
      }
      if (selector._tag === 'SliceElementSelector') {
        if (selected._tag !== 'SliceValue') {
          throw new RangeError('MIR verifier allowed a slice selector on a non-slice value')
        }
        const index = readI32(selector.index).value
        if (index < 0 || index >= selected.length) {
          return {
            _tag: 'Blocked',
            step: blockedStep({
              _tag: 'Trap',
              function: fn.id,
              reason: `slice index ${index} is outside length ${selected.length} in ${fn.id.module}.${fn.id.name}`,
              span: selector.provenance.span,
            }),
          }
        }
        const backing = cell(selected).value
        if (backing._tag !== 'ArrayValue') {
          throw new RangeError('MIR slice cell does not contain an array value')
        }
        const element = backing.elements.at(selected.base + index)
        if (element === undefined) {
          throw new RangeError('MIR slice range exceeds its backing cell')
        }
        indexes.push(index)
        selected = element
        continue
      }
      if (selected._tag !== 'ArrayValue') {
        throw new RangeError('MIR verifier allowed an element selector on a non-array value')
      }
      const index =
        selector.index._tag === 'Proven'
          ? selector.index.value
          : readI32(selector.index.local).value
      if (index < 0 || index >= selector.length) {
        return {
          _tag: 'Blocked',
          step: blockedStep({
            _tag: 'Trap',
            function: fn.id,
            reason: `array index ${index} is outside length ${selector.length} in ${fn.id.module}.${fn.id.name}`,
            span: selector.provenance.span,
          }),
        }
      }
      const element = selected.elements.at(index)
      if (element === undefined)
        throw new RangeError('MIR verifier allowed an incomplete array value')
      indexes.push(index)
      selected = element
    }
    return { _tag: 'Resolved', selected, indexes: Object.freeze(indexes) }
  }

  const replacePlace = (
    current: Value,
    selectors: ReadonlyArray<Mir.PlaceSelector>,
    indexes: ReadonlyArray<number>,
    replacement: Value,
    depth = 0,
  ): Value => {
    const selector = selectors.at(depth)
    if (selector === undefined) return replacement
    const ordinal = indexes.at(depth)
    if (ordinal === undefined) throw new RangeError('Checked place omitted one selector index')
    if (selector._tag === 'FieldSelector') {
      if (current._tag !== 'AggregateValue') throw new RangeError('Invalid aggregate replacement')
      return Object.freeze({
        _tag: 'AggregateValue',
        type: current.type,
        fields: Object.freeze(
          current.fields.map((field) =>
            field.field.ordinal === selector.field.ordinal
              ? Object.freeze({
                  field: field.field,
                  value: replacePlace(field.value, selectors, indexes, replacement, depth + 1),
                })
              : field,
          ),
        ),
      })
    }
    if (selector._tag === 'SliceElementSelector') {
      if (current._tag !== 'SliceValue') throw new RangeError('Invalid slice replacement')
      const backing = cell(current)
      if (backing.value._tag !== 'ArrayValue') {
        throw new RangeError('Invalid slice backing cell replacement')
      }
      const absolute = current.base + ordinal
      const updated: ArrayValue = Object.freeze({
        _tag: 'ArrayValue',
        type: backing.value.type,
        elements: Object.freeze(
          backing.value.elements.map((element, index) =>
            index === absolute
              ? replacePlace(element, selectors, indexes, replacement, depth + 1)
              : element,
          ),
        ),
      })
      state.cells.set(cellKey(current.frame, current.cell), {
        value: updated,
        fromCall: backing.fromCall,
      })
      return current
    }
    if (current._tag !== 'ArrayValue') throw new RangeError('Invalid array replacement')
    return Object.freeze({
      _tag: 'ArrayValue',
      type: current.type,
      elements: Object.freeze(
        current.elements.map((element, index) =>
          index === ordinal
            ? replacePlace(element, selectors, indexes, replacement, depth + 1)
            : element,
        ),
      ),
    })
  }

  let regionOrdinal = fn.entry.ordinal
  for (;;) {
    const region = regions.get(regionOrdinal)
    if (region === undefined) {
      return blockedStep({
        _tag: 'Trap',
        function: fn.id,
        reason: `missing region r${regionOrdinal}`,
        span: argumentSpanFallback(fn),
      })
    }

    const regionSpan =
      region._tag === 'ConditionalRegion' || region._tag === 'LoopRegion'
        ? region.provenance.span
        : region._tag === 'OperationRegion'
          ? (region.operations.at(0)?.provenance.span ?? region.outcome.provenance.span)
          : (region.releases.at(0)?.provenance.span ?? region.outcome.provenance.span)
    trace.push(
      Object.freeze({
        _tag: 'RegionEntry',
        function: fn.id,
        region: region.id.ordinal,
        ...(region.ownerLoop === undefined ? {} : { loop: region.ownerLoop.ordinal }),
        span: regionSpan,
      }),
    )

    if (region._tag === 'ConditionalRegion') {
      const taken = readI32(region.condition).value !== 0
      trace.push(
        Object.freeze({
          _tag: 'Condition',
          function: fn.id,
          region: region.id.ordinal,
          ...(region.ownerLoop === undefined ? {} : { loop: region.ownerLoop.ordinal }),
          span: region.provenance.span,
        }),
      )
      regionOrdinal = taken ? region.taken.ordinal : region.otherwise.ordinal
      continue
    }
    if (region._tag === 'LoopRegion') {
      regionOrdinal = region.condition.ordinal
      continue
    }

    const executeOperations = (operations: ReadonlyArray<Mir.Operation>): Step | undefined => {
      for (const operation of operations) {
        switch (operation._tag) {
          case 'Match': {
            const scrutinee = read(operation.scrutinee).value
            const activeMember =
              scrutinee._tag === 'UnionValue'
                ? scrutinee.member
                : scrutinee._tag === 'AggregateValue'
                  ? scrutinee.type
                  : undefined
            const payload =
              scrutinee._tag === 'UnionValue'
                ? scrutinee.payload
                : scrutinee._tag === 'AggregateValue'
                  ? scrutinee
                  : undefined
            if (activeMember === undefined || payload === undefined) {
              throw new RangeError('MIR verifier allowed matching a scalar value')
            }
            trace.push(
              Object.freeze({
                _tag: 'MatchDispatch',
                function: fn.id,
                match: operation.provenance.span.start,
                member: activeMember,
                access: operation.access,
                span: operation.provenance.span,
              }),
            )
            const decision = operation.decisions.find((candidate) =>
              Type.equals(candidate.member, activeMember),
            )
            if (decision === undefined) {
              throw new RangeError('MIR verifier allowed a match without its active member')
            }
            let selected = false
            for (const candidateId of decision.candidates) {
              const arm = operation.arms.find(
                (candidate) => candidate.id.ordinal === candidateId.ordinal,
              )
              if (arm === undefined) {
                throw new RangeError('MIR verifier allowed a missing match candidate')
              }
              trace.push(
                Object.freeze({
                  _tag: 'MatchCandidate',
                  function: fn.id,
                  match: operation.provenance.span.start,
                  arm: arm.id.ordinal,
                  member: activeMember,
                  access: operation.access,
                  span: arm.provenance.span,
                }),
              )
              for (const binding of arm.bindings) {
                const bound = selectFieldPath(payload, binding.path)
                write(binding.destination, { value: bound, fromCall: false })
                trace.push(
                  Object.freeze({
                    _tag: 'MatchCandidate',
                    function: fn.id,
                    match: operation.provenance.span.start,
                    arm: arm.id.ordinal,
                    member: activeMember,
                    access: operation.access,
                    binding: binding.id.ordinal,
                    path: binding.path,
                    value: bound,
                    span: binding.provenance.span,
                  }),
                )
              }
              if (arm.guard !== undefined) {
                const guardStep = executeOperations(arm.guard.operations)
                if (guardStep !== undefined) return guardStep
                if (readI32(arm.guard.result).value === 0) continue
              }
              trace.push(
                Object.freeze({
                  _tag: 'MatchSelected',
                  function: fn.id,
                  match: operation.provenance.span.start,
                  arm: arm.id.ordinal,
                  member: activeMember,
                  access: operation.access,
                  span: arm.provenance.span,
                }),
              )
              const selectedStep = executeOperations(arm.selected.operations)
              if (selectedStep !== undefined) return selectedStep
              for (const cleanup of arm.selected.cleanup) {
                const owner = selectFieldPath(payload, cleanup.path)
                const members = cleanupMembers(cleanup.cleanup, owner)
                trace.push(
                  Object.freeze({
                    _tag: 'MatchCleanup',
                    function: fn.id,
                    match: operation.provenance.span.start,
                    arm: arm.id.ordinal,
                    member: activeMember,
                    access: operation.access,
                    path: cleanup.path,
                    ...(members.length === 0 ? {} : { members }),
                    span: arm.provenance.span,
                  }),
                )
              }
              const result = read(arm.selected.result)
              write(operation.destination, result)
              if (arm.selected.endBorrow) {
                trace.push(
                  Object.freeze({
                    _tag: 'MatchBorrowEnd',
                    function: fn.id,
                    match: operation.provenance.span.start,
                    arm: arm.id.ordinal,
                    member: activeMember,
                    access: operation.access,
                    span: arm.provenance.span,
                  }),
                )
              }
              selected = true
              break
            }
            if (!selected) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: `exhaustive match rejected every guard in ${fn.id.module}.${fn.id.name}`,
                span: operation.provenance.span,
              })
            }
            break
          }
          case 'Literal':
            write(operation.destination, {
              value:
                operation.type._tag === 'Usize'
                  ? usizeValue(BigInt(operation.value))
                  : value(Number(operation.value)),
              fromCall: false,
            })
            break
          case 'Move':
            write(operation.destination, read(operation.source))
            break
          case 'BeginLoan': {
            const source = read(operation.root)
            const slice =
              operation.sourceType._tag === 'Slice'
                ? source.value
                : (() => {
                    if (source.value._tag !== 'ArrayValue') {
                      throw new RangeError('MIR verifier allowed borrowing a non-array value')
                    }
                    const key = cellKey(frame, operation.root.ordinal)
                    if (!state.cells.has(key)) state.cells.set(key, source)
                    return Object.freeze({
                      _tag: 'SliceValue' as const,
                      frame,
                      cell: operation.root.ordinal,
                      base: 0,
                      length: operation.sourceType.type.length,
                    })
                  })()
            if (slice._tag !== 'SliceValue') {
              throw new RangeError('MIR verifier allowed reborrowing a non-slice value')
            }
            write(operation.destination, { value: slice, fromCall: source.fromCall })
            break
          }
          case 'EndLoan':
            break
          case 'SliceLength': {
            const slice = read(operation.slice).value
            if (slice._tag !== 'SliceValue') {
              throw new RangeError('MIR verifier allowed slice length on a non-slice value')
            }
            write(operation.destination, { value: value(slice.length), fromCall: false })
            break
          }
          case 'ConvertUnion': {
            const source = read(operation.source).value
            const mapping =
              operation.conversion === 'Inject'
                ? operation.mappings.at(0)
                : source._tag === 'UnionValue'
                  ? operation.mappings.find((candidate) =>
                      Type.equals(candidate.source, source.member),
                    )
                  : undefined
            const payload =
              operation.conversion === 'Inject' && source._tag === 'AggregateValue'
                ? source
                : operation.conversion === 'Widen' && source._tag === 'UnionValue'
                  ? source.payload
                  : undefined
            if (mapping === undefined || payload === undefined) {
              throw new RangeError('MIR verifier allowed an invalid logical union conversion')
            }
            const converted: UnionValue = Object.freeze({
              _tag: 'UnionValue',
              type: operation.targetType.type,
              member: mapping.target,
              payload,
            })
            write(operation.destination, { value: converted, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'UnionConversion',
                function: fn.id,
                conversion: operation.conversion,
                source: operation.sourceType.type,
                target: operation.targetType.type,
                member: converted.member,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'ValidateLayout': {
            const bytes = readInteger(operation.bytes)
            const alignment = readInteger(operation.alignment)
            if (bytes._tag !== 'UsizeValue' || alignment._tag !== 'UsizeValue') {
              throw new RangeError('MIR verifier allowed non-Usize layout construction')
            }
            const valid = alignment.value > 0n && (alignment.value & (alignment.value - 1n)) === 0n
            const member = valid ? Type.layout : Type.invalidAlignment
            const entry = program.layout.entries.find((candidate) =>
              Type.equals(candidate.type, member),
            )
            if (entry?._tag !== 'LayoutEntry' || entry.representation._tag !== 'Aggregate') {
              throw new RangeError('Target plan omitted an intrinsic layout validation member')
            }
            const payload: AggregateValue = Object.freeze({
              _tag: 'AggregateValue',
              type: member,
              fields: Object.freeze(
                entry.representation.fields.map((field) =>
                  Object.freeze({
                    field: field.id,
                    value: field.name === 'bytes' ? bytes : alignment,
                  }),
                ),
              ),
            })
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'UnionValue',
                type: operation.type.type,
                member,
                payload,
              }),
              fromCall: false,
            })
            break
          }
          case 'RepeatLayout': {
            const layout = read(operation.layout).value
            const count = readInteger(operation.count)
            if (layout._tag !== 'AggregateValue' || count._tag !== 'UsizeValue') {
              throw new RangeError('MIR verifier allowed invalid repeated-layout operands')
            }
            const entry = program.layout.entries.find((candidate) =>
              Type.equals(candidate.type, Type.layout),
            )
            if (entry?._tag !== 'LayoutEntry' || entry.representation._tag !== 'Aggregate') {
              throw new RangeError('Target plan omitted Layout')
            }
            const representation = entry.representation
            const fieldValue = (name: string): UsizeValue | undefined => {
              const field = representation.fields.find((candidate) => candidate.name === name)
              const value = layout.fields.find(
                (candidate) => candidate.field.ordinal === field?.id.ordinal,
              )?.value
              return value?._tag === 'UsizeValue' ? value : undefined
            }
            const bytes = fieldValue('bytes')
            const alignment = fieldValue('alignment')
            if (bytes === undefined || alignment === undefined) {
              throw new RangeError('Layout payload omitted bytes or alignment')
            }
            const maximum =
              program.layout.target.pointerSize === 4 ? 4294967295n : 18446744073709551615n
            const overflow = count.value !== 0n && bytes.value > maximum / count.value
            const member = overflow ? Type.layoutOverflow : Type.layout
            const memberEntry = program.layout.entries.find((candidate) =>
              Type.equals(candidate.type, member),
            )
            if (
              memberEntry?._tag !== 'LayoutEntry' ||
              memberEntry.representation._tag !== 'Aggregate'
            ) {
              throw new RangeError('Target plan omitted a repeated-layout result member')
            }
            const total = usizeValue(overflow ? 0n : bytes.value * count.value)
            const payload: AggregateValue = Object.freeze({
              _tag: 'AggregateValue',
              type: member,
              fields: Object.freeze(
                memberEntry.representation.fields.map((field) =>
                  Object.freeze({
                    field: field.id,
                    value: field.name === 'bytes' ? total : alignment,
                  }),
                ),
              ),
            })
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'UnionValue',
                type: operation.type.type,
                member,
                payload,
              }),
              fromCall: false,
            })
            break
          }
          case 'Allocate': {
            const layout = read(operation.layout).value
            if (layout._tag !== 'AggregateValue' || !Type.equals(layout.type, Type.layout)) {
              throw new RangeError('MIR verifier allowed allocation with a non-Layout value')
            }
            const entry = program.layout.entries.find((candidate) =>
              Type.equals(candidate.type, Type.layout),
            )
            if (entry?._tag !== 'LayoutEntry' || entry.representation._tag !== 'Aggregate') {
              throw new RangeError('Target plan omitted Layout')
            }
            const representation = entry.representation
            const fieldValue = (name: string): UsizeValue | undefined => {
              const field = representation.fields.find((candidate) => candidate.name === name)
              const found = layout.fields.find(
                (candidate) => candidate.field.ordinal === field?.id.ordinal,
              )?.value
              return found?._tag === 'UsizeValue' ? found : undefined
            }
            const bytes = fieldValue('bytes')
            const alignment = fieldValue('alignment')
            if (bytes === undefined || alignment === undefined)
              throw new RangeError('Layout payload omitted bytes or alignment')
            const ticket = state.nextAllocation
            state.nextAllocation += 1
            state.allocations.set(ticket, { active: true })
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'AllocationValue',
                type: Type.allocation,
                ticket,
                bytes: bytes.value,
                alignment: alignment.value,
              }),
              fromCall: false,
            })
            break
          }
          case 'Binary': {
            const leftValue = readInteger(operation.left)
            const rightValue = readInteger(operation.right)
            if (leftValue._tag !== rightValue._tag) {
              throw new RangeError('MIR verifier allowed mixed integer operands')
            }
            const unsigned = leftValue._tag === 'UsizeValue'
            const left = BigInt(leftValue.value)
            const right = BigInt(rightValue.value)
            if (
              operation.operator === 'Equals' ||
              operation.operator === 'NotEquals' ||
              operation.operator === 'LessThan' ||
              operation.operator === 'LessOrEqual' ||
              operation.operator === 'GreaterThan' ||
              operation.operator === 'GreaterOrEqual'
            ) {
              const holds =
                operation.operator === 'Equals'
                  ? left === right
                  : operation.operator === 'NotEquals'
                    ? left !== right
                    : operation.operator === 'LessThan'
                      ? left < right
                      : operation.operator === 'LessOrEqual'
                        ? left <= right
                        : operation.operator === 'GreaterThan'
                          ? left > right
                          : left >= right
              write(operation.destination, {
                value: value(holds ? 1 : 0),
                fromCall: false,
              })
              break
            }
            if (
              (operation.operator === 'Divide' || operation.operator === 'Remainder') &&
              right === 0n
            ) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'division by zero',
                span: operation.provenance.span,
              })
            }
            const exact =
              operation.operator === 'Add'
                ? left + right
                : operation.operator === 'Subtract'
                  ? left - right
                  : operation.operator === 'Multiply'
                    ? left * right
                    : operation.operator === 'Divide'
                      ? left / right
                      : left % right
            const maximum =
              program.layout.target.pointerSize === 4 ? 4294967295n : 18446744073709551615n
            if (
              (unsigned && exact < 0n) ||
              (unsigned && exact > maximum) ||
              (!unsigned && (exact < -2147483648n || exact > 2147483647n))
            ) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: unsigned && exact < 0n ? 'arithmetic underflow' : 'arithmetic overflow',
                span: operation.provenance.span,
              })
            }
            write(operation.destination, {
              value: unsigned ? usizeValue(exact) : value(Number(exact)),
              fromCall: false,
            })
            break
          }
          case 'Construct': {
            const aggregate: AggregateValue = Object.freeze({
              _tag: 'AggregateValue',
              type: operation.type.type,
              fields: Object.freeze(
                operation.fields.map((field) =>
                  Object.freeze({ field: field.field, value: read(field.value).value }),
                ),
              ),
            })
            write(operation.destination, { value: aggregate, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'Construct',
                function: fn.id,
                type: aggregate.type,
                fieldCount: aggregate.fields.length,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'ConstructArray': {
            const array: ArrayValue = Object.freeze({
              _tag: 'ArrayValue',
              type: operation.type.type,
              elements: Object.freeze(operation.elements.map((element) => read(element).value)),
            })
            write(operation.destination, { value: array, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'ArrayConstruct',
                function: fn.id,
                type: array.type,
                elementCount: array.elements.length,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'Project': {
            const aggregate = read(operation.source).value
            if (aggregate._tag !== 'AggregateValue') {
              throw new RangeError('MIR verifier allowed projection from a scalar value')
            }
            const selected = aggregate.fields.find(
              (candidate) =>
                candidate.field.ordinal === operation.field.ordinal &&
                candidate.field.struct.sourceId === operation.field.struct.sourceId &&
                candidate.field.struct.ordinal === operation.field.struct.ordinal,
            )
            if (selected === undefined) {
              throw new RangeError('MIR verifier allowed projection of a missing aggregate field')
            }
            write(operation.destination, { value: selected.value, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'Project',
                function: fn.id,
                type: aggregate.type,
                field: operation.field,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'ReadPlace': {
            let selected = read(operation.root).value
            const selectors: Array<PlaceReadTraceEvent['selectors'][number]> = []
            for (const selector of operation.selectors) {
              if (selector._tag === 'FieldSelector') {
                if (selected._tag !== 'AggregateValue') {
                  throw new RangeError(
                    'MIR verifier allowed a field selector on a non-struct value',
                  )
                }
                const field = selected.fields.find(
                  (candidate) =>
                    candidate.field.ordinal === selector.field.ordinal &&
                    candidate.field.struct.sourceId === selector.field.struct.sourceId &&
                    candidate.field.struct.ordinal === selector.field.struct.ordinal,
                )
                if (field === undefined) {
                  throw new RangeError('MIR verifier allowed a missing field selector')
                }
                selected = field.value
                selectors.push(Object.freeze({ _tag: 'Field', field: selector.field }))
                continue
              }
              if (selector._tag === 'SliceElementSelector') {
                if (selected._tag !== 'SliceValue') {
                  throw new RangeError('MIR verifier allowed a slice selector on a non-slice value')
                }
                const index = readI32(selector.index).value
                if (index < 0 || index >= selected.length) {
                  return blockedStep({
                    _tag: 'Trap',
                    function: fn.id,
                    reason: `slice index ${index} is outside length ${selected.length} in ${fn.id.module}.${fn.id.name}`,
                    span: selector.provenance.span,
                  })
                }
                const backing = cell(selected).value
                if (backing._tag !== 'ArrayValue') {
                  throw new RangeError('MIR slice cell does not contain an array value')
                }
                const element = backing.elements.at(selected.base + index)
                if (element === undefined) {
                  throw new RangeError('MIR slice range exceeds its backing cell')
                }
                selectors.push(
                  Object.freeze({
                    _tag: 'Element',
                    array: backing.type,
                    index,
                    bounds: 'Checked',
                    span: selector.provenance.span,
                  }),
                )
                selected = element
                continue
              }
              if (selected._tag !== 'ArrayValue') {
                throw new RangeError(
                  'MIR verifier allowed an element selector on a non-array value',
                )
              }
              const index =
                selector.index._tag === 'Proven'
                  ? selector.index.value
                  : readI32(selector.index.local).value
              if (index < 0 || index >= selector.length) {
                return blockedStep({
                  _tag: 'Trap',
                  function: fn.id,
                  reason: `array index ${index} is outside length ${selector.length} in ${fn.id.module}.${fn.id.name}`,
                  span: selector.provenance.span,
                })
              }
              const element = selected.elements.at(index)
              if (element === undefined) {
                throw new RangeError('MIR verifier allowed an incomplete array value')
              }
              selectors.push(
                Object.freeze({
                  _tag: 'Element',
                  array: selected.type,
                  index,
                  bounds: selector.index._tag === 'Proven' ? 'Proven' : 'Checked',
                  span: selector.provenance.span,
                }),
              )
              selected = element
            }
            write(operation.destination, { value: selected, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'PlaceRead',
                function: fn.id,
                selectors: Object.freeze(selectors),
                value: selected,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'CheckPlace': {
            const resolved = resolvePlace(operation.root, operation.selectors)
            if (resolved._tag === 'Blocked') return resolved.step
            checkedPlaces.set(operation.selectors, resolved.indexes)
            trace.push(
              Object.freeze({
                _tag: 'WriteCheck',
                function: fn.id,
                region: region.id.ordinal,
                ...(region.ownerLoop === undefined ? {} : { loop: region.ownerLoop.ordinal }),
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'WritePlace': {
            const indexes = checkedPlaces.get(operation.selectors)
            if (indexes === undefined)
              throw new RangeError('MIR write executed without its precheck')
            const root = read(operation.root)
            const replacement = read(operation.source)
            if (operation.replacement === 'Owned') {
              const previous = resolvePlace(operation.root, operation.selectors)
              if (previous._tag === 'Blocked') return previous.step
              const members =
                previous.selected._tag === 'UnionValue'
                  ? Object.freeze([previous.selected.member])
                  : Object.freeze([])
              trace.push(
                Object.freeze({
                  _tag: 'ReplacementCleanup',
                  function: fn.id,
                  region: region.id.ordinal,
                  ...(region.ownerLoop === undefined ? {} : { loop: region.ownerLoop.ordinal }),
                  ...(members.length === 0 ? {} : { members }),
                  span: operation.provenance.span,
                }),
              )
            }
            write(operation.root, {
              value: replacePlace(root.value, operation.selectors, indexes, replacement.value),
              fromCall: replacement.fromCall,
            })
            checkedPlaces.delete(operation.selectors)
            trace.push(
              Object.freeze({
                _tag: 'Replacement',
                function: fn.id,
                region: region.id.ordinal,
                ...(region.ownerLoop === undefined ? {} : { loop: region.ownerLoop.ordinal }),
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'Drop': {
            const dropped = read(operation.local).value
            if (operation.cleanup._tag === 'AllocationCleanup') {
              if (dropped._tag !== 'AllocationValue')
                throw new RangeError('Allocation cleanup lost its private reclaim ticket')
              const ticket = state.allocations.get(dropped.ticket)
              if (ticket === undefined || !ticket.active)
                throw new RangeError('Allocation reclaim ticket was consumed more than once')
              ticket.active = false
            }
            const members = cleanupMembers(operation.cleanup, dropped)
            trace.push(
              Object.freeze({
                _tag: 'Cleanup',
                function: fn.id,
                local: operation.local.ordinal,
                ...(members.length === 0 ? {} : { members }),
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'MakeEffect': {
            const captures = operation.captures.map((capture): Value => {
              if (capture.access === 'Copy' || capture.access === 'Take')
                return read(capture.source).value
              const key = cellKey(frame, capture.source.ordinal)
              if (!state.cells.has(key)) state.cells.set(key, read(capture.source))
              return Object.freeze({
                _tag: 'EffectBorrowValue',
                frame,
                cell: capture.source.ordinal,
                access: capture.access,
              })
            })
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'EffectValue',
                type: operation.type.type,
                site: operation.type.site,
                runner: operation.runner,
                runnerTypeArguments: operation.runnerTypeArguments,
                captures: Object.freeze(captures),
              }),
              fromCall: false,
            })
            break
          }
          case 'PackEffectOutcome': {
            const payload = read(operation.source).value
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'EffectOutcomeValue',
                type: operation.type.type,
                tag: operation.tag,
                payload,
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: operation.tag === 0 ? 'EffectSuccess' : 'EffectFailure',
                function: fn.id,
                tag: operation.tag,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'UnpackEffectSuccess': {
            const outcome = read(operation.source)
            if (outcome.value._tag !== 'EffectOutcomeValue' || outcome.value.tag !== 0) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'attempted to unpack a failed effect outcome as success',
                span: operation.provenance.span,
              })
            }
            write(operation.destination, { value: outcome.value.payload, fromCall: true })
            break
          }
          case 'CatchEffect': {
            const protectedTarget = functionFor(
              program,
              operation.protectedTarget,
              operation.protectedTypeArguments,
            )
            if (protectedTarget === undefined)
              return blockedStep({
                _tag: 'MissingFunction',
                target: operation.protectedTarget,
                span: operation.provenance.span,
              })
            trace.push(
              Object.freeze({
                _tag: 'Call',
                caller: fn.id,
                target: operation.protectedTarget,
                callerInstance: fn.instance,
                targetInstance: protectedTarget.instance,
                span: operation.provenance.span,
              }),
            )
            const protectedArguments = operation.protectedArguments.map((argument) =>
              read(argument),
            )
            protectedArguments.forEach((argument, ordinal) => {
              trace.push(
                Object.freeze({
                  _tag: 'Binding',
                  target: operation.protectedTarget,
                  targetInstance: protectedTarget.instance,
                  callSpan: operation.provenance.span,
                  argumentOrdinal: ordinal,
                  parameterOrdinal: ordinal,
                  value: argument.value,
                  fromCall: argument.fromCall,
                  span: operation.provenance.span,
                }),
              )
            })
            const protectedResult = executeFunction(
              program,
              protectedTarget,
              protectedArguments.map((argument) => argument.value),
              Object.freeze([...active, protectedTarget.instance]),
              trace,
              state,
            )
            if (protectedResult._tag === 'Blocked') return protectedResult
            if (protectedResult.value._tag !== 'EffectValue')
              throw new RangeError('MIR effect catch factory returned a non-Effect value')
            write(operation.protectedValue, { value: protectedResult.value, fromCall: true })
            const protectedRunner = functionFor(
              program,
              operation.protectedRunner,
              operation.protectedTypeArguments,
            )
            if (protectedRunner === undefined)
              return blockedStep({
                _tag: 'MissingFunction',
                target: operation.protectedRunner,
                span: operation.provenance.span,
              })
            const protectedExecution = executeFunction(
              program,
              protectedRunner,
              protectedResult.value.captures,
              Object.freeze([...active, protectedRunner.instance]),
              trace,
              state,
            )
            if (protectedExecution._tag === 'Blocked') return protectedExecution
            if (protectedExecution.value._tag !== 'EffectOutcomeValue')
              throw new RangeError('MIR effect catch runner returned a non-outcome value')
            const protectedOutcome = protectedExecution.value
            write(operation.protectedOutcome, { value: protectedOutcome, fromCall: true })
            if (protectedOutcome.tag === 0) {
              write(operation.destination, {
                value: protectedOutcome.payload,
                fromCall: true,
              })
              break
            }
            if (protectedOutcome.tag !== operation.handledTag)
              throw new RangeError('MIR effect catch reached an unmatched failure tag')
            const handlerTarget = functionFor(program, operation.handlerTarget, Object.freeze([]))
            if (handlerTarget === undefined)
              return blockedStep({
                _tag: 'MissingFunction',
                target: operation.handlerTarget,
                span: operation.provenance.span,
              })
            trace.push(
              Object.freeze({
                _tag: 'Call',
                caller: fn.id,
                target: operation.handlerTarget,
                callerInstance: fn.instance,
                targetInstance: handlerTarget.instance,
                span: operation.provenance.span,
              }),
              Object.freeze({
                _tag: 'Binding',
                target: operation.handlerTarget,
                targetInstance: handlerTarget.instance,
                callSpan: operation.provenance.span,
                argumentOrdinal: 0,
                parameterOrdinal: 0,
                value: protectedOutcome.payload,
                fromCall: true,
                span: operation.provenance.span,
              }),
            )
            const handlerResult = executeFunction(
              program,
              handlerTarget,
              [protectedOutcome.payload],
              Object.freeze([...active, handlerTarget.instance]),
              trace,
              state,
            )
            if (handlerResult._tag === 'Blocked') return handlerResult
            if (handlerResult.value._tag !== 'EffectValue')
              throw new RangeError('MIR catch handler factory returned a non-Effect value')
            write(operation.handlerValue, { value: handlerResult.value, fromCall: true })
            const handlerRunner = functionFor(program, operation.handlerRunner, Object.freeze([]))
            if (handlerRunner === undefined)
              return blockedStep({
                _tag: 'MissingFunction',
                target: operation.handlerRunner,
                span: operation.provenance.span,
              })
            const handlerExecution = executeFunction(
              program,
              handlerRunner,
              handlerResult.value.captures,
              Object.freeze([...active, handlerRunner.instance]),
              trace,
              state,
            )
            if (handlerExecution._tag === 'Blocked') return handlerExecution
            if (
              handlerExecution.value._tag !== 'EffectOutcomeValue' ||
              handlerExecution.value.tag !== 0
            )
              throw new RangeError('MIR catch handler did not return an infallible effect outcome')
            write(operation.handlerOutcome, { value: handlerExecution.value, fromCall: true })
            write(operation.destination, { value: handlerExecution.value.payload, fromCall: true })
            break
          }
          case 'RetryEffect': {
            const protectedTarget =
              operation.protectedTarget === undefined
                ? undefined
                : functionFor(program, operation.protectedTarget, operation.protectedTypeArguments)
            const protectedRunner = functionFor(
              program,
              operation.protectedRunner,
              operation.protectedTypeArguments,
            )
            if (
              (operation.protectedTarget !== undefined && protectedTarget === undefined) ||
              protectedRunner === undefined
            )
              return blockedStep({
                _tag: 'MissingFunction',
                target:
                  operation.protectedTarget !== undefined && protectedTarget === undefined
                    ? operation.protectedTarget
                    : operation.protectedRunner,
                span: operation.provenance.span,
              })
            const protectedArguments = operation.protectedArguments.map((argument) =>
              read(argument),
            )
            const protectedValue =
              protectedTarget === undefined
                ? read(operation.protectedValue).value
                : executeFunction(
                    program,
                    protectedTarget,
                    protectedArguments.map((argument) => argument.value),
                    Object.freeze([...active, protectedTarget.instance]),
                    trace,
                    state,
                  )
            if ('_tag' in protectedValue && protectedValue._tag === 'Blocked') return protectedValue
            const effectValue =
              protectedTarget === undefined
                ? protectedValue
                : protectedValue._tag === 'Value'
                  ? protectedValue.value
                  : undefined
            if (effectValue?._tag !== 'EffectValue')
              throw new RangeError('MIR retry factory returned a non-Effect value')
            write(operation.protectedValue, { value: effectValue, fromCall: true })
            const retries = Math.max(0, readI32(operation.retries).value)
            let last: EffectOutcomeValue | undefined
            for (let attempt = 0; attempt <= retries; attempt += 1) {
              trace.push(
                Object.freeze({
                  _tag: 'Call',
                  caller: fn.id,
                  target: operation.protectedRunner,
                  callerInstance: fn.instance,
                  targetInstance: protectedRunner.instance,
                  span: operation.provenance.span,
                }),
              )
              const execution = executeFunction(
                program,
                protectedRunner,
                effectValue.captures,
                Object.freeze([...active, protectedRunner.instance]),
                trace,
                state,
              )
              if (execution._tag === 'Blocked') return execution
              if (execution.value._tag !== 'EffectOutcomeValue')
                throw new RangeError('MIR retry runner returned a non-outcome value')
              last = execution.value
              write(operation.protectedOutcome, { value: last, fromCall: true })
              if (last.tag === 0) {
                write(operation.destination, { value: last.payload, fromCall: true })
                break
              }
            }
            if (last?.tag === 0) break
            if (last === undefined || operation.propagationType === undefined)
              throw new RangeError('MIR retry exhausted without a propagation contract')
            const mapping = operation.tagMappings.find((candidate) => candidate.source === last.tag)
            if (mapping === undefined)
              throw new RangeError('MIR retry has no canonical failure-tag mapping')
            const propagated: EffectOutcomeValue = Object.freeze({
              _tag: 'EffectOutcomeValue',
              type: operation.propagationType.type,
              tag: mapping.target,
              payload: last.payload,
            })
            trace.push(
              Object.freeze({
                _tag: 'EffectFailure',
                function: fn.id,
                tag: mapping.target,
                span: operation.provenance.span,
              }),
              Object.freeze({
                _tag: 'Return',
                function: fn.id,
                instance: fn.instance,
                value: propagated,
                span: operation.provenance.span,
              }),
            )
            return Object.freeze({ _tag: 'Value', value: propagated })
          }
          case 'RunEffect': {
            const target = functionFor(program, operation.target, operation.typeArguments)
            if (target === undefined)
              return blockedStep({
                _tag: 'MissingFunction',
                target: operation.target,
                span: operation.provenance.span,
              })
            trace.push(
              Object.freeze({
                _tag: 'Call',
                caller: fn.id,
                target: operation.target,
                callerInstance: fn.instance,
                targetInstance: target.instance,
                span: operation.provenance.span,
              }),
            )
            const arguments_ = operation.arguments.map((argument) => read(argument))
            arguments_.forEach((argument, ordinal) => {
              trace.push(
                Object.freeze({
                  _tag: 'Binding',
                  target: operation.target,
                  targetInstance: target.instance,
                  callSpan: operation.provenance.span,
                  argumentOrdinal: ordinal,
                  parameterOrdinal: ordinal,
                  value: argument.value,
                  fromCall: argument.fromCall,
                  span: operation.provenance.span,
                }),
              )
            })
            const result = executeFunction(
              program,
              target,
              arguments_.map((argument) => argument.value),
              Object.freeze([...active, target.instance]),
              trace,
              state,
            )
            if (result._tag === 'Blocked') return result
            if (result.value._tag !== 'EffectOutcomeValue')
              throw new RangeError('MIR propagated effect returned a non-outcome value')
            const effectOutcome = result.value
            write(operation.outcome, { value: effectOutcome, fromCall: true })
            if (effectOutcome.tag === 0) {
              write(operation.destination, { value: effectOutcome.payload, fromCall: true })
              break
            }
            const mapping = operation.tagMappings.find(
              (candidate) => candidate.source === effectOutcome.tag,
            )
            if (mapping === undefined)
              throw new RangeError('MIR propagated effect has no canonical failure-tag mapping')
            const propagated: EffectOutcomeValue = Object.freeze({
              _tag: 'EffectOutcomeValue',
              type: operation.propagationType.type,
              tag: mapping.target,
              payload: effectOutcome.payload,
            })
            trace.push(
              Object.freeze({
                _tag: 'EffectFailure',
                function: fn.id,
                tag: mapping.target,
                span: operation.provenance.span,
              }),
              Object.freeze({
                _tag: 'Return',
                function: fn.id,
                instance: fn.instance,
                value: propagated,
                span: operation.provenance.span,
              }),
            )
            return Object.freeze({ _tag: 'Value', value: propagated })
          }
          case 'RunEffectValue': {
            const effect = read(operation.effect).value
            if (effect._tag !== 'EffectValue')
              throw new RangeError('MIR attempted to run a non-Effect value')
            const target = functionFor(program, operation.runner, operation.runnerTypeArguments)
            if (target === undefined)
              return blockedStep({
                _tag: 'MissingFunction',
                target: operation.runner,
                span: operation.provenance.span,
              })
            trace.push(
              Object.freeze({
                _tag: 'Call',
                caller: fn.id,
                target: operation.runner,
                callerInstance: fn.instance,
                targetInstance: target.instance,
                span: operation.provenance.span,
              }),
            )
            const result = executeFunction(
              program,
              target,
              effect.captures,
              Object.freeze([...active, target.instance]),
              trace,
              state,
            )
            if (result._tag === 'Blocked') return result
            if (result.value._tag !== 'EffectOutcomeValue')
              throw new RangeError('MIR Effect runner returned a non-outcome value')
            const effectOutcome = result.value
            write(operation.outcome, { value: effectOutcome, fromCall: true })
            if (effectOutcome.tag === 0) {
              write(operation.destination, { value: effectOutcome.payload, fromCall: true })
              break
            }
            if (operation.propagationType === undefined)
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'unhandled Effect failure escaped an infallible context',
                span: operation.provenance.span,
              })
            const mapping = operation.tagMappings.find(
              (candidate) => candidate.source === effectOutcome.tag,
            )
            if (mapping === undefined)
              throw new RangeError('MIR Effect runner has no failure-tag mapping')
            const propagated: EffectOutcomeValue = Object.freeze({
              _tag: 'EffectOutcomeValue',
              type: operation.propagationType.type,
              tag: mapping.target,
              payload: effectOutcome.payload,
            })
            return Object.freeze({ _tag: 'Value', value: propagated })
          }
          case 'Call': {
            const target = functionFor(program, operation.target, operation.typeArguments)
            if (target === undefined) {
              return blockedStep({
                _tag: 'MissingFunction',
                target: operation.target,
                span: operation.provenance.span,
              })
            }
            trace.push(
              Object.freeze({
                _tag: 'Call',
                caller: fn.id,
                target: operation.target,
                callerInstance: fn.instance,
                targetInstance: target.instance,
                span: operation.provenance.span,
              }),
            )
            const cycleStart = active.findIndex((candidate) =>
              sameInstance(candidate, target.instance),
            )
            if (cycleStart >= 0) {
              return blockedStep({
                _tag: 'RecursiveCycle',
                cycle: Object.freeze([...active.slice(cycleStart), target.instance]),
                closingCallSpan: operation.provenance.span,
              })
            }
            const argumentStates = operation.arguments.map((argument) => read(argument))
            argumentStates.forEach((state, ordinal) => {
              trace.push(
                Object.freeze({
                  _tag: 'Binding',
                  target: operation.target,
                  targetInstance: target.instance,
                  callSpan: operation.provenance.span,
                  argumentOrdinal: ordinal,
                  parameterOrdinal: ordinal,
                  value: state.value,
                  fromCall: state.fromCall,
                  span: operation.provenance.span,
                }),
              )
            })
            const result = executeFunction(
              program,
              target,
              argumentStates.map((state) => state.value),
              Object.freeze([...active, target.instance]),
              trace,
              state,
            )
            if (result._tag === 'Blocked') return result
            write(operation.destination, { value: result.value, fromCall: true })
            break
          }
        }
      }
      return undefined
    }
    const operations = region._tag === 'OperationRegion' ? region.operations : region.releases
    const operationStep = executeOperations(operations)
    if (operationStep !== undefined) return operationStep
    const outcome = region.outcome
    switch (outcome._tag) {
      case 'Return': {
        const result = read(outcome.value)
        trace.push(
          Object.freeze({
            _tag: 'Return',
            function: fn.id,
            instance: fn.instance,
            value: result.value,
            span: outcome.provenance.span,
          }),
        )
        return Object.freeze({ _tag: 'Value', value: result.value })
      }
      case 'Forward':
        regionOrdinal = outcome.target.ordinal
        break
      case 'Repeat': {
        const loop = loops.get(outcome.loop.ordinal)
        if (loop === undefined) throw new RangeError('MIR verifier allowed a missing repeat loop')
        trace.push(
          Object.freeze({
            _tag: 'Repeat',
            function: fn.id,
            region: region.id.ordinal,
            loop: outcome.loop.ordinal,
            span: outcome.provenance.span,
          }),
        )
        regionOrdinal = loop.id.ordinal
        break
      }
      case 'Exit': {
        const loop = loops.get(outcome.loop.ordinal)
        if (loop === undefined) throw new RangeError('MIR verifier allowed a missing exit loop')
        trace.push(
          Object.freeze({
            _tag: 'Exit',
            function: fn.id,
            region: region.id.ordinal,
            loop: outcome.loop.ordinal,
            span: outcome.provenance.span,
          }),
        )
        regionOrdinal = loop.following.ordinal
        break
      }
      case 'Yield': {
        const loop = conditionOwners.get(region.id.ordinal)
        if (loop === undefined) throw new RangeError('MIR verifier allowed an unowned yield')
        const enter = readI32(loop.conditionValue).value !== 0
        trace.push(
          Object.freeze({
            _tag: enter ? 'Iteration' : 'Condition',
            function: fn.id,
            region: region.id.ordinal,
            loop: loop.loop.ordinal,
            span: outcome.provenance.span,
          }),
        )
        regionOrdinal = enter ? loop.body.ordinal : loop.following.ordinal
        break
      }
      case 'Trap':
        return blockedStep({
          _tag: 'Trap',
          function: fn.id,
          reason: outcome.reason,
          span: outcome.provenance.span,
        })
    }
  }
}

const argumentSpanFallback = (fn: Mir.MirFunction): SourceSpan.SourceSpan => {
  const region = fn.regions.find((candidate) => candidate.id.ordinal === fn.entry.ordinal)
  const span =
    region?._tag === 'ConditionalRegion' || region?._tag === 'LoopRegion'
      ? region.provenance.span
      : region?._tag === 'OperationRegion'
        ? (region.operations.at(0)?.provenance.span ?? region.outcome.provenance.span)
        : (region?.releases.at(0)?.provenance.span ?? region?.outcome.provenance.span)
  if (span === undefined) {
    throw new RangeError(`Lowered function ${fn.id.name} has no regions`)
  }
  return span
}

const firstFunctionSpan = (program: Mir.Module): SourceSpan.SourceSpan => {
  const first = program.functions.at(0)
  return first === undefined ? raiseNoSpan() : argumentSpanFallback(first)
}

/** Executes the lowered program from the discovered entry, replaying MIR operations as a trace. */
export const evaluate = (discovery: Instances.Discovery, program: Mir.Module): Outcome => {
  const violations = Mir.verify(program)
  if (violations.length > 0) {
    return Object.freeze({
      _tag: 'Blocked',
      entry: discovery.entry._tag === 'Resolved' ? discovery.entry.key.declaration : undefined,
      reason: Object.freeze({ _tag: 'InvalidMir', violations }),
      trace: Object.freeze([]),
    })
  }
  if (discovery.entry._tag !== 'Resolved') {
    return Object.freeze({
      _tag: 'Blocked',
      entry: undefined,
      reason: Object.freeze({ _tag: 'UnavailableEntry', reason: discovery.entry.reason }),
      trace: Object.freeze([]),
    })
  }

  const entry = discovery.entry.key.declaration
  const fn = functionFor(program, entry, discovery.entry.key.typeArguments)
  if (fn === undefined) {
    return Object.freeze({
      _tag: 'Blocked',
      entry,
      reason: Object.freeze({
        _tag: 'MissingFunction',
        target: entry,
        span: firstFunctionSpan(program),
      }),
      trace: Object.freeze([]),
    })
  }

  const trace: Array<TraceEvent> = []
  trace.push(
    Object.freeze({
      _tag: 'Entry',
      function: entry,
      instance: fn.instance,
      span: argumentSpanFallback(fn),
    }),
  )
  const result = executeFunction(program, fn, [], Object.freeze([fn.instance]), trace, {
    nextFrame: 0,
    nextAllocation: 0,
    cells: new Map(),
    allocations: new Map(),
  })
  if (result._tag === 'Blocked') {
    return Object.freeze({
      _tag: 'Blocked',
      entry,
      reason: result.reason,
      trace: Object.freeze([...trace]),
    })
  }
  if (result.value._tag !== 'I32Value') {
    throw new RangeError('Bootstrap entry returned a non-I32 value')
  }
  return Object.freeze({
    _tag: 'Completed',
    entry,
    result: result.value,
    trace: Object.freeze([...trace]),
  })
}

const raiseNoSpan = (): never => {
  throw new RangeError('Lowered program has no functions to attach a span to')
}
