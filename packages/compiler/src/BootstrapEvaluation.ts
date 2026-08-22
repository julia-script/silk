import * as BootstrapArithmetic from './BootstrapArithmetic.js'
import * as BootstrapEffect from './BootstrapEffect.js'
import type {
  CallRequest,
  LocalState,
  MachineRequest,
  OriginTransferRequest,
  RelayTransferRequest,
  Step,
  TransferStep,
} from './BootstrapMachine.js'
import * as BootstrapOsIntrinsics from './BootstrapOsIntrinsics.js'
import * as BootstrapPlace from './BootstrapPlace.js'
import * as BootstrapStorage from './BootstrapStorage.js'
import type * as ChildProcess from './ChildProcess.js'
import type * as CleanupPlan from './CleanupPlan.js'
import * as FloatingPoint from './FloatingPoint.js'
import type * as Hir from './Hir.js'
import type * as HostInput from './HostInput.js'
import * as Instances from './Instances.js'
import * as IntrinsicAvailability from './IntrinsicAvailability.js'
import * as Mir from './Mir.js'
import * as MirVerification from './MirVerification.js'
import type * as OsFileSystemHost from './OsFileSystemHost.js'
import * as Scalar from './Scalar.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as StandardInput from './StandardInput.js'
import type * as StandardStreams from './StandardStreams.js'
import * as Transcendental from './Transcendental.js'
import * as Type from './Type.js'

/**
 * The closed bootstrap interpreter, executing the lowered MIR program from the entry instance
 * discovery resolved. It is the semantics oracle for the coming native differential checks and
 * the second consumer keeping MIR's meaning in MIR. A severable leaf: nothing in the pipeline
 * depends on it.
 */

/** One exact fixed- or target-width integer. */
import type {
  AggregateValue,
  ArrayValue,
  CharacterValue,
  FloatValue,
  IntegerValue,
  SharedCoreValue,
  SliceValue,
  StaticViewValue,
  StringStorage,
  StringValue,
  UnionValue,
  Value,
} from './BootstrapValue.js'

export type {
  AggregateValue,
  AllocationValue,
  ArrayValue,
  CallableBorrowValue,
  CallableValue,
  CharacterValue,
  EffectBorrowValue,
  EffectCompositeValue,
  EffectOutcomeValue,
  EffectValue,
  FloatValue,
  IntegerValue,
  RawBufferValue,
  ReferenceValue,
  SharedCoreValue,
  SliceValue,
  SlotValue,
  StaticViewValue,
  StringStorage,
  StringValue,
  UnionValue,
  Value,
} from './BootstrapValue.js'

import type {
  ActiveFrame,
  BlockedReason,
  EffectTraceEvent,
  EntryTraceEvent,
  Outcome,
  PlaceReadTraceEvent,
  TraceEvent,
} from './BootstrapTrace.js'

export type {
  ActiveFrame,
  AllocationTraceEvent,
  ArrayConstructTraceEvent,
  BindingTraceEvent,
  Blocked,
  BlockedReason,
  CallableTraceEvent,
  CallTraceEvent,
  CleanupTraceEvent,
  Completed,
  ConstructTraceEvent,
  ControlTraceEvent,
  CoroutineFrameTraceEvent,
  EffectTraceEvent,
  EntryTraceEvent,
  MatchTraceEvent,
  OsCallTraceEvent,
  Outcome,
  PlaceReadTraceEvent,
  ProjectTraceEvent,
  ReturnTraceEvent,
  StandardStreamTraceEvent,
  StringTraceEvent,
  TraceEvent,
  Trap,
  UnhandledFailure,
  UnionConversionTraceEvent,
} from './BootstrapTrace.js'

const integerValue = (type: Scalar.IntegerSpelling, input: bigint | number): IntegerValue =>
  Object.freeze({ _tag: 'IntegerValue', type, value: BigInt(input) })

const characterValue = (input: number): CharacterValue =>
  Object.freeze({ _tag: 'CharacterValue', value: input })

const floatValue = (type: Scalar.FloatSpelling, bits: bigint): FloatValue =>
  Object.freeze({ _tag: 'FloatValue', type, bits: BigInt.asUintN(type === 'f32' ? 32 : 64, bits) })

const borrowKey = (borrow: Hir.BorrowId): string =>
  `${borrow.function.sourceId}:${borrow.function.ordinal}:${borrow.callSpan.start}:${borrow.callSpan.end}:${borrow.ordinal}`

const floatingBits = (self: FloatValue): FloatingPoint.Bits =>
  Object.freeze({ width: self.type === 'f32' ? 32 : 64, bits: self.bits })

const floatingUnary = (
  operation: Extract<Mir.Operation, { readonly _tag: 'FloatUnary' }>['operation'],
  self: FloatValue,
): Value => {
  const bits = floatingBits(self)
  if (operation === 'Negate')
    return floatValue(self.type, self.bits ^ (1n << BigInt(bits.width - 1)))
  if (operation === 'Sqrt') return floatValue(self.type, FloatingPoint.squareRoot(bits).bits)
  const result =
    operation === 'IsNaN'
      ? FloatingPoint.isNotANumber(bits)
      : operation === 'IsInfinite'
        ? FloatingPoint.isInfinite(bits)
        : operation === 'IsFinite'
          ? FloatingPoint.isFiniteNumber(bits)
          : operation === 'IsNormal'
            ? FloatingPoint.isNormal(bits)
            : operation === 'IsSubnormal'
              ? FloatingPoint.isSubnormal(bits)
              : FloatingPoint.isSignNegative(bits)
  return integerValue('i32', result ? 1 : 0)
}

const floatingBinary = (
  operation: Mir.BinaryOperator,
  left: FloatValue,
  right: FloatValue,
): Value => {
  const leftBits = floatingBits(left)
  const rightBits = floatingBits(right)
  const leftNumber = FloatingPoint.toNumber(leftBits)
  const rightNumber = FloatingPoint.toNumber(rightBits)
  if (
    operation === 'Equals' ||
    operation === 'NotEquals' ||
    operation === 'LessThan' ||
    operation === 'LessOrEqual' ||
    operation === 'GreaterThan' ||
    operation === 'GreaterOrEqual'
  ) {
    const result =
      operation === 'Equals'
        ? leftNumber === rightNumber
        : operation === 'NotEquals'
          ? leftNumber !== rightNumber
          : operation === 'LessThan'
            ? leftNumber < rightNumber
            : operation === 'LessOrEqual'
              ? leftNumber <= rightNumber
              : operation === 'GreaterThan'
                ? leftNumber > rightNumber
                : leftNumber >= rightNumber
    return integerValue('i32', result ? 1 : 0)
  }
  if (operation === 'TotalOrder')
    return integerValue(
      'i32',
      FloatingPoint.totalOrderKey(leftBits) <= FloatingPoint.totalOrderKey(rightBits) ? 1 : 0,
    )
  const result =
    operation === 'Add'
      ? leftNumber + rightNumber
      : operation === 'Subtract'
        ? leftNumber - rightNumber
        : operation === 'Multiply'
          ? leftNumber * rightNumber
          : operation === 'Divide'
            ? leftNumber / rightNumber
            : leftNumber % rightNumber
  const encoded = FloatingPoint.fromNumber(result, leftBits.width)
  return floatValue(left.type, encoded.bits)
}

const blockedStep = (reason: BlockedReason): Step =>
  Object.freeze({ _tag: 'Blocked', reason: Object.freeze(reason) })

interface EvaluationState {
  nextFrame: number
  nextAllocation: number
  nextCallable: number
  nextCoroutineFrame: number
  steps: number
  readonly maxSteps: number
  readonly maxCallDepth: number
  readonly maxExecutionStackBytes: number
  executionStackBytes: number
  activeFrames: ReadonlyArray<ActiveFrame>
  readonly cells: Map<string, LocalState>
  readonly allocations: BootstrapStorage.Allocations
  readonly callables: Map<number, { state: 'Available' | 'Running' | 'Consumed' | 'Released' }>
  readonly activeLoans: Set<string>
  readonly stringLoans: Set<string>
  readonly standardStreams?: StandardStreams.Provider
  readonly standardInput?: StandardInput.Provider
  readonly childProcess?: ChildProcess.Provider
  /** The captured streams of the most recent completed execution, indexed by stream selector. */
  readonly processCaptures: Array<ReadonlyArray<number>>
  readonly hostInput?: HostInput.Provider
  readonly osFileSystem?: OsFileSystemHost.Provider
}

interface TransferContext {
  readonly step: TransferStep
  readonly pending: Array<{
    readonly activation: ActivationRecord
    readonly state?: Mir.CoroutineFrameState
  }>
  readonly parent?: TransferContext
}

interface ActivationRecord extends ActiveFrame {
  readonly locals: Map<number, LocalState>
  readonly cells: EvaluationState['cells']
  continuation?: FunctionExecution
  pendingCall?: CallRequest
  cleanupState?: CleanupPlan.CleanupPlan['_tag']
  coroutineFrame?: {
    readonly ticket: number
    readonly bytes: number
    point: Mir.SuspensionPointId
  }
}

type FunctionExecution = Generator<MachineRequest, Step, Step>
type CleanupExecution = Generator<MachineRequest, Step | undefined, Step>
type OperationsExecution = Generator<MachineRequest, Step | undefined, Step>

const cellKey = (frame: number, cell: number): string => `${frame}:${cell}`

function* executeFunction(
  program: Mir.Module,
  fn: Mir.MirFunction,
  activation: ActivationRecord,
  trace: Array<TraceEvent>,
  state: EvaluationState,
): FunctionExecution {
  const frame = activation.frame
  const locals = activation.locals

  const callFunction = function* (
    target: Mir.MirFunction,
    arguments_: ReadonlyArray<Value>,
    span: SourceSpan.SourceSpan,
  ): FunctionExecution {
    const request: CallRequest = Object.freeze({
      _tag: 'CallRequest',
      target,
      arguments: arguments_,
      span,
    })
    activation.pendingCall = request
    const result: Step = yield request
    delete activation.pendingCall
    return result
  }

  const relayTransfers = function* (
    initial: Step,
    state_?: Mir.CoroutineFrameState,
  ): FunctionExecution {
    let result = initial
    while (result._tag === 'Transfer') {
      const request: RelayTransferRequest = Object.freeze({
        _tag: 'RelayTransferRequest',
        transfer: result,
        ...(state_ === undefined ? {} : { state: state_ }),
      })
      result = yield request
    }
    return result
  }

  const suspensionFor = (
    operation: Mir.Operation,
  ): Mir.SuspendEffectRegion | Mir.RunSuspendableEffectRegion | undefined =>
    fn.suspension?.regions.find((region) => region.operation === operation)

  const callEffectRunner = function* (
    target: Mir.MirFunction,
    arguments_: ReadonlyArray<Value>,
    operation: Extract<
      Mir.Operation,
      { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'ReifyEffect' }
    >,
  ): FunctionExecution {
    const control = suspensionFor(operation)
    if (control?._tag === 'SuspendEffectRegion') {
      const child: CallRequest = Object.freeze({
        _tag: 'CallRequest',
        target,
        arguments: arguments_,
        span: operation.provenance.span,
      })
      const request: OriginTransferRequest = Object.freeze({
        _tag: 'OriginTransferRequest',
        origin: control,
        child,
      })
      return yield request
    }
    const result = yield* callFunction(target, arguments_, operation.provenance.span)
    if (result._tag !== 'Transfer') return result
    if (control?._tag !== 'RunSuspendableEffectRegion') return result
    const frameState = control.relay.state
    return yield* relayTransfers(result, frameState)
  }

  const evaluationLimit = (
    kind: 'Steps' | 'CallDepth',
    limit: number,
    count: number,
    span: SourceSpan.SourceSpan,
  ): Step =>
    blockedStep({
      _tag: 'EvaluationLimit',
      kind,
      limit,
      count,
      function: fn.id,
      span,
      activeFrames: Object.freeze([...state.activeFrames]),
    })

  const read = (local: Mir.LocalId): LocalState => {
    const direct = state.cells.get(cellKey(frame, local.ordinal)) ??
      locals.get(local.ordinal) ?? { value: integerValue('i32', 0), fromCall: false }
    if (direct.value._tag !== 'EffectBorrowValue' && direct.value._tag !== 'CallableBorrowValue')
      return direct
    return (
      state.cells.get(cellKey(direct.value.frame, direct.value.cell)) ?? {
        value: integerValue('i32', 0),
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

  const placeAccess: BootstrapPlace.Access = {
    readIndex: (local) => readInteger(local, 'usize').value,
    sliceElement: (slice, index) => {
      if (slice.ticket !== undefined) {
        const allocation = state.allocations.get(slice.ticket)
        const element = allocation?.values.get(String(slice.base + index))
        if (allocation === undefined || !allocation.active || element === undefined)
          throw new RangeError('MIR RawBuffer slice selected uninitialized storage')
        return element
      }
      const backing = BootstrapPlace.selectStored(
        cell(slice).value,
        slice.selectors ?? Object.freeze([]),
        slice.indexes ?? Object.freeze([]),
      )
      if (backing._tag !== 'ArrayValue')
        throw new RangeError('MIR slice cell does not contain an array value')
      const element = backing.elements.at(slice.base + index)
      if (element === undefined) throw new RangeError('MIR slice range exceeds its backing cell')
      return element
    },
    replaceSliceElement: (slice, index, value) => {
      if (slice.ticket !== undefined) {
        const allocation = state.allocations.get(slice.ticket)
        const absolute = slice.base + index
        if (
          allocation === undefined ||
          !allocation.active ||
          !allocation.values.has(String(absolute))
        )
          throw new RangeError('Invalid RawBuffer slice replacement')
        allocation.values.set(String(absolute), value)
        return slice
      }
      const key = cellKey(slice.frame, slice.cell)
      const stored = state.cells.get(key)
      if (stored === undefined) throw new RangeError('Invalid slice backing cell replacement')
      const prefixSelectors = slice.selectors ?? Object.freeze([])
      const prefixIndexes = slice.indexes ?? Object.freeze([])
      const backing = BootstrapPlace.selectStored(stored.value, prefixSelectors, prefixIndexes)
      if (backing._tag !== 'ArrayValue')
        throw new RangeError('Invalid slice backing cell replacement')
      const absolute = slice.base + index
      const updated: ArrayValue = Object.freeze({
        _tag: 'ArrayValue',
        type: backing.type,
        elements: Object.freeze(
          backing.elements.map((element, ordinal) => (ordinal === absolute ? value : element)),
        ),
      })
      state.cells.set(key, {
        value: BootstrapPlace.replacePlaceByIndexes(
          stored.value,
          prefixSelectors,
          prefixIndexes,
          updated,
          placeAccess,
        ),
        fromCall: stored.fromCall,
      })
      return slice
    },
  }

  const referenced = (local: Mir.LocalId): LocalState => {
    const reference = read(local).value
    if (reference._tag !== 'ReferenceValue') {
      throw new RangeError('MIR raw storage operation lost its whole-value reference')
    }
    const found = state.cells.get(cellKey(reference.frame, reference.cell))
    if (found === undefined)
      throw new RangeError('MIR reference points at a missing evaluator cell')
    const selected = BootstrapPlace.walkPlace(
      found.value,
      reference.selectors,
      reference.indexes,
      placeAccess,
    )
    if (selected._tag === 'OutOfBounds')
      throw new RangeError('MIR reference selector is outside its checked place')
    return Object.freeze({ value: selected.selected, fromCall: found.fromCall })
  }

  const readInteger = (local: Mir.LocalId, expected?: Scalar.IntegerSpelling): IntegerValue => {
    const found = read(local).value
    const localType = fn.localTypes.at(local.ordinal)
    const semantic = localType === undefined ? undefined : Mir.semanticType(localType)
    const scalar = typeof semantic === 'string' ? Scalar.find(semantic) : undefined
    const expectedType = expected ?? (scalar?.category === 'Integer' ? scalar.spelling : undefined)
    if (
      found._tag !== 'IntegerValue' ||
      (expectedType !== undefined && found.type !== expectedType)
    ) {
      throw new RangeError(`MIR verifier allowed aggregate local %${local.ordinal} as an integer`)
    }
    return found
  }

  const readCharacter = (local: Mir.LocalId): CharacterValue => {
    const found = read(local).value
    if (found._tag !== 'CharacterValue')
      throw new RangeError(`MIR verifier allowed non-char local %${local.ordinal} as a char`)
    return found
  }

  const readFloat = (local: Mir.LocalId): FloatValue => {
    const found = read(local).value
    if (found._tag !== 'FloatValue')
      throw new RangeError(`MIR verifier allowed non-float local %${local.ordinal} as a float`)
    return found
  }

  /**
   * Releases one owned value through its complete cleanup plan: hooks run before their field
   * cleanup, struct fields release in declaration order, and every reclaim ticket is consumed
   * exactly once. Returns a blocked step when a hook call blocks or unsafe code already
   * consumed a ticket this plan still owns.
   */
  const releaseThroughPlan = function* (
    cleanup: CleanupPlan.CleanupPlan,
    owner: Value,
    provenance: Mir.Provenance,
    localOrdinal: number,
  ): CleanupExecution {
    activation.cleanupState = cleanup._tag
    switch (cleanup._tag) {
      case 'NoCleanup':
      case 'ParameterCleanup':
        return undefined
      case 'AllocationCleanup': {
        if (owner._tag !== 'AllocationValue')
          throw new RangeError('Allocation cleanup lost its private reclaim ticket')
        if (!BootstrapStorage.release(state.allocations, owner.ticket, false)) {
          // A caller obligation, not a compiler invariant: unsafe code can reach a second
          // release, and the run must stop as a trap rather than take down the host.
          return blockedStep({
            _tag: 'Trap',
            function: fn.id,
            reason: 'Allocation reclaim ticket was consumed more than once',
            span: provenance.span,
          })
        }
        trace.push(
          Object.freeze({
            _tag: 'AllocationRelease',
            function: fn.id,
            ticket: owner.ticket,
            span: provenance.span,
          }),
        )
        return undefined
      }
      case 'RawBufferCleanup': {
        if (owner._tag !== 'RawBufferValue')
          throw new RangeError('RawBuffer cleanup lost its private reclaim ticket')
        if (!BootstrapStorage.release(state.allocations, owner.ticket, true)) {
          return blockedStep({
            _tag: 'Trap',
            function: fn.id,
            reason: 'RawBuffer reclaim ticket was consumed more than once',
            span: provenance.span,
          })
        }
        trace.push(
          Object.freeze({
            _tag: 'AllocationRelease',
            function: fn.id,
            ticket: owner.ticket,
            span: provenance.span,
          }),
        )
        return undefined
      }
      case 'CallableCleanup': {
        if (owner._tag !== 'CallableValue')
          throw new RangeError('Callable cleanup lost its evaluator identity')
        const callable = state.callables.get(owner.ticket)
        if (callable === undefined)
          throw new RangeError('Callable cleanup referenced a missing evaluator identity')
        const wasAvailable = callable.state === 'Available'
        if (callable.state !== 'Consumed') callable.state = 'Released'
        trace.push(
          Object.freeze({
            _tag: 'CallableCleanup',
            function: fn.id,
            ticket: owner.ticket,
            mode: owner.type.mode,
            span: provenance.span,
          }),
        )
        if (!wasAvailable) return undefined
        for (const slot of cleanup.slots) {
          const capture = owner.captures.find((candidate) => candidate.ordinal === slot.ordinal)
          if (capture === undefined) continue
          const blocked = yield* releaseThroughPlan(
            slot.cleanup,
            capture.value,
            provenance,
            localOrdinal,
          )
          if (blocked !== undefined) return blocked
        }
        return undefined
      }
      case 'EffectCleanup': {
        if (owner._tag !== 'EffectValue')
          throw new RangeError('Effect cleanup lost its evaluator identity')
        for (const slot of cleanup.slots) {
          const capture = owner.captures.at(slot.ordinal)
          if (capture === undefined) continue
          const blocked = yield* releaseThroughPlan(slot.cleanup, capture, provenance, localOrdinal)
          if (blocked !== undefined) return blocked
        }
        return undefined
      }
      case 'EffectCompositeCleanup': {
        if (owner._tag !== 'EffectCompositeValue')
          throw new RangeError('Effect composite cleanup lost its selected alternative')
        const selected = cleanup.alternatives.at(owner.alternative)
        if (selected === undefined)
          throw new RangeError('Effect composite cleanup selected an absent alternative')
        return yield* releaseThroughPlan(selected, owner.effect, provenance, localOrdinal)
      }
      case 'HookCleanup': {
        const target = BootstrapEffect.functionFor(program, cleanup.hook, cleanup.typeArguments)
        if (target === undefined) {
          return blockedStep({
            _tag: 'MissingFunction',
            target: cleanup.hook,
            span: provenance.span,
          })
        }
        const key = cellKey(frame, localOrdinal)
        state.cells.set(key, { value: owner, fromCall: false })
        trace.push(
          Object.freeze({
            _tag: 'Call',
            frame: activation.frame,
            depth: activation.depth,
            caller: fn.id,
            target: cleanup.hook,
            callerInstance: fn.instance,
            targetInstance: target.instance,
            span: provenance.span,
          }),
        )
        const reference: Value = Object.freeze({
          _tag: 'ReferenceValue' as const,
          frame,
          cell: localOrdinal,
          selectors: Object.freeze([]),
          indexes: Object.freeze([]),
        })
        const result = yield* callFunction(target, [reference], provenance.span)
        if (result._tag === 'Blocked') return result
        if (result._tag === 'Transfer') return result
        const updated = state.cells.get(key)?.value ?? owner
        return yield* releaseThroughPlan(cleanup.inner, updated, provenance, localOrdinal)
      }
      case 'StructCleanup': {
        if (owner._tag !== 'AggregateValue') return undefined
        for (const field of cleanup.fields) {
          const entry = owner.fields.find(
            (candidate) => candidate.field.ordinal === field.field.ordinal,
          )
          if (entry === undefined) continue
          const blocked = yield* releaseThroughPlan(
            field.cleanup,
            entry.value,
            provenance,
            localOrdinal,
          )
          if (blocked !== undefined) return blocked
        }
        return undefined
      }
      case 'ArrayCleanup': {
        if (owner._tag !== 'ArrayValue') return undefined
        for (const element of owner.elements) {
          const blocked = yield* releaseThroughPlan(
            cleanup.element,
            element,
            provenance,
            localOrdinal,
          )
          if (blocked !== undefined) return blocked
        }
        return undefined
      }
      case 'UnionCleanup': {
        if (owner._tag !== 'UnionValue') return undefined
        const activeCase = cleanup.cases.find((candidate) =>
          Type.equals(candidate.member, owner.member),
        )
        return activeCase === undefined
          ? undefined
          : yield* releaseThroughPlan(activeCase.cleanup, owner.payload, provenance, localOrdinal)
      }
      case 'RepresentedCallableCleanup':
        // Lowering resolves this obligation against the shared realization before MIR exists, so
        // reaching it here would mean an aggregate is about to skip the owned captures of the
        // callable it stores. Fail loudly rather than release nothing.
        throw new RangeError('Stored callable cleanup reached evaluation unresolved')
    }
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
  const checkedPlaces = new Map<string, ReadonlyArray<number>>()

  const invokeCallableTarget = function* (
    target: Hir.CallableTarget,
    typeArguments: ReadonlyArray<Type.GenericArgument>,
    arguments_: ReadonlyArray<Value>,
    span: SourceSpan.SourceSpan,
  ): FunctionExecution {
    if (target._tag === 'DeclarationCallableTarget') {
      const callee = BootstrapEffect.functionFor(program, target.declaration, typeArguments)
      if (callee === undefined) {
        return blockedStep({ _tag: 'MissingFunction', target: target.declaration, span })
      }
      trace.push(
        Object.freeze({
          _tag: 'Call',
          frame: activation.frame,
          depth: activation.depth,
          caller: fn.id,
          target: target.declaration,
          callerInstance: fn.instance,
          targetInstance: callee.instance,
          span,
        }),
      )
      arguments_.forEach((argument, ordinal) => {
        trace.push(
          Object.freeze({
            _tag: 'Binding',
            frame: state.nextFrame,
            depth: activation.depth + 1,
            target: target.declaration,
            targetInstance: callee.instance,
            callSpan: span,
            argumentOrdinal: ordinal,
            parameterOrdinal: ordinal,
            value: argument,
            fromCall: true,
            span,
          }),
        )
      })
      return yield* callFunction(callee, arguments_, span)
    }

    const operation = target.operation
    const conversionTarget = Scalar.conversionTarget(operation)
    const actorScalar = Scalar.find(target.actor)
    if (actorScalar?.category === 'Floating') {
      const first = arguments_.at(0)
      if (operation === 'FromBits') {
        if (first === undefined || first._tag !== 'IntegerValue')
          throw new RangeError('MIR verifier allowed invalid float bits')
        return Object.freeze({
          _tag: 'Value',
          value: floatValue(actorScalar.spelling, BigInt(first.value)),
        })
      }
      if (first?._tag !== 'FloatValue')
        throw new RangeError('MIR verifier allowed invalid float callable')
      if (operation === 'ToBits') {
        const targetType: Scalar.IntegerSpelling = actorScalar.spelling === 'f32' ? 'u32' : 'u64'
        return Object.freeze({ _tag: 'Value', value: integerValue(targetType, first.bits) })
      }
      const floatTarget = Scalar.floatConversionTarget(operation)
      if (floatTarget !== undefined) {
        const converted = FloatingPoint.fromNumber(
          FloatingPoint.toNumber(floatingBits(first)),
          floatTarget.spelling === 'f32' ? 32 : 64,
        )
        return Object.freeze({
          _tag: 'Value',
          value: floatValue(floatTarget.spelling, converted.bits),
        })
      }
      if (conversionTarget !== undefined) {
        const number = FloatingPoint.toNumber(floatingBits(first))
        const exact = Number.isFinite(number) ? BigInt(Math.trunc(number)) : undefined
        const range = Scalar.range(
          conversionTarget,
          program.layout.target.pointerSize === 4 ? 32 : 64,
        )
        if (exact === undefined || exact < range.minimum || exact > range.maximum)
          return blockedStep({
            _tag: 'Trap',
            function: fn.id,
            reason: 'float conversion out of range',
            span,
          })
        return Object.freeze({
          _tag: 'Value',
          value: integerValue(conversionTarget.spelling, exact),
        })
      }
      if (
        operation === 'Negate' ||
        operation === 'IsNaN' ||
        operation === 'IsInfinite' ||
        operation === 'IsFinite' ||
        operation === 'IsNormal' ||
        operation === 'IsSubnormal' ||
        operation === 'IsSignNegative'
      )
        return Object.freeze({ _tag: 'Value', value: floatingUnary(operation, first) })
      const second = arguments_.at(1)
      if (second?._tag === 'FloatValue' && Mir.isBinaryOperator(operation))
        return Object.freeze({ _tag: 'Value', value: floatingBinary(operation, first, second) })
    }
    const floatTarget = Scalar.floatConversionTarget(operation)
    if (actorScalar?.category === 'Integer' && floatTarget !== undefined) {
      const first = arguments_.at(0)
      if (first === undefined || first._tag !== 'IntegerValue')
        throw new RangeError('MIR verifier allowed invalid integer-to-float callable')
      const encoded = FloatingPoint.fromNumber(
        Number(BigInt(first.value)),
        floatTarget.spelling === 'f32' ? 32 : 64,
      )
      return Object.freeze({ _tag: 'Value', value: floatValue(floatTarget.spelling, encoded.bits) })
    }
    if (operation === 'CheckedConvertToChar') {
      const subject = arguments_.at(0)
      if (actorScalar?.category !== 'Character' || subject?._tag !== 'IntegerValue')
        throw new RangeError('MIR verifier allowed an invalid checked char callable')
      const exact = BigInt(subject.value)
      const succeeded = Scalar.isUnicodeScalarValue(exact)
      const semantic = Type.option('char')
      if (!Type.isUnion(semantic))
        throw new RangeError('Canonical Option did not normalize to a structural union')
      const member = succeeded ? Type.some('char') : Type.none
      const entry = program.layout.entries.find((candidate) => Type.equals(candidate.type, member))
      if (entry?._tag !== 'LayoutEntry' || entry.representation._tag !== 'Aggregate')
        throw new RangeError('Target plan omitted a canonical callable Option member')
      return Object.freeze({
        _tag: 'Value',
        value: Object.freeze({
          _tag: 'UnionValue',
          type: semantic,
          member,
          payload: Object.freeze({
            _tag: 'AggregateValue',
            type: member,
            fields: Object.freeze(
              succeeded
                ? entry.representation.fields.map((field) =>
                    Object.freeze({ field: field.id, value: characterValue(Number(exact)) }),
                  )
                : [],
            ),
          }),
        }),
      })
    }
    if (Scalar.isCheckedOperation(operation)) {
      const source = Scalar.find(target.actor)
      const resultScalar = conversionTarget ?? source
      const leftValue = arguments_.at(0)
      const rightValue = arguments_.at(1)
      if (
        source?.category !== 'Integer' ||
        resultScalar?.category !== 'Integer' ||
        leftValue === undefined ||
        leftValue._tag !== 'IntegerValue'
      )
        throw new RangeError('MIR verifier allowed an invalid checked callable')
      const left = BigInt(leftValue.value)
      const right =
        rightValue !== undefined && rightValue._tag === 'IntegerValue'
          ? BigInt(rightValue.value)
          : undefined
      const exact = BootstrapArithmetic.checked(operation, left, right)
      const range = Scalar.range(resultScalar, program.layout.target.pointerSize === 4 ? 32 : 64)
      const succeeded = exact !== undefined && exact >= range.minimum && exact <= range.maximum
      const semantic = Type.option(resultScalar.spelling)
      if (!Type.isUnion(semantic))
        throw new RangeError('Canonical Option did not normalize to a structural union')
      const member = succeeded ? Type.some(resultScalar.spelling) : Type.none
      const entry = program.layout.entries.find((candidate) => Type.equals(candidate.type, member))
      if (entry?._tag !== 'LayoutEntry' || entry.representation._tag !== 'Aggregate')
        throw new RangeError('Target plan omitted a canonical callable Option member')
      return Object.freeze({
        _tag: 'Value',
        value: Object.freeze({
          _tag: 'UnionValue',
          type: semantic,
          member,
          payload: Object.freeze({
            _tag: 'AggregateValue',
            type: member,
            fields: Object.freeze(
              succeeded
                ? entry.representation.fields.map((field) =>
                    Object.freeze({
                      field: field.id,
                      value: integerValue(resultScalar.spelling, exact),
                    }),
                  )
                : [],
            ),
          }),
        }),
      })
    }
    if (conversionTarget !== undefined) {
      const subject = arguments_.at(0)
      if (actorScalar?.category === 'Character' && subject?._tag === 'CharacterValue')
        return Object.freeze({
          _tag: 'Value',
          value: integerValue(conversionTarget.spelling, BigInt(subject.value)),
        })
      if (subject === undefined || subject._tag !== 'IntegerValue')
        throw new RangeError('MIR verifier allowed a non-integer conversion argument')
      const exact = BigInt(subject.value)
      const range = Scalar.range(
        conversionTarget,
        program.layout.target.pointerSize === 4 ? 32 : 64,
      )
      if (exact < range.minimum || exact > range.maximum)
        return blockedStep({
          _tag: 'Trap',
          function: fn.id,
          reason: 'integer conversion out of range',
          span,
        })
      return Object.freeze({
        _tag: 'Value',
        value: integerValue(conversionTarget.spelling, exact),
      })
    }
    if (
      operation === 'Not' ||
      operation === 'Negate' ||
      operation === 'BitNot' ||
      operation === 'WrappingNegate' ||
      operation === 'SaturatingNegate'
    ) {
      const subject = arguments_.at(0)
      if (subject === undefined || subject._tag !== 'IntegerValue') {
        throw new RangeError('MIR verifier allowed a non-scalar unary callable argument')
      }
      if (operation === 'Not')
        return Object.freeze({
          _tag: 'Value',
          value: integerValue('i32', subject.value === 0n ? 1n : 0n),
        })
      const scalar = Scalar.find(target.actor)
      if (scalar === undefined || scalar.category !== 'Integer') {
        throw new RangeError('MIR verifier allowed a non-integer negate callable')
      }
      const pointerBits = program.layout.target.pointerSize === 4 ? 32 : 64
      const width = Scalar.bits(scalar, pointerBits)
      const raw = BigInt(subject.value)
      const exact =
        operation === 'BitNot'
          ? scalar.signedness === 'Signed'
            ? BigInt.asIntN(width, ~raw)
            : BigInt.asUintN(width, ~raw)
          : -raw
      const range = Scalar.range(scalar, pointerBits)
      if (operation === 'Negate' && (exact < range.minimum || exact > range.maximum)) {
        return blockedStep({ _tag: 'Trap', function: fn.id, reason: 'arithmetic overflow', span })
      }
      return Object.freeze({
        _tag: 'Value',
        value: integerValue(
          scalar.spelling,
          operation === 'WrappingNegate'
            ? scalar.signedness === 'Signed'
              ? BigInt.asIntN(width, exact)
              : BigInt.asUintN(width, exact)
            : operation === 'SaturatingNegate'
              ? exact > range.maximum
                ? range.maximum
                : exact < range.minimum
                  ? range.minimum
                  : exact
              : exact,
        ),
      })
    }
    if (Mir.isBinaryOperator(operation)) {
      const leftValue = arguments_.at(0)
      const rightValue = arguments_.at(1)
      const scalar = Scalar.find(target.actor)
      if (leftValue?._tag !== 'IntegerValue' || rightValue?._tag !== 'IntegerValue')
        throw new RangeError('MIR verifier allowed invalid integer binary callable arguments')
      const compared = BootstrapArithmetic.compare(operation, leftValue.value, rightValue.value)
      if (compared !== undefined)
        return Object.freeze({
          _tag: 'Value',
          value: integerValue('i32', compared ? 1n : 0n),
        })
      if (scalar?.category !== 'Integer')
        throw new RangeError('MIR verifier allowed a non-integer binary callable')
      const result = BootstrapArithmetic.integralBinary(
        operation,
        scalar,
        program.layout.target.pointerSize === 4 ? 32 : 64,
        leftValue.value,
        rightValue.value,
      )
      if (result._tag === 'Trap')
        return blockedStep({ _tag: 'Trap', function: fn.id, reason: result.reason, span })
      return Object.freeze({
        _tag: 'Value',
        value:
          result._tag === 'Comparison'
            ? integerValue('i32', result.value ? 1 : 0)
            : integerValue(result.type, result.value),
      })
    }
    return blockedStep({
      _tag: 'Trap',
      function: fn.id,
      reason: `bootstrap callable ${target.actor}.${target.operation} is unavailable`,
      span,
    })
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
    let effectiveSelectors = selectors
    let capturedIndexes: ReadonlyArray<number> = Object.freeze([])
    if (selected._tag === 'ReferenceValue') {
      const target = state.cells.get(cellKey(selected.frame, selected.cell))
      if (target === undefined)
        throw new RangeError('MIR reference points at a missing evaluator cell')
      effectiveSelectors = Object.freeze([...selected.selectors, ...selectors])
      capturedIndexes = selected.indexes
      selected = target.value
    }
    const resolved = BootstrapPlace.walkPlace(
      selected,
      effectiveSelectors,
      capturedIndexes,
      placeAccess,
    )
    if (resolved._tag === 'Resolved') return resolved
    return {
      _tag: 'Blocked',
      step: blockedStep({
        _tag: 'Trap',
        function: fn.id,
        reason:
          (resolved.selector._tag === 'SliceElementSelector' ? 'slice' : 'array') +
          ' index ' +
          String(resolved.index) +
          ' is outside length ' +
          String(resolved.length) +
          ' in ' +
          fn.id.module +
          '.' +
          fn.id.name,
        span: resolved.selector.provenance.span,
      }),
    }
  }

  const replacePlace = (
    current: Value,
    selectors: ReadonlyArray<Mir.PlaceSelector>,
    indexes: ReadonlyArray<number>,
    replacement: Value,
  ): Value =>
    BootstrapPlace.replacePlaceByIndexes(current, selectors, indexes, replacement, placeAccess)

  const replaceReferenced = (local: Mir.LocalId, replacement: Value): void => {
    const reference = read(local).value
    if (reference._tag !== 'ReferenceValue') {
      throw new RangeError('OS intrinsic output is not an exclusive reference')
    }
    const key = cellKey(reference.frame, reference.cell)
    const target = state.cells.get(key)
    if (target === undefined) throw new RangeError('OS intrinsic output references a missing cell')
    state.cells.set(key, {
      value: replacePlace(target.value, reference.selectors, reference.indexes, replacement),
      fromCall: target.fromCall,
    })
  }

  const bytesOfView = (viewed: SliceValue | StaticViewValue): ReadonlyArray<number> => {
    if (viewed._tag === 'StaticViewValue') return viewed.bytes
    if (viewed.ticket !== undefined) {
      const allocation = state.allocations.get(viewed.ticket)
      if (allocation === undefined || !allocation.active)
        throw new RangeError('OS intrinsic received released byte storage')
      return Object.freeze(
        Array.from({ length: viewed.length }, (_, index) => {
          const selected = allocation.values.get(String(viewed.base + index))
          if (selected?._tag !== 'IntegerValue' || selected.type !== 'u8')
            throw new RangeError('OS intrinsic received uninitialized byte storage')
          return Number(selected.value)
        }),
      )
    }
    const backing = cell(viewed).value
    if (backing._tag !== 'ArrayValue') throw new RangeError('OS byte slice lost its array')
    return Object.freeze(
      backing.elements.slice(viewed.base, viewed.base + viewed.length).map((selected) => {
        if (selected._tag !== 'IntegerValue' || selected.type !== 'u8')
          throw new RangeError('OS intrinsic received a non-byte slice')
        return Number(selected.value)
      }),
    )
  }

  const byteView = (local: Mir.LocalId): ReadonlyArray<number> => {
    const viewed = read(local).value
    if (viewed._tag !== 'SliceValue' && viewed._tag !== 'StaticViewValue')
      throw new RangeError('OS intrinsic expected a byte slice')
    return bytesOfView(viewed)
  }

  const stringView = (string: StringValue): SliceValue | StaticViewValue =>
    string.storage._tag === 'RuntimeSliceStorage'
      ? string.storage.view
      : Object.freeze({
          _tag: 'StaticViewValue',
          data: string.storage.data,
          bytes: string.storage.bytes,
          length: string.byteLength,
        })

  const stringBytes = (string: StringValue): ReadonlyArray<number> => {
    if (string.heldLoans.some((loan) => !state.activeLoans.has(loan))) {
      throw new RangeError('MIR string uses backing storage after its lexical loan ended')
    }
    const bytes = bytesOfView(stringView(string))
    if (bytes.length !== string.byteLength) {
      throw new RangeError('MIR string byte length disagrees with its backing storage')
    }
    return bytes
  }

  const writeByteView = (local: Mir.LocalId, bytes: ReadonlyArray<number>): void => {
    const viewed = read(local).value
    if (viewed._tag !== 'SliceValue' || bytes.length > viewed.length)
      throw new RangeError('OS intrinsic output exceeds its byte slice')
    if (viewed.ticket !== undefined) {
      const allocation = state.allocations.get(viewed.ticket)
      if (allocation === undefined || !allocation.active)
        throw new RangeError('OS intrinsic output uses released storage')
      for (const [index, byte] of bytes.entries()) {
        allocation.values.set(String(viewed.base + index), integerValue('u8', BigInt(byte)))
      }
      return
    }
    const key = cellKey(viewed.frame, viewed.cell)
    const backing = state.cells.get(key)
    if (backing?.value._tag !== 'ArrayValue') throw new RangeError('OS output slice lost its array')
    const next: ArrayValue = Object.freeze({
      _tag: 'ArrayValue',
      type: backing.value.type,
      elements: Object.freeze(
        backing.value.elements.map((element, index) => {
          const byte = bytes.at(index - viewed.base)
          return byte === undefined ? element : integerValue('u8', BigInt(byte))
        }),
      ),
    })
    state.cells.set(key, { value: next, fromCall: backing.fromCall })
  }

  const optionValue = (element: Type.Type, payload?: Value): UnionValue => {
    const semantic = Type.option(element)
    if (!Type.isUnion(semantic)) throw new RangeError('OS Option result did not normalize')
    const member = payload === undefined ? Type.none : Type.some(element)
    const entry = program.layout.entries.find((candidate) => Type.equals(candidate.type, member))
    if (entry?._tag !== 'LayoutEntry' || entry.representation._tag !== 'Aggregate')
      throw new RangeError('Target plan omitted an OS Option member')
    return Object.freeze({
      _tag: 'UnionValue',
      type: semantic,
      member,
      payload: Object.freeze({
        _tag: 'AggregateValue',
        type: member,
        fields: Object.freeze(
          payload === undefined
            ? []
            : entry.representation.fields.map((field) =>
                Object.freeze({ field: field.id, value: payload }),
              ),
        ),
      }),
    })
  }

  const handleValue = (handle: OsFileSystemHost.Handle): AggregateValue => {
    const entry = program.layout.entries.find((candidate) =>
      Type.equals(candidate.type, Type.osHandle),
    )
    if (entry?._tag !== 'LayoutEntry' || entry.representation._tag !== 'Aggregate')
      throw new RangeError('Target plan omitted OsHandle')
    return Object.freeze({
      _tag: 'AggregateValue',
      type: Type.osHandle,
      fields: Object.freeze(
        entry.representation.fields.map((field) =>
          Object.freeze({
            field: field.id,
            value:
              field.name === '$identity'
                ? integerValue('usize', BigInt(handle.identity))
                : integerValue(
                    'i32',
                    field.name === '$kind' ? (handle.kind === 'File' ? 0 : 1) : 1,
                  ),
          }),
        ),
      ),
    })
  }

  const hostHandle = (local: Mir.LocalId): OsFileSystemHost.Handle => {
    const selected =
      read(local).value._tag === 'ReferenceValue' ? referenced(local).value : read(local).value
    if (selected._tag !== 'AggregateValue' || !Type.equals(selected.type, Type.osHandle))
      throw new RangeError('OS intrinsic expected OsHandle')
    const identity = selected.fields.at(0)?.value
    const kind = selected.fields.at(1)?.value
    const active = selected.fields.at(2)?.value
    if (
      identity?._tag !== 'IntegerValue' ||
      identity.type !== 'usize' ||
      kind?._tag !== 'IntegerValue' ||
      kind.type !== 'i32' ||
      active?._tag !== 'IntegerValue' ||
      active.type !== 'i32' ||
      active.value !== 1n
    )
      throw new RangeError('OS intrinsic expected one live OsHandle')
    return Object.freeze({
      identity: Number(identity.value),
      kind: kind.value === 0n ? 'File' : 'Directory',
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
      const taken = readInteger(region.condition, 'i32').value !== 0n
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

    const executeOperations = function* (
      operations: ReadonlyArray<Mir.Operation>,
    ): OperationsExecution {
      for (const operation of operations) {
        if (state.steps >= state.maxSteps) {
          return evaluationLimit('Steps', state.maxSteps, state.steps, operation.provenance.span)
        }
        state.steps += 1
        switch (operation._tag) {
          case 'ShortCircuit': {
            const decided = readInteger(operation.left, 'i32').value !== 0n
            // `&&` decides on a false left operand, `||` on a true one. Only the undecided case
            // executes the nested right-operand operations at all.
            if (decided === (operation.operator === 'Or')) {
              write(operation.destination, {
                value: integerValue('i32', decided ? 1 : 0),
                fromCall: false,
              })
              break
            }
            const rightStep = yield* executeOperations(operation.right.operations)
            if (rightStep !== undefined) return rightStep
            write(operation.destination, read(operation.right.result))
            break
          }
          case 'Match': {
            const scrutinee = read(operation.scrutinee).value
            const activeMember =
              scrutinee._tag === 'UnionValue'
                ? scrutinee.member
                : scrutinee._tag === 'AggregateValue'
                  ? scrutinee.type
                  : Mir.semanticType(operation.scrutineeType)
            const payload =
              scrutinee._tag === 'UnionValue'
                ? scrutinee.payload
                : scrutinee._tag === 'AggregateValue'
                  ? scrutinee
                  : scrutinee
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
                const bound = BootstrapStorage.selectFieldPath(payload, binding.path)
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
                const guardStep = yield* executeOperations(arm.guard.operations)
                if (guardStep !== undefined) return guardStep
                if (readInteger(arm.guard.result, 'i32').value === 0n) continue
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
              const selectedStep = yield* executeOperations(arm.selected.operations)
              if (selectedStep !== undefined) return selectedStep
              for (const cleanup of arm.selected.cleanup) {
                const owner = BootstrapStorage.selectFieldPath(payload, cleanup.path)
                const members = BootstrapStorage.cleanupMembers(cleanup.cleanup, owner)
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
            {
              const semantic = Mir.semanticType(operation.type)
              const integer = typeof semantic === 'string' && Scalar.isIntegerSpelling(semantic)
              const floating = typeof semantic === 'string' && Scalar.isFloatSpelling(semantic)
              const character = typeof semantic === 'string' && Scalar.isCharacterSpelling(semantic)
              write(operation.destination, {
                value: floating
                  ? floatValue(semantic, BigInt(operation.value))
                  : integer
                    ? integerValue(semantic, BigInt(operation.value))
                    : character
                      ? characterValue(Number(operation.value))
                      : integerValue('i32', Number(operation.value)),
                fromCall: false,
              })
            }
            break
          case 'StaticView': {
            const data = program.staticData?.find((candidate) => candidate.id === operation.data)
            if (data === undefined || data.bytes.length !== operation.length) {
              throw new RangeError('MIR verifier allowed a missing or mismatched static-data view')
            }
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'StaticViewValue',
                data: data.id,
                bytes: data.bytes,
                length: data.bytes.length,
              }),
              fromCall: false,
            })
            break
          }
          case 'StaticString': {
            const data = program.staticData?.find((candidate) => candidate.id === operation.data)
            if (data?.kind !== 'Text' || !data.utf8 || data.bytes.length !== operation.byteLength) {
              throw new RangeError('MIR verifier allowed a missing or mismatched static string')
            }
            const string: StringValue = Object.freeze({
              _tag: 'StringValue',
              storage: Object.freeze({
                _tag: 'StaticTextStorage',
                data: data.id,
                bytes: data.bytes,
              }),
              bytes: data.bytes,
              byteLength: data.bytes.length,
              heldLoans: Object.freeze([]),
            })
            write(operation.destination, { value: string, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'StringStatic',
                function: fn.id,
                storage: string.storage._tag,
                byteLength: string.byteLength,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'StringFromUtf8Unchecked': {
            const bytes = read(operation.bytes).value
            if (bytes._tag !== 'SliceValue' && bytes._tag !== 'StaticViewValue') {
              throw new RangeError('MIR verifier allowed string formation from a non-byte view')
            }
            const heldLoans = Object.freeze(operation.heldLoans.map(borrowKey))
            if (heldLoans.some((loan) => !state.activeLoans.has(loan))) {
              throw new RangeError('MIR string formation lost its active backing loan')
            }
            const storage: StringStorage =
              bytes._tag === 'StaticViewValue'
                ? Object.freeze({
                    _tag: 'StaticByteStorage',
                    data: bytes.data,
                    bytes: bytes.bytes,
                  })
                : Object.freeze({ _tag: 'RuntimeSliceStorage', view: bytes })
            for (const loan of heldLoans) state.stringLoans.add(loan)
            const string: StringValue = Object.freeze({
              _tag: 'StringValue',
              storage,
              bytes: bytesOfView(bytes),
              byteLength: bytes.length,
              heldLoans,
            })
            write(operation.destination, { value: string, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'StringRuntime',
                function: fn.id,
                storage: string.storage._tag,
                byteLength: string.byteLength,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'StringUtf8Bytes': {
            const string = read(operation.string).value
            if (string._tag !== 'StringValue')
              throw new RangeError('MIR verifier allowed byte viewing of a non-string')
            stringBytes(string)
            write(operation.destination, { value: stringView(string), fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'StringBytes',
                function: fn.id,
                storage: string.storage._tag,
                byteLength: string.byteLength,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'StringByteLength': {
            const string = read(operation.string).value
            if (string._tag !== 'StringValue')
              throw new RangeError('MIR verifier allowed byte length of a non-string')
            stringBytes(string)
            write(operation.destination, {
              value: integerValue('usize', BigInt(string.byteLength)),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'StringByteLength',
                function: fn.id,
                storage: string.storage._tag,
                byteLength: string.byteLength,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'StringEqualsExact': {
            const left = read(operation.left).value
            const right = read(operation.right).value
            if (left._tag !== 'StringValue' || right._tag !== 'StringValue')
              throw new RangeError('MIR verifier allowed exact equality of non-string values')
            const leftBytes = stringBytes(left)
            const rightBytes = stringBytes(right)
            const equal =
              leftBytes.length === rightBytes.length &&
              leftBytes.every((byte, index) => byte === rightBytes.at(index))
            const result = operation.negated ? !equal : equal
            write(operation.destination, {
              value: integerValue('i32', result ? 1 : 0),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'StringEqualsExact',
                function: fn.id,
                result,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'Move':
            write(operation.destination, read(operation.source))
            break
          case 'BeginLoan': {
            state.activeLoans.add(borrowKey(operation.borrow))
            const source = read(operation.root)
            if (operation.type._tag === 'Reference') {
              const capturedIndexes = Object.freeze(
                operation.selectors.map((selector) => {
                  if (selector._tag === 'FieldSelector') return selector.field.ordinal
                  if (selector._tag === 'SliceElementSelector')
                    return Number(readInteger(selector.index, 'usize').value)
                  return selector.index._tag === 'Proven'
                    ? selector.index.value
                    : Number(readInteger(selector.index.local, 'usize').value)
                }),
              )
              const inherited =
                source.value._tag === 'ReferenceValue'
                  ? source.value
                  : Object.freeze({
                      _tag: 'ReferenceValue' as const,
                      frame,
                      cell: operation.root.ordinal,
                      selectors: Object.freeze([]),
                      indexes: Object.freeze([]),
                    })
              const key = cellKey(inherited.frame, inherited.cell)
              if (!state.cells.has(key)) state.cells.set(key, source)
              write(operation.destination, {
                value: Object.freeze({
                  _tag: 'ReferenceValue' as const,
                  frame: inherited.frame,
                  cell: inherited.cell,
                  selectors: Object.freeze([...inherited.selectors, ...operation.selectors]),
                  indexes: Object.freeze([...inherited.indexes, ...capturedIndexes]),
                }),
                fromCall: source.fromCall,
              })
              break
            }
            if (operation.sourceType._tag === 'Nominal') {
              throw new RangeError('MIR verifier allowed a nominal slice root')
            }
            const slice =
              operation.sourceType._tag === 'Slice'
                ? source.value
                : (() => {
                    if (operation.sourceType._tag !== 'FixedArray') {
                      throw new RangeError('MIR verifier allowed a non-array slice root')
                    }
                    const resolved = resolvePlace(operation.root, operation.selectors)
                    if (resolved._tag === 'Blocked') return resolved
                    if (resolved.selected._tag !== 'ArrayValue')
                      throw new RangeError('MIR verifier allowed borrowing a non-array place')
                    const key = cellKey(frame, operation.root.ordinal)
                    if (!state.cells.has(key)) state.cells.set(key, source)
                    return Object.freeze({
                      _tag: 'SliceValue' as const,
                      frame,
                      cell: operation.root.ordinal,
                      base: 0,
                      length: operation.sourceType.type.length,
                      selectors: Object.freeze(
                        operation.selectors.filter(
                          (
                            selector,
                          ): selector is Extract<
                            Mir.PlaceSelector,
                            { readonly _tag: 'FieldSelector' | 'ElementSelector' }
                          > => selector._tag !== 'SliceElementSelector',
                        ),
                      ),
                      indexes: resolved.indexes,
                    })
                  })()
            if (slice._tag === 'Blocked') return slice.step
            if (slice._tag !== 'SliceValue') {
              throw new RangeError('MIR verifier allowed reborrowing a non-slice value')
            }
            write(operation.destination, { value: slice, fromCall: source.fromCall })
            break
          }
          case 'EndLoan': {
            const loan = borrowKey(operation.borrow)
            state.activeLoans.delete(loan)
            if (state.stringLoans.delete(loan)) {
              trace.push(
                Object.freeze({
                  _tag: 'StringLoanEnd',
                  function: fn.id,
                  loan,
                  span: operation.provenance.span,
                }),
              )
            }
            break
          }
          case 'SliceLength': {
            const slice = read(operation.slice).value
            if (slice._tag !== 'SliceValue' && slice._tag !== 'StaticViewValue') {
              throw new RangeError('MIR verifier allowed slice length on a non-slice value')
            }
            write(operation.destination, {
              value: integerValue('usize', BigInt(slice.length)),
              fromCall: false,
            })
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
              operation.conversion === 'Inject'
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
                source: Mir.semanticType(operation.sourceType),
                target: operation.targetType.type,
                member: converted.member,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'ValidateLayout': {
            const bytes = readInteger(operation.bytes, 'usize')
            const alignment = readInteger(operation.alignment, 'usize')
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
            const count = readInteger(operation.count, 'usize')
            if (layout._tag !== 'AggregateValue') {
              throw new RangeError('MIR verifier allowed invalid repeated-layout operands')
            }
            const entry = program.layout.entries.find((candidate) =>
              Type.equals(candidate.type, Type.layout),
            )
            if (entry?._tag !== 'LayoutEntry' || entry.representation._tag !== 'Aggregate') {
              throw new RangeError('Target plan omitted Layout')
            }
            const representation = entry.representation
            const fieldValue = (name: string): IntegerValue | undefined => {
              const field = representation.fields.find((candidate) => candidate.name === name)
              const value = layout.fields.find(
                (candidate) => candidate.field.ordinal === field?.id.ordinal,
              )?.value
              return value?._tag === 'IntegerValue' && value.type === 'usize' ? value : undefined
            }
            const bytes = fieldValue('bytes')
            const alignment = fieldValue('alignment')
            if (bytes === undefined || alignment === undefined) {
              throw new RangeError('Layout payload omitted bytes or alignment')
            }
            const maximum =
              program.layout.target.pointerSize === 4 ? 4294967295n : 18446744073709551615n
            const stride =
              alignment.value === 0n
                ? 0n
                : ((bytes.value + alignment.value - 1n) / alignment.value) * alignment.value
            const overflow = count.value !== 0n && stride > maximum / count.value
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
            const total = integerValue('usize', overflow ? 0n : stride * count.value)
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
            const fieldValue = (name: string): IntegerValue | undefined => {
              const field = representation.fields.find((candidate) => candidate.name === name)
              const found = layout.fields.find(
                (candidate) => candidate.field.ordinal === field?.id.ordinal,
              )?.value
              return found?._tag === 'IntegerValue' && found.type === 'usize' ? found : undefined
            }
            const bytes = fieldValue('bytes')
            const alignment = fieldValue('alignment')
            if (bytes === undefined || alignment === undefined)
              throw new RangeError('Layout payload omitted bytes or alignment')
            const ticket = state.nextAllocation
            state.nextAllocation += 1
            BootstrapStorage.allocate(state.allocations, ticket)
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
            trace.push(
              Object.freeze({
                _tag: 'AllocationAcquire',
                function: fn.id,
                ticket,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'HostWrite':
          case 'OsCall': {
            const boundary = BootstrapOsIntrinsics.execute(
              {
                state,
                fn,
                trace,
                read,
                write,
                cell,
                readInteger,
                replaceReferenced,
                byteView,
                writeByteView,
                optionValue,
                handleValue,
                hostHandle,
              },
              operation,
            )
            if (boundary !== undefined) return boundary
            break
          }
          case 'RawBufferFrom': {
            const allocation = read(operation.allocation).value
            const count = readInteger(operation.count, 'usize')
            if (allocation._tag !== 'AllocationValue') {
              throw new RangeError('MIR verifier allowed invalid RawBuffer construction operands')
            }
            const expectedBytes = BigInt(operation.stride) * count.value
            if (
              allocation.bytes !== expectedBytes ||
              allocation.alignment !== BigInt(operation.elementAlignment)
            ) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer allocation layout does not match its element type and count',
                span: operation.provenance.span,
              })
            }
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'RawBufferValue',
                type: operation.type.type,
                ticket: allocation.ticket,
                count: count.value,
                element: operation.element,
                stride: operation.stride,
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'RawBufferForm',
                function: fn.id,
                ticket: allocation.ticket,
                count: count.value,
                element: operation.element,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'SharedFromAllocation': {
            const allocation = read(operation.allocation).value
            const value = read(operation.value).value
            if (allocation._tag !== 'AllocationValue')
              throw new RangeError('MIR verifier allowed invalid local-shared allocation')
            if (
              allocation.bytes !== BigInt(operation.block.size) ||
              allocation.alignment !== BigInt(operation.block.alignment) ||
              !BootstrapStorage.initializeShared(
                state.allocations,
                allocation.ticket,
                operation.element,
                operation.block.provenance,
                value,
              )
            )
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'Local-shared allocation provenance or initializedness mismatch',
                span: operation.provenance.span,
              })
            const core: SharedCoreValue = Object.freeze({
              _tag: 'SharedCoreValue',
              type: operation.type.type,
              ticket: allocation.ticket,
              element: operation.element,
            })
            write(operation.destination, { value: core, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'SharedInitialize',
                function: fn.id,
                ticket: allocation.ticket,
                element: operation.element,
                strong: 1n,
                access: 'Available',
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'RawBufferCount': {
            const buffer = referenced(operation.buffer).value
            if (buffer._tag !== 'RawBufferValue') {
              throw new RangeError('MIR verifier allowed RawBuffer.count on another value')
            }
            write(operation.destination, {
              value: integerValue('usize', buffer.count),
              fromCall: false,
            })
            break
          }
          case 'RawBufferView': {
            const buffer = referenced(operation.buffer).value
            const offset = readInteger(operation.offset, 'usize').value
            const length = readInteger(operation.length, 'usize').value
            if (buffer._tag !== 'RawBufferValue') {
              throw new RangeError('MIR verifier allowed RawBuffer.view on another value')
            }
            if (
              offset > buffer.count ||
              length > buffer.count - offset ||
              offset > BigInt(Number.MAX_SAFE_INTEGER) ||
              length > BigInt(Number.MAX_SAFE_INTEGER)
            ) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer view range is out of bounds',
                span: operation.provenance.span,
              })
            }
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'SliceValue',
                frame: -1,
                cell: -1,
                base: Number(offset),
                length: Number(length),
                ticket: buffer.ticket,
              }),
              fromCall: false,
            })
            break
          }
          case 'RawBufferRead': {
            const buffer = referenced(operation.buffer).value
            const index = readInteger(operation.index, 'usize')
            if (buffer._tag !== 'RawBufferValue') {
              throw new RangeError('MIR verifier allowed invalid RawBuffer.read operands')
            }
            if (index.value >= buffer.count) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer read index is out of bounds',
                span: operation.provenance.span,
              })
            }
            if (!Type.equals(buffer.element, operation.element)) {
              throw new RangeError('MIR verifier allowed mismatched RawBuffer read provenance')
            }
            const selected = BootstrapStorage.read(state.allocations, buffer.ticket, index.value)
            if (selected === undefined) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer.read requires live initialized storage',
                span: operation.provenance.span,
              })
            }
            write(operation.destination, { value: selected, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'RawBufferRead',
                function: fn.id,
                ticket: buffer.ticket,
                index: index.value,
                element: operation.element,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'RawBufferSlot': {
            const buffer = referenced(operation.buffer).value
            const index = readInteger(operation.index, 'usize')
            if (buffer._tag !== 'RawBufferValue') {
              throw new RangeError('MIR verifier allowed invalid RawBuffer.slot operands')
            }
            if (index.value >= buffer.count) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer slot index is out of bounds',
                span: operation.provenance.span,
              })
            }
            if (!Type.equals(buffer.element, operation.element)) {
              throw new RangeError('MIR verifier allowed mismatched RawBuffer element provenance')
            }
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'SlotValue',
                type: operation.type.type,
                ticket: buffer.ticket,
                index: index.value,
                element: operation.element,
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'SlotProject',
                function: fn.id,
                ticket: buffer.ticket,
                index: index.value,
                element: operation.element,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'RawBufferCopy': {
            const buffer = referenced(operation.buffer).value
            const offset = readInteger(operation.offset, 'usize').value
            const length = readInteger(operation.length, 'usize').value
            const source = read(operation.source).value
            if (
              buffer._tag !== 'RawBufferValue' ||
              (source._tag !== 'SliceValue' && source._tag !== 'StaticViewValue')
            ) {
              throw new RangeError('MIR verifier allowed invalid RawBuffer.copy operands')
            }
            if (!Type.equals(buffer.element, operation.element)) {
              throw new RangeError('MIR verifier allowed mismatched RawBuffer copy provenance')
            }
            if (
              length > BigInt(source.length) ||
              offset > buffer.count ||
              length > buffer.count - offset ||
              offset > BigInt(Number.MAX_SAFE_INTEGER) ||
              length > BigInt(Number.MAX_SAFE_INTEGER)
            ) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer copy range is out of bounds',
                span: operation.provenance.span,
              })
            }
            const destination = state.allocations.get(buffer.ticket)
            if (destination === undefined || !destination.active) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer.copy requires live destination storage',
                span: operation.provenance.span,
              })
            }
            const count = Number(length)
            // A source range is raw storage, a borrowed array cell, or immutable static data.
            const sourceStorage =
              source._tag === 'StaticViewValue' || source.ticket === undefined
                ? undefined
                : state.allocations.get(source.ticket)
            const backing =
              source._tag === 'SliceValue' && source.ticket === undefined
                ? cell(source).value
                : undefined
            if (
              source._tag === 'SliceValue' &&
              source.ticket !== undefined &&
              (sourceStorage === undefined || !sourceStorage.active)
            ) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer.copy requires live initialized source storage',
                span: operation.provenance.span,
              })
            }
            if (backing !== undefined && backing._tag !== 'ArrayValue') {
              throw new RangeError('MIR RawBuffer.copy source slice lost its array')
            }
            // Reading the whole range before any write makes an overlapping source and
            // destination behave as if the elements travelled through an intermediate buffer.
            const moved: Array<Value> = []
            for (let index = 0; index < count; index += 1) {
              const selected =
                source._tag === 'StaticViewValue'
                  ? (() => {
                      const byte = source.bytes.at(index)
                      return byte === undefined ? undefined : integerValue('u8', BigInt(byte))
                    })()
                  : sourceStorage === undefined
                    ? backing?._tag === 'ArrayValue'
                      ? backing.elements.at(source.base + index)
                      : undefined
                    : sourceStorage.values.get(String(source.base + index))
              if (selected === undefined) {
                return blockedStep({
                  _tag: 'Trap',
                  function: fn.id,
                  reason: 'RawBuffer.copy requires live initialized source storage',
                  span: operation.provenance.span,
                })
              }
              moved.push(selected)
            }
            // A move of a Copy element leaves the source readable, which is what the byte-level
            // backends do for every element type. Only raw storage tracks per-slot initialization,
            // so only a raw-storage-backed source range gives its slots up.
            if (
              !operation.retainsSource &&
              sourceStorage !== undefined &&
              source._tag === 'SliceValue'
            ) {
              for (let index = 0; index < count; index += 1) {
                sourceStorage.values.delete(String(source.base + index))
              }
            }
            for (const [index, selected] of moved.entries()) {
              destination.values.set(String(offset + BigInt(index)), selected)
            }
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'AggregateValue',
                type: Type.unit,
                fields: Object.freeze([]),
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'RawBufferCopy',
                function: fn.id,
                ticket: buffer.ticket,
                index: offset,
                count: length,
                element: operation.element,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'RawBufferFill': {
            const buffer = referenced(operation.buffer).value
            const offset = readInteger(operation.offset, 'usize').value
            const length = readInteger(operation.length, 'usize').value
            const value = readInteger(operation.value)
            if (buffer._tag !== 'RawBufferValue') {
              throw new RangeError('MIR verifier allowed invalid RawBuffer.fill operands')
            }
            if (
              offset > buffer.count ||
              length > buffer.count - offset ||
              offset > BigInt(Number.MAX_SAFE_INTEGER) ||
              length > BigInt(Number.MAX_SAFE_INTEGER)
            ) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer fill range is out of bounds',
                span: operation.provenance.span,
              })
            }
            if (
              !BootstrapStorage.fill(
                state.allocations,
                buffer.ticket,
                offset,
                Number(length),
                integerValue('u8', value.value),
              )
            ) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer.fill requires live storage',
                span: operation.provenance.span,
              })
            }
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'AggregateValue',
                type: Type.unit,
                fields: Object.freeze([]),
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'RawBufferFill',
                function: fn.id,
                ticket: buffer.ticket,
                index: offset,
                count: length,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'SlotWrite': {
            const slot = read(operation.slot).value
            if (slot._tag !== 'SlotValue' || !Type.equals(slot.element, operation.element)) {
              throw new RangeError('MIR verifier allowed Slot.write with mismatched provenance')
            }
            if (
              !BootstrapStorage.write(
                state.allocations,
                slot.ticket,
                slot.index,
                read(operation.value).value,
              )
            ) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'Slot.write requires live uninitialized storage',
                span: operation.provenance.span,
              })
            }
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'AggregateValue',
                type: Type.unit,
                fields: Object.freeze([]),
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'SlotWrite',
                function: fn.id,
                ticket: slot.ticket,
                index: slot.index,
                element: operation.element,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'SlotTake': {
            const slot = read(operation.slot).value
            if (slot._tag !== 'SlotValue' || !Type.equals(slot.element, operation.element)) {
              throw new RangeError('MIR verifier allowed Slot.take with mismatched provenance')
            }
            const selected = BootstrapStorage.take(state.allocations, slot.ticket, slot.index)
            if (selected === undefined) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'Slot.take requires live initialized storage',
                span: operation.provenance.span,
              })
            }
            write(operation.destination, { value: selected, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'SlotTake',
                function: fn.id,
                ticket: slot.ticket,
                index: slot.index,
                element: operation.element,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'SlotCopy': {
            const slot = read(operation.slot).value
            if (slot._tag !== 'SlotValue' || !Type.equals(slot.element, operation.element)) {
              throw new RangeError('MIR verifier allowed Slot.copy with mismatched provenance')
            }
            const selected = BootstrapStorage.read(state.allocations, slot.ticket, slot.index)
            if (selected === undefined) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'Slot.copy requires live initialized storage',
                span: operation.provenance.span,
              })
            }
            write(operation.destination, { value: selected, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'SlotCopy',
                function: fn.id,
                ticket: slot.ticket,
                index: slot.index,
                element: operation.element,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'SlotDrop': {
            const slot = read(operation.slot).value
            if (slot._tag !== 'SlotValue' || !Type.equals(slot.element, operation.element)) {
              throw new RangeError('MIR verifier allowed Slot.drop with mismatched provenance')
            }
            const selected = BootstrapStorage.read(state.allocations, slot.ticket, slot.index)
            if (selected === undefined) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'Slot.drop requires live initialized storage',
                span: operation.provenance.span,
              })
            }
            const blocked = yield* releaseThroughPlan(
              operation.cleanup,
              selected,
              operation.provenance,
              operation.slot.ordinal,
            )
            if (blocked !== undefined) return blocked
            if (!BootstrapStorage.drop(state.allocations, slot.ticket, slot.index))
              throw new RangeError('evaluator slot disappeared during cleanup')
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'AggregateValue',
                type: Type.unit,
                fields: Object.freeze([]),
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'SlotDrop',
                function: fn.id,
                ticket: slot.ticket,
                index: slot.index,
                element: operation.element,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'Binary': {
            const leftType = fn.localTypes.at(operation.left.ordinal)
            const semantic = leftType === undefined ? undefined : Mir.semanticType(leftType)
            if (Scalar.isFloatSpelling(semantic)) {
              write(operation.destination, {
                value: floatingBinary(
                  operation.operator,
                  readFloat(operation.left),
                  readFloat(operation.right),
                ),
                fromCall: false,
              })
              break
            }
            if (Scalar.isCharacterSpelling(semantic)) {
              const left = readCharacter(operation.left).value
              const right = readCharacter(operation.right).value
              const result =
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
                          : operation.operator === 'GreaterOrEqual'
                            ? left >= right
                            : undefined
              if (result === undefined)
                throw new RangeError('MIR verifier allowed a non-comparison char operation')
              write(operation.destination, {
                value: integerValue('i32', result ? 1 : 0),
                fromCall: false,
              })
              break
            }
            const rightType = fn.localTypes.at(operation.right.ordinal)
            const operand = leftType === undefined ? undefined : Mir.semanticType(leftType)
            const scalar = Scalar.isSpelling(operand) ? Scalar.find(operand) : undefined
            if (
              rightType === undefined ||
              operand === undefined ||
              !Type.equals(Mir.semanticType(rightType), operand) ||
              !Scalar.isSpelling(operand)
            )
              throw new RangeError('MIR verifier allowed mixed integer operands')
            const left = readInteger(operation.left).value
            const right = readInteger(operation.right).value
            const compared = BootstrapArithmetic.compare(operation.operator, left, right)
            if (compared !== undefined) {
              write(operation.destination, {
                value: integerValue('i32', compared ? 1n : 0n),
                fromCall: false,
              })
              break
            }
            if (scalar?.category !== 'Integer')
              throw new RangeError('MIR verifier allowed a non-integer binary operand')
            const result = BootstrapArithmetic.integralBinary(
              operation.operator,
              scalar,
              program.layout.target.pointerSize === 4 ? 32 : 64,
              left,
              right,
            )
            if (result._tag === 'Trap')
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: result.reason,
                span: operation.provenance.span,
              })
            write(operation.destination, {
              value:
                result._tag === 'Comparison'
                  ? integerValue('i32', result.value ? 1 : 0)
                  : integerValue(result.type, result.value),
              fromCall: false,
            })
            break
          }
          case 'ConvertInteger': {
            const subject = readInteger(operation.source)
            const target = Scalar.find(operation.type._tag)
            if (target === undefined || target.category !== 'Integer')
              throw new RangeError('MIR verifier allowed a non-integer conversion target')
            const exact = BigInt(subject.value)
            const range = Scalar.range(target, program.layout.target.pointerSize === 4 ? 32 : 64)
            if (exact < range.minimum || exact > range.maximum)
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'integer conversion out of range',
                span: operation.provenance.span,
              })
            write(operation.destination, {
              value: integerValue(target.spelling, exact),
              fromCall: false,
            })
            break
          }
          case 'ConvertScalar': {
            const sourceType = Scalar.find(operation.sourceType._tag)
            const targetType = Scalar.find(operation.type._tag)
            if (sourceType?.category === 'Character' && targetType?.spelling === 'u32') {
              write(operation.destination, {
                value: integerValue('u32', BigInt(readCharacter(operation.source).value)),
                fromCall: false,
              })
              break
            }
            if (sourceType?.category === 'Floating' && targetType?.category === 'Floating') {
              const source = readFloat(operation.source)
              const encoded = FloatingPoint.fromNumber(
                FloatingPoint.toNumber(floatingBits(source)),
                targetType.spelling === 'f32' ? 32 : 64,
              )
              write(operation.destination, {
                value: floatValue(targetType.spelling, encoded.bits),
                fromCall: false,
              })
              break
            }
            if (sourceType?.category === 'Floating' && targetType?.category === 'Integer') {
              const number = FloatingPoint.toNumber(floatingBits(readFloat(operation.source)))
              const exact = Number.isFinite(number) ? BigInt(Math.trunc(number)) : undefined
              const range = Scalar.range(
                targetType,
                program.layout.target.pointerSize === 4 ? 32 : 64,
              )
              if (exact === undefined || exact < range.minimum || exact > range.maximum)
                return blockedStep({
                  _tag: 'Trap',
                  function: fn.id,
                  reason: 'float conversion out of range',
                  span: operation.provenance.span,
                })
              write(operation.destination, {
                value: integerValue(targetType.spelling, exact),
                fromCall: false,
              })
              break
            }
            if (sourceType?.category === 'Integer' && targetType?.category === 'Floating') {
              const encoded = FloatingPoint.fromNumber(
                Number(BigInt(readInteger(operation.source).value)),
                targetType.spelling === 'f32' ? 32 : 64,
              )
              write(operation.destination, {
                value: floatValue(targetType.spelling, encoded.bits),
                fromCall: false,
              })
              break
            }
            throw new RangeError('MIR verifier allowed an invalid scalar conversion')
          }
          case 'ReinterpretScalar': {
            const target = Scalar.find(operation.type._tag)
            const subject = read(operation.source).value
            if (target?.category === 'Floating') {
              if (subject._tag !== 'IntegerValue')
                throw new RangeError('MIR verifier allowed invalid float reinterpretation')
              write(operation.destination, {
                value: floatValue(target.spelling, BigInt(subject.value)),
                fromCall: false,
              })
            } else if (target?.category === 'Integer' && subject._tag === 'FloatValue') {
              write(operation.destination, {
                value: integerValue(target.spelling, subject.bits),
                fromCall: false,
              })
            } else throw new RangeError('MIR verifier allowed invalid scalar reinterpretation')
            break
          }
          case 'FloatUnary':
            write(operation.destination, {
              value: floatingUnary(operation.operation, readFloat(operation.source)),
              fromCall: false,
            })
            break
          case 'FloatTranscendental': {
            const source = readFloat(operation.source)
            const result = Transcendental.evaluate(operation.operation, floatingBits(source))
            write(operation.destination, {
              value: floatValue(source.type, result.bits),
              fromCall: false,
            })
            break
          }
          case 'CheckedScalar': {
            const operands = operation.operands.map((operand) => BigInt(readInteger(operand).value))
            const left = operands.at(0)
            const right = operands.at(1)
            const source = Scalar.find(operation.sourceType._tag)
            const target = Scalar.find(operation.valueType._tag)
            const characterConversion =
              operation.operation === 'CheckedConvertToChar' &&
              source?.spelling === 'u32' &&
              target?.category === 'Character'
            if (
              left === undefined ||
              source?.category !== 'Integer' ||
              (target?.category !== 'Integer' && !characterConversion)
            )
              throw new RangeError('MIR verifier allowed an invalid checked scalar operation')
            const arithmetic = BootstrapArithmetic.checked(operation.operation, left, right)
            const success =
              arithmetic !== undefined &&
              (characterConversion
                ? Scalar.isUnicodeScalarValue(arithmetic)
                : target?.category === 'Integer' &&
                  (() => {
                    const range = Scalar.range(
                      target,
                      program.layout.target.pointerSize === 4 ? 32 : 64,
                    )
                    return arithmetic >= range.minimum && arithmetic <= range.maximum
                  })())
            const member = success ? operation.success : operation.failure
            const entry = program.layout.entries.find((candidate) =>
              Type.equals(candidate.type, member),
            )
            if (entry?._tag !== 'LayoutEntry' || entry.representation._tag !== 'Aggregate')
              throw new RangeError('Target plan omitted a canonical Option member')
            const payload: AggregateValue = Object.freeze({
              _tag: 'AggregateValue',
              type: member,
              fields: Object.freeze(
                success
                  ? entry.representation.fields.map((field) =>
                      Object.freeze({
                        field: field.id,
                        value:
                          target.category === 'Character'
                            ? characterValue(Number(arithmetic))
                            : integerValue(target.spelling, arithmetic),
                      }),
                    )
                  : [],
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
            let effectiveSelectors = operation.selectors
            let capturedIndexes: ReadonlyArray<number> = Object.freeze([])
            const selectors: Array<PlaceReadTraceEvent['selectors'][number]> = []
            if (selected._tag === 'ReferenceValue') {
              const target = state.cells.get(cellKey(selected.frame, selected.cell))
              if (target === undefined)
                throw new RangeError('MIR reference points at a missing evaluator cell')
              effectiveSelectors = Object.freeze([...selected.selectors, ...operation.selectors])
              capturedIndexes = selected.indexes
              selected = target.value
            }
            for (const [ordinal, selector] of effectiveSelectors.entries()) {
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
                if (selected._tag !== 'SliceValue' && selected._tag !== 'StaticViewValue') {
                  throw new RangeError('MIR verifier allowed a slice selector on a non-slice value')
                }
                const captured = capturedIndexes.at(ordinal)
                const exactIndex =
                  captured === undefined
                    ? readInteger(selector.index, 'usize').value
                    : BigInt(captured)
                if (exactIndex >= BigInt(selected.length)) {
                  return blockedStep({
                    _tag: 'Trap',
                    function: fn.id,
                    reason: `slice index ${exactIndex} is outside length ${selected.length} in ${fn.id.module}.${fn.id.name}`,
                    span: selector.provenance.span,
                  })
                }
                const index = Number(exactIndex)
                if (selected._tag === 'StaticViewValue') {
                  const byte = selected.bytes.at(index)
                  if (byte === undefined) {
                    throw new RangeError('MIR static view range exceeds its immutable bytes')
                  }
                  selectors.push(
                    Object.freeze({
                      _tag: 'StaticElement',
                      data: selected.data,
                      index,
                      bounds: 'Checked',
                      span: selector.provenance.span,
                    }),
                  )
                  selected = integerValue('u8', BigInt(byte))
                  continue
                }
                if (selected.ticket !== undefined) {
                  const allocation = state.allocations.get(selected.ticket)
                  const absolute = selected.base + index
                  const element = allocation?.values.get(String(absolute))
                  if (allocation === undefined || !allocation.active || element === undefined) {
                    throw new RangeError('MIR RawBuffer slice selected uninitialized storage')
                  }
                  selectors.push(
                    Object.freeze({
                      _tag: 'RawBufferElement',
                      ticket: selected.ticket,
                      index: absolute,
                      bounds: 'Checked',
                      span: selector.provenance.span,
                    }),
                  )
                  selected = element
                  continue
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
                capturedIndexes.at(ordinal) ??
                (selector.index._tag === 'Proven'
                  ? selector.index.value
                  : Number(readInteger(selector.index.local, 'usize').value))
              if (index < 0 || !Number.isSafeInteger(index) || index >= selector.length) {
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
            checkedPlaces.set(BootstrapPlace.selectorKey(operation.selectors), resolved.indexes)
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
            const placeKey = BootstrapPlace.selectorKey(operation.selectors)
            const indexes = checkedPlaces.get(placeKey)
            if (indexes === undefined)
              throw new RangeError('MIR write executed without its precheck')
            const root = read(operation.root)
            const replacement = read(operation.source)
            // A reference root writes through the borrow: replace within the referenced cell
            // and store it back where the loan began, leaving the reference local untouched.
            if (root.value._tag === 'ReferenceValue' && operation.selectors.length > 0) {
              const key = cellKey(root.value.frame, root.value.cell)
              const target = state.cells.get(key)
              if (target === undefined)
                throw new RangeError('MIR reference points at a missing evaluator cell')
              const effectiveSelectors = Object.freeze([
                ...root.value.selectors,
                ...operation.selectors,
              ])
              state.cells.set(key, {
                value: replacePlace(target.value, effectiveSelectors, indexes, replacement.value),
                fromCall: replacement.fromCall,
              })
              checkedPlaces.delete(placeKey)
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
            checkedPlaces.delete(placeKey)
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
            const blocked = yield* releaseThroughPlan(
              operation.cleanup,
              dropped,
              operation.provenance,
              operation.local.ordinal,
            )
            if (blocked !== undefined) return blocked
            const members = BootstrapStorage.cleanupMembers(operation.cleanup, dropped)
            trace.push(
              Object.freeze({
                _tag: 'Cleanup',
                frame: activation.frame,
                depth: activation.depth,
                function: fn.id,
                local: operation.local.ordinal,
                ...(members.length === 0 ? {} : { members }),
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'MakeCallable': {
            const ticket = state.nextCallable
            state.nextCallable += 1
            state.callables.set(ticket, { state: 'Available' })
            const captures = operation.captures.map((capture) => {
              const captured: Value =
                capture.access === 'Copy' || capture.access === 'Take'
                  ? read(capture.source).value
                  : (() => {
                      const key = cellKey(frame, capture.source.ordinal)
                      if (!state.cells.has(key)) state.cells.set(key, read(capture.source))
                      return Object.freeze({
                        _tag: 'CallableBorrowValue' as const,
                        frame,
                        cell: capture.source.ordinal,
                        access: capture.access,
                      })
                    })()
              return Object.freeze({
                ordinal: capture.ordinal,
                parameterOrdinal: capture.parameterOrdinal,
                access: capture.access,
                value: captured,
              })
            })
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'CallableValue',
                ticket,
                type: operation.type.type,
                target: operation.target,
                typeArguments: operation.typeArguments,
                captures: Object.freeze(captures),
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'CallableConstruct',
                function: fn.id,
                ticket,
                mode: operation.type.type.mode,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'ApplyCallable': {
            const stored =
              operation.callable === undefined ? undefined : read(operation.callable).value
            if (stored !== undefined && stored._tag !== 'CallableValue') {
              throw new RangeError('MIR verifier allowed applying a non-callable evaluator value')
            }
            const target = stored?.target ?? operation.target
            if (target === undefined)
              throw new RangeError('MIR verifier allowed a callable application without identity')
            const ticket = stored?.ticket
            const callableState = ticket === undefined ? undefined : state.callables.get(ticket)
            if (ticket !== undefined && callableState === undefined)
              throw new RangeError('Callable application referenced a missing evaluator identity')
            if (ticket !== undefined && callableState !== undefined) {
              if (callableState.state !== 'Available') {
                trace.push(
                  Object.freeze({
                    _tag: 'CallableRejected',
                    function: fn.id,
                    ticket,
                    mode: stored?.type.mode ?? operation.callableType.mode,
                    span: operation.provenance.span,
                  }),
                )
                return blockedStep({
                  _tag: 'InvalidCallableReuse',
                  function: fn.id,
                  ticket,
                  state: callableState.state,
                  span: operation.provenance.span,
                })
              }
              callableState.state =
                stored?.type.mode === 'Take' || operation.access === 'Take' ? 'Consumed' : 'Running'
            }
            const captureValues = (
              stored?.captures ??
              operation.captures.map((capture) => {
                const captured: Value =
                  operation.callable === undefined ||
                  capture.access === 'Copy' ||
                  capture.access === 'Take'
                    ? read(capture.source).value
                    : Object.freeze({
                        _tag: 'CallableBorrowValue' as const,
                        frame,
                        cell: capture.source.ordinal,
                        access: capture.access,
                      })
                return Object.freeze({ ...capture, value: captured })
              })
            ).map((capture) => {
              const captured = capture.value
              const resolved =
                captured._tag === 'CallableBorrowValue'
                  ? state.cells.get(cellKey(captured.frame, captured.cell))?.value
                  : captured
              if (resolved === undefined)
                throw new RangeError('Callable capture references a missing evaluator cell')
              return Object.freeze({ parameterOrdinal: capture.parameterOrdinal, value: resolved })
            })
            const parameters = new Map<number, Value>()
            operation.arguments.forEach((argument, ordinal) => {
              parameters.set(ordinal, read(argument).value)
            })
            for (const capture of captureValues)
              parameters.set(capture.parameterOrdinal, capture.value)
            const arguments_ = Object.freeze(
              [...parameters.entries()]
                .sort(([left], [right]) => left - right)
                .map(([, argument]) => argument),
            )
            trace.push(
              Object.freeze({
                _tag: 'CallableApply',
                function: fn.id,
                ticket: ticket ?? -1,
                mode: stored?.type.mode ?? operation.callableType.mode,
                span: operation.provenance.span,
              }),
            )
            const result = yield* invokeCallableTarget(
              target,
              stored?.typeArguments ?? operation.typeArguments,
              arguments_,
              operation.provenance.span,
            )
            if (callableState?.state === 'Running') callableState.state = 'Available'
            if (result._tag === 'Blocked') return result
            if (result._tag === 'Transfer') return result
            write(operation.destination, { value: result.value, fromCall: true })
            break
          }
          case 'MakeEffect':
          case 'PackEffectComposite':
          case 'PackEffectOutcome':
          case 'PackEffectFailureUnion':
          case 'UnpackEffectSuccess':
          case 'PropagateEffectFailure':
          case 'RunEffect':
          case 'RunEffectComposite':
          case 'RunEffectValue':
          case 'RunStaticEffect':
          case 'ReifyEffect': {
            const effectStep = yield* BootstrapEffect.execute(
              {
                program,
                fn,
                activation,
                frame,
                state,
                trace,
                read,
                write,
                callEffectRunner,
                callFunction,
                executeOperations,
              },
              operation,
            )
            if (effectStep !== undefined) return effectStep
            break
          }
          case 'CloseEffectEntry': {
            const target = BootstrapEffect.functionFor(
              program,
              operation.target,
              operation.typeArguments,
            )
            const runner = BootstrapEffect.functionFor(
              program,
              operation.runner,
              operation.typeArguments,
            )
            if (target === undefined || runner === undefined)
              return blockedStep({
                _tag: 'MissingFunction',
                target: target === undefined ? operation.target : operation.runner,
                span: operation.provenance.span,
              })
            trace.push(
              Object.freeze({
                _tag: 'Call',
                frame: activation.frame,
                depth: activation.depth,
                caller: fn.id,
                target: operation.target,
                callerInstance: fn.instance,
                targetInstance: target.instance,
                span: operation.provenance.span,
              }),
            )
            const result = yield* callFunction(target, [], operation.provenance.span)
            if (result._tag === 'Blocked') return result
            if (result._tag === 'Transfer') return result
            if (result.value._tag !== 'EffectValue')
              throw new RangeError('MIR effect entry constructor returned a non-Effect value')
            const effect = result.value
            write(operation.effect, { value: effect, fromCall: true })
            trace.push(
              Object.freeze({
                _tag: 'Call',
                frame: activation.frame,
                depth: activation.depth,
                caller: fn.id,
                target: operation.runner,
                callerInstance: fn.instance,
                targetInstance: runner.instance,
                span: operation.provenance.span,
              }),
            )
            const called = yield* callFunction(runner, effect.captures, operation.provenance.span)
            const execution = yield* relayTransfers(called)
            if (execution._tag === 'Blocked') return execution
            if (execution._tag === 'Transfer')
              throw new RangeError('Effect entry retained a relayed suspension transfer')
            if (execution.value._tag !== 'EffectOutcomeValue')
              throw new RangeError('MIR effect entry runner returned a non-outcome value')
            const effectOutcome = execution.value
            write(operation.outcome, { value: effectOutcome, fromCall: true })
            if (effectOutcome.tag === 0) {
              write(operation.destination, { value: integerValue('i32', 0), fromCall: true })
              break
            }
            const failure = operation.failures.find(
              (candidate) => candidate.tag === effectOutcome.tag,
            )
            if (failure === undefined)
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: `effect entry returned invalid failure tag ${effectOutcome.tag}`,
                span: operation.provenance.span,
              })
            write(failure.payload, { value: effectOutcome.payload, fromCall: true })
            const released = yield* releaseThroughPlan(
              failure.cleanup,
              effectOutcome.payload,
              operation.provenance,
              failure.payload.ordinal,
            )
            if (released !== undefined) return released
            trace.push(
              Object.freeze({
                _tag: 'EffectFailure',
                phase: 'Closed',
                frame,
                depth: activation.depth,
                function: fn.id,
                tag: failure.tag,
                identity: failure.identity,
                span: operation.provenance.span,
              }),
            )
            write(operation.destination, {
              value: integerValue('i32', 1),
              fromCall: true,
            })
            break
          }
          case 'Call': {
            const target = BootstrapEffect.functionFor(
              program,
              operation.target,
              operation.typeArguments,
            )
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
                frame: activation.frame,
                depth: activation.depth,
                caller: fn.id,
                target: operation.target,
                callerInstance: fn.instance,
                targetInstance: target.instance,
                span: operation.provenance.span,
              }),
            )
            const argumentStates = operation.arguments.map((argument) => read(argument))
            argumentStates.forEach((argumentState, ordinal) => {
              trace.push(
                Object.freeze({
                  _tag: 'Binding',
                  frame: state.nextFrame,
                  depth: activation.depth + 1,
                  target: operation.target,
                  targetInstance: target.instance,
                  callSpan: operation.provenance.span,
                  argumentOrdinal: ordinal,
                  parameterOrdinal: ordinal,
                  value: argumentState.value,
                  fromCall: argumentState.fromCall,
                  span: operation.provenance.span,
                }),
              )
            })
            const result = yield* callFunction(
              target,
              argumentStates.map((state) => state.value),
              operation.provenance.span,
            )
            if (result._tag === 'Blocked') return result
            if (result._tag === 'Transfer') return result
            write(operation.destination, { value: result.value, fromCall: true })
            break
          }
        }
      }
      return undefined
    }
    const operations = region._tag === 'OperationRegion' ? region.operations : region.releases
    const operationStep = yield* executeOperations(operations)
    if (operationStep !== undefined) return operationStep
    const outcome = region.outcome
    switch (outcome._tag) {
      case 'Return': {
        const result = read(outcome.value)
        trace.push(
          Object.freeze({
            _tag: 'Return',
            frame: activation.frame,
            depth: activation.depth,
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
        const enter = readInteger(loop.conditionValue, 'i32').value !== 0n
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

const makeActivation = (
  program: Mir.Module,
  fn: Mir.MirFunction,
  arguments_: ReadonlyArray<Value>,
  depth: number,
  trace: Array<TraceEvent>,
  state: EvaluationState,
): ActivationRecord => {
  const frame = state.nextFrame
  state.nextFrame += 1
  const locals = new Map<number, LocalState>()
  arguments_.forEach((argument, ordinal) => {
    locals.set(ordinal, { value: argument, fromCall: false })
  })
  const activation: ActivationRecord = {
    frame,
    depth,
    function: fn.id,
    instance: fn.instance,
    locals,
    cells: state.cells,
  }
  trace.push(
    Object.freeze({
      _tag: 'Entry',
      frame,
      depth,
      function: fn.id,
      instance: fn.instance,
      span: argumentSpanFallback(fn),
    }),
  )
  activation.continuation = executeFunction(program, fn, activation, trace, state)
  return activation
}

const activeFrameSnapshot = (stack: ReadonlyArray<ActivationRecord>): ReadonlyArray<ActiveFrame> =>
  Object.freeze(
    stack.map(({ frame, depth, function: function_, instance }) =>
      Object.freeze({ frame, depth, function: function_, instance }),
    ),
  )

const suspensionSpan = (program: Mir.Module, point: Mir.SuspensionPointId): SourceSpan.SourceSpan =>
  program.functions
    .flatMap((fn) => fn.suspension?.regions ?? [])
    .find(
      (region) =>
        region.point.sourceId === point.sourceId &&
        region.point.spanStart === point.spanStart &&
        region.point.spanEnd === point.spanEnd &&
        region.point.ordinal === point.ordinal,
    )?.provenance.span ?? firstFunctionSpan(program)

const executeMachine = (
  program: Mir.Module,
  entry: Mir.MirFunction,
  trace: Array<TraceEvent>,
  state: EvaluationState,
): Step => {
  const stack: Array<ActivationRecord> = [makeActivation(program, entry, [], 1, trace, state)]
  let transfer: TransferContext | undefined
  let resumed: Step | undefined

  const releaseActivationFrame = (activation: ActivationRecord): void => {
    const frame = activation.coroutineFrame
    if (frame === undefined) return
    state.executionStackBytes -= frame.bytes
    trace.push(
      Object.freeze({
        _tag: 'CoroutineFrameComplete',
        function: activation.function,
        point: frame.point,
        ticket: frame.ticket,
        span: suspensionSpan(program, frame.point),
      }),
    )
    delete activation.coroutineFrame
  }

  const publishTransfer = (context: TransferContext): BlockedReason | undefined => {
    for (const pending of context.pending) {
      const frameState = pending.state
      if (frameState === undefined) continue
      const existing = pending.activation.coroutineFrame
      if (existing === undefined) {
        const bytes =
          program.coroutineFrames?.entries.find(
            (candidate) =>
              Instances.keyText(candidate.function) ===
              Instances.keyText(pending.activation.instance),
          )?.size ?? 0
        if (state.executionStackBytes + bytes > state.maxExecutionStackBytes) {
          return Object.freeze({
            _tag: 'Trap',
            function: pending.activation.function,
            reason: 'private execution stack exhausted',
            span: suspensionSpan(program, frameState.point),
          })
        }
        const ticket = state.nextCoroutineFrame
        state.nextCoroutineFrame += 1
        state.executionStackBytes += bytes
        pending.activation.coroutineFrame = {
          ticket,
          bytes,
          point: frameState.point,
        }
        trace.push(
          Object.freeze({
            _tag: 'CoroutineFramePush',
            function: pending.activation.function,
            point: frameState.point,
            ticket,
            span: suspensionSpan(program, frameState.point),
          }),
        )
      } else {
        existing.point = frameState.point
      }
      const frame = pending.activation.coroutineFrame
      if (frame !== undefined)
        trace.push(
          Object.freeze({
            _tag: 'CoroutineFrameStateTransition',
            function: pending.activation.function,
            point: frameState.point,
            ticket: frame.ticket,
            span: suspensionSpan(program, frameState.point),
          }),
        )
    }
    const origin = context.pending.at(0)?.activation
    if (origin === undefined)
      throw new RangeError('Evaluator transfer lost its originating activation')
    stack.push(
      makeActivation(
        program,
        context.step.child.target,
        context.step.child.arguments,
        origin.depth + 1,
        trace,
        state,
      ),
    )
    trace.push(
      Object.freeze({
        _tag: 'SuspensionChildStart',
        function: context.step.child.target.id,
        point: context.step.origin.point,
        span: suspensionSpan(program, context.step.origin.point),
      }),
    )
    return undefined
  }
  while (stack.length > 0) {
    const suspended: Array<ActivationRecord> = []
    for (let current = transfer; current !== undefined; current = current.parent)
      suspended.unshift(...[...current.pending].reverse().map((pending) => pending.activation))
    state.activeFrames = activeFrameSnapshot([...suspended, ...stack])
    const activation = stack.at(-1)
    if (activation?.continuation === undefined) {
      throw new RangeError('Evaluator activation lost its continuation')
    }
    const advanced =
      resumed === undefined ? activation.continuation.next() : activation.continuation.next(resumed)
    resumed = undefined
    if (advanced.done) {
      stack.pop()
      if (advanced.value._tag === 'Blocked') return advanced.value
      releaseActivationFrame(activation)
      if (stack.length === 0 && transfer !== undefined) {
        let next = transfer.pending.shift()
        while (next === undefined && transfer.parent !== undefined) {
          transfer = transfer.parent
          next = transfer.pending.shift()
        }
        if (next !== undefined) {
          stack.push(next.activation)
          if (
            next.state === undefined &&
            advanced.value._tag === 'Value' &&
            advanced.value.value._tag === 'EffectOutcomeValue'
          )
            trace.push(
              Object.freeze({
                _tag: 'SuspensionChildComplete',
                function: next.activation.function,
                point: transfer.step.origin.point,
                outcome: advanced.value.value.tag === 0 ? 'Success' : 'Failure',
                span: suspensionSpan(program, transfer.step.origin.point),
              }),
            )
          if (
            next.activation.coroutineFrame !== undefined &&
            advanced.value._tag === 'Value' &&
            advanced.value.value._tag === 'EffectOutcomeValue'
          )
            trace.push(
              Object.freeze({
                _tag: 'CoroutineFrameResume',
                function: next.activation.function,
                point: next.activation.coroutineFrame.point,
                ticket: next.activation.coroutineFrame.ticket,
                outcome: advanced.value.value.tag === 0 ? 'Success' : 'Failure',
                span: suspensionSpan(program, next.activation.coroutineFrame.point),
              }),
            )
          resumed = advanced.value
          continue
        }
        transfer = undefined
        return advanced.value
      }
      if (stack.length === 0) return advanced.value
      resumed = advanced.value
      continue
    }
    const request = advanced.value
    if (request._tag === 'OriginTransferRequest') {
      const step: TransferStep = Object.freeze({
        _tag: 'Transfer',
        origin: request.origin,
        child: request.child,
      })
      transfer = {
        step,
        pending: [Object.freeze({ activation })],
        ...(transfer === undefined ? {} : { parent: transfer }),
      }
      trace.push(
        Object.freeze({
          _tag: 'SuspensionOrigin',
          function: activation.function,
          point: request.origin.point,
          span: suspensionSpan(program, request.origin.point),
        }),
      )
      stack.pop()
      if (stack.length > 0) {
        resumed = step
        continue
      }
      const storageFailure = publishTransfer(transfer)
      if (storageFailure !== undefined) return blockedStep(storageFailure)
      continue
    }
    if (request._tag === 'RelayTransferRequest') {
      if (transfer === undefined || transfer.step !== request.transfer)
        throw new RangeError('Evaluator relay lost its originating transfer')
      transfer.pending.push(
        Object.freeze({
          activation,
          ...(request.state === undefined ? {} : { state: request.state }),
        }),
      )
      stack.pop()
      if (stack.length > 0) {
        resumed = transfer.step
        continue
      }
      const storageFailure = publishTransfer(transfer)
      if (storageFailure !== undefined) return blockedStep(storageFailure)
      continue
    }
    let suspendedDepth = 0
    for (let current = transfer; current !== undefined; current = current.parent)
      suspendedDepth += current.pending.length
    if (stack.length + suspendedDepth >= state.maxCallDepth) {
      for (;;) {
        const attemptedBinding = trace.at(-1)
        if (attemptedBinding?._tag !== 'Binding' || attemptedBinding.frame !== state.nextFrame)
          break
        trace.pop()
      }
      const attemptedCall = trace.at(-1)
      if (
        attemptedCall?._tag === 'Call' &&
        attemptedCall.frame === activation.frame &&
        attemptedCall.target.module === request.target.id.module &&
        attemptedCall.target.name === request.target.id.name &&
        attemptedCall.span.start === request.span.start &&
        attemptedCall.span.end === request.span.end
      ) {
        trace.pop()
      }
      return blockedStep({
        _tag: 'EvaluationLimit',
        kind: 'CallDepth',
        limit: state.maxCallDepth,
        count: stack.length + suspendedDepth,
        function: activation.function,
        span: request.span,
        activeFrames: state.activeFrames,
      })
    }
    stack.push(
      makeActivation(
        program,
        request.target,
        request.arguments,
        activation.depth + 1,
        trace,
        state,
      ),
    )
  }
  throw new RangeError('Evaluator activation machine stopped without a result')
}

export const defaultMaxSteps = 1_000_000
export const defaultMaxCallDepth = 1_024
export const defaultMaxExecutionStackBytes = Number.MAX_SAFE_INTEGER

const evaluationLimitOption = (
  name: string,
  value: number | undefined,
  fallback: number,
): number => {
  const selected = value ?? fallback
  if (!Number.isSafeInteger(selected) || selected <= 0) {
    throw new RangeError(`${name} must be a positive safe integer`)
  }
  return selected
}

/** Explicit host services available to one deterministic evaluation. */
export interface Options {
  readonly standardStreams?: StandardStreams.Provider
  readonly standardInput?: StandardInput.Provider
  readonly childProcess?: ChildProcess.Provider
  readonly hostInput?: HostInput.Provider
  readonly osFileSystem?: OsFileSystemHost.Provider
  readonly maxSteps?: number
  readonly maxCallDepth?: number
  /** Host-only deterministic bound for compiler-private coroutine-frame storage. */
  readonly maxExecutionStackBytes?: number
}

/** Executes the lowered program from the discovered entry, replaying MIR operations as a trace. */
export const evaluate = (
  discovery: Instances.Discovery,
  program: Mir.Module,
  options: Options = {},
): Outcome => {
  const maxSteps = evaluationLimitOption('maxSteps', options.maxSteps, defaultMaxSteps)
  const maxCallDepth = evaluationLimitOption(
    'maxCallDepth',
    options.maxCallDepth,
    defaultMaxCallDepth,
  )
  const maxExecutionStackBytes = evaluationLimitOption(
    'maxExecutionStackBytes',
    options.maxExecutionStackBytes,
    defaultMaxExecutionStackBytes,
  )
  const availability = IntrinsicAvailability.select(program.intrinsics, 'Evaluator')
  if (availability._tag === 'Unavailable') {
    return Object.freeze({
      _tag: 'Blocked',
      entry: discovery.entry._tag === 'Resolved' ? discovery.entry.key.declaration : undefined,
      reason: Object.freeze({
        _tag: 'IntrinsicTargetUnavailable',
        diagnostics: availability.diagnostics,
      }),
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
  const violations = MirVerification.verify(program)
  if (violations.length > 0) {
    return Object.freeze({
      _tag: 'Blocked',
      entry: discovery.entry._tag === 'Resolved' ? discovery.entry.key.declaration : undefined,
      reason: Object.freeze({ _tag: 'InvalidMir', violations }),
      trace: Object.freeze([]),
    })
  }
  const machine = Mir.machineEntry(program)
  const entry = machine.declaration
  const fn = BootstrapEffect.functionFor(program, entry, machine.typeArguments)
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
  const result = executeMachine(program, fn, trace, {
    nextFrame: 0,
    nextAllocation: 0,
    nextCallable: 0,
    nextCoroutineFrame: 0,
    steps: 0,
    maxSteps,
    maxCallDepth,
    maxExecutionStackBytes,
    executionStackBytes: 0,
    activeFrames: Object.freeze([]),
    cells: new Map(),
    allocations: new Map(),
    callables: new Map(),
    activeLoans: new Set(),
    stringLoans: new Set(),
    ...(options.standardStreams === undefined ? {} : { standardStreams: options.standardStreams }),
    ...(options.standardInput === undefined ? {} : { standardInput: options.standardInput }),
    ...(options.childProcess === undefined ? {} : { childProcess: options.childProcess }),
    processCaptures: [Object.freeze([]), Object.freeze([])],
    ...(options.hostInput === undefined ? {} : { hostInput: options.hostInput }),
    ...(options.osFileSystem === undefined ? {} : { osFileSystem: options.osFileSystem }),
  })
  if (result._tag === 'Blocked') {
    if (result.reason._tag === 'Trap') {
      const frozenTrace = Object.freeze([...trace])
      const history = BootstrapEffect.causalHistory(frozenTrace, 'Trap')
      return Object.freeze({
        _tag: 'Trap',
        classification: 'Trap',
        entry,
        status: 2,
        reason: result.reason.reason,
        provenance: result.reason.span,
        logicalPath: BootstrapEffect.longestCausalPath(
          history,
          BootstrapEffect.logicalPathAt(frozenTrace, frozenTrace.length - 1),
        ),
        history,
        trace: frozenTrace,
      })
    }
    return Object.freeze({
      _tag: 'Blocked',
      entry,
      reason: result.reason,
      trace: Object.freeze([...trace]),
    })
  }
  if (result._tag === 'Transfer')
    throw new RangeError('Bootstrap evaluator returned a private suspension transfer')
  if (result.value._tag !== 'IntegerValue' || result.value.type !== 'i32') {
    throw new RangeError('Bootstrap entry returned a non-i32 value')
  }
  const status = result.value
  const statusCode = Number(status.value)
  if (program.entry._tag === 'EffectEntry' && statusCode !== 0) {
    const closedFailure = [...trace]
      .reverse()
      .find(
        (event): event is EffectTraceEvent =>
          event._tag === 'EffectFailure' && event.phase === 'Closed',
      )
    const failure = program.entry.failures.find((candidate) => candidate.tag === closedFailure?.tag)
    if (failure === undefined) {
      const provenance = argumentSpanFallback(fn)
      const frozenTrace = Object.freeze([...trace])
      const history = BootstrapEffect.causalHistory(frozenTrace, 'Trap')
      return Object.freeze({
        _tag: 'Trap',
        classification: 'Trap',
        entry,
        status: 2,
        reason: 'effect entry returned failure status without a closed typed failure',
        provenance,
        logicalPath: BootstrapEffect.longestCausalPath(
          history,
          BootstrapEffect.logicalPathAt(frozenTrace, frozenTrace.length - 1),
        ),
        history,
        trace: frozenTrace,
      })
    }
    const frozenTrace = Object.freeze([...trace])
    const history = BootstrapEffect.causalHistory(frozenTrace, 'TypedFailure', failure.identity)
    const cause = history.at(-1)
    return Object.freeze({
      _tag: 'UnhandledFailure',
      classification: 'TypedFailure',
      entry,
      status: 1,
      tag: failure.tag,
      identity: failure.identity,
      provenance: cause?.provenance ?? argumentSpanFallback(fn),
      logicalPath: BootstrapEffect.longestCausalPath(
        history,
        BootstrapEffect.logicalPathAt(frozenTrace, frozenTrace.length - 1),
      ),
      history,
      trace: frozenTrace,
    })
  }
  const frozenTrace = Object.freeze([...trace])
  const history = BootstrapEffect.causalHistory(frozenTrace, 'Success')
  const root = frozenTrace.find(
    (event): event is EntryTraceEvent =>
      event._tag === 'Entry' && !BootstrapEffect.isPhysicalEntryAdapter(event.function.name),
  )
  const provenance =
    [...frozenTrace].reverse().find((event) => event._tag === 'Return')?.span ??
    root?.span ??
    argumentSpanFallback(fn)
  return Object.freeze({
    _tag: 'Completed',
    classification: 'Success',
    entry,
    status: statusCode,
    result: result.value,
    provenance,
    logicalPath:
      root === undefined
        ? Object.freeze([])
        : Object.freeze([Object.freeze({ function: root.function, provenance: root.span })]),
    history,
    trace: frozenTrace,
  })
}

const raiseNoSpan = (): never => {
  throw new RangeError('Lowered program has no functions to attach a span to')
}
