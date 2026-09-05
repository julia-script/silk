import * as Alignment from '@silklang/llvm/Alignment'
import * as LlvmBlock from '@silklang/llvm/Block'
import type * as Builder from '@silklang/llvm/Builder'
import * as Constant from '@silklang/llvm/Constant'
import type * as FunctionActor from '@silklang/llvm/Function'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import type * as LlvmType from '@silklang/llvm/Type'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as CleanupPlan from './CleanupPlan.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import * as Layout from './Layout.js'
import * as LayoutVerify from './LayoutVerify.js'
import * as LocalSharedControlBlock from './LocalSharedControlBlock.js'
import * as LocalSharedPayloadCleanup from './LocalSharedPayloadCleanup.js'
import * as Match from './Match.js'
import * as Mir from './Mir.js'
import * as MovePath from './MovePath.js'
import * as NativeArith from './NativeArith.js'
import * as NativeCall from './NativeCall.js'
import * as NativeExecutionOperation from './NativeExecutionOperation.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import type * as NativeLoweringContext from './NativeLoweringContext.js'
import * as NativeStorage from './NativeStorage.js'
import * as NativeType from './NativeType.js'
import * as SilkType from './Type.js'

/** Resolves one field's byte offset from an aggregate layout. */
export const fieldOffset = (layout: Layout.Plan, type: SilkType.Type, name: string): number => {
  const planned = Layout.entry(layout, type)
  if (planned?.representation._tag !== 'Aggregate')
    throw new RangeError(`LLVM raw storage lost aggregate ${SilkType.encode(type)}`)
  const field = planned.representation.fields.find((candidate) => candidate.name === name)
  if (field === undefined) throw new RangeError(`LLVM raw storage lost field ${name}`)
  return field.offset
}

export interface FailureContext {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly program: Mir.Module
  readonly i32: LlvmType.Type
  readonly types: NativeType.LoweringContext
  readonly arith: NativeArith.LaneContext
}

/** Repacks one semantic failure payload into the target Effect outcome layout. */
export const failurePayload = Effect.fnUntraced(function* (
  context: FailureContext,
  source: ReadonlyArray<Value.Input>,
  sourceType: DeclarationFacts.SemanticType,
  sourceTag: Value.Input | undefined,
  targetType: SilkType.Effect,
  mappings: ReadonlyArray<{ readonly source: number; readonly target: number }>,
  label: string,
): Effect.fn.Return<ReadonlyArray<Value.Input>, LlvmError.LlvmError> {
  const targetShape = Layout.callingShape(context.program.layout, targetType)
  if (targetShape?.tree._tag !== 'OutcomeShape')
    throw new RangeError('LLVM failure propagation lost its target calling shape')
  const payload: Array<Value.Input> = []
  for (const [targetOrdinal, targetLane] of targetShape.lanes.slice(1).entries()) {
    let selected: Value.Input = yield* Constant.nullValue(
      context.builder,
      NativeType.laneType(context.types, targetLane),
    )
    for (const [mappingOrdinal, mapping] of [...mappings].reverse().entries()) {
      const repacking = Layout.failurePayloadRepacking(
        context.program.layout,
        sourceType,
        mapping.source,
        targetType,
        mapping.target,
      )
      if (repacking === undefined)
        throw new RangeError('LLVM failure propagation has an invalid member mapping')
      const lane = repacking.lanes.find((candidate) => candidate.targetOrdinal === targetOrdinal)
      const sourceValue = lane === undefined ? undefined : source.at(lane.sourceOrdinal)
      let candidate: Value.Input = yield* Constant.nullValue(
        context.builder,
        NativeType.laneType(context.types, targetLane),
      )
      if (lane !== undefined && sourceValue !== undefined) {
        const member = yield* NativeArith.coerceLane(
          context.arith,
          sourceValue,
          lane.source,
          lane.member,
          `${label}_${targetOrdinal}_${mappingOrdinal}_member`,
        )
        candidate = yield* NativeArith.coerceLane(
          context.arith,
          member,
          lane.member,
          lane.target,
          `${label}_${targetOrdinal}_${mappingOrdinal}_carrier`,
        )
      }
      if (sourceTag === undefined) {
        selected = candidate
        continue
      }
      const matches = yield* FunctionBody.integerCompare(
        context.body,
        'eq',
        sourceTag,
        yield* Constant.integerSigned(context.builder, context.i32, BigInt(mapping.source)),
        `${label}_${targetOrdinal}_${mappingOrdinal}_matches`,
      )
      selected = yield* FunctionBody.select(
        context.body,
        matches,
        candidate,
        selected,
        `${label}_${targetOrdinal}_${mappingOrdinal}_select`,
      )
    }
    payload.push(selected)
  }
  return Object.freeze(payload)
})

/** Field paths to reclaim contexts, or undefined when guarded structural cleanup is required. */
export const reclaimContextPaths = (
  plan: CleanupPlan.CleanupPlan,
  prefix: ReadonlyArray<DeclarationFacts.FieldId> = [],
): ReadonlyArray<ReadonlyArray<DeclarationFacts.FieldId>> | undefined => {
  if (!CleanupPlan.reclaims(plan)) return CleanupPlan.hasHook(plan) ? undefined : []
  switch (plan._tag) {
    case 'NoCleanup':
    case 'ParameterCleanup':
      return []
    case 'AllocationCleanup':
    case 'RawBufferCleanup':
      return [prefix]
    case 'StructCleanup': {
      const collected: Array<ReadonlyArray<DeclarationFacts.FieldId>> = []
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

export interface Context {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly program: Mir.Module
  readonly i8: LlvmType.Type
  readonly i32: LlvmType.Type
  readonly pointer: LlvmType.Type
  readonly usizeType?: LlvmType.Type
  readonly free?: FunctionActor.Function
  readonly coroutineFramePop?: FunctionActor.Function
  readonly resumeThunks: ReadonlyMap<
    string,
    {
      readonly handle: FunctionActor.Function
      readonly layout: Mir.CoroutineFrameTargetStateLayout
    }
  >
  readonly declared: ReadonlyArray<NativeLoweringContext.DeclaredFunction>
  readonly types: NativeType.LoweringContext
  readonly lanePointers: NativeLanePointer.Context
  readonly call: NativeCall.Context
  readonly arith: NativeArith.LaneContext
  readonly storage: NativeStorage.Context
  /** The module's out-of-line Execution release; see `NativeExecutionOperation.emitReleaseHelper`. */
  readonly executionRelease?: FunctionActor.Function
}

/**
 * Releases one owned value's lanes through its complete cleanup plan: hooks run
 * against a stack materialization before their inner cleanup sees the (possibly
 * mutated) lanes, struct fields release in declaration order, and every ticket-backed
 * lane calls the release shim exactly once.
 */
interface Initialization {
  readonly state: MovePath.State
  readonly flags: NonNullable<Mir.DropOperation['initialization']>['flags']
  readonly path?: MovePath.Path
}

const childInitialization = (
  self: Initialization | undefined,
  selector: MovePath.Selector,
): Initialization | undefined => {
  if (self === undefined) return undefined
  const key = MovePath.key([selector])
  const child = self.state.children.find((candidate) => MovePath.key([candidate.selector]) === key)
  return {
    state: child?.state ?? MovePath.make(self.state.initialization),
    flags: self.flags,
    path: [...(self.path ?? []), selector],
  }
}

export const dropThroughPlan = Effect.fnUntraced(function* (
  context: Context,
  plan: CleanupPlan.CleanupPlan,
  values: ReadonlyArray<Value.Input>,
  tag: string,
  localSharedBlock?: LocalSharedControlBlock.Plan,
  initialization?: Initialization,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  const {
    builder,
    body,
    program,
    i8,
    i32,
    pointer,
    usizeType,
    free,
    declared,
    lanePointers,
    call,
    arith,
    storage,
    types,
  } = context
  if (initialization?.state.initialization === 'Missing') return
  if (initialization?.state.initialization === 'Maybe') {
    const path = initialization.path ?? []
    const flag = initialization.flags
      .filter(
        (candidate) =>
          candidate.path.length <= path.length && MovePath.overlaps(candidate.path, path),
      )
      .sort((left, right) => right.path.length - left.path.length)
      .at(0)
    if (flag === undefined) throw new RangeError('Conditional cleanup lost its initialization flag')
    const condition = yield* FunctionBody.integerCompare(body, 'ne', NativeStorage.readScalar(storage, flag.local), yield* Constant.integerSigned(builder, i32, 0n), `${tag}_is_initialized`)
    const selected = yield* LlvmBlock.make(body, `${tag}_initialized`)
    const following = yield* LlvmBlock.make(body, `${tag}_initialization_next`)
    yield* FunctionBody.conditionalBranch(body, condition, selected, following)
    yield* LlvmBlock.setInsertionPoint(body, selected)
    yield* dropThroughPlan(context, plan, values, tag, localSharedBlock, {
      ...initialization,
      state: { ...initialization.state, initialization: 'Initialized' },
    })
    yield* FunctionBody.branch(body, following)
    yield* LlvmBlock.setInsertionPoint(body, following)
    yield* NativeStorage.reloadRoots(storage, `${tag}_initialization_next`)
    return
  }
  const semanticLanesOf = (type: SilkType.Type): ReadonlyArray<Layout.CallingLane> => {
    const shape = Layout.callingShape(program.layout, type)
    if (shape === undefined)
      throw new RangeError(`LLVM cleanup lost calling shape for ${SilkType.encode(type)}`)
    return shape.lanes
  }
  switch (plan._tag) {
    case 'NoCleanup':
    case 'ParameterCleanup':
      return
    case 'ExecutionCleanup': {
      const base = values.at(0)
      if (base === undefined || context.executionRelease === undefined)
        throw new RangeError('LLVM Execution cleanup lost its release helper')
      // A synchronous call, not a bare `callDirect`: the helper may run user drop hooks, so the
      // caller's address-taken roots reload exactly as they did for the former inline expansion.
      yield* NativeCall.callSynchronous(
        call.synchronous,
        { handle: context.executionRelease, resultLaneCount: 0, suspendable: false },
        [base],
        `${tag}_release`,
      )
      return
    }
    case 'WakeCleanup':
      return yield* NativeExecutionOperation.dropWake(context, values, tag)
    case 'CallableCleanup': {
      if (plan.environment._tag !== 'CallableEnvironmentIdentity')
        throw new RangeError('LLVM callable cleanup lost its specialized environment')
      for (const slot of plan.slots) {
        const range = Layout.callableCaptureRange(
          program.layout,
          plan.environment.identity,
          slot.ordinal,
        )
        if (range === undefined)
          throw new RangeError('LLVM callable cleanup lost an owned capture lane')
        yield* dropThroughPlan(
          context,
          slot.cleanup,
          Object.freeze(values.slice(range.laneOffset, range.laneOffset + range.laneCount)),
          `${tag}_callable${slot.ordinal}`,
        )
      }
      return
    }
    case 'NominalUnionCleanup': {
      if (
        plan.variants.every(
          (variant) => !variant.fields.some((field) => CleanupPlan.hasEffect(field.cleanup)),
        )
      )
        return
      const shape = Layout.callingShape(program.layout, plan.type)
      const tagValue = values.at(0)
      if (shape?.tree._tag !== 'NominalUnionShape' || tagValue === undefined)
        throw new RangeError('LLVM nominal union cleanup lost its shape')
      for (const variant of plan.variants) {
        if (!variant.fields.some((field) => CleanupPlan.hasEffect(field.cleanup))) continue
        const matches = yield* FunctionBody.integerCompare(
          body,
          'eq',
          tagValue,
          yield* Constant.integerSigned(builder, i32, BigInt(variant.ordinal)),
          `${tag}_v${variant.ordinal}_is`,
        )
        const selectedBlock = yield* LlvmBlock.make(body, `${tag}_v${variant.ordinal}_drop`)
        const followingBlock = yield* LlvmBlock.make(body, `${tag}_v${variant.ordinal}_next`)
        yield* FunctionBody.conditionalBranch(body, matches, selectedBlock, followingBlock)
        yield* LlvmBlock.setInsertionPoint(body, selectedBlock)
        const identity = Match.nominalUnionVariant(
          plan.type,
          plan.type,
          variant.variant,
          variant.ordinal,
        )
        for (const [fieldOrdinal, field] of variant.fields.entries()) {
          if (!CleanupPlan.hasEffect(field.cleanup)) continue
          const physical = Layout.coverageFieldSlots(shape, identity, [field.field])
          const targetLanes = semanticLanesOf(field.cleanup.type)
          const fieldValues: Array<Value.Input> = []
          for (const [targetOrdinal, ordinal] of physical?.entries() ?? []) {
            const value = values.at(ordinal)
            const sourceLane = shape.lanes.at(ordinal)
            const targetLane = targetLanes.at(targetOrdinal)
            if (value === undefined || sourceLane === undefined || targetLane === undefined)
              continue
            fieldValues.push(
              yield* NativeArith.coerceLane(
                arith,
                value,
                sourceLane,
                targetLane,
                `${tag}_v${variant.ordinal}_f${fieldOrdinal}_${targetOrdinal}_lane`,
              ),
            )
          }
          if (fieldValues.length !== targetLanes.length)
            throw new RangeError('LLVM nominal union cleanup lost a field payload lane')
          yield* dropThroughPlan(
            context,
            field.cleanup,
            Object.freeze(fieldValues),
            `${tag}_v${variant.ordinal}_f${fieldOrdinal}`,
            undefined,
            childInitialization(
              childInitialization(initialization, { _tag: 'Variant', ordinal: variant.ordinal }),
              { _tag: 'Field', ordinal: field.field.ordinal },
            ),
          )
        }
        yield* FunctionBody.branch(body, followingBlock)
        yield* LlvmBlock.setInsertionPoint(body, followingBlock)
        yield* NativeStorage.reloadRoots(storage, `${tag}_v${variant.ordinal}_next`)
      }
      return
    }
    case 'EffectCleanup':
      for (const slot of plan.slots) {
        if (!CleanupPlan.hasEffect(slot.cleanup)) continue
        const selected = Object.freeze(
          values.slice(slot.laneOffset, slot.laneOffset + slot.laneCount),
        )
        if (selected.length !== slot.laneCount)
          throw new RangeError(
            `LLVM Effect cleanup ${tag} lost slot ${slot.ordinal} lanes ${slot.laneOffset}+${slot.laneCount} from ${values.length} value(s)`,
          )
        yield* dropThroughPlan(context, slot.cleanup, selected, `${tag}_effect${slot.ordinal}`)
      }
      return
    case 'EffectCompositeCleanup': {
      const choice = values.at(0)
      if (choice === undefined) throw new RangeError('LLVM Effect composite cleanup lost its tag')
      const following = yield* LlvmBlock.make(body, `${tag}_effect_composite_following`)
      for (const [ordinal, alternative] of plan.alternatives.entries()) {
        const selected = yield* LlvmBlock.make(body, `${tag}_effect_composite_${ordinal}`)
        const otherwise = yield* LlvmBlock.make(
          body,
          `${tag}_effect_composite_${ordinal}_otherwise`,
        )
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            choice,
            yield* Constant.integerSigned(builder, i32, BigInt(ordinal)),
            `${tag}_effect_composite_is_${ordinal}`,
          ),
          selected,
          otherwise,
        )
        yield* LlvmBlock.setInsertionPoint(body, selected)
        yield* dropThroughPlan(
          context,
          alternative,
          Object.freeze(values.slice(1)),
          `${tag}_${ordinal}`,
        )
        yield* FunctionBody.branch(body, following)
        yield* LlvmBlock.setInsertionPoint(body, otherwise)
      }
      yield* FunctionBody.branch(body, following)
      yield* LlvmBlock.setInsertionPoint(body, following)
      return
    }
    case 'LocalSharedCoreCleanup': {
      const baseAddress = values.at(0)
      const elementLayout = Layout.entry(program.layout, plan.element)
      const block =
        localSharedBlock ??
        (elementLayout === undefined
          ? undefined
          : LocalSharedControlBlock.plan(program.layout.target, plan.element, elementLayout))
      if (
        baseAddress === undefined ||
        block?._tag !== 'LocalSharedControlBlockPlan' ||
        usizeType === undefined
      )
        throw new RangeError('LLVM local-shared cleanup lost its control-block plan')
      const wordType = usizeType
      const base = yield* FunctionBody.cast(body, 'inttoptr', baseAddress, pointer, `${tag}_base`)
      const countPointer = yield* NativeLanePointer.lanePointer(
        lanePointers,
        body,
        base,
        block.strongOffset,
        `${tag}_strong_ptr`,
      )
      const count = yield* FunctionBody.load(body, wordType, countPointer, `${tag}_strong`)
      const nonLast = yield* FunctionBody.integerCompare(
        body,
        'ugt',
        count,
        yield* Constant.integerUnsigned(builder, wordType, 1n),
        `${tag}_non_last`,
      )
      const decrement = yield* LlvmBlock.make(body, `${tag}_decrement`)
      const last = yield* LlvmBlock.make(body, `${tag}_last`)
      const following = yield* LlvmBlock.make(body, `${tag}_following`)
      yield* FunctionBody.conditionalBranch(body, nonLast, decrement, last)
      yield* LlvmBlock.setInsertionPoint(body, decrement)
      yield* FunctionBody.store(
        body,
        yield* FunctionBody.binary(
          body,
          'sub',
          count,
          yield* Constant.integerUnsigned(builder, wordType, 1n),
          `${tag}_decremented`,
        ),
        countPointer,
      )
      yield* FunctionBody.branch(body, following)
      yield* LlvmBlock.setInsertionPoint(body, last)
      const loadLanes = Effect.fnUntraced(function* (
        type: SilkType.Type,
        byteOffset: number,
        laneTag: string,
      ) {
        const loaded: Array<Value.Input> = []
        for (const [ordinal, lane] of semanticLanesOf(type).entries()) {
          const offset = LayoutVerify.laneOffset(program.layout, type, lane.path)
          if (offset === undefined) throw new RangeError('LLVM local-shared cleanup lost a lane')
          loaded.push(
            yield* FunctionBody.load(
              body,
              NativeType.laneType(types, lane),
              yield* NativeLanePointer.lanePointer(
                lanePointers,
                body,
                base,
                byteOffset + offset,
                `${laneTag}_${ordinal}_ptr`,
              ),
              `${laneTag}_${ordinal}`,
            ),
          )
        }
        return Object.freeze(loaded)
      })
      const helper = declared.find((candidate) =>
        Mir.matchesInstance(candidate.fn, LocalSharedPayloadCleanup.declaration, [plan.element]),
      )
      if (helper === undefined)
        throw new RangeError('LLVM local-shared cleanup lost its payload helper')
      yield* FunctionBody.callDirect(
        body,
        helper.handle,
        yield* loadLanes(plan.element, block.valueOffset, `${tag}_value`),
        `${tag}_value_cleanup`,
      )
      yield* dropThroughPlan(
        context,
        plan.allocation,
        yield* loadLanes(SilkType.allocation, block.allocationOffset, `${tag}_allocation`),
        `${tag}_allocation`,
      )
      yield* FunctionBody.branch(body, following)
      yield* LlvmBlock.setInsertionPoint(body, following)
      return
    }
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
      if (layoutEntry === undefined)
        throw new RangeError(`LLVM hook cleanup lost the layout for ${SilkType.encode(plan.type)}`)
      const base = yield* FunctionBody.alloca(body, i8, `${tag}_hook_storage`, {
        count: yield* Constant.integerUnsigned(builder, i32, BigInt(layoutEntry.size)),
        alignment: yield* Alignment.fromByteUnits(layoutEntry.alignment),
      })
      const lanes = semanticLanesOf(plan.type)
      for (const [ordinal, lane] of lanes.entries()) {
        const offset = LayoutVerify.laneOffset(program.layout, plan.type, lane.path)
        const stored = values.at(ordinal)
        if (offset === undefined || stored === undefined)
          throw new RangeError(
            `LLVM hook cleanup ${tag} lost lane ${ordinal}/${lanes.length} for ${SilkType.encode(plan.type)} from ${values.length} value(s)`,
          )
        yield* FunctionBody.store(
          body,
          stored,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            base,
            offset,
            `${tag}_store${ordinal}`,
          ),
        )
      }
      yield* NativeCall.callValues(call, target, [base], `${tag}_hook`)
      const reloaded: Array<Value.Input> = []
      for (const [ordinal, lane] of lanes.entries()) {
        const offset = LayoutVerify.laneOffset(program.layout, plan.type, lane.path)
        if (offset === undefined) throw new RangeError('LLVM hook cleanup lost a lane offset')
        reloaded.push(
          yield* FunctionBody.load(
            body,
            NativeType.laneType(types, lane),
            yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              base,
              offset,
              `${tag}_reload${ordinal}_ptr`,
            ),
            `${tag}_reload${ordinal}`,
          ),
        )
      }
      yield* dropThroughPlan(context, plan.inner, Object.freeze(reloaded), `${tag}_inner`)
      return
    }
    case 'StructCleanup': {
      const lanes = semanticLanesOf(plan.type)
      for (const [fieldOrdinal, field] of plan.fields.entries()) {
        if (!CleanupPlan.hasEffect(field.cleanup)) continue
        const fieldValues = lanes.flatMap((lane, index) => {
          const first = lane.path.at(0)
          const value = values.at(index)
          return first !== undefined &&
            first._tag === 'FieldId' &&
            value !== undefined &&
            DeclarationFacts.sameFieldId(first, field.field)
            ? [value]
            : []
        })
        yield* dropThroughPlan(
          context,
          field.cleanup,
          Object.freeze(fieldValues),
          `${tag}_f${fieldOrdinal}`,
          undefined,
          childInitialization(initialization, { _tag: 'Field', ordinal: field.field.ordinal }),
        )
      }
      return
    }
    case 'ArrayCleanup': {
      if (!CleanupPlan.hasEffect(plan.element)) return
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
          context,
          plan.element,
          Object.freeze(elementValues),
          `${tag}_e${index}`,
          undefined,
          childInitialization(initialization, { _tag: 'ConstantIndex', index }),
        )
      }
      return
    }
    case 'UnionCleanup': {
      if (plan.cases.every((entry) => !CleanupPlan.hasEffect(entry.cleanup))) return
      // Hook-bearing or structurally unsupported cases branch on the live tag and lower
      // the complete plan. Plain reclaim paths select a null context for inactive cases,
      // which libc free ignores.
      const shape = Layout.callingShape(program.layout, plan.type)
      const tagValue = values.at(0)
      if (shape === undefined || tagValue === undefined) {
        throw new RangeError('LLVM union cleanup lost its shape')
      }
      for (const caseEntry of plan.cases) {
        const paths =
          initialization === undefined ? reclaimContextPaths(caseEntry.cleanup) : undefined
        if (paths === undefined) {
          const matches = yield* FunctionBody.integerCompare(
            body,
            'eq',
            tagValue,
            yield* Constant.integerSigned(builder, i32, BigInt(caseEntry.ordinal)),
            `${tag}_u${caseEntry.ordinal}_is`,
          )
          const selectedBlock = yield* LlvmBlock.make(body, `${tag}_u${caseEntry.ordinal}_drop`)
          const followingBlock = yield* LlvmBlock.make(body, `${tag}_u${caseEntry.ordinal}_next`)
          yield* FunctionBody.conditionalBranch(body, matches, selectedBlock, followingBlock)
          yield* LlvmBlock.setInsertionPoint(body, selectedBlock)
          const physical = Layout.memberFieldSlots(shape, caseEntry.member, [])
          const targetLanes = semanticLanesOf(caseEntry.member)
          const selected: Array<Value.Input> = []
          for (const [targetOrdinal, ordinal] of physical?.entries() ?? []) {
            const value = values.at(ordinal)
            const sourceLane = shape.lanes.at(ordinal)
            const targetLane = targetLanes.at(targetOrdinal)
            if (value === undefined || sourceLane === undefined || targetLane === undefined)
              continue
            selected.push(
              yield* NativeArith.coerceLane(
                arith,
                value,
                sourceLane,
                targetLane,
                `${tag}_u${caseEntry.ordinal}_${targetOrdinal}_lane`,
              ),
            )
          }
          if (selected.length !== targetLanes.length) {
            throw new RangeError('LLVM union cleanup lost a member payload lane')
          }
          yield* dropThroughPlan(
            context,
            caseEntry.cleanup,
            Object.freeze(selected),
            `${tag}_u${caseEntry.ordinal}`,
            undefined,
            childInitialization(initialization, { _tag: 'Variant', ordinal: caseEntry.ordinal }),
          )
          yield* FunctionBody.branch(body, followingBlock)
          yield* LlvmBlock.setInsertionPoint(body, followingBlock)
          // The arm just emitted is only one of this join's two predecessors, so
          // anything it left in the value cache is unreadable here — a Drop hook
          // reloads its receiver after calling out, and those loads live in the arm.
          // Reloading re-roots the cache in this block, which is both valid SSA and
          // the value the arm may have just mutated.
          yield* NativeStorage.reloadRoots(storage, `${tag}_u${caseEntry.ordinal}_next`)
          continue
        }
        if (paths.length === 0) continue
        if (free === undefined || usizeType === undefined) {
          throw new RangeError('LLVM union reclaim cleanup lost its release helper')
        }
        const zero = yield* Constant.integerUnsigned(builder, usizeType, 0n)
        for (const [pathOrdinal, path] of paths.entries()) {
          const slots = Layout.memberFieldSlots(shape, caseEntry.member, path)
          const contextSlot = slots?.at(4)
          const context = contextSlot === undefined ? undefined : values.at(contextSlot)
          if (context === undefined) {
            throw new RangeError('LLVM union cleanup lost a reclaim lane')
          }
          const matches = yield* FunctionBody.integerCompare(
            body,
            'eq',
            tagValue,
            yield* Constant.integerSigned(builder, i32, BigInt(caseEntry.ordinal)),
            `${tag}_u${caseEntry.ordinal}_${pathOrdinal}_is`,
          )
          const guarded = yield* FunctionBody.select(
            body,
            matches,
            context,
            zero,
            `${tag}_u${caseEntry.ordinal}_${pathOrdinal}_context`,
          )
          yield* FunctionBody.callDirect(body, free, [
            yield* FunctionBody.cast(
              body,
              'inttoptr',
              guarded,
              pointer,
              `${tag}_u${caseEntry.ordinal}_${pathOrdinal}_pointer`,
            ),
          ])
        }
      }
      return
    }
  }
})
