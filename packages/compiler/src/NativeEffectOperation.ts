import * as LlvmBlock from '@silk-effect/llvm/Block'
import * as Constant from '@silk-effect/llvm/Constant'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import type * as Value from '@silk-effect/llvm/Value'
import * as Effect from 'effect/Effect'
import * as CleanupPlan from './CleanupPlan.js'
import type * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import type { LinearOperation } from './MirLinearization.js'
import * as SilkType from './Type.js'

/** Whether one MIR operation requires the native allocation ABI. */
export const needsAllocation = (operation: Mir.Operation): boolean =>
  operation._tag === 'Allocate' ||
  operation._tag === 'RawBufferFrom' ||
  operation._tag === 'RawBufferCount' ||
  operation._tag === 'RawBufferSlot' ||
  operation._tag === 'RawBufferRead' ||
  operation._tag === 'RawBufferView' ||
  operation._tag === 'RawBufferCopy' ||
  operation._tag === 'RawBufferFill' ||
  operation._tag === 'SlotWrite' ||
  operation._tag === 'SlotTake' ||
  operation._tag === 'SlotCopy' ||
  operation._tag === 'SlotDrop' ||
  (operation._tag === 'CloseEffectEntry' &&
    operation.failures.some((failure) => CleanupPlan.reclaims(failure.cleanup))) ||
  (operation._tag === 'Drop' && CleanupPlan.reclaims(operation.cleanup))

import * as NativeAggregate from './NativeAggregate.js'
import * as NativeCall from './NativeCall.js'
import * as NativeFunction from './NativeFunction.js'
import type { LoweringContext } from './NativeOperation.js'
import * as NativeSuspension from './NativeSuspension.js'

type Operation = Extract<
  LinearOperation,
  {
    readonly _tag:
      | 'Drop'
      | 'MakeEffect'
      | 'MakeCallable'
      | 'PackEffectComposite'
      | 'PackEffectOutcome'
      | 'PackEffectFailureUnion'
      | 'UnpackEffectSuccess'
      | 'RunEffect'
      | 'RunEffectComposite'
      | 'RunEffectValue'
      | 'RunStaticEffect'
      | 'ReifyEffect'
      | 'CloseEffectEntry'
  }
>

export const emit = Effect.fnUntraced(function* (context: LoweringContext, operation: Operation) {
  const {
    addressStorage,
    body,
    builder,
    call,
    coerceLane,
    declared,
    cleanup,
    ensureAddressRoot,
    entry,
    failure,
    i32,
    laneType,
    lanesFor,
    locals,
    mutableStorage,
    reloadMutableRoots,
    suspension,
    storeMutable,
    suspensionRegions,
    valueLanesFor,
  } = context
  const initialTrapBlock = context.state.trapBlock
  let trapBlock = initialTrapBlock
  const checkOrdinal = context.state.checkOrdinal
  switch (operation._tag) {
    case 'Drop': {
      if (CleanupPlan.hasEffect(operation.cleanup)) {
        yield* NativeAggregate.dropThroughPlan(
          cleanup,
          operation.cleanup,
          NativeFunction.readLocal(locals, operation.local),
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
        if (field === undefined) throw new RangeError('Effect capture lost its environment field')
        if (field.representation !== 'Borrow') {
          captured.push(...NativeFunction.readLocal(locals, capture.source))
          continue
        }
        yield* ensureAddressRoot(capture.source)
        const base = addressStorage.get(capture.source.ordinal)
        if (base === undefined) throw new RangeError('Effect borrowed capture lost its storage')
        captured.push(base)
      }
      if (captured.length !== lanesFor(operation.type).length)
        throw new RangeError('Effect environment capture lanes do not match its plan')
      locals.set(operation.destination.ordinal, Object.freeze(captured))
      break
    }
    case 'PackEffectComposite': {
      const source = [...NativeFunction.readLocal(locals, operation.source)]
      const sourceType = entry.fn.localTypes.at(operation.source.ordinal)
      if (sourceType?._tag !== 'EffectValue')
        throw new RangeError('LLVM Effect composite lost its selected alternative')
      const sourceLanes = lanesFor(sourceType)
      const targetLanes = lanesFor(operation.type)
      const values: Array<Value.Input> = [
        yield* Constant.integerSigned(builder, i32, BigInt(operation.alternative)),
      ]
      for (const [ordinal, targetLane] of targetLanes.slice(1).entries()) {
        const input = source.at(ordinal)
        const sourceLane = sourceLanes.at(ordinal)
        values.push(
          input === undefined || sourceLane === undefined
            ? yield* Constant.nullValue(builder, laneType(targetLane))
            : yield* coerceLane(
                input,
                sourceLane,
                targetLane,
                `effect_composite${operation.destination.ordinal}_${ordinal}`,
              ),
        )
      }
      locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'PackEffectOutcome': {
      const source = [...NativeFunction.readLocal(locals, operation.source)]
      const sourceType = entry.fn.localTypes.at(operation.source.ordinal)
      if (sourceType === undefined) throw new RangeError('LLVM effect outcome lost its source type')
      const sourceLanes = valueLanesFor(sourceType)
      const targetLanes = lanesFor(operation.type)
      const values: Array<Value.Input> = [
        yield* Constant.integerSigned(builder, i32, BigInt(operation.tag)),
      ]
      for (const [ordinal, targetLane] of targetLanes.slice(1).entries()) {
        const input = source.at(ordinal)
        const sourceLane = sourceLanes.at(ordinal)
        values.push(
          input === undefined || sourceLane === undefined
            ? yield* Constant.nullValue(builder, laneType(targetLane))
            : yield* coerceLane(
                input,
                sourceLane,
                targetLane,
                `effect_outcome${operation.destination.ordinal}_${ordinal}_payload`,
              ),
        )
      }
      locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'PackEffectFailureUnion': {
      const source = NativeFunction.readLocal(locals, operation.source)
      const sourceTag = source.at(0)
      if (sourceTag === undefined) throw new RangeError('Effect failure union lost its tag lane')
      let mappedTag: Value.Input = yield* Constant.integerSigned(builder, i32, -1n)
      for (const [ordinal, mapping] of operation.mappings.entries()) {
        const matches = yield* FunctionBody.integerCompare(
          body,
          'eq',
          sourceTag,
          yield* Constant.integerSigned(builder, i32, BigInt(mapping.source)),
          `effect_failure_union${operation.destination.ordinal}_${ordinal}`,
        )
        mappedTag = yield* FunctionBody.select(
          body,
          matches,
          yield* Constant.integerSigned(builder, i32, BigInt(mapping.target)),
          mappedTag,
          `effect_failure_union${operation.destination.ordinal}_${ordinal}_tag`,
        )
      }
      const values: Array<Value.Input> = [
        mappedTag,
        ...(yield* NativeAggregate.failurePayload(
          failure,
          source,
          operation.sourceType.type,
          sourceTag,
          operation.type.type,
          operation.mappings,
          `effect_failure_union${operation.destination.ordinal}_payload`,
        )),
      ]
      locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'UnpackEffectSuccess': {
      const count = lanesFor(operation.type).length
      locals.set(
        operation.destination.ordinal,
        Object.freeze(NativeFunction.readLocal(locals, operation.source).slice(1, 1 + count)),
      )
      break
    }
    case 'RunEffect': {
      const target = declared.find((candidate) =>
        Mir.matchesInstance(candidate.fn, operation.target, operation.typeArguments),
      )
      if (target === undefined)
        throw new RangeError('Backend cannot resolve propagated effect target')
      const runArguments = operation.arguments.flatMap((argument) => [
        ...NativeFunction.readLocal(locals, argument),
      ])
      if (
        yield* NativeSuspension.emitOrigin(
          suspension,
          operation,
          runArguments,
          `effect_run${operation.destination.ordinal}`,
        )
      )
        break
      const suspensionRegion = suspensionRegions.get(operation)
      const outcomeValues = yield* NativeSuspension.joinOutcome(
        suspension,
        operation,
        yield* NativeCall.callValues(
          call,
          target,
          runArguments,
          `effect_run${operation.destination.ordinal}`,
          suspensionRegion?._tag === 'RunSuspendableEffectRegion' ? suspensionRegion : undefined,
        ),
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
        const source = yield* Constant.integerSigned(builder, i32, BigInt(mapping.source))
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
      // Owners still live at this site release before the failure leaves the function
      // through their complete cleanup plans, matching the Drop lowering.
      for (const release of operation.releases ?? []) {
        if (!CleanupPlan.hasEffect(release.cleanup)) continue
        yield* NativeAggregate.dropThroughPlan(
          cleanup,
          release.cleanup,
          NativeFunction.readLocal(locals, release.local),
          `propagation_release${release.local.ordinal}`,
        )
      }
      const returned: Array<Value.Input> = [
        mappedTag,
        ...(yield* NativeAggregate.failurePayload(
          failure,
          outcomeValues,
          operation.outcomeType.type,
          tag,
          operation.propagationType.type,
          operation.tagMappings,
          `effect_run${operation.destination.ordinal}_payload`,
        )),
      ]
      if (entry.suspendable) {
        yield* NativeSuspension.returnStep(
          suspension.returns,
          0n,
          Object.freeze(returned),
          'propagated_effect_step',
        )
      } else if (returned.length === 1) {
        const single = returned.at(0)
        if (single === undefined) throw new RangeError('Effect propagation lost its tag')
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
      // Both arms of this outcome dispatch reach here, so neither arm's cached
      // values are readable in the join. Reloading re-roots them at this block.
      yield* reloadMutableRoots(`effect_run${operation.destination.ordinal}_following`)
      const storage = mutableStorage.get(operation.destination.ordinal)
      if (storage === undefined) throw new RangeError('Effect run destination is not materialized')
      const loaded: Array<Value.Input> = []
      for (const [lane, pointer] of storage.entries()) {
        const callingLane = lanesFor(operation.type).at(lane)
        if (callingLane === undefined) throw new RangeError('Effect run destination lost a lane')
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
    case 'RunEffectComposite': {
      const compositeValues = NativeFunction.readLocal(locals, operation.effect)
      const choice = compositeValues.at(0)
      const compositeType = entry.fn.localTypes.at(operation.effect.ordinal)
      if (choice === undefined || compositeType?._tag !== 'EffectComposite')
        throw new RangeError('LLVM Effect composite lost its tag or representation')
      const compositeLanes = lanesFor(compositeType)
      const joinedOutcomeLanes = lanesFor(operation.outcomeType)
      const following = yield* LlvmBlock.make(
        body,
        `effect_composite${operation.destination.ordinal}_following`,
      )
      for (const [alternativeOrdinal, alternative] of operation.alternatives.entries()) {
        const selected = yield* LlvmBlock.make(
          body,
          `effect_composite${operation.destination.ordinal}_alternative${alternativeOrdinal}`,
        )
        const otherwise = yield* LlvmBlock.make(
          body,
          `effect_composite${operation.destination.ordinal}_otherwise${alternativeOrdinal}`,
        )
        const selectedTag = yield* Constant.integerSigned(builder, i32, BigInt(alternativeOrdinal))
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            choice,
            selectedTag,
            `effect_composite${operation.destination.ordinal}_is${alternativeOrdinal}`,
          ),
          selected,
          otherwise,
        )
        yield* LlvmBlock.setInsertionPoint(body, selected)
        const target = declared.find((candidate) =>
          Mir.matchesInstance(candidate.fn, alternative.runner, alternative.runnerTypeArguments),
        )
        if (target === undefined)
          throw new RangeError(
            `Backend cannot resolve Effect composite runner ${alternative.runner.module}.${alternative.runner.name}`,
          )
        const captureLanes = lanesFor(alternative.type)
        const effectArguments: Array<Value.Input> = []
        for (const [ordinal, targetLane] of captureLanes.entries()) {
          const input = compositeValues.at(ordinal + 1)
          const sourceLane = compositeLanes.at(ordinal + 1)
          if (input === undefined || sourceLane === undefined)
            throw new RangeError('LLVM Effect composite lost a capture lane')
          effectArguments.push(
            yield* coerceLane(
              input,
              sourceLane,
              targetLane,
              `effect_composite${operation.destination.ordinal}_${alternativeOrdinal}_capture${ordinal}`,
            ),
          )
        }
        effectArguments.push(
          ...alternative.arguments.flatMap((argument) => [
            ...NativeFunction.readLocal(locals, argument),
          ]),
        )
        const called = yield* NativeCall.callValues(
          call,
          target,
          effectArguments,
          `effect_composite${operation.destination.ordinal}_${alternativeOrdinal}`,
        )
        const sourceOutcomeType: Extract<Mir.Type, { readonly _tag: 'EffectOutcome' }> =
          Object.freeze({ _tag: 'EffectOutcome', type: alternative.type.type })
        const sourceOutcomeLanes = lanesFor(sourceOutcomeType)
        const sourceTag = called.at(0)
        if (sourceTag === undefined)
          throw new RangeError('LLVM Effect composite runner lost its outcome tag')
        let mappedTag: Value.Input = sourceTag
        for (const [mappingOrdinal, mapping] of alternative.tagMappings.entries()) {
          const matches = yield* FunctionBody.integerCompare(
            body,
            'eq',
            sourceTag,
            yield* Constant.integerSigned(builder, i32, BigInt(mapping.source)),
            `effect_composite${operation.destination.ordinal}_${alternativeOrdinal}_tag${mappingOrdinal}`,
          )
          mappedTag = yield* FunctionBody.select(
            body,
            matches,
            yield* Constant.integerSigned(builder, i32, BigInt(mapping.target)),
            mappedTag,
            `effect_composite${operation.destination.ordinal}_${alternativeOrdinal}_mapped${mappingOrdinal}`,
          )
        }
        const joined: Array<Value.Input> = [mappedTag]
        for (const [ordinal, targetLane] of joinedOutcomeLanes.slice(1).entries()) {
          const input = called.at(ordinal + 1)
          const sourceLane = sourceOutcomeLanes.at(ordinal + 1)
          joined.push(
            input === undefined || sourceLane === undefined
              ? yield* Constant.nullValue(builder, laneType(targetLane))
              : yield* coerceLane(
                  input,
                  sourceLane,
                  targetLane,
                  `effect_composite${operation.destination.ordinal}_${alternativeOrdinal}_outcome${ordinal}`,
                ),
          )
        }
        yield* storeMutable(operation.outcome, Object.freeze(joined))
        yield* FunctionBody.branch(body, following)
        yield* LlvmBlock.setInsertionPoint(body, otherwise)
      }
      if (trapBlock === undefined)
        trapBlock = yield* LlvmBlock.make(body, 'effect_composite_invalid_tag')
      yield* FunctionBody.branch(body, trapBlock)
      yield* LlvmBlock.setInsertionPoint(body, following)
      yield* reloadMutableRoots(`effect_composite${operation.destination.ordinal}_following`)
      const outcomeStorage = mutableStorage.get(operation.outcome.ordinal)
      if (outcomeStorage === undefined)
        throw new RangeError('Effect composite outcome is not materialized')
      const outcomeValues: Array<Value.Input> = []
      for (const [ordinal, pointer] of outcomeStorage.entries()) {
        const lane = joinedOutcomeLanes.at(ordinal)
        if (lane === undefined) throw new RangeError('Effect composite outcome lost a lane')
        outcomeValues.push(
          yield* FunctionBody.load(
            body,
            laneType(lane),
            pointer,
            `effect_composite${operation.destination.ordinal}_outcome${ordinal}`,
          ),
        )
      }
      locals.set(operation.outcome.ordinal, Object.freeze(outcomeValues))
      const resultLaneCount = lanesFor(operation.type).length
      if (operation.propagationType === undefined) {
        locals.set(
          operation.destination.ordinal,
          Object.freeze(outcomeValues.slice(1, 1 + resultLaneCount)),
        )
        break
      }
      const tag = outcomeValues.at(0)
      if (tag === undefined) throw new RangeError('Effect composite outcome lost its tag')
      const succeeded = yield* FunctionBody.integerCompare(
        body,
        'eq',
        tag,
        yield* Constant.integerSigned(builder, i32, 0n),
        `effect_composite_success${operation.destination.ordinal}`,
      )
      const successBlock = yield* LlvmBlock.make(
        body,
        `effect_composite${operation.destination.ordinal}_success`,
      )
      const failureBlock = yield* LlvmBlock.make(
        body,
        `effect_composite${operation.destination.ordinal}_failure`,
      )
      const completed = yield* LlvmBlock.make(
        body,
        `effect_composite${operation.destination.ordinal}_completed`,
      )
      yield* FunctionBody.conditionalBranch(body, succeeded, successBlock, failureBlock)
      yield* LlvmBlock.setInsertionPoint(body, successBlock)
      yield* storeMutable(
        operation.destination,
        Object.freeze(outcomeValues.slice(1, 1 + resultLaneCount)),
      )
      yield* FunctionBody.branch(body, completed)
      yield* LlvmBlock.setInsertionPoint(body, failureBlock)
      let propagatedTag: Value.Input = yield* Constant.integerSigned(builder, i32, -1n)
      for (const [ordinal, mapping] of operation.tagMappings.entries()) {
        const matches = yield* FunctionBody.integerCompare(
          body,
          'eq',
          tag,
          yield* Constant.integerSigned(builder, i32, BigInt(mapping.source)),
          `effect_composite_propagation_tag${operation.destination.ordinal}_${ordinal}`,
        )
        propagatedTag = yield* FunctionBody.select(
          body,
          matches,
          yield* Constant.integerSigned(builder, i32, BigInt(mapping.target)),
          propagatedTag,
          `effect_composite_propagated_tag${operation.destination.ordinal}_${ordinal}`,
        )
      }
      for (const release of operation.releases ?? []) {
        if (!CleanupPlan.hasEffect(release.cleanup)) continue
        yield* NativeAggregate.dropThroughPlan(
          cleanup,
          release.cleanup,
          NativeFunction.readLocal(locals, release.local),
          `effect_composite_release${release.local.ordinal}`,
        )
      }
      const returned: Array<Value.Input> = [
        propagatedTag,
        ...(yield* NativeAggregate.failurePayload(
          failure,
          outcomeValues,
          operation.outcomeType.type,
          tag,
          operation.propagationType.type,
          operation.tagMappings,
          `effect_composite${operation.destination.ordinal}_payload`,
        )),
      ]
      if (entry.suspendable) {
        yield* NativeSuspension.returnStep(
          suspension.returns,
          0n,
          Object.freeze(returned),
          'propagated_effect_composite_step',
        )
      } else {
        yield* FunctionBody.returnValue(
          body,
          returned.length === 1
            ? (returned.at(0) ?? propagatedTag)
            : yield* FunctionBody.buildAggregate(
                body,
                entry.resultType,
                Object.freeze(returned.slice(0, operation.propagationLaneCount)),
                'propagated_effect_composite',
              ),
        )
      }
      yield* LlvmBlock.setInsertionPoint(body, completed)
      yield* reloadMutableRoots(`effect_composite${operation.destination.ordinal}_completed`)
      const destinationStorage = mutableStorage.get(operation.destination.ordinal)
      if (destinationStorage === undefined)
        throw new RangeError('Effect composite destination is not materialized')
      const loaded: Array<Value.Input> = []
      for (const [ordinal, pointer] of destinationStorage.entries()) {
        const lane = lanesFor(operation.type).at(ordinal)
        if (lane === undefined) throw new RangeError('Effect composite destination lost a lane')
        loaded.push(
          yield* FunctionBody.load(
            body,
            laneType(lane),
            pointer,
            `effect_composite${operation.destination.ordinal}_${ordinal}`,
          ),
        )
      }
      locals.set(operation.destination.ordinal, Object.freeze(loaded))
      break
    }
    case 'RunEffectValue':
    case 'RunStaticEffect': {
      const logicalInputs =
        operation._tag === 'RunStaticEffect'
          ? [...operation.captures.map((capture) => capture.source), ...operation.arguments]
          : undefined
      const target = declared.find(
        (candidate) =>
          Mir.matchesInstance(candidate.fn, operation.runner, operation.runnerTypeArguments) &&
          (operation._tag !== 'RunStaticEffect' ||
            (logicalInputs !== undefined &&
              candidate.fn.result._tag === 'EffectOutcome' &&
              SilkType.equals(candidate.fn.result.type, operation.outcomeType.type) &&
              candidate.fn.parameterCount === logicalInputs.length &&
              logicalInputs.every((input, ordinal) => {
                const actual = entry.fn.localTypes.at(input.ordinal)
                const expected = candidate.fn.localTypes.at(ordinal)
                return (
                  actual !== undefined &&
                  expected !== undefined &&
                  SilkType.equals(Mir.semanticType(actual), Mir.semanticType(expected))
                )
              }))),
      )
      if (target === undefined)
        throw new RangeError(
          `Backend cannot resolve Effect value runner ${operation.runner.module}.${operation.runner.name}<${operation.runnerTypeArguments.map(SilkType.encodeGenericArgument).join(', ')}>`,
        )
      const effectArguments = [
        ...(operation._tag === 'RunEffectValue'
          ? NativeFunction.readLocal(locals, operation.effect)
          : operation.captures.flatMap((capture) => [
              ...NativeFunction.readLocal(locals, capture.source),
            ])),
        ...operation.arguments.flatMap((argument) => [
          ...NativeFunction.readLocal(locals, argument),
        ]),
      ]
      if (operation._tag !== 'RunStaticEffect') {
        if (
          yield* NativeSuspension.emitOrigin(
            suspension,
            operation,
            effectArguments,
            `effect_value_run${operation.destination.ordinal}`,
          )
        )
          break
      }
      const suspensionRegion =
        operation._tag === 'RunStaticEffect' ? undefined : suspensionRegions.get(operation)
      const called = yield* NativeCall.callValues(
        call,
        target,
        effectArguments,
        `effect_value_run${operation.destination.ordinal}`,
        suspensionRegion?._tag === 'RunSuspendableEffectRegion' ? suspensionRegion : undefined,
      )
      const outcomeValues =
        operation._tag === 'RunStaticEffect'
          ? called
          : yield* NativeSuspension.joinOutcome(
              suspension,
              operation,
              called,
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
        const source = yield* Constant.integerSigned(builder, i32, BigInt(mapping.source))
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
      // Owners still live at this site release before the failure leaves the function
      // through their complete cleanup plans, matching the Drop lowering.
      for (const release of operation.releases ?? []) {
        if (!CleanupPlan.hasEffect(release.cleanup)) continue
        yield* NativeAggregate.dropThroughPlan(
          cleanup,
          release.cleanup,
          NativeFunction.readLocal(locals, release.local),
          `propagation_release${release.local.ordinal}`,
        )
      }
      const returned: Array<Value.Input> = [
        mappedTag,
        ...(yield* NativeAggregate.failurePayload(
          failure,
          outcomeValues,
          operation.outcomeType.type,
          tag,
          operation.propagationType.type,
          operation.tagMappings,
          `effect_value${operation.destination.ordinal}_payload`,
        )),
      ]
      if (entry.suspendable) {
        yield* NativeSuspension.returnStep(
          suspension.returns,
          0n,
          Object.freeze(returned),
          'propagated_effect_value_step',
        )
      } else {
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
      }
      yield* LlvmBlock.setInsertionPoint(body, followingBlock)
      // Both arms of this outcome dispatch reach here, so neither arm's cached
      // values are readable in the join. Reloading re-roots them at this block.
      yield* reloadMutableRoots(`effect_value${operation.destination.ordinal}_following`)
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
    case 'ReifyEffect': {
      const target = declared.find((candidate) =>
        Mir.matchesInstance(candidate.fn, operation.runner, operation.runnerTypeArguments),
      )
      if (target === undefined) throw new RangeError('Backend cannot resolve Effect result runner')
      const reifyArguments = [
        ...NativeFunction.readLocal(locals, operation.effect),
        ...operation.arguments.flatMap((argument) => [
          ...NativeFunction.readLocal(locals, argument),
        ]),
      ]
      if (
        yield* NativeSuspension.emitOrigin(
          suspension,
          operation,
          reifyArguments,
          `effect_result_run${operation.destination.ordinal}`,
        )
      )
        break
      const suspensionRegion = suspensionRegions.get(operation)
      const outcomeValues = yield* NativeSuspension.joinOutcome(
        suspension,
        operation,
        yield* NativeCall.callValues(
          call,
          target,
          reifyArguments,
          `effect_result_run${operation.destination.ordinal}`,
          suspensionRegion?._tag === 'RunSuspendableEffectRegion' ? suspensionRegion : undefined,
        ),
        `effect_result_run${operation.destination.ordinal}`,
      )
      locals.set(operation.outcome.ordinal, outcomeValues)
      const tag = outcomeValues.at(0)
      if (tag === undefined) throw new RangeError('Effect result lost its outcome tag')
      const zero = yield* Constant.integerSigned(builder, i32, 0n)
      const succeeded = yield* FunctionBody.integerCompare(
        body,
        'eq',
        tag,
        zero,
        `effect_result_success${operation.destination.ordinal}`,
      )
      const successBlock = yield* LlvmBlock.make(
        body,
        `effect_result${operation.destination.ordinal}_success`,
      )
      const failureBlock = yield* LlvmBlock.make(
        body,
        `effect_result${operation.destination.ordinal}_failure`,
      )
      const followingBlock = yield* LlvmBlock.make(
        body,
        `effect_result${operation.destination.ordinal}_following`,
      )
      yield* FunctionBody.conditionalBranch(body, succeeded, successBlock, failureBlock)
      const destinationLanes = operation.resultShape.lanes
      const destinationPayloadLanes = destinationLanes.slice(1)
      const outcomeLanes = operation.outcomeShape.lanes
      const writeBranch = Effect.fnUntraced(function* (
        outerTag: number,
        values: ReadonlyArray<Value.Input>,
        lanes: ReadonlyArray<Layout.CallingLane>,
        label: string,
      ) {
        const branch: Array<Value.Input> = [
          yield* Constant.integerSigned(builder, i32, BigInt(outerTag)),
        ]
        for (const [ordinal, targetLane] of destinationPayloadLanes.entries()) {
          const input = values.at(ordinal)
          const sourceLane = lanes.at(ordinal)
          branch.push(
            input === undefined || sourceLane === undefined
              ? yield* Constant.nullValue(builder, laneType(targetLane))
              : yield* coerceLane(input, sourceLane, targetLane, `${label}_${ordinal}`),
          )
        }
        yield* storeMutable(operation.destination, Object.freeze(branch))
      })
      yield* LlvmBlock.setInsertionPoint(body, successBlock)
      const successLaneCount =
        operation.outcomeShape.tree._tag === 'OutcomeShape'
          ? operation.outcomeShape.tree.success.laneCount
          : 0
      yield* writeBranch(
        operation.successTag,
        Object.freeze(outcomeValues.slice(1, 1 + successLaneCount)),
        Object.freeze(outcomeLanes.slice(1, 1 + successLaneCount)),
        `effect_result${operation.destination.ordinal}_success`,
      )
      yield* FunctionBody.branch(body, followingBlock)
      yield* LlvmBlock.setInsertionPoint(body, failureBlock)
      if (SilkType.failureMembers(operation.outcomeType.type).length === 0) {
        if (trapBlock === undefined)
          trapBlock = yield* LlvmBlock.make(body, 'effect_result_invalid_tag')
        yield* FunctionBody.branch(body, trapBlock)
      } else {
        const failureValues: Array<Value.Input> = []
        if (SilkType.isUnion(operation.failureValueType)) {
          failureValues.push(
            yield* FunctionBody.binary(
              body,
              'sub',
              tag,
              yield* Constant.integerSigned(builder, i32, 1n),
              `effect_result${operation.destination.ordinal}_failure_tag`,
            ),
          )
        }
        failureValues.push(...outcomeValues.slice(1))
        const failureLanes: Array<Layout.CallingLane> = []
        if (SilkType.isUnion(operation.failureValueType)) {
          const failureTagLane = operation.failureValueShape.lanes.at(0)
          if (failureTagLane === undefined)
            throw new RangeError('Effect result lost its failure-union tag lane')
          failureLanes.push(failureTagLane)
        }
        failureLanes.push(...outcomeLanes.slice(1))
        yield* writeBranch(
          operation.failureTag,
          Object.freeze(failureValues),
          Object.freeze(failureLanes),
          `effect_result${operation.destination.ordinal}_failure`,
        )
        yield* FunctionBody.branch(body, followingBlock)
      }
      yield* LlvmBlock.setInsertionPoint(body, followingBlock)
      // Both arms of this outcome dispatch reach here, so neither arm's cached
      // values are readable in the join. Reloading re-roots them at this block.
      yield* reloadMutableRoots(`effect_result${operation.destination.ordinal}_following`)
      const storage = mutableStorage.get(operation.destination.ordinal)
      if (storage === undefined)
        throw new RangeError('Effect result destination is not materialized')
      const loaded: Array<Value.Input> = []
      for (const [ordinal, pointer] of storage.entries()) {
        const lane = destinationLanes.at(ordinal)
        if (lane === undefined) throw new RangeError('Effect result destination lost a lane')
        loaded.push(
          yield* FunctionBody.load(
            body,
            laneType(lane),
            pointer,
            `effect_result${operation.destination.ordinal}_${ordinal}`,
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
        throw new RangeError('Backend cannot resolve effect entry constructor or runner')
      const effectValues = yield* NativeCall.callValues(call, target, [], 'effect_entry_make')
      locals.set(operation.effect.ordinal, effectValues)
      const outcomeValues = yield* NativeCall.callValues(
        call,
        runner,
        effectValues,
        'effect_entry_run',
      )
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
        const otherwise = yield* LlvmBlock.make(body, `effect_entry_tag${failure.tag}_otherwise`)
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
        const payloadType = entry.fn.localTypes.at(failure.payload.ordinal)
        if (payloadType === undefined)
          throw new RangeError('Effect entry failure lost its payload type')
        const payloadLaneCount = lanesFor(payloadType).length
        const payload = outcomeValues.slice(1, 1 + payloadLaneCount)
        if (payload.length !== payloadLaneCount) {
          throw new RangeError('Effect entry failure lost its typed payload lanes')
        }
        locals.set(failure.payload.ordinal, Object.freeze(payload))
        if (CleanupPlan.hasEffect(failure.cleanup)) {
          yield* NativeAggregate.dropThroughPlan(
            cleanup,
            failure.cleanup,
            Object.freeze(payload),
            `effect_entry_cleanup${failure.tag}`,
          )
        }
        yield* storeMutable(
          operation.destination,
          Object.freeze([yield* Constant.integerSigned(builder, i32, 1n)]),
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
      // The success arm and every failure-tag arm reach here, so no arm's cached
      // values are readable in the join — and the failure arms run cleanup, which
      // reloads. Reloading re-roots the cache at this block.
      yield* reloadMutableRoots(`effect_entry${operation.destination.ordinal}_following`)
      const storage = mutableStorage.get(operation.destination.ordinal)
      const pointer = storage?.at(0)
      if (pointer === undefined) throw new RangeError('Effect entry status is not materialized')
      locals.set(
        operation.destination.ordinal,
        Object.freeze([yield* FunctionBody.load(body, i32, pointer, 'effect_entry_status')]),
      )
      break
    }
  }
  if (trapBlock !== initialTrapBlock) context.state.trapBlock = trapBlock
  context.state.checkOrdinal = checkOrdinal
})
