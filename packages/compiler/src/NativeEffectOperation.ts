import * as LlvmBlock from '@silklang/llvm/Block'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as CleanupPlan from './CleanupPlan.js'
import type * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import type { LinearOperation } from './MirLinearization.js'
import * as NativeAggregate from './NativeAggregate.js'
import * as NativeArith from './NativeArith.js'
import * as NativeCall from './NativeCall.js'
import type { Context } from './NativeOperationContext.js'
import * as NativeStorage from './NativeStorage.js'
import * as NativeSuspension from './NativeSuspension.js'
import * as NativeType from './NativeType.js'
import * as SilkType from './Type.js'

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
      | 'CatchEffect'
      | 'CloseEffectEntry'
  }
>

export const emit = Effect.fnUntraced(function* (context: Context, operation: Operation) {
  const {
    arith,
    body,
    builder,
    call,
    declared,
    cleanup,
    entry,
    failure,
    i32,
    storage: nativeStorage,
    suspension,
    suspensionRegions,
    types,
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
          NativeStorage.readLocal(nativeStorage, operation.local),
          `drop${operation.local.ordinal}`,
          operation.localShared?.block,
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
          captured.push(...NativeStorage.readLocal(nativeStorage, capture.source))
          continue
        }
        yield* NativeStorage.ensureAddressRoot(nativeStorage, capture.source)
        const base = nativeStorage.addressStorage.get(capture.source.ordinal)
        if (base === undefined) throw new RangeError('Effect borrowed capture lost its storage')
        captured.push(base)
      }
      if (captured.length !== NativeType.lanesFor(types, operation.type).length)
        throw new RangeError('Effect environment capture lanes do not match its plan')
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(captured))
      break
    }
    case 'PackEffectComposite': {
      const source = [...NativeStorage.readLocal(nativeStorage, operation.source)]
      const sourceType = entry.fn.localTypes.at(operation.source.ordinal)
      if (sourceType?._tag !== 'EffectValue')
        throw new RangeError('LLVM Effect composite lost its selected alternative')
      const sourceLanes = NativeType.lanesFor(types, sourceType)
      const targetLanes = NativeType.lanesFor(types, operation.type)
      const values: Array<Value.Input> = [
        yield* Constant.integerSigned(builder, i32, BigInt(operation.alternative)),
      ]
      for (const [ordinal, targetLane] of targetLanes.slice(1).entries()) {
        const input = source.at(ordinal)
        const sourceLane = sourceLanes.at(ordinal)
        values.push(
          input === undefined || sourceLane === undefined
            ? yield* Constant.nullValue(builder, NativeType.laneType(types, targetLane))
            : yield* NativeArith.coerceLane(
                arith.lane,
                input,
                sourceLane,
                targetLane,
                `effect_composite${operation.destination.ordinal}_${ordinal}`,
              ),
        )
      }
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'PackEffectOutcome': {
      const source = [...NativeStorage.readLocal(nativeStorage, operation.source)]
      const sourceType = entry.fn.localTypes.at(operation.source.ordinal)
      if (sourceType === undefined) throw new RangeError('LLVM effect outcome lost its source type')
      const sourceLanes = NativeType.valueLanesFor(types, sourceType)
      const targetLanes = NativeType.lanesFor(types, operation.type)
      const values: Array<Value.Input> = [
        yield* Constant.integerSigned(builder, i32, BigInt(operation.tag)),
      ]
      for (const [ordinal, targetLane] of targetLanes.slice(1).entries()) {
        const input = source.at(ordinal)
        const sourceLane = sourceLanes.at(ordinal)
        values.push(
          input === undefined || sourceLane === undefined
            ? yield* Constant.nullValue(builder, NativeType.laneType(types, targetLane))
            : yield* NativeArith.coerceLane(
                arith.lane,
                input,
                sourceLane,
                targetLane,
                `effect_outcome${operation.destination.ordinal}_${ordinal}_payload`,
              ),
        )
      }
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'PackEffectFailureUnion': {
      const source = NativeStorage.readLocal(nativeStorage, operation.source)
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
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(values))
      break
    }
    case 'UnpackEffectSuccess': {
      const count = NativeType.lanesFor(types, operation.type).length
      nativeStorage.locals.set(
        operation.destination.ordinal,
        Object.freeze(NativeStorage.readLocal(nativeStorage, operation.source).slice(1, 1 + count)),
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
        ...NativeStorage.readLocal(nativeStorage, argument),
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
      nativeStorage.locals.set(operation.outcome.ordinal, outcomeValues)
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
      const resultLaneCount = NativeType.lanesFor(types, operation.type).length
      yield* LlvmBlock.setInsertionPoint(body, successBlock)
      yield* NativeStorage.storeMutable(
        nativeStorage,
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
          NativeStorage.readLocal(nativeStorage, release.local),
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
      yield* NativeStorage.reloadRoots(
        nativeStorage,
        `effect_run${operation.destination.ordinal}_following`,
      )
      const storage = nativeStorage.mutableStorage.get(operation.destination.ordinal)
      if (storage === undefined) throw new RangeError('Effect run destination is not materialized')
      const loaded: Array<Value.Input> = []
      for (const [lane, pointer] of storage.entries()) {
        const callingLane = NativeType.lanesFor(types, operation.type).at(lane)
        if (callingLane === undefined) throw new RangeError('Effect run destination lost a lane')
        loaded.push(
          yield* FunctionBody.load(
            body,
            NativeType.laneType(types, callingLane),
            pointer,
            `effect_run${operation.destination.ordinal}_${lane}`,
          ),
        )
      }
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(loaded))
      break
    }
    case 'RunEffectComposite': {
      const compositeValues = NativeStorage.readLocal(nativeStorage, operation.effect)
      const choice = compositeValues.at(0)
      const compositeType = entry.fn.localTypes.at(operation.effect.ordinal)
      if (choice === undefined || compositeType?._tag !== 'EffectComposite')
        throw new RangeError('LLVM Effect composite lost its tag or representation')
      const compositeLanes = NativeType.lanesFor(types, compositeType)
      const joinedOutcomeLanes = NativeType.lanesFor(types, operation.outcomeType)
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
        const captureLanes = NativeType.lanesFor(types, alternative.type)
        const effectArguments: Array<Value.Input> = []
        for (const [ordinal, targetLane] of captureLanes.entries()) {
          const input = compositeValues.at(ordinal + 1)
          const sourceLane = compositeLanes.at(ordinal + 1)
          if (input === undefined || sourceLane === undefined)
            throw new RangeError('LLVM Effect composite lost a capture lane')
          effectArguments.push(
            yield* NativeArith.coerceLane(
              arith.lane,
              input,
              sourceLane,
              targetLane,
              `effect_composite${operation.destination.ordinal}_${alternativeOrdinal}_capture${ordinal}`,
            ),
          )
        }
        effectArguments.push(
          ...alternative.arguments.flatMap((argument) => [
            ...NativeStorage.readLocal(nativeStorage, argument),
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
        const sourceOutcomeLanes = NativeType.lanesFor(types, sourceOutcomeType)
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
              ? yield* Constant.nullValue(builder, NativeType.laneType(types, targetLane))
              : yield* NativeArith.coerceLane(
                  arith.lane,
                  input,
                  sourceLane,
                  targetLane,
                  `effect_composite${operation.destination.ordinal}_${alternativeOrdinal}_outcome${ordinal}`,
                ),
          )
        }
        yield* NativeStorage.storeMutable(nativeStorage, operation.outcome, Object.freeze(joined))
        yield* FunctionBody.branch(body, following)
        yield* LlvmBlock.setInsertionPoint(body, otherwise)
      }
      if (trapBlock === undefined)
        trapBlock = yield* LlvmBlock.make(body, 'effect_composite_invalid_tag')
      yield* FunctionBody.branch(body, trapBlock)
      yield* LlvmBlock.setInsertionPoint(body, following)
      yield* NativeStorage.reloadRoots(
        nativeStorage,
        `effect_composite${operation.destination.ordinal}_following`,
      )
      const outcomeStorage = nativeStorage.mutableStorage.get(operation.outcome.ordinal)
      if (outcomeStorage === undefined)
        throw new RangeError('Effect composite outcome is not materialized')
      const outcomeValues: Array<Value.Input> = []
      for (const [ordinal, pointer] of outcomeStorage.entries()) {
        const lane = joinedOutcomeLanes.at(ordinal)
        if (lane === undefined) throw new RangeError('Effect composite outcome lost a lane')
        outcomeValues.push(
          yield* FunctionBody.load(
            body,
            NativeType.laneType(types, lane),
            pointer,
            `effect_composite${operation.destination.ordinal}_outcome${ordinal}`,
          ),
        )
      }
      nativeStorage.locals.set(operation.outcome.ordinal, Object.freeze(outcomeValues))
      const resultLaneCount = NativeType.lanesFor(types, operation.type).length
      if (operation.propagationType === undefined) {
        nativeStorage.locals.set(
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
      yield* NativeStorage.storeMutable(
        nativeStorage,
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
          NativeStorage.readLocal(nativeStorage, release.local),
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
      yield* NativeStorage.reloadRoots(
        nativeStorage,
        `effect_composite${operation.destination.ordinal}_completed`,
      )
      const destinationStorage = nativeStorage.mutableStorage.get(operation.destination.ordinal)
      if (destinationStorage === undefined)
        throw new RangeError('Effect composite destination is not materialized')
      const loaded: Array<Value.Input> = []
      for (const [ordinal, pointer] of destinationStorage.entries()) {
        const lane = NativeType.lanesFor(types, operation.type).at(ordinal)
        if (lane === undefined) throw new RangeError('Effect composite destination lost a lane')
        loaded.push(
          yield* FunctionBody.load(
            body,
            NativeType.laneType(types, lane),
            pointer,
            `effect_composite${operation.destination.ordinal}_${ordinal}`,
          ),
        )
      }
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(loaded))
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
          ? NativeStorage.readLocal(nativeStorage, operation.effect)
          : operation.captures.flatMap((capture) => [
              ...NativeStorage.readLocal(nativeStorage, capture.source),
            ])),
        ...operation.arguments.flatMap((argument) => [
          ...NativeStorage.readLocal(nativeStorage, argument),
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
      nativeStorage.locals.set(operation.outcome.ordinal, outcomeValues)
      const resultLaneCount = NativeType.lanesFor(types, operation.type).length
      if (operation.propagationType === undefined) {
        nativeStorage.locals.set(
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
      yield* NativeStorage.storeMutable(
        nativeStorage,
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
          NativeStorage.readLocal(nativeStorage, release.local),
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
      yield* NativeStorage.reloadRoots(
        nativeStorage,
        `effect_value${operation.destination.ordinal}_following`,
      )
      const storage = nativeStorage.mutableStorage.get(operation.destination.ordinal)
      if (storage === undefined)
        throw new RangeError('Effect value run destination is not materialized')
      const loaded: Array<Value.Input> = []
      for (const [lane, pointer] of storage.entries()) {
        const callingLane = NativeType.lanesFor(types, operation.type).at(lane)
        if (callingLane === undefined)
          throw new RangeError('Effect value run destination lost a lane')
        loaded.push(
          yield* FunctionBody.load(
            body,
            NativeType.laneType(types, callingLane),
            pointer,
            `effect_value${operation.destination.ordinal}_${lane}`,
          ),
        )
      }
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(loaded))
      break
    }
    case 'CatchEffect': {
      const target = declared.find((candidate) =>
        Mir.matchesInstance(candidate.fn, operation.runner, operation.runnerTypeArguments),
      )
      if (target === undefined) throw new RangeError('Backend cannot resolve Effect result runner')
      const reifyArguments = [
        ...NativeStorage.readLocal(nativeStorage, operation.effect),
        ...operation.arguments.flatMap((argument) => [
          ...NativeStorage.readLocal(nativeStorage, argument),
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
      nativeStorage.locals.set(operation.outcome.ordinal, outcomeValues)
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
      nativeStorage.locals.set(
        operation.destination.ordinal,
        Object.freeze([
          yield* FunctionBody.cast(
            body,
            'zext',
            succeeded,
            i32,
            `effect_result_success_flag${operation.destination.ordinal}`,
          ),
        ]),
      )
      const outcomeLanes = operation.outcomeShape.lanes
      const successLaneCount =
        operation.outcomeShape.tree._tag === 'OutcomeShape'
          ? operation.outcomeShape.tree.success.laneCount
          : 0
      const coerce = Effect.fnUntraced(function* (
        values: ReadonlyArray<Value.Input>,
        sourceLanes: ReadonlyArray<Layout.CallingLane>,
        targetLanes: ReadonlyArray<Layout.CallingLane>,
        label: string,
      ) {
        const coerced: Array<Value.Input> = []
        for (const [ordinal, targetLane] of targetLanes.entries()) {
          const input = values.at(ordinal)
          const sourceLane = sourceLanes.at(ordinal)
          coerced.push(
            input === undefined || sourceLane === undefined
              ? yield* Constant.nullValue(builder, NativeType.laneType(types, targetLane))
              : yield* NativeArith.coerceLane(
                  arith.lane,
                  input,
                  sourceLane,
                  targetLane,
                  `${label}_${ordinal}`,
                ),
          )
        }
        return Object.freeze(coerced)
      })
      nativeStorage.locals.set(
        operation.successValue.ordinal,
        yield* coerce(
          Object.freeze(outcomeValues.slice(1, 1 + successLaneCount)),
          Object.freeze(outcomeLanes.slice(1, 1 + successLaneCount)),
          operation.successShape.lanes,
          `effect_result${operation.destination.ordinal}_success`,
        ),
      )
      const failureValues: Array<Value.Input> = []
      const failureLanes: Array<Layout.CallingLane> = []
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
        const failureTagLane = operation.failureValueShape.lanes.at(0)
        if (failureTagLane === undefined)
          throw new RangeError('Effect result lost its failure-union tag lane')
        failureLanes.push(failureTagLane)
      }
      failureValues.push(...outcomeValues.slice(1))
      failureLanes.push(...outcomeLanes.slice(1))
      nativeStorage.locals.set(
        operation.failureValue.ordinal,
        yield* coerce(
          Object.freeze(failureValues),
          Object.freeze(failureLanes),
          operation.failureValueShape.lanes,
          `effect_result${operation.destination.ordinal}_failure`,
        ),
      )
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
      nativeStorage.locals.set(operation.effect.ordinal, effectValues)
      const outcomeValues = yield* NativeCall.callValues(
        call,
        runner,
        effectValues,
        'effect_entry_run',
      )
      nativeStorage.locals.set(operation.outcome.ordinal, outcomeValues)
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
      yield* NativeStorage.storeMutable(
        nativeStorage,
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
        const payloadLaneCount = NativeType.lanesFor(types, payloadType).length
        const payload = outcomeValues.slice(1, 1 + payloadLaneCount)
        if (payload.length !== payloadLaneCount) {
          throw new RangeError('Effect entry failure lost its typed payload lanes')
        }
        nativeStorage.locals.set(failure.payload.ordinal, Object.freeze(payload))
        if (CleanupPlan.hasEffect(failure.cleanup)) {
          yield* NativeAggregate.dropThroughPlan(
            cleanup,
            failure.cleanup,
            Object.freeze(payload),
            `effect_entry_cleanup${failure.tag}`,
          )
        }
        yield* NativeStorage.storeMutable(
          nativeStorage,
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
      yield* NativeStorage.reloadRoots(
        nativeStorage,
        `effect_entry${operation.destination.ordinal}_following`,
      )
      const storage = nativeStorage.mutableStorage.get(operation.destination.ordinal)
      const pointer = storage?.at(0)
      if (pointer === undefined) throw new RangeError('Effect entry status is not materialized')
      nativeStorage.locals.set(
        operation.destination.ordinal,
        Object.freeze([yield* FunctionBody.load(body, i32, pointer, 'effect_entry_status')]),
      )
      break
    }
  }
  if (trapBlock !== initialTrapBlock) context.state.trapBlock = trapBlock
  context.state.checkOrdinal = checkOrdinal
})
