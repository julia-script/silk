import * as NativePayload from './NativePayload.js'
import * as ValueStorage from './ValueStorage.js'
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
import * as NativeDiagnosticOutcome from './NativeDiagnosticOutcome.js'
import * as NativeDiagnosticContext from './NativeDiagnosticContext.js'
import * as NativeDiagnosticText from './NativeDiagnosticText.js'
import * as NativeReturn from './NativeReturn.js'
import * as NativeOwnedPlace from './NativeOwnedPlace.js'
import type { Context } from './NativeOperationContext.js'
import * as NativeStorage from './NativeStorage.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import * as NativeSuspension from './NativeSuspension.js'
import * as NativeTermination from './NativeTermination.js'
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
      | 'UnpackEffectComposite'
      | 'PackEffectOutcome'
      | 'PackEffectFailureUnion'
      | 'UnpackEffectSuccess'
      | 'RunEffect'
      | 'RunEffectComposite'
      | 'RunEffectValue'
      | 'RunStaticEffect'
      | 'CatchEffect'
      | 'DiagnosticUnhandled'
  }
>

/** Restores success lanes from the wider shared success/failure outcome representation. */
const successPayload = Effect.fnUntraced(function* (
  context: Context,
  values: ReadonlyArray<Value.Input>,
  outcome: Mir.LocalId,
  result: Mir.Type,
  name: string,
) {
  const source = context.storage.fn.localTypes.at(outcome.ordinal)
  if (source === undefined) throw new RangeError('Effect success lost its outcome type')
  const sourceLanes = NativeType.lanesFor(context.types, source).slice(1)
  const resultLanes = NativeType.lanesFor(context.types, result)
  const payload: Array<Value.Input> = []
  for (const [ordinal, lane] of resultLanes.entries()) {
    const value = values.at(ordinal + 1)
    const sourceLane = sourceLanes.at(ordinal)
    if (value === undefined || sourceLane === undefined)
      throw new RangeError('Effect success lost a payload lane')
    payload.push(
      yield* NativeArith.coerceLane(
        context.arith.lane,
        value,
        sourceLane,
        lane,
        `${name}_${ordinal}`,
      ),
    )
  }
  return Object.freeze(payload)
})

/** Materializes only the selected logical member at an outcome/capture boundary. */
const memberPayload = Effect.fnUntraced(function* (
  context: Context,
  local: Mir.LocalId,
  tag: number,
  name: string,
) {
  const type = context.storage.fn.localTypes.at(local.ordinal)
  if (type?._tag !== 'EffectOutcome' && type?._tag !== 'EffectComposite')
    throw new RangeError('Member conversion requires a planned outcome or composite')
  const member = ValueStorage.find(
    context.program.layout,
    type._tag === 'EffectOutcome' ? 'Outcome' : 'CompositeCarrier',
    type.type,
  )?.members.find((member) => member.tag === tag)
  if (member === undefined) throw new RangeError('Member conversion lost its storage binding')
  return yield* NativePayload.materialize(
    NativePayload.project(
      NativePayload.local(context.storage, local),
      member.lanes.map((lane) => lane.slot),
      {
        source: NativeType.lanesFor(context.types, type),
        target: member.lanes.map((lane) => lane.lane),
      },
    ),
    context.cleanup,
    name,
  )
})

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
  const checkOrdinal = context.state.checkOrdinal
  switch (operation._tag) {
    case 'DiagnosticUnhandled': {
      const lane = NativeType.lanesFor(types, operation.type).at(0)
      if (lane === undefined) throw new RangeError('Terminal observation lost its usize result')
      const diagnostic = call.synchronous.diagnostic
      const result =
        diagnostic === undefined
          ? yield* Constant.integerUnsigned(builder, NativeType.laneType(types, lane), 0n)
          : yield* NativeDiagnosticContext.unhandled(diagnostic)
      yield* NativeStorage.writeLocal(
        nativeStorage,
        operation.destination.ordinal,
        Object.freeze([result]),
      )
      break
    }
    case 'Drop': {
      if (CleanupPlan.hasEffect(operation.cleanup)) {
        let values = NativePayload.local(nativeStorage, operation.local)
        if (operation.selectors !== undefined && operation.selectors.length > 0) {
          const root = entry.fn.localTypes.at(operation.local.ordinal)
          const place =
            root === undefined
              ? undefined
              : NativeOwnedPlace.make(
                  cleanup.program.layout,
                  Mir.semanticType(root),
                  operation.selectors,
                )
          if (place === undefined)
            throw new RangeError('Owned place cleanup lost its verified projection')
          values = NativePayload.project(values, place.slots, {
            source: place.source.lanes,
            target: place.target.lanes,
          })
        }
        yield* NativeAggregate.dropThroughPlan(
          cleanup,
          operation.cleanup,
          values,
          `drop${operation.local.ordinal}`,
          operation.localShared?.block,
          operation.initialization,
        )
      }
      break
    }
    case 'MakeEffect':
    case 'MakeCallable': {
      const fields =
        operation._tag === 'MakeEffect'
          ? operation.type.environment.fields
          : (operation.type.environment?.fields ?? Object.freeze([]))
      if (operation._tag === 'MakeCallable' && operation.base !== undefined) {
        yield* NativeStorage.constructField(nativeStorage, operation.destination, operation.base, 0)
      }
      // Callable captures name their field ordinal; a staged section's start past the base fields.
      const fieldOrdinals =
        operation._tag === 'MakeCallable'
          ? operation.captures.map((capture) => capture.ordinal)
          : operation.captures.map((_, ordinal) => ordinal)
      for (const [offset, capture] of operation.captures.entries()) {
        const field = fields.at(fieldOrdinals.at(offset) ?? -1)
        if (field === undefined) throw new RangeError('Effect capture lost its environment field')
        if (field.representation !== 'Borrow') {
          yield* NativeStorage.constructField(
            nativeStorage,
            operation.destination,
            capture.source,
            field.offset,
          )
          continue
        }
        yield* NativeStorage.ensureAddressRoot(nativeStorage, capture.source)
        const base = yield* NativeStorage.addressOf(nativeStorage, capture.source)
        const destination = nativeStorage.addressStorage.get(operation.destination.ordinal)
        if (destination === undefined) throw new RangeError('Borrowed capture lost its destination')
        yield* FunctionBody.store(
          body,
          base,
          yield* NativeLanePointer.lanePointer(
            context.lanePointers,
            body,
            destination,
            field.offset,
            `capture${operation.destination.ordinal}_${offset}`,
          ),
        )
      }
      break
    }
    case 'UnpackEffectComposite': {
      const source = yield* memberPayload(
        context,
        operation.source,
        operation.alternative,
        `effect_choice${operation.destination.ordinal}`,
      )
      const sourceType = entry.fn.localTypes.at(operation.source.ordinal)
      const tag = yield* NativeStorage.readLane(nativeStorage, operation.source, 0)
      if (sourceType?._tag !== 'EffectComposite' || tag === undefined)
        throw new RangeError('LLVM Effect choice projection lost its representation')
      const values: Array<Value.Input> = []
      for (const [ordinal] of NativeType.lanesFor(types, operation.type).entries()) {
        const input = source.at(ordinal)
        if (input === undefined)
          throw new RangeError('LLVM Effect choice projection lost a capture lane')
        values.push(input)
      }
      yield* NativeStorage.writeLocal(
        nativeStorage,
        operation.destination.ordinal,
        Object.freeze(values),
      )
      yield* NativeStorage.writeLocal(
        nativeStorage,
        operation.matched.ordinal,
        Object.freeze([
          yield* FunctionBody.cast(
            body,
            'zext',
            yield* FunctionBody.integerCompare(
              body,
              'eq',
              tag,
              yield* Constant.integerSigned(builder, i32, BigInt(operation.alternative)),
              `effect_choice${operation.destination.ordinal}_matched`,
            ),
            i32,
            `effect_choice${operation.destination.ordinal}_flag`,
          ),
        ]),
      )
      break
    }
    case 'PackEffectComposite': {
      const source = [...(yield* NativeStorage.materialize(nativeStorage, operation.source))]
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
      yield* NativeStorage.writeLocal(
        nativeStorage,
        operation.destination.ordinal,
        Object.freeze(values),
      )
      break
    }
    case 'PackEffectOutcome': {
      const source = [...(yield* NativeStorage.materialize(nativeStorage, operation.source))]
      const sourceType = entry.fn.localTypes.at(operation.source.ordinal)
      if (sourceType === undefined) throw new RangeError('LLVM effect outcome lost its source type')
      const sourceLanes = NativeType.valueLanesFor(types, sourceType)
      const targetLanes = NativeType.lanesFor(types, operation.type)
      const diagnostic = call.synchronous.diagnostic
      const diagnosticSlot = diagnostic?.outcomes.get(operation.destination.ordinal)
      if (diagnostic !== undefined && diagnosticSlot !== undefined) {
        if (operation.tag === 0) yield* NativeDiagnosticOutcome.release(diagnosticSlot, diagnostic)
        else
          yield* NativeDiagnosticOutcome.produce(
            diagnosticSlot,
            diagnostic,
            yield* NativeDiagnosticText.literal(
              diagnostic,
              NativeTermination.identityOf(operation.type.type, operation.tag),
              `${entry.symbol}.failure${operation.destination.ordinal}.identity`,
            ),
            yield* NativeDiagnosticText.literal(
              diagnostic,
              NativeDiagnosticText.origin(
                context.termination.module,
                entry.fn,
                operation.provenance.span,
              ),
              `${entry.symbol}.failure${operation.destination.ordinal}.origin`,
            ),
          )
      }
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
      yield* NativeStorage.writeLocal(
        nativeStorage,
        operation.destination.ordinal,
        Object.freeze(values),
      )
      break
    }
    case 'PackEffectFailureUnion': {
      const source = yield* NativeStorage.materialize(nativeStorage, operation.source)
      const sourceTag = source.at(0)
      if (sourceTag === undefined) throw new RangeError('Effect failure union lost its tag lane')
      let mappedTag: Value.Input = yield* Constant.integerSigned(builder, i32, -1n)
      const diagnostic = call.synchronous.diagnostic
      const diagnosticSlot = diagnostic?.outcomes.get(operation.destination.ordinal)
      let diagnosticIdentity: readonly [Value.Input, Value.Input] | undefined
      if (diagnostic !== undefined && diagnosticSlot !== undefined)
        diagnosticIdentity = [
          yield* Constant.nullValue(builder, diagnostic.pointer),
          yield* Constant.integerUnsigned(builder, diagnostic.word, 0n),
        ]
      for (const [ordinal, mapping] of operation.mappings.entries()) {
        const matches = yield* FunctionBody.integerCompare(
          body,
          'eq',
          sourceTag,
          yield* Constant.integerSigned(builder, i32, BigInt(mapping.source)),
          `effect_failure_union${operation.destination.ordinal}_${ordinal}`,
        )
        if (diagnostic !== undefined && diagnosticIdentity !== undefined) {
          const identity = yield* NativeDiagnosticText.literal(
            diagnostic,
            NativeTermination.identityOf(operation.type.type, mapping.target),
            `${entry.symbol}.failure${operation.destination.ordinal}.${ordinal}.identity`,
          )
          diagnosticIdentity = [
            yield* FunctionBody.select(
              body,
              matches,
              identity[0],
              diagnosticIdentity[0],
              `failure_identity${ordinal}`,
            ),
            yield* FunctionBody.select(
              body,
              matches,
              identity[1],
              diagnosticIdentity[1],
              `failure_identity_length${ordinal}`,
            ),
          ]
        }
        mappedTag = yield* FunctionBody.select(
          body,
          matches,
          yield* Constant.integerSigned(builder, i32, BigInt(mapping.target)),
          mappedTag,
          `effect_failure_union${operation.destination.ordinal}_${ordinal}_tag`,
        )
      }
      if (
        diagnostic !== undefined &&
        diagnosticSlot !== undefined &&
        diagnosticIdentity !== undefined
      )
        yield* NativeDiagnosticOutcome.produce(
          diagnosticSlot,
          diagnostic,
          diagnosticIdentity,
          yield* NativeDiagnosticText.literal(
            diagnostic,
            NativeDiagnosticText.origin(
              context.termination.module,
              entry.fn,
              operation.provenance.span,
            ),
            `${entry.symbol}.failure${operation.destination.ordinal}.origin`,
          ),
        )
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
      yield* NativeStorage.writeLocal(
        nativeStorage,
        operation.destination.ordinal,
        Object.freeze(values),
      )
      break
    }
    case 'UnpackEffectSuccess': {
      yield* NativeStorage.writeLocal(
        nativeStorage,
        operation.destination.ordinal,
        yield* memberPayload(
          context,
          operation.source,
          0,
          `effect_unpack${operation.destination.ordinal}`,
        ),
      )
      break
    }
    case 'RunEffect': {
      const target = declared.find((candidate) =>
        Mir.matchesInstance(
          candidate.fn,
          operation.target,
          operation.typeArguments,
          operation.staticArguments,
        ),
      )
      if (target === undefined)
        throw new RangeError('Backend cannot resolve propagated effect target')
      const runArguments = (yield* NativeStorage.materializeArguments(
        nativeStorage,
        operation.arguments,
      )).flat()
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
      yield* NativeStorage.writeLocal(nativeStorage, operation.outcome.ordinal, outcomeValues)
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
      yield* LlvmBlock.setInsertionPoint(body, successBlock)
      yield* NativeStorage.writeJoin(
        nativeStorage,
        operation.destination,
        yield* successPayload(
          context,
          outcomeValues,
          operation.outcome,
          operation.type,
          `effect_success${operation.destination.ordinal}`,
        ),
      )
      yield* FunctionBody.branch(body, followingBlock)
      yield* LlvmBlock.setInsertionPoint(body, failureBlock)
      yield* NativeTermination.storePropagated(
        context.termination,
        operation.outcome,
        operation.provenance.span,
      )
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
          NativePayload.local(nativeStorage, release.local),
          `propagation_release${release.local.ordinal}`,
          undefined,
          release.initialization,
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
      yield* NativeReturn.complete(
        suspension.returns,
        Object.freeze(returned.slice(0, operation.propagationLaneCount)),
        'propagated_effect',
        operation.outcome,
      )
      yield* LlvmBlock.setInsertionPoint(body, followingBlock)
      // Both arms of this outcome dispatch reach here, so neither arm's cached
      // values are readable in the join. Reloading re-roots them at this block.
      yield* NativeStorage.reloadRoots(
        nativeStorage,
        `effect_run${operation.destination.ordinal}_following`,
        operation.destination,
      )
      yield* NativeStorage.reloadLocal(
        nativeStorage,
        operation.destination,
        `effect_run${operation.destination.ordinal}`,
      )
      break
    }
    case 'RunEffectComposite': {
      const choice = yield* NativeStorage.readLane(nativeStorage, operation.effect, 0)
      const compositeType = entry.fn.localTypes.at(operation.effect.ordinal)
      if (choice === undefined || compositeType?._tag !== 'EffectComposite')
        throw new RangeError('LLVM Effect composite lost its tag or representation')
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
        const effectArguments = [
          ...(yield* memberPayload(
            context,
            operation.effect,
            alternativeOrdinal,
            `effect_composite${operation.destination.ordinal}_${alternativeOrdinal}`,
          )),
        ]
        effectArguments.push(
          ...(yield* NativeStorage.materializeArguments(
            nativeStorage,
            alternative.arguments,
          )).flat(),
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
        const sourceTag = called.values.at(0)
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
          const input = called.values.at(ordinal + 1)
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
        yield* NativeStorage.writeJoin(
          nativeStorage,
          operation.outcome,
          yield* NativeDiagnosticOutcome.accept(call.synchronous.diagnostic, operation.outcome, {
            ...called,
            values: Object.freeze(joined),
          }),
        )
        yield* FunctionBody.branch(body, following)
        yield* LlvmBlock.setInsertionPoint(body, otherwise)
      }
      yield* FunctionBody.branch(
        body,
        yield* NativeTermination.trapBlock(
          context.termination,
          'invalid effect outcome tag',
          operation.provenance.span,
        ),
      )
      yield* LlvmBlock.setInsertionPoint(body, following)
      yield* NativeStorage.reloadRoots(
        nativeStorage,
        `effect_composite${operation.destination.ordinal}_following`,
        operation.outcome,
      )
      const outcomeValues = yield* NativeStorage.materialize(nativeStorage, operation.outcome)
      if (operation.propagationType === undefined) {
        yield* NativeStorage.writeLocal(
          nativeStorage,
          operation.destination.ordinal,
          yield* successPayload(
            context,
            outcomeValues,
            operation.outcome,
            operation.type,
            `effect_success${operation.destination.ordinal}`,
          ),
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
      yield* NativeStorage.writeJoin(
        nativeStorage,
        operation.destination,
        yield* successPayload(
          context,
          outcomeValues,
          operation.outcome,
          operation.type,
          `effect_success${operation.destination.ordinal}`,
        ),
      )
      yield* FunctionBody.branch(body, completed)
      yield* LlvmBlock.setInsertionPoint(body, failureBlock)
      yield* NativeTermination.storePropagated(
        context.termination,
        operation.outcome,
        operation.provenance.span,
      )
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
          NativePayload.local(nativeStorage, release.local),
          `effect_composite_release${release.local.ordinal}`,
          undefined,
          release.initialization,
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
      yield* NativeReturn.complete(
        suspension.returns,
        Object.freeze(returned.slice(0, operation.propagationLaneCount)),
        'propagated_effect_composite',
        operation.outcome,
      )
      yield* LlvmBlock.setInsertionPoint(body, completed)
      yield* NativeStorage.reloadRoots(
        nativeStorage,
        `effect_composite${operation.destination.ordinal}_completed`,
        operation.destination,
      )
      yield* NativeStorage.reloadLocal(
        nativeStorage,
        operation.destination,
        `effect_composite${operation.destination.ordinal}`,
      )
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
          Mir.matchesInstance(
            candidate.fn,
            operation.runner,
            operation.runnerTypeArguments,
            operation.runnerStaticArguments,
          ) &&
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
                  Mir.acceptsRuntimeOperand(Mir.semanticType(actual), Mir.semanticType(expected))
                )
              }))),
      )
      if (target === undefined)
        throw new RangeError(
          `Backend cannot resolve Effect value runner ${operation.runner.module}.${operation.runner.name}<${operation.runnerTypeArguments.map(SilkType.encodeGenericArgument).join(', ')}>`,
        )
      const effectArguments = [
        ...(operation._tag === 'RunEffectValue'
          ? yield* NativeStorage.materialize(nativeStorage, operation.effect)
          : (yield* NativeStorage.materializeArguments(
              nativeStorage,
              operation.captures.map((capture) => capture.source),
            )).flat()),
        ...(yield* NativeStorage.materializeArguments(nativeStorage, operation.arguments)).flat(),
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
          ? yield* NativeDiagnosticOutcome.accept(
              call.synchronous.diagnostic,
              operation.outcome,
              called,
            )
          : yield* NativeSuspension.joinOutcome(
              suspension,
              operation,
              called,
              `effect_value_run${operation.destination.ordinal}`,
            )
      yield* NativeStorage.writeLocal(nativeStorage, operation.outcome.ordinal, outcomeValues)
      if (operation.propagationType === undefined) {
        yield* NativeStorage.writeLocal(
          nativeStorage,
          operation.destination.ordinal,
          yield* successPayload(
            context,
            outcomeValues,
            operation.outcome,
            operation.type,
            `effect_success${operation.destination.ordinal}`,
          ),
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
      yield* NativeStorage.writeJoin(
        nativeStorage,
        operation.destination,
        yield* successPayload(
          context,
          outcomeValues,
          operation.outcome,
          operation.type,
          `effect_success${operation.destination.ordinal}`,
        ),
      )
      yield* FunctionBody.branch(body, followingBlock)
      yield* LlvmBlock.setInsertionPoint(body, failureBlock)
      yield* NativeTermination.storePropagated(
        context.termination,
        operation.outcome,
        operation.provenance.span,
      )
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
          NativePayload.local(nativeStorage, release.local),
          `propagation_release${release.local.ordinal}`,
          undefined,
          release.initialization,
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
      yield* NativeReturn.complete(
        suspension.returns,
        Object.freeze(returned.slice(0, operation.propagationLaneCount)),
        'propagated_effect_value',
        operation.outcome,
      )
      yield* LlvmBlock.setInsertionPoint(body, followingBlock)
      // Both arms of this outcome dispatch reach here, so neither arm's cached
      // values are readable in the join. Reloading re-roots them at this block.
      yield* NativeStorage.reloadRoots(
        nativeStorage,
        `effect_value${operation.destination.ordinal}_following`,
        operation.destination,
      )
      yield* NativeStorage.reloadLocal(
        nativeStorage,
        operation.destination,
        `effect_value${operation.destination.ordinal}`,
      )
      break
    }
    case 'CatchEffect': {
      const target = declared.find((candidate) =>
        Mir.matchesInstance(candidate.fn, operation.runner, operation.runnerTypeArguments),
      )
      if (target === undefined) throw new RangeError('Backend cannot resolve Effect result runner')
      const reifyArguments = [
        ...(yield* NativeStorage.materialize(nativeStorage, operation.effect)),
        ...(yield* NativeStorage.materializeArguments(nativeStorage, operation.arguments)).flat(),
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
      yield* NativeStorage.writeLocal(nativeStorage, operation.outcome.ordinal, outcomeValues)
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
      yield* NativeStorage.writeLocal(
        nativeStorage,
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
      const successValues = yield* coerce(
        Object.freeze(outcomeValues.slice(1, 1 + successLaneCount)),
        Object.freeze(outcomeLanes.slice(1, 1 + successLaneCount)),
        operation.successShape.lanes,
        `effect_result${operation.destination.ordinal}_success`,
      )
      yield* NativeStorage.writeLocal(nativeStorage, operation.successValue.ordinal, successValues)
      yield* NativeStorage.storeMutable(nativeStorage, operation.successValue, successValues)
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
      const coercedFailureValues = yield* coerce(
        Object.freeze(failureValues),
        Object.freeze(failureLanes),
        operation.failureValueShape.lanes,
        `effect_result${operation.destination.ordinal}_failure`,
      )
      yield* NativeStorage.writeLocal(
        nativeStorage,
        operation.failureValue.ordinal,
        coercedFailureValues,
      )
      yield* NativeStorage.storeMutable(nativeStorage, operation.failureValue, coercedFailureValues)
      break
    }
  }
  context.state.checkOrdinal = checkOrdinal
})
