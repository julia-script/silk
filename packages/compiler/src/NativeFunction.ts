import type * as Builder from '@silk-effect/llvm/Builder'
import * as Constant from '@silk-effect/llvm/Constant'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import type * as LlvmError from '@silk-effect/llvm/LlvmError'
import type * as LlvmType from '@silk-effect/llvm/Type'
import type * as Value from '@silk-effect/llvm/Value'
import * as Effect from 'effect/Effect'
import * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import { destinationOf, type LinearBlock, opensRuntimeContinuation } from './MirLinearization.js'

export interface MutableRoots {
  readonly mutable: ReadonlySet<number>
  readonly address: ReadonlySet<number>
}

/** Per-function LLVM storage state used by mutation and address-taking lowering. */
export interface StorageContext {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly byteType: LlvmType.Type
  readonly offsetType: LlvmType.Type
  readonly fn: Mir.MirFunction
  readonly layout: Layout.Plan
  readonly mutableRoots: ReadonlySet<number>
  readonly mutableStorage: ReadonlyMap<number, ReadonlyArray<Value.Input>>
  readonly addressStorage: ReadonlyMap<number, Value.Input>
  readonly locals: Map<number, ReadonlyArray<Value.Input>>
  readonly valueLanesFor: (type: Mir.Type) => ReadonlyArray<Layout.CallingLane>
  readonly laneType: (lane: Layout.CallingLane) => LlvmType.Type
}

/** Reloads memory-backed roots at a control-flow join into the current SSA cache. */
export const reloadMutableRoots = Effect.fnUntraced(function* (
  context: StorageContext,
  tag: string,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  for (const root of [...context.mutableRoots].sort((left, right) => left - right)) {
    const storage = context.mutableStorage.get(root)
    if (storage === undefined) continue
    const loaded: Array<Value.Input> = []
    const logicalType = context.fn.localTypes.at(root)
    if (logicalType === undefined) throw new RangeError('Mutable root lost its type')
    for (const [lane, pointer] of storage.entries()) {
      const callingLane = context.valueLanesFor(logicalType).at(lane)
      if (callingLane === undefined) throw new RangeError('Mutable root lost a lane')
      loaded.push(
        yield* FunctionBody.load(
          context.body,
          context.laneType(callingLane),
          pointer,
          `mut${root}_${lane}_load_${tag}`,
        ),
      )
    }
    context.locals.set(root, Object.freeze(loaded))
  }
})

/** Stores every physical lane of an address-taken root into its stable byte storage. */
export const storeAddressRootValues = Effect.fnUntraced(function* (
  context: StorageContext,
  root: number,
  values: ReadonlyArray<Value.Input>,
  name: string,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  const base = context.addressStorage.get(root)
  const logicalType = context.fn.localTypes.at(root)
  if (base === undefined || logicalType === undefined)
    throw new RangeError(`Backend lost address storage for %${root}`)
  for (const [ordinal, lane] of context.valueLanesFor(logicalType).entries()) {
    const offset = Layout.laneOffset(context.layout, Mir.semanticType(logicalType), lane.path)
    const stored = values.at(ordinal)
    if (offset === undefined || stored === undefined)
      throw new RangeError(`Backend lost address lane ${ordinal} for %${root}`)
    yield* FunctionBody.store(
      context.body,
      stored,
      yield* FunctionBody.getElementPtr(
        context.body,
        context.byteType,
        base,
        [yield* Constant.integerUnsigned(context.builder, context.offsetType, BigInt(offset))],
        `${name}_${ordinal}_ptr`,
      ),
    )
  }
})

/** Finds locals that need stable stack storage across mutation, calls, and suspension control. */
export const mutableRoots = (
  fn: Mir.MirFunction,
  blocks: ReadonlyArray<LinearBlock>,
): MutableRoots => {
  const assignments = new Map<number, number>()
  for (const operation of blocks.flatMap((block) => block.operations)) {
    const destination = destinationOf(operation)
    if (destination !== undefined)
      assignments.set(destination.ordinal, (assignments.get(destination.ordinal) ?? 0) + 1)
  }
  const continuationLocals = blocks.flatMap((block) => {
    if (
      block.terminator._tag === 'Return' ||
      block.terminator._tag === 'Trap' ||
      block.terminator._tag === 'PropagateEffectFailure'
    )
      return []
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
  const runtimeContinuationDestinations = blocks.flatMap((block) =>
    block.operations.flatMap((operation) => {
      if (!opensRuntimeContinuation(operation) || operation._tag === 'Binary') return []
      const destination = destinationOf(operation)
      return destination === undefined ? [] : [destination.ordinal]
    }),
  )
  const borrowedCaptureRoots = new Set(
    blocks.flatMap((block) =>
      block.operations.flatMap((operation) =>
        operation._tag === 'MakeEffect' || operation._tag === 'MakeCallable'
          ? operation.captures.flatMap((capture, ordinal) =>
              (
                operation._tag === 'MakeEffect'
                  ? operation.type.environment.fields.at(ordinal)?.representation === 'Borrow'
                  : capture.access === 'Shared' || capture.access === 'Exclusive'
              )
                ? [capture.source.ordinal]
                : [],
            )
          : [],
      ),
    ),
  )
  const mutable = new Set([
    ...blocks.flatMap((block) =>
      block.operations.flatMap((operation) =>
        operation._tag === 'WritePlace' &&
        fn.localTypes.at(operation.root.ordinal)?._tag !== 'Slice'
          ? [operation.root.ordinal]
          : [],
      ),
    ),
    ...[...assignments].flatMap(([ordinal, count]) => (count > 1 ? [ordinal] : [])),
    ...continuationLocals,
    ...runtimeContinuationDestinations,
    ...blocks.flatMap((block) =>
      block.operations.flatMap((operation) =>
        operation._tag === 'RunEffectComposite'
          ? [operation.outcome.ordinal, operation.destination.ordinal]
          : [],
      ),
    ),
    ...(fn.suspension?.regions ?? []).flatMap((region) =>
      region._tag === 'RunSuspendableEffectRegion' && region.relay.state !== undefined
        ? [
            region.operation.destination.ordinal,
            region.operation.outcome.ordinal,
            ...region.relay.state.slots.map((slot) => slot.local.ordinal),
          ]
        : [],
    ),
    ...borrowedCaptureRoots,
  ])
  const address = new Set([
    ...blocks.flatMap((block) =>
      block.operations.flatMap((operation) =>
        operation._tag === 'BeginLoan' &&
        operation.sourceType._tag !== 'Slice' &&
        fn.localTypes.at(operation.root.ordinal)?._tag !== 'Reference'
          ? [operation.root.ordinal]
          : [],
      ),
    ),
    ...borrowedCaptureRoots,
  ])
  for (const root of address) mutable.add(root)
  return Object.freeze({ mutable, address })
}
