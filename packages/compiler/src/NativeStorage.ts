import * as Alignment from '@silklang/llvm/Alignment'
import type * as Builder from '@silklang/llvm/Builder'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import type * as LlvmType from '@silklang/llvm/Type'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import * as NativeType from './NativeType.js'
import * as NativePlace from './NativePlace.js'
import * as NativeValue from './NativeValue.js'

/** Mutable and addressable storage owned by one lowered native function body. */
export interface Context {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly byteType: LlvmType.Type
  readonly offsetType: LlvmType.Type
  readonly fn: Mir.MirFunction
  readonly layout: Layout.Plan
  readonly mutableRoots: ReadonlySet<number>
  /** Active block-local reference intervals, plus hidden suspension-frame inputs. */
  readonly blockRoots: Set<number>
  readonly mutableStorage: ReadonlyMap<number, ReadonlyArray<Value.Input>>
  readonly addressRoots: ReadonlySet<number>
  readonly addressStorage: Map<number, Value.Input>
  /** Run outcomes with no payload consumer; their diagnostic slots remain independently live. */
  readonly transientOutcomes: ReadonlySet<number>
  readonly locals: Map<number, NativeValue.NativeValue>
  readonly types: NativeType.LoweringContext
  readonly lanePointers: NativeLanePointer.Context
  readonly sequences: { materialize: number; reload: number }
}

/** Reads a local representation, rebinding its place to the current stack/frame base. */
export const readLocal = (context: Context, local: Mir.LocalId): NativeValue.NativeValue => {
  const type = context.fn.localTypes.at(local.ordinal)
  if (type !== undefined && NativeValue.classify(context.layout, type) === 'Empty')
    return Object.freeze({ _tag: 'Empty' })
  const base = context.addressStorage.get(local.ordinal)
  if (
    type !== undefined &&
    base !== undefined &&
    ['Place', 'BorrowedPlace'].includes(NativeValue.classify(context.layout, type))
  )
    return {
      ...NativePlace.make(context.layout, type, base),
      indirect: type._tag === 'EnvironmentBorrow',
    }
  const found = context.locals.get(local.ordinal)
  if (found === undefined) throw new RangeError(`Backend read undefined local %${local.ordinal}`)
  return found
}

/** Reads the only lane of a scalar lowered local. */
export const readScalar = Effect.fnUntraced(function* (context: Context, local: Mir.LocalId) {
  const type = context.fn.localTypes.at(local.ordinal)
  if (type === undefined || NativeType.valueLanesFor(context.types, type).length !== 1)
    throw new RangeError(`Backend expected scalar local %${local.ordinal}`)
  return yield* readLane(context, local, 0)
})

/** Loads a selected lane without creating an aggregate cache. */
export const readLane = Effect.fnUntraced(function* (
  context: Context,
  local: Mir.LocalId,
  ordinal: number,
) {
  const value = readLocal(context, local)
  if (value._tag === 'NativePlace')
    return yield* NativePlace.loadLane(
      value,
      context,
      ordinal,
      `place${local.ordinal}_${ordinal}_${context.sequences.materialize++}`,
    )
  const lane = value._tag === 'Direct' ? value.values.at(ordinal) : undefined
  if (lane === undefined) throw new RangeError('Local lost its selected lane')
  return lane
})

/** Updates one selected slot; only bounded direct descriptors are reconstructed. */
export const writeLane = Effect.fnUntraced(function* (
  context: Context,
  local: Mir.LocalId,
  ordinal: number,
  input: Value.Input,
) {
  const value = readLocal(context, local)
  if (value._tag === 'NativePlace') {
    yield* NativePlace.storeLane(
      value,
      context,
      ordinal,
      input,
      `write${local.ordinal}_${ordinal}_${context.sequences.materialize++}`,
    )
    return
  }
  if (value._tag !== 'Direct' || value.values.at(ordinal) === undefined)
    throw new RangeError('Local lost its writable lane')
  const values = [...value.values]
  values[ordinal] = input
  context.locals.set(local.ordinal, { _tag: 'Direct', values })
  yield* commitLocal(context, local)
  if (context.addressRoots.has(local.ordinal))
    yield* storeAddressValues(context, local.ordinal, values, `write${local.ordinal}`)
})

/** Explicit conversion from canonical storage to the compiler-planned boundary lanes. */
export const materialize = Effect.fnUntraced(function* (
  context: Context,
  local: Mir.LocalId,
): Effect.fn.Return<ReadonlyArray<Value.Input>, LlvmError.LlvmError> {
  const value = readLocal(context, local)
  if (value._tag === 'Empty') return Object.freeze([])
  if (value._tag === 'Direct') return value.values
  return yield* NativePlace.loadLanes(
    value,
    context,
    `place${local.ordinal}_${context.sequences.materialize++}`,
  )
})

/** Materializes an ordered parameter list at a call or transport boundary. */
export const materializeArguments = Effect.fnUntraced(function* (
  context: Context,
  locals: ReadonlyArray<Mir.LocalId>,
) {
  const arguments_: Array<ReadonlyArray<Value.Input>> = []
  for (const local of locals) arguments_.push(yield* materialize(context, local))
  return arguments_
})

/** Commits only direct mutable values; places already contain their current bytes. */
export const commitLocal = Effect.fnUntraced(function* (context: Context, root: Mir.LocalId) {
  const local = readLocal(context, root)
  if (local._tag === 'Direct') yield* storeMutable(context, root, local.values)
})

/** Initializes a predecessor's join destination, including aggregate bytes and scalar slots. */
export const writeJoin = Effect.fnUntraced(function* (
  context: Context,
  root: Mir.LocalId,
  values: ReadonlyArray<Value.Input>,
) {
  yield* writeLocal(context, root.ordinal, values)
  yield* commitLocal(context, root)
})

/** Selects writable storage without building an aggregate-wide pointer vector. */
export const slotPointer = Effect.fnUntraced(function* (
  context: Context,
  root: Mir.LocalId,
  ordinal: number,
  tag: string,
) {
  const type = context.fn.localTypes.at(root.ordinal)
  const base = context.addressStorage.get(root.ordinal)
  if (
    type !== undefined &&
    base !== undefined &&
    NativeValue.classify(context.layout, type) !== 'Direct'
  )
    return yield* NativePlace.lanePointer(
      {
        ...NativePlace.make(context.layout, type, base),
        indirect: type._tag === 'EnvironmentBorrow',
      },
      context,
      ordinal,
      tag,
    )
  const pointer = context.mutableStorage.get(root.ordinal)?.at(ordinal)
  if (pointer === undefined) throw new RangeError(`Local %${root.ordinal} has no slot ${ordinal}`)
  return pointer
})

/** Joins refresh bounded direct values only. Aggregate storage is already authoritative. */
export const reloadLocal = Effect.fnUntraced(function* (
  context: Context,
  root: Mir.LocalId,
  tag: string,
) {
  const type = context.fn.localTypes.at(root.ordinal)
  if (type === undefined) throw new RangeError('Joined local lost its type')
  if (NativeValue.classify(context.layout, type) !== 'Direct') return
  const values: Array<Value.Input> = []
  for (const [ordinal, lane] of NativeType.valueLanesFor(context.types, type).entries()) {
    values.push(
      yield* FunctionBody.load(
        context.body,
        NativeType.laneType(context.types, lane),
        yield* slotPointer(context, root, ordinal, `${tag}_${ordinal}_ptr`),
        `${tag}_${ordinal}`,
      ),
    )
  }
  context.locals.set(root.ordinal, Object.freeze({ _tag: 'Direct', values: Object.freeze(values) }))
})

/** Places an incoming boundary payload in its destination; aggregate lanes are never cached. */
export const writeLocal = Effect.fnUntraced(function* (
  context: Context,
  root: number,
  values: ReadonlyArray<Value.Input>,
) {
  if (context.transientOutcomes.has(root)) return
  const type = context.fn.localTypes.at(root)
  if (type === undefined) throw new RangeError('Local write lost its type')
  const kind = NativeValue.classify(context.layout, type)
  if (kind === 'Empty') {
    context.locals.set(root, Object.freeze({ _tag: 'Empty' }))
    return
  }
  if (kind === 'Direct') {
    context.locals.set(root, Object.freeze({ _tag: 'Direct', values }))
    return
  }
  const base = context.addressStorage.get(root)
  if (base === undefined) throw new RangeError(`Aggregate local %${root} has no destination`)
  const place = {
    ...NativePlace.make(context.layout, type, base),
    indirect: type._tag === 'EnvironmentBorrow',
  }
  yield* NativePlace.storeLanes(place, context, values, `place${root}_store`)
  context.locals.set(root, place)
})

/** A value assignment preserves independent destination bytes; MIR owns initialization transfer. */
export const copyLocal = Effect.fnUntraced(function* (
  context: Context,
  destination: Mir.LocalId,
  source: Mir.LocalId,
) {
  const value = readLocal(context, source)
  if (value._tag !== 'NativePlace') {
    yield* writeLocal(context, destination.ordinal, value._tag === 'Empty' ? [] : value.values)
    return
  }
  const type = context.fn.localTypes.at(destination.ordinal)
  if (type !== undefined && NativeValue.classify(context.layout, type) === 'Empty') return
  if (type !== undefined && NativeValue.classify(context.layout, type) === 'Direct') {
    yield* writeLocal(
      context,
      destination.ordinal,
      yield* NativePlace.loadLanes(value, context, `copy${destination.ordinal}`),
    )
    return
  }
  const base = context.addressStorage.get(destination.ordinal)
  if (type === undefined || base === undefined)
    throw new RangeError('Aggregate copy lost its destination')
  const place = {
    ...NativePlace.make(context.layout, type, base),
    indirect: type._tag === 'EnvironmentBorrow',
  }
  yield* NativePlace.copy(place, context, value)
  context.locals.set(destination.ordinal, place)
})

/** Initializes one canonical field directly from its source, without reading its siblings. */
export const constructField = Effect.fnUntraced(function* (
  context: Context,
  destination: Mir.LocalId,
  source: Mir.LocalId,
  offset: number,
  stored?:
    | Extract<Mir.Type, { readonly _tag: 'EffectValue' }>['storage']
    | Extract<Mir.Type, { readonly _tag: 'CallableValue' }>['storage'],
) {
  const value = readLocal(context, source)
  if (value._tag === 'Empty') return
  const parent = readLocal(context, destination)
  if (parent._tag !== 'NativePlace') throw new RangeError('Aggregate construction lost its place')
  let type = context.fn.localTypes.at(source.ordinal)
  if (type === undefined) throw new RangeError('Field source lost its type')
  if (stored?._tag === 'StoredEffectField' && type._tag === 'EffectValue')
    type = { ...type, storage: stored }
  if (stored?._tag === 'StoredCallableField' && type._tag === 'CallableValue')
    type = { ...type, storage: stored }
  const field = yield* NativePlace.project(
    parent,
    context,
    type,
    offset,
    `construct${destination.ordinal}_${offset}`,
    type._tag === 'EffectComposite' ? 'StoredComposite' : 'Value',
  )
  if (value._tag === 'NativePlace') yield* NativePlace.transfer(field, context, value)
  else
    for (const [ordinal, lane] of value.values.entries())
      yield* NativePlace.storeLane(
        field,
        context,
        ordinal,
        lane,
        `construct${destination.ordinal}_${offset}_${ordinal}`,
      )
})

/** Copies a selected canonical field into an independent destination, or loads a bounded primitive. */
export const projectLocal = Effect.fnUntraced(function* (
  context: Context,
  destination: Mir.LocalId,
  source: Mir.LocalId,
  offset: number,
) {
  const type = context.fn.localTypes.at(destination.ordinal)
  if (type === undefined) throw new RangeError('Projection lost its destination type')
  if (NativeValue.classify(context.layout, type) === 'Empty') return
  const parent = readLocal(context, source)
  if (parent._tag !== 'NativePlace') throw new RangeError('Projection lost its root place')
  const selected = yield* NativePlace.project(
    parent,
    context,
    type,
    offset,
    `project${destination.ordinal}`,
    type._tag === 'EffectComposite' ? 'StoredComposite' : 'Value',
  )
  yield* receivePlace(context, destination, selected)
})

/** Receives a selected memory value without reconstructing its enclosing aggregate. */
export const receivePlace = Effect.fnUntraced(function* (
  context: Context,
  destination: Mir.LocalId,
  selected: NativePlace.NativePlace,
) {
  const type = context.fn.localTypes.at(destination.ordinal)
  if (type === undefined) throw new RangeError('Memory read lost its destination type')
  if (NativeValue.classify(context.layout, type) === 'Empty') return
  const target = context.addressStorage.get(destination.ordinal)
  if (NativeValue.classify(context.layout, type) === 'Place') {
    if (target === undefined) throw new RangeError('Projection lost its aggregate destination')
    yield* NativePlace.transfer(NativePlace.make(context.layout, type, target), context, selected)
    return
  }
  const values: Array<Value.Input> = []
  for (const [ordinal] of NativeType.valueLanesFor(context.types, type).entries())
    values.push(
      yield* NativePlace.loadLane(
        selected,
        context,
        ordinal,
        `project${destination.ordinal}_${ordinal}`,
      ),
    )
  yield* writeLocal(context, destination.ordinal, Object.freeze(values))
})

/** Writes one selected value into existing memory, preserving independent value semantics. */
export const sendPlace = Effect.fnUntraced(function* (
  context: Context,
  destination: NativePlace.NativePlace,
  source: Mir.LocalId,
) {
  const value = readLocal(context, source)
  if (value._tag === 'Empty') return
  if (value._tag === 'NativePlace') yield* NativePlace.transfer(destination, context, value)
  else
    yield* NativePlace.storeLanes(
      destination,
      context,
      value.values,
      `write${source.ordinal}_${context.sequences.materialize++}`,
    )
})

/**
 * Reloads the current block's memory-backed roots at a control-flow join. Reloading every
 * mutable local in the function here creates a roots × joins expansion, even for locals
 * discarded long before this join. Each block activates inputs at entry and newly
 * defined locals at their first definition, then retires them after their last reference.
 * Hidden suspension-frame inputs remain pinned throughout the block.
 * A caller which immediately reloads its result explicitly can exclude that root here;
 * otherwise the general pass emits a second, unused load for every result lane.
 */
export const reloadRoots = Effect.fnUntraced(function* (
  context: Context,
  tag: string,
  explicitlyReloaded?: Mir.LocalId,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  for (const root of [...context.blockRoots].sort((left, right) => left - right)) {
    if (root === explicitlyReloaded?.ordinal) continue
    const storage = context.mutableStorage.get(root)
    if (storage === undefined) continue
    const loaded: Array<Value.Input> = []
    const logicalType = context.fn.localTypes.at(root)
    if (logicalType === undefined) throw new RangeError('Mutable root lost its type')
    for (const [lane, pointer] of storage.entries()) {
      const callingLane = NativeType.valueLanesFor(context.types, logicalType).at(lane)
      if (callingLane === undefined) throw new RangeError('Mutable root lost a lane')
      loaded.push(
        yield* FunctionBody.load(
          context.body,
          NativeType.laneType(context.types, callingLane),
          pointer,
          `mut${root}_${lane}_load_${tag}`,
        ),
      )
    }
    context.locals.set(root, Object.freeze({ _tag: 'Direct', values: Object.freeze(loaded) }))
  }
})

/**
 * Projects the canonical backing slots of an addressable local. Mutable and borrowed
 * access must use these same bytes: a separate mutable copy requires a full lane-vector
 * copy after every possible alias write. The self-hosted CLI emitted 443,375 such reloads.
 * Build these pointers where the base is established, and rebuild them when suspension
 * selects persistent frame storage instead of the initial stack allocation.
 */
export const addressLanes = Effect.fnUntraced(function* (context: Context, root: number) {
  const base = context.addressStorage.get(root)
  const logicalType = context.fn.localTypes.at(root)
  if (base === undefined || logicalType === undefined)
    throw new RangeError(`Backend lost address storage for %${root}`)
  const pointers: Array<Value.Input> = []
  for (const [ordinal, lane] of NativeType.valueLanesFor(context.types, logicalType).entries()) {
    const offset = NativeType.addressLaneOffset(context.layout, logicalType, lane, ordinal)
    if (offset === undefined) throw new RangeError(`Backend lost address lane ${ordinal}`)
    pointers.push(
      yield* NativeLanePointer.lanePointer(
        context.lanePointers,
        context.body,
        base,
        offset,
        `addr${root}_lane${ordinal}`,
      ),
    )
  }
  return Object.freeze(pointers)
})

/** Stores every physical lane of an address-taken root into its stable byte storage. */
export const storeAddressValues = Effect.fnUntraced(function* (
  context: Context,
  root: number,
  values: ReadonlyArray<Value.Input>,
  name: string,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  const base = context.addressStorage.get(root)
  const logicalType = context.fn.localTypes.at(root)
  if (base === undefined || logicalType === undefined)
    throw new RangeError(`Backend lost address storage for %${root}`)
  for (const [ordinal, lane] of NativeType.valueLanesFor(context.types, logicalType).entries()) {
    const offset = NativeType.addressLaneOffset(context.layout, logicalType, lane, ordinal)
    const stored = values.at(ordinal)
    if (offset === undefined || stored === undefined)
      throw new RangeError(`Backend lost address lane ${ordinal} for %${root}`)
    yield* FunctionBody.store(
      context.body,
      stored,
      yield* NativeLanePointer.lanePointer(
        context.lanePointers,
        context.body,
        base,
        offset,
        `${name}_${ordinal}_ptr`,
      ),
    )
  }
})

/** Commits one mutable direct scalar or bounded descriptor to its backing slots. */
export const storeMutable = Effect.fnUntraced(function* (
  context: Context,
  root: Mir.LocalId,
  values: ReadonlyArray<Value.Input>,
) {
  const storage = context.mutableStorage.get(root.ordinal)
  if (storage === undefined) return
  for (const [lane, pointer] of storage.entries()) {
    const stored = values.at(lane)
    if (stored === undefined)
      throw new RangeError(
        `Mutable root %${root.ordinal} lost physical lane ${lane} from ${values.length} values`,
      )
    yield* FunctionBody.store(context.body, stored, pointer)
  }
})

/**
 * Commits a direct scalar or bounded descriptor before its address escapes. Aggregate
 * places already name the authoritative bytes; they need no materialization or refresh.
 */
export const materializeAddressRoot = Effect.fnUntraced(function* (
  context: Context,
  root: Mir.LocalId,
) {
  const value = readLocal(context, root)
  if (value._tag === 'NativePlace' || value._tag === 'Empty') return
  const materializeId = context.sequences.materialize++
  if (
    !context.addressStorage.has(root.ordinal) ||
    context.fn.localTypes.at(root.ordinal) === undefined
  )
    throw new RangeError(`Backend lost address storage for %${root.ordinal}`)
  yield* storeAddressValues(
    context,
    root.ordinal,
    value.values,
    `addr${root.ordinal}_${materializeId}`,
  )
})

/** Resolves an addressable local's referent, including a rebound borrowed environment. */
export const addressOf = Effect.fnUntraced(function* (context: Context, root: Mir.LocalId) {
  const value = readLocal(context, root)
  if (value._tag === 'NativePlace')
    return yield* NativePlace.base(value, context, `borrow${root.ordinal}_base`)
  const base = context.addressStorage.get(root.ordinal)
  if (base === undefined) throw new RangeError('Addressable local lost its storage')
  return base
})

/** Allocates addressable storage for one root on demand and materializes it. */
export const ensureAddressRoot = Effect.fnUntraced(function* (context: Context, root: Mir.LocalId) {
  if (!context.addressStorage.has(root.ordinal)) {
    const logicalType = context.fn.localTypes.at(root.ordinal)
    const layout =
      logicalType === undefined ? undefined : NativeType.addressLayout(context.layout, logicalType)
    if (logicalType === undefined || layout === undefined)
      throw new RangeError(`Backend cannot materialize callable capture %${root.ordinal}`)
    context.addressStorage.set(
      root.ordinal,
      yield* FunctionBody.alloca(context.body, context.byteType, `callable_addr${root.ordinal}`, {
        count: yield* Constant.integerUnsigned(
          context.builder,
          context.offsetType,
          BigInt(layout.size),
        ),
        alignment: yield* Alignment.fromByteUnits(layout.alignment),
      }),
    )
  }
  yield* materializeAddressRoot(context, root)
})

/**
 * Refreshes addressable direct scalars and bounded descriptors after an alias write.
 * Aggregate places keep their canonical backing address and load selected fields when used.
 */
export const reloadAddressRoots = Effect.fnUntraced(function* (context: Context) {
  for (const root of [...context.addressRoots].sort((left, right) => left - right))
    yield* reloadAddressRoot(context, root)
})

/**
 * Refreshes a referenced direct local's bounded SSA value after an alias write. Other
 * blocks reload their direct inputs from the same backing slots on entry. Aggregate and
 * borrowed places are excluded: their current payload is read through their backing address.
 */
export const reloadAddressRoot = Effect.fnUntraced(function* (context: Context, root: number) {
  if (!context.blockRoots.has(root)) return
  const type = context.fn.localTypes.at(root)
  if (type !== undefined && NativeValue.classify(context.layout, type) !== 'Direct') return
  const reloadId = context.sequences.reload++
  const pointers = context.mutableStorage.get(root)
  const logicalType = context.fn.localTypes.at(root)
  if (pointers === undefined || logicalType === undefined)
    throw new RangeError(`Backend lost address storage for %${root}`)
  const values: Array<Value.Input> = []
  for (const [ordinal, lane] of NativeType.valueLanesFor(context.types, logicalType).entries()) {
    const pointer = pointers.at(ordinal)
    if (pointer === undefined) throw new RangeError(`Backend lost address lane ${ordinal}`)
    values.push(
      yield* FunctionBody.load(
        context.body,
        NativeType.laneType(context.types, lane),
        pointer,
        `reload${root}_${ordinal}_${reloadId}`,
      ),
    )
  }
  const frozen = Object.freeze(values)
  context.locals.set(root, Object.freeze({ _tag: 'Direct', values: frozen }))
})
