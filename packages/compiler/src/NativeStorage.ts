import * as Alignment from '@silklang/llvm/Alignment'
import type * as Builder from '@silklang/llvm/Builder'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import type * as LlvmType from '@silklang/llvm/Type'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as Layout from './Layout.js'
import * as LayoutVerify from './LayoutVerify.js'
import * as Mir from './Mir.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import * as NativeType from './NativeType.js'

/** Mutable and addressable storage owned by one lowered native function body. */
export interface Context {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly byteType: LlvmType.Type
  readonly offsetType: LlvmType.Type
  readonly fn: Mir.MirFunction
  readonly layout: Layout.Plan
  readonly mutableRoots: ReadonlySet<number>
  readonly mutableStorage: ReadonlyMap<number, ReadonlyArray<Value.Input>>
  readonly addressRoots: ReadonlySet<number>
  readonly addressStorage: Map<number, Value.Input>
  readonly locals: Map<number, ReadonlyArray<Value.Input>>
  readonly types: NativeType.LoweringContext
  readonly lanePointers: NativeLanePointer.Context
  readonly sequences: { materialize: number; reload: number }
}

/** Reads one lowered local's complete physical lane vector. */
export const readLocal = (
  context: Pick<Context, 'locals'>,
  local: Mir.LocalId,
): ReadonlyArray<Value.Input> => {
  const found = context.locals.get(local.ordinal)
  if (found === undefined) throw new RangeError(`Backend read undefined local %${local.ordinal}`)
  return found
}

/** Reads the only lane of a scalar lowered local. */
export const readScalar = (context: Pick<Context, 'locals'>, local: Mir.LocalId): Value.Input => {
  const found = readLocal(context, local)
  const scalar = found.at(0)
  if (scalar === undefined || found.length !== 1)
    throw new RangeError(`Backend expected scalar local %${local.ordinal}`)
  return scalar
}

/** Reloads memory-backed roots at a control-flow join into the current SSA cache. */
export const reloadRoots = Effect.fnUntraced(function* (
  context: Context,
  tag: string,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  for (const root of [...context.mutableRoots].sort((left, right) => left - right)) {
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
    context.locals.set(root, Object.freeze(loaded))
  }
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
    const offset = LayoutVerify.laneOffset(context.layout, Mir.semanticType(logicalType), lane.path)
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

/** Stores one mutable root's current lane vector. */
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

/** Copies the current SSA lanes of one root into its addressable storage. */
export const materializeAddressRoot = Effect.fnUntraced(function* (
  context: Context,
  root: Mir.LocalId,
) {
  const materializeId = context.sequences.materialize++
  if (
    !context.addressStorage.has(root.ordinal) ||
    context.fn.localTypes.at(root.ordinal) === undefined
  )
    throw new RangeError(`Backend lost address storage for %${root.ordinal}`)
  yield* storeAddressValues(
    context,
    root.ordinal,
    readLocal(context, root),
    `addr${root.ordinal}_${materializeId}`,
  )
})

/** Allocates addressable storage for one root on demand and materializes it. */
export const ensureAddressRoot = Effect.fnUntraced(function* (context: Context, root: Mir.LocalId) {
  if (!context.addressStorage.has(root.ordinal)) {
    const logicalType = context.fn.localTypes.at(root.ordinal)
    const layout =
      logicalType === undefined
        ? undefined
        : Layout.entry(context.layout, Mir.semanticType(logicalType))
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

/** Reloads one addressable root into SSA and mirrors it to mutable storage. */
export const reloadAddressRoot = Effect.fnUntraced(function* (context: Context, root: number) {
  const reloadId = context.sequences.reload++
  const base = context.addressStorage.get(root)
  const logicalType = context.fn.localTypes.at(root)
  if (base === undefined || logicalType === undefined)
    throw new RangeError(`Backend lost address storage for %${root}`)
  const values: Array<Value.Input> = []
  for (const [ordinal, lane] of NativeType.valueLanesFor(context.types, logicalType).entries()) {
    const offset = LayoutVerify.laneOffset(context.layout, Mir.semanticType(logicalType), lane.path)
    if (offset === undefined) throw new RangeError(`Backend lost address lane ${ordinal}`)
    values.push(
      yield* FunctionBody.load(
        context.body,
        NativeType.laneType(context.types, lane),
        yield* NativeLanePointer.lanePointer(
          context.lanePointers,
          context.body,
          base,
          offset,
          `reload${root}_${ordinal}_${reloadId}_ptr`,
        ),
        `reload${root}_${ordinal}_${reloadId}`,
      ),
    )
  }
  const frozen = Object.freeze(values)
  context.locals.set(root, frozen)
  yield* storeMutable(context, Object.freeze({ _tag: 'Local', ordinal: root }), frozen)
})
