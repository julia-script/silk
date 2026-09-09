import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import type * as Mir from './Mir.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import * as NativePlace from './NativePlace.js'
import * as NativeStorage from './NativeStorage.js'
import * as NativeType from './NativeType.js'

/** Projects a payload within its canonical frame extent, without retaining a stack-derived pointer. */
export const place = Effect.fnUntraced(function* (
  context: NativeStorage.Context,
  frame: Value.Input,
  field: Mir.CoroutineFramePayloadField,
  tag: string,
) {
  const value = NativePlace.make(
    context.layout,
    field.type,
    yield* NativeLanePointer.lanePointer(
      context.lanePointers,
      context.body,
      frame,
      field.offset,
      tag,
    ),
  )
  if (value.size > field.size || value.alignment > field.alignment)
    throw new RangeError(
      `Frame payload ${tag} (${field.type._tag}, ${field.access._tag}) is ${field.size}/${field.alignment} but its place is ${value.size}/${value.alignment}`,
    )
  return value
})

/**
 * Retains payloads in their planned frame extent. A borrowed environment retains only its
 * referent pointer; borrowed slices and strings retain their complete bounded descriptor.
 */
export const retain = Effect.fnUntraced(function* (
  context: NativeStorage.Context,
  frame: Value.Input,
  field: Mir.CoroutineFramePayloadField,
  tag: string,
) {
  const source = NativeStorage.readLocal(context, field.local)
  if (source._tag === 'Empty') return
  if (field.type._tag === 'EnvironmentBorrow') {
    if (source._tag !== 'NativePlace')
      throw new RangeError('Borrowed frame field lost its referent')
    yield* FunctionBody.store(
      context.body,
      yield* NativePlace.base(source, context, `${tag}_borrow`),
      yield* NativeLanePointer.lanePointer(
        context.lanePointers,
        context.body,
        frame,
        field.offset,
        tag,
      ),
    )
    return
  }
  const destination = yield* place(context, frame, field, tag)
  if (source._tag === 'NativePlace') yield* NativePlace.copy(destination, context, source)
  else
    for (const [ordinal, value] of source.values.entries())
      yield* NativePlace.storeLane(destination, context, ordinal, value, `${tag}_${ordinal}`)
})

/** Restores the live payload into the invocation's current destinations, not a lane cache. */
export const restore = Effect.fnUntraced(function* (
  context: NativeStorage.Context,
  frame: Value.Input,
  field: Mir.CoroutineFramePayloadField,
  tag: string,
) {
  if (field.type._tag === 'EnvironmentBorrow') {
    const slot = context.addressStorage.get(field.local.ordinal)
    if (slot === undefined) throw new RangeError('Borrowed frame payload lost its pointer slot')
    const pointer = yield* NativeLanePointer.lanePointer(
      context.lanePointers,
      context.body,
      frame,
      field.offset,
      `${tag}_slot`,
    )
    yield* FunctionBody.store(
      context.body,
      yield* FunctionBody.load(context.body, context.types.pointer, pointer, tag),
      slot,
    )
    return
  }
  const source = yield* place(context, frame, field, tag)
  const destination = NativeStorage.readLocal(context, field.local)
  if (destination._tag === 'Empty') return
  if (destination._tag === 'NativePlace') {
    yield* NativePlace.copy(destination, context, source)
    return
  }
  const values: Array<Value.Input> = []
  for (const [ordinal] of NativeType.valueLanesFor(context.types, field.type).entries())
    values.push(yield* NativePlace.loadLane(source, context, ordinal, `${tag}_${ordinal}`))
  yield* NativeStorage.writeJoin(context, field.local, values)
})
