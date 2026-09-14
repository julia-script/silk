import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as Hir from './Hir.js'
import * as Layout from './Layout.js'
import * as NativePlace from './NativePlace.js'
import type * as Mir from './Mir.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import * as NativeType from './NativeType.js'

export interface Context {
  readonly body: FunctionBody.FunctionBody
  readonly program: Mir.Module
  readonly types: NativeType.LoweringContext
  readonly lanePointers: NativeLanePointer.Context
}

/** Restores positional capture groups using the target's ordinary internal calling convention. */
export const capturedArguments = Effect.fnUntraced(function* (
  context: Context,
  type: Extract<Mir.Type, { readonly _tag: 'CallableValue' }>,
  values: ReadonlyArray<Value.Input>,
  tag: string,
) {
  const groups: Array<{
    readonly parameterOrdinal: number
    readonly values: ReadonlyArray<Value.Input>
  }> = []
  const borrowed =
    type.target?._tag === 'DeclarationCallableTarget' &&
    Hir.isAnonymousCallableId(type.target.declaration)
  let cursor = 0
  for (const field of type.environment?.fields ?? []) {
    const lanes = Layout.callableFieldLanes(context.program.layout, field)
    if (field.representation !== 'Borrow') {
      groups.push({
        parameterOrdinal: field.parameterOrdinal,
        values: Object.freeze(values.slice(cursor, cursor + lanes.length)),
      })
      cursor += lanes.length
      continue
    }
    const base = values.at(cursor++)
    if (base === undefined) throw new RangeError('Callable borrowed environment lost its pointer')
    if (borrowed) {
      groups.push({ parameterOrdinal: field.parameterOrdinal, values: Object.freeze([base]) })
      continue
    }
    const captured = yield* NativePlace.loadLanes(
      NativePlace.stored(context.program.layout, field.type, base),
      context,
      `${tag}_capture${field.ordinal}`,
    )
    groups.push({ parameterOrdinal: field.parameterOrdinal, values: Object.freeze(captured) })
  }
  return Object.freeze(groups)
})
