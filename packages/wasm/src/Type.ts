import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Builder from './Builder.js'
import * as Handle from './internal/Handle.js'
import * as ModuleState from './internal/ModuleState.js'
import * as ValType from './ValType.js'
import { invalidState, type WasmError } from './WasmError.js'

/**
 * Opaque builder-owned identity for a structurally interned function type.
 *
 * @category types
 * @since 0.0.0
 */
export interface Type extends Handle.Handle<'Type'> {}

/**
 * Materialized parameter and result sequences of an interned function type.
 *
 * @category types
 * @since 0.0.0
 */
export interface FuncSignature {
  readonly params: ReadonlyArray<ValType.ValType>
  readonly results: ReadonlyArray<ValType.ValType>
}

/** @internal */
const key = (
  params: ReadonlyArray<ValType.ValType>,
  results: ReadonlyArray<ValType.ValType>,
): string => `${params.map(ValType.text).join(' ')}->${results.map(ValType.text).join(' ')}`

/**
 * Interns the function type `(params) -> (results)` and returns its handle.
 *
 * **Details**
 *
 * Types are interned structurally: two calls with the same parameter and result sequences return
 * the same handle, and the emitted type section contains one entry per distinct signature.
 *
 * **Example** (An `(i32, i32) -> i32` signature)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silk-effect/wasm/Builder'
 * import * as Type from '@silk-effect/wasm/Type'
 * import * as ValType from '@silk-effect/wasm/ValType'
 *
 * const program = Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   return yield* Type.func(builder, [ValType.i32, ValType.i32], [ValType.i32])
 * })
 * ```
 *
 * @category types
 * @since 0.0.0
 */
export const func = Effect.fn('Type.func')(function* (
  builder: Builder.Builder,
  params: ReadonlyArray<ValType.ValType>,
  results: ReadonlyArray<ValType.ValType>,
): Effect.fn.Return<Type, WasmError> {
  return yield* ModuleState.mutate(builder, 'Type.func', (state, owner) => {
    const typeKey = key(params, results)
    const existing = state.typeKeys.get(typeKey)
    if (existing !== undefined) {
      const handle = state.typeHandles[existing]
      if (handle === undefined) {
        return Result.fail(
          invalidState({
            operation: 'Type.func',
            message: 'Type table handle is missing',
            state: existing,
          }),
        )
      }
      return Result.succeed(handle)
    }
    const index = state.types.length
    state.types.push({
      params: Object.freeze([...params]),
      results: Object.freeze([...results]),
    })
    const handle: Type = Handle.make('Type', owner, index)
    state.typeHandles.push(handle)
    state.typeKeys.set(typeKey, index)
    return Result.succeed(handle)
  })
})

/**
 * Reads back the parameter and result sequences of an interned function type.
 *
 * @category types
 * @since 0.0.0
 */
export const signature = Effect.fn('Type.signature')(function* (
  builder: Builder.Builder,
  type: Type,
): Effect.fn.Return<FuncSignature, WasmError> {
  return yield* ModuleState.mutate(builder, 'Type.signature', (state, owner) =>
    Result.flatMap(Handle.resolve(owner, type, 'Type', 'Type.signature'), (index) => {
      const funcType = state.types[index]
      if (funcType === undefined) {
        return Result.fail(
          invalidState({
            operation: 'Type.signature',
            message: 'Type table entry is missing',
            state: index,
          }),
        )
      }
      return Result.succeed({ params: funcType.params, results: funcType.results })
    }),
  )
})
