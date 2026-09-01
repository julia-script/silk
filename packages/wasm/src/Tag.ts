import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Builder from './Builder.js'
import * as Handle from './internal/Handle.js'
import * as ModuleState from './internal/ModuleState.js'
import * as NameCheck from './internal/NameCheck.js'
import type * as Type from './Type.js'
import { invalidInput, type WasmError } from './WasmError.js'

/**
 * Opaque builder-owned identity for an imported or defined exception tag.
 *
 * @category tags
 * @since 0.0.0
 */
export interface Tag extends Handle.Handle<'Tag'> {}

/**
 * Options accepted by tag declarations.
 *
 * @category tags
 * @since 0.0.0
 */
export interface Options {
  /** Name used for text identifiers and the binary `name` custom section. */
  readonly name?: string
}

/** @internal */
export const checkTagType = (
  state: ModuleState.MutableState,
  typeIndex: number,
  operation: string,
): Result.Result<void, WasmError> => {
  const funcType = ModuleState.funcTypeAt(state.types, typeIndex)
  if (funcType === undefined || funcType.results.length > 0) {
    return Result.fail(
      invalidInput({
        operation,
        message: 'A tag type must be a function type with no results',
        input: funcType?.results.length,
      }),
    )
  }
  return Result.succeed(undefined)
}

/**
 * Declares a defined exception tag with an interned function type.
 *
 * **Details**
 *
 * The tag's type describes the exception payload: its parameters are the values carried by a
 * `throw` and delivered to catching labels. The type's result sequence must be empty.
 *
 * **Gotchas**
 *
 * Tags are identities, not structures: two tags declared with the same payload type are
 * different exceptions, and a `catch` matches only the exact tag it names. To catch an exception
 * across a module boundary, import and export the one tag rather than redeclaring it.
 *
 * **Example** (A tag carrying one error code)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silklang/wasm/Builder'
 * import * as Tag from '@silklang/wasm/Tag'
 * import * as Type from '@silklang/wasm/Type'
 * import * as ValType from '@silklang/wasm/ValType'
 *
 * const program = Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   const payload = yield* Type.func(builder, [ValType.i32], [])
 *   return yield* Tag.make(builder, payload, { name: 'error' })
 * })
 * ```
 *
 * @category tags
 * @since 0.0.0
 */
export const make = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  type: Type.Type,
  options: Options = {},
): Effect.fn.Return<Tag, WasmError> {
  return yield* ModuleState.mutate(builder, 'Tag.make', (state, owner) =>
    Result.gen(function* () {
      const typeIndex = yield* Handle.resolve(owner, type, 'Type', 'Tag.make')
      yield* checkTagType(state, typeIndex, 'Tag.make')
      yield* NameCheck.ensureFresh(state.tags, options.name, 'Tag.make')
      const index = state.tags.length
      state.tags.push({
        typeIndex,
        name: options.name,
        importSource: undefined,
      })
      const handle: Tag = Handle.make('Tag', owner, index)
      state.tagHandles.push(handle)
      return handle
    }),
  )
})
