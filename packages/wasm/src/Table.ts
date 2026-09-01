import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Builder from './Builder.js'
import * as Handle from './internal/Handle.js'
import * as LimitsCheck from './internal/LimitsCheck.js'
import * as ModuleState from './internal/ModuleState.js'
import * as NameCheck from './internal/NameCheck.js'
import type * as Limits from './Limits.js'
import type * as ValType from './ValType.js'
import type { WasmError } from './WasmError.js'

/**
 * Opaque builder-owned identity for an imported or defined table.
 *
 * @category tables
 * @since 0.0.0
 */
export interface Table extends Handle.Handle<'Table'> {}

/**
 * Options accepted by table declarations.
 *
 * @category tables
 * @since 0.0.0
 */
export interface Options {
  /** Name used for text identifiers and the binary `name` custom section. */
  readonly name?: string
  /** Address type used to index the table. Defaults to `'i32'`. */
  readonly addressType?: 'i32' | 'i64'
}

/**
 * Declares a defined table holding one reference type.
 *
 * **Details**
 *
 * `limits` count elements. Every slot holds the same reference type, and a table declared with a
 * nullable type starts filled with null.
 *
 * **Gotchas**
 *
 * A non-nullable element type has no default value, so the table needs an initial value the
 * builder cannot supply. Such a module encodes successfully and is rejected by the host at
 * instantiation ("needs initial value"), not by the emitters — prefer a nullable element type
 * unless an active element segment covers the whole table.
 *
 * **Example** (A one-element function table)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silklang/wasm/Builder'
 * import * as Table from '@silklang/wasm/Table'
 * import * as ValType from '@silklang/wasm/ValType'
 *
 * const program = Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   return yield* Table.make(builder, ValType.funcref, { min: 1 })
 * })
 * ```
 *
 * @category tables
 * @since 0.0.0
 */
export const make = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  refType: ValType.RefType,
  limits: Limits.Limits,
  options: Options = {},
): Effect.fn.Return<Table, WasmError> {
  return yield* ModuleState.mutate(builder, 'Table.make', (state, owner) =>
    Result.gen(function* () {
      const addressType = options.addressType ?? 'i32'
      const checked = yield* LimitsCheck.check(limits, 'table', addressType, 'Table.make')
      yield* NameCheck.ensureFresh(state.tables, options.name, 'Table.make')
      const index = state.tables.length
      state.tables.push({
        refType,
        limits: checked,
        addressType,
        name: options.name,
        importSource: undefined,
      })
      const handle: Table = Handle.make('Table', owner, index)
      state.tableHandles.push(handle)
      return handle
    }),
  )
})
