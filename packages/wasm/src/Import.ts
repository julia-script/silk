/**
 * Imported functions, tables, memories, and globals.
 *
 * Imports return the same handle kinds as definitions, so an imported entity is usable anywhere
 * a defined one is. Imported entities occupy the lowest indices of each index space in the
 * emitted module regardless of declaration order; the builder computes that numbering at
 * emission, so declaring imports late never invalidates existing references.
 *
 * @since 0.0.0
 */
import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Builder from './Builder.js'
import type * as Func from './Func.js'
import type * as Global from './Global.js'
import * as Handle from './internal/Handle.js'
import * as LimitsCheck from './internal/LimitsCheck.js'
import * as ModuleState from './internal/ModuleState.js'
import * as NameCheck from './internal/NameCheck.js'
import type * as Limits from './Limits.js'
import type * as Memory from './Memory.js'
import type * as Table from './Table.js'
import type * as Type from './Type.js'
import type * as ValType from './ValType.js'
import type { WasmError } from './WasmError.js'

/**
 * Options accepted by import declarations.
 *
 * @category imports
 * @since 0.0.0
 */
export interface Options {
  /** Name used for text identifiers and the binary `name` custom section. */
  readonly name?: string
}

/**
 * Imports a function with an interned function type.
 *
 * **Example** (Importing a logging function)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silk-effect/wasm/Builder'
 * import * as Import from '@silk-effect/wasm/Import'
 * import * as Type from '@silk-effect/wasm/Type'
 * import * as ValType from '@silk-effect/wasm/ValType'
 *
 * const program = Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   const signature = yield* Type.func(builder, [ValType.i32], [])
 *   return yield* Import.func(builder, 'env', 'log', signature)
 * })
 * ```
 *
 * @category imports
 * @since 0.0.0
 */
export const func = Effect.fn('Import.func')(function* (
  builder: Builder.Builder,
  module: string,
  field: string,
  type: Type.Type,
  options: Options = {},
): Effect.fn.Return<Func.Func, WasmError> {
  return yield* ModuleState.mutate(builder, 'Import.func', (state, owner) =>
    Result.gen(function* () {
      const typeIndex = yield* Handle.resolve(owner, type, 'Type', 'Import.func')
      yield* NameCheck.ensureFresh(state.funcs, options.name, 'Import.func')
      const index = state.funcs.length
      state.funcs.push({
        typeIndex,
        name: options.name,
        importSource: { module, field },
        definition: undefined,
      })
      const handle: Func.Func = Handle.make('Func', owner, index)
      state.funcHandles.push(handle)
      return handle
    }),
  )
})

/**
 * Imports a table holding one reference type.
 *
 * @category imports
 * @since 0.0.0
 */
export const table = Effect.fn('Import.table')(function* (
  builder: Builder.Builder,
  module: string,
  field: string,
  refType: ValType.RefType,
  limits: Limits.Limits,
  options: Options = {},
): Effect.fn.Return<Table.Table, WasmError> {
  return yield* ModuleState.mutate(builder, 'Import.table', (state, owner) =>
    Result.gen(function* () {
      const checked = yield* LimitsCheck.check(limits, 0xffffffff, 'Import.table')
      yield* NameCheck.ensureFresh(state.tables, options.name, 'Import.table')
      const index = state.tables.length
      state.tables.push({
        refType,
        limits: checked,
        name: options.name,
        importSource: { module, field },
      })
      const handle: Table.Table = Handle.make('Table', owner, index)
      state.tableHandles.push(handle)
      return handle
    }),
  )
})

/**
 * Imports a memory sized in 64 KiB pages.
 *
 * @category imports
 * @since 0.0.0
 */
export const memory = Effect.fn('Import.memory')(function* (
  builder: Builder.Builder,
  module: string,
  field: string,
  limits: Limits.Limits,
  options: Options = {},
): Effect.fn.Return<Memory.Memory, WasmError> {
  return yield* ModuleState.mutate(builder, 'Import.memory', (state, owner) =>
    Result.gen(function* () {
      const checked = yield* LimitsCheck.check(limits, 65536, 'Import.memory')
      yield* NameCheck.ensureFresh(state.memories, options.name, 'Import.memory')
      const index = state.memories.length
      state.memories.push({
        limits: checked,
        name: options.name,
        importSource: { module, field },
      })
      const handle: Memory.Memory = Handle.make('Memory', owner, index)
      state.memoryHandles.push(handle)
      return handle
    }),
  )
})

/**
 * Imports a global with a value type and mutability.
 *
 * @category imports
 * @since 0.0.0
 */
export const global = Effect.fn('Import.global')(function* (
  builder: Builder.Builder,
  module: string,
  field: string,
  valType: ValType.ValType,
  mutable: boolean,
  options: Options = {},
): Effect.fn.Return<Global.Global, WasmError> {
  return yield* ModuleState.mutate(builder, 'Import.global', (state, owner) =>
    Result.gen(function* () {
      yield* NameCheck.ensureFresh(state.globals, options.name, 'Import.global')
      const index = state.globals.length
      state.globals.push({
        valType,
        mutable,
        name: options.name,
        importSource: { module, field },
        init: undefined,
      })
      const handle: Global.Global = Handle.make('Global', owner, index)
      state.globalHandles.push(handle)
      return handle
    }),
  )
})
