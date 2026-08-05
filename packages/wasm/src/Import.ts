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
import * as TagActor from './Tag.js'
import type * as Type from './Type.js'
import type * as ValType from './ValType.js'
import { invalidInput, type WasmError } from './WasmError.js'

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
 * Options accepted by table imports.
 *
 * @category imports
 * @since 0.0.0
 */
export interface TableOptions extends Options {
  /** Address type used to index the table. Defaults to `'i32'`. */
  readonly addressType?: 'i32' | 'i64'
}

/**
 * Options accepted by memory imports.
 *
 * @category imports
 * @since 0.0.0
 */
export interface MemoryOptions extends Options {
  /** Marks the memory shared for use with atomic operations. Requires a maximum size. */
  readonly shared?: boolean
  /** Address type used to index the memory. Defaults to `'i32'`. */
  readonly addressType?: 'i32' | 'i64'
}

/**
 * Imports a function with an interned function type.
 *
 * **When to use**
 *
 * Use for any function the host supplies. The returned handle is the same kind `Func.declare`
 * returns, so an imported function is callable, exportable, and table-storable exactly like a
 * defined one — the only difference is that it has no body and `Func.define` rejects it.
 *
 * **Gotchas**
 *
 * `module` and `field` are the host's lookup path, not the local name: pass `options.name`
 * separately for the identifier that appears in text output and the `name` section.
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
  options: TableOptions = {},
): Effect.fn.Return<Table.Table, WasmError> {
  return yield* ModuleState.mutate(builder, 'Import.table', (state, owner) =>
    Result.gen(function* () {
      const addressType = options.addressType ?? 'i32'
      const checked = yield* LimitsCheck.check(limits, 'table', addressType, 'Import.table')
      yield* NameCheck.ensureFresh(state.tables, options.name, 'Import.table')
      const index = state.tables.length
      state.tables.push({
        refType,
        limits: checked,
        addressType,
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
  options: MemoryOptions = {},
): Effect.fn.Return<Memory.Memory, WasmError> {
  return yield* ModuleState.mutate(builder, 'Import.memory', (state, owner) =>
    Result.gen(function* () {
      const addressType = options.addressType ?? 'i32'
      const checked = yield* LimitsCheck.check(limits, 'memory', addressType, 'Import.memory')
      yield* NameCheck.ensureFresh(state.memories, options.name, 'Import.memory')
      const shared = options.shared ?? false
      if (shared && checked.max === undefined) {
        return yield* Result.fail(
          invalidInput({
            operation: 'Import.memory',
            message: 'A shared memory must declare a maximum size',
            input: limits,
          }),
        )
      }
      const index = state.memories.length
      state.memories.push({
        limits: checked,
        shared,
        addressType,
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
 * **Details**
 *
 * An imported global needs no initializer — the host supplies its value. An imported immutable
 * global is the only global a constant expression may read, which makes this the way to
 * parameterize segment offsets and other initializers at instantiation time.
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

/**
 * Imports an exception tag with an interned function type whose results are empty.
 *
 * @category imports
 * @since 0.0.0
 */
export const tag = Effect.fn('Import.tag')(function* (
  builder: Builder.Builder,
  module: string,
  field: string,
  type: Type.Type,
  options: Options = {},
): Effect.fn.Return<TagActor.Tag, WasmError> {
  return yield* ModuleState.mutate(builder, 'Import.tag', (state, owner) =>
    Result.gen(function* () {
      const typeIndex = yield* Handle.resolve(owner, type, 'Type', 'Import.tag')
      yield* TagActor.checkTagType(state, typeIndex, 'Import.tag')
      yield* NameCheck.ensureFresh(state.tags, options.name, 'Import.tag')
      const index = state.tags.length
      state.tags.push({
        typeIndex,
        name: options.name,
        importSource: { module, field },
      })
      const handle: TagActor.Tag = Handle.make('Tag', owner, index)
      state.tagHandles.push(handle)
      return handle
    }),
  )
})
