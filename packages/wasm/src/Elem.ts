/**
 * Element segments.
 *
 * An element segment carries a list of constant reference expressions. Items may be given as
 * `Func` handles (shorthand for a `ref.func` expression) or as explicit constant expressions.
 * Active segments copy into a table at instantiation; passive segments feed `table.init`;
 * declarative segments only make functions referenceable by `ref.func` inside bodies.
 *
 * @since 0.0.0
 */
import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Builder from './Builder.js'
import type * as ConstExpr from './ConstExpr.js'
import type * as Func from './Func.js'
import * as Instr from './Instr.js'
import * as ConstExprCheck from './internal/ConstExprCheck.js'
import * as Handle from './internal/Handle.js'
import * as ModuleState from './internal/ModuleState.js'
import * as NameCheck from './internal/NameCheck.js'
import type * as OwnedHandle from './internal/OwnedHandle.js'
import type * as Table from './Table.js'
import * as ValType from './ValType.js'
import { invalidState, type WasmError } from './WasmError.js'

/**
 * Opaque builder-owned identity for an element segment.
 *
 * @category element segments
 * @since 0.0.0
 */
export interface Elem extends Handle.Handle<'Elem'> {}

/**
 * One element item: a function handle shorthand or an explicit constant expression.
 *
 * @category element segments
 * @since 0.0.0
 */
export type Item = Func.Func | ConstExpr.ConstExpr

/**
 * Options accepted by element segment declarations.
 *
 * @category element segments
 * @since 0.0.0
 */
export interface Options {
  /** Name used for text identifiers and the binary `name` custom section. */
  readonly name?: string
}

/** @internal */
const normalize = (items: ReadonlyArray<Item>): ReadonlyArray<ReadonlyArray<Instr.Instr>> =>
  Object.freeze(
    items.map((item) =>
      Array.isArray(item)
        ? Object.freeze([...(item as ConstExpr.ConstExpr)])
        : Object.freeze([Instr.refFunc(item as Func.Func)]),
    ),
  )

/** @internal */
const checkItems = (
  state: ModuleState.MutableState,
  owner: OwnedHandle.Owner,
  items: ReadonlyArray<ReadonlyArray<Instr.Instr>>,
  refType: ValType.RefType,
  operation: string,
): Result.Result<void, WasmError> =>
  Result.gen(function* () {
    for (const item of items) {
      yield* ConstExprCheck.check(state, owner, item, refType, operation)
    }
  })

/** @internal */
const push = (
  state: ModuleState.MutableState,
  owner: OwnedHandle.Owner,
  entry: Omit<ModuleState.ElemEntry, never>,
): Elem => {
  const index = state.elems.length
  state.elems.push(entry)
  const handle: Elem = Handle.make('Elem', owner, index)
  state.elemHandles.push(handle)
  return handle
}

/**
 * Declares an active element segment copied into a table at instantiation.
 *
 * **Details**
 *
 * The offset is a constant expression producing an `i32`. The segment's reference type is the
 * table's reference type, and every item must be a constant expression of that type.
 *
 * @category element segments
 * @since 0.0.0
 */
export const active = Effect.fn('Elem.active')(function* (
  builder: Builder.Builder,
  table: Table.Table,
  offset: ConstExpr.ConstExpr,
  items: ReadonlyArray<Item>,
  options: Options = {},
): Effect.fn.Return<Elem, WasmError> {
  return yield* ModuleState.mutate(builder, 'Elem.active', (state, owner) =>
    Result.gen(function* () {
      const tableIndex = yield* Handle.resolve(owner, table, 'Table', 'Elem.active')
      const tableEntry = state.tables[tableIndex]
      if (tableEntry === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'Elem.active',
            message: 'Table entry is missing',
            state: tableIndex,
          }),
        )
      }
      const offsetType = tableEntry.addressType === 'i64' ? ValType.i64 : ValType.i32
      yield* ConstExprCheck.check(state, owner, offset, offsetType, 'Elem.active')
      yield* NameCheck.ensureFresh(state.elems, options.name, 'Elem.active')
      const normalized = normalize(items)
      yield* checkItems(state, owner, normalized, tableEntry.refType, 'Elem.active')
      return push(state, owner, {
        refType: tableEntry.refType,
        mode: { _tag: 'Active', table: tableIndex, offset: Object.freeze([...offset]) },
        items: normalized,
        name: options.name,
      })
    }),
  )
})

/**
 * Declares a passive element segment for later use with `table.init`.
 *
 * @category element segments
 * @since 0.0.0
 */
export const passive = Effect.fn('Elem.passive')(function* (
  builder: Builder.Builder,
  refType: ValType.RefType,
  items: ReadonlyArray<Item>,
  options: Options = {},
): Effect.fn.Return<Elem, WasmError> {
  return yield* ModuleState.mutate(builder, 'Elem.passive', (state, owner) =>
    Result.gen(function* () {
      yield* NameCheck.ensureFresh(state.elems, options.name, 'Elem.passive')
      const normalized = normalize(items)
      yield* checkItems(state, owner, normalized, refType, 'Elem.passive')
      return push(state, owner, {
        refType,
        mode: { _tag: 'Passive' },
        items: normalized,
        name: options.name,
      })
    }),
  )
})

/**
 * Declares a declarative element segment that makes functions referenceable by `ref.func`.
 *
 * @category element segments
 * @since 0.0.0
 */
export const declarative = Effect.fn('Elem.declarative')(function* (
  builder: Builder.Builder,
  refType: ValType.RefType,
  items: ReadonlyArray<Item>,
  options: Options = {},
): Effect.fn.Return<Elem, WasmError> {
  return yield* ModuleState.mutate(builder, 'Elem.declarative', (state, owner) =>
    Result.gen(function* () {
      yield* NameCheck.ensureFresh(state.elems, options.name, 'Elem.declarative')
      const normalized = normalize(items)
      yield* checkItems(state, owner, normalized, refType, 'Elem.declarative')
      return push(state, owner, {
        refType,
        mode: { _tag: 'Declarative' },
        items: normalized,
        name: options.name,
      })
    }),
  )
})
