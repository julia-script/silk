/**
 * Type definitions: function, struct, and array types, interned as canonicalized recursive
 * groups.
 *
 * Every definition — including a plain `Type.func` — is a recursive group; singletons are
 * groups of one. Groups are canonicalized iso-recursively: structurally equivalent groups
 * resolve to the same handles and emit once, so handle identity remains the package's
 * type-equality primitive.
 *
 * @since 0.0.0
 */
import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Builder from './Builder.js'
import * as Handle from './internal/Handle.js'
import * as ModuleState from './internal/ModuleState.js'
import * as NameCheck from './internal/NameCheck.js'
import type * as OwnedHandle from './internal/OwnedHandle.js'
import * as Subtype from './internal/Subtype.js'
import type * as ValType from './ValType.js'
import { invalidInput, invalidState, type WasmError } from './WasmError.js'

/**
 * Opaque builder-owned identity for a canonicalized type.
 *
 * @category types
 * @since 0.0.0
 */
export interface Type extends Handle.Handle<'Type'> {}

/**
 * Materialized parameter and result sequences of a function type.
 *
 * @category types
 * @since 0.0.0
 */
export interface FuncSignature {
  readonly params: ReadonlyArray<ValType.ValType>
  readonly results: ReadonlyArray<ValType.ValType>
}

/**
 * One struct field or the array element: a storage type (a value type or the packed `i8`/`i16`
 * forms) and mutability.
 *
 * @category types
 * @since 0.0.0
 */
export interface Field {
  readonly storage: ValType.ValType | 'i8' | 'i16'
  readonly mutable?: boolean
}

/**
 * Options shared by all type definitions.
 *
 * **Details**
 *
 * A declared `supertype` must itself have been declared `final: false`, and the definition must
 * structurally match it — a struct may append fields, a function may widen parameters and narrow
 * results, and an immutable field may narrow. Mismatches are rejected at declaration.
 *
 * **Gotchas**
 *
 * Types are final by default, matching the binary format's default but not the intuition that a
 * fresh type is open. A type intended as a base must set `final: false` when it is created;
 * there is no way to reopen it afterwards.
 *
 * @category types
 * @since 0.0.0
 */
export interface Options {
  /** Name used for text identifiers and the binary `name` custom section. */
  readonly name?: string
  /** Declared supertype; must be non-final and structurally matched. */
  readonly supertype?: Type
  /** Whether the type is final (the default) or open for subtyping. */
  readonly final?: boolean
}

/**
 * One member definition inside a recursive group.
 *
 * @category types
 * @since 0.0.0
 */
export type Definition =
  | (Options & {
      readonly kind: 'func'
      readonly params: ReadonlyArray<ValType.ValType>
      readonly results: ReadonlyArray<ValType.ValType>
    })
  | (Options & { readonly kind: 'struct'; readonly fields: ReadonlyArray<Field> })
  | (Options & { readonly kind: 'array'; readonly field: Field })

/** @internal */
const storageOf = (field: Field): ModuleState.StorageType =>
  field.storage === 'i8' || field.storage === 'i16'
    ? { _tag: 'Packed', width: field.storage }
    : field.storage

/** @internal */
const fieldOf = (field: Field): ModuleState.FieldType =>
  Object.freeze({ storage: storageOf(field), mutable: field.mutable ?? false })

/** @internal */
const compositeOf = (definition: Definition): ModuleState.CompositeType => {
  switch (definition.kind) {
    case 'func':
      return {
        _tag: 'Func',
        params: Object.freeze([...definition.params]),
        results: Object.freeze([...definition.results]),
      }
    case 'struct':
      return { _tag: 'Struct', fields: Object.freeze(definition.fields.map(fieldOf)) }
    case 'array':
      return { _tag: 'Array', field: fieldOf(definition.field) }
  }
}

/** @internal */
const storageKey = (resolve: (type: Type) => number, storage: ModuleState.StorageType): string => {
  if ('width' in storage && storage._tag === 'Packed') return storage.width
  const valType = storage as ValType.ValType
  if (valType._tag !== 'Ref') return valType._tag
  const heap =
    valType.heapType._tag === 'Abstract'
      ? valType.heapType.kind
      : `t${resolve(valType.heapType.type)}`
  return `ref:${valType.nullable ? 'n' : ''}${heap}`
}

/** @internal */
const compositeKey = (
  resolve: (type: Type) => number,
  composite: ModuleState.CompositeType,
): string => {
  switch (composite._tag) {
    case 'Func': {
      const value = (type: ValType.ValType) => storageKey(resolve, type)
      return `f(${composite.params.map(value).join(',')})(${composite.results.map(value).join(',')})`
    }
    case 'Struct':
      return `s(${composite.fields
        .map((field) => `${field.mutable ? 'm' : 'c'}${storageKey(resolve, field.storage)}`)
        .join(',')})`
    case 'Array':
      return `a(${composite.field.mutable ? 'm' : 'c'}${storageKey(resolve, composite.field.storage)})`
  }
}

/** @internal */
const defineGroup = (
  state: ModuleState.MutableState,
  owner: OwnedHandle.Owner,
  count: number,
  define: (members: ReadonlyArray<Type>) => ReadonlyArray<Definition>,
  operation: string,
): Result.Result<ReadonlyArray<Type>, WasmError> =>
  Result.gen(function* () {
    if (!Number.isInteger(count) || count < 1) {
      return yield* Result.fail(
        invalidInput({
          operation,
          message: 'A recursive group needs at least one member',
          input: count,
        }),
      )
    }
    const start = state.types.length
    const provisional = Array.from({ length: count }, (_, position) =>
      Handle.make<'Type'>('Type', owner, start + position),
    )
    const definitions = define(provisional)
    const rollback = (): void => {
      for (const handle of provisional) Handle.invalidate(handle)
    }
    if (definitions.length !== count) {
      rollback()
      return yield* Result.fail(
        invalidInput({
          operation,
          message: `The group must define exactly ${count} member(s)`,
          input: definitions.length,
        }),
      )
    }

    // Resolve a member handle to its (provisional) type index for keying and storage.
    const resolveIndex = (type: Type): number => {
      const resolved = Handle.resolve(owner, type, 'Type', operation)
      if (Result.isFailure(resolved)) {
        throw new TypeKeyAbort(resolved.failure)
      }
      return Result.getOrThrow(resolved)
    }
    const keyed = (index: number): string => (index >= start ? `r${index - start}` : `x${index}`)

    let key: string
    const members: Array<ModuleState.SubType> = []
    try {
      for (const definition of definitions) {
        const composite = compositeOf(definition)
        const supertype =
          definition.supertype === undefined ? undefined : resolveIndex(definition.supertype)
        members.push({
          composite,
          supertype,
          final: definition.final ?? true,
          name: definition.name,
        })
      }
      key = members
        .map(
          (member) =>
            `${compositeKey((type) => resolveIndex(type), member.composite)}|${
              member.supertype === undefined ? '' : keyed(member.supertype)
            }|${member.final ? 'f' : 'o'}`,
        )
        .join(';')
        .replace(/t(\d+)/g, (whole, digits: string) => {
          const index = Number(digits)
          return index >= start ? `r${index - start}` : whole
        })
    } catch (failure) {
      rollback()
      if (failure instanceof TypeKeyAbort) return yield* Result.fail(failure.error)
      throw failure
    }

    const existing = state.typeKeys.get(key)
    if (existing !== undefined) {
      rollback()
      const handles = Array.from({ length: count }, (_, position) => {
        const handle = state.typeHandles[existing + position]
        if (handle === undefined) {
          throw new TypeKeyAbort(
            invalidState({ operation, message: 'Type table handle is missing', state: existing }),
          )
        }
        return handle
      })
      return Object.freeze(handles)
    }

    // Validate names and supertypes against the provisional table.
    state.types.push(...members)
    for (const [position, member] of members.entries()) {
      const revert = () => {
        state.types.length = start
        rollback()
      }
      const fresh = NameCheck.ensureFresh(
        state.types.filter((_, index) => index !== start + position),
        member.name,
        operation,
      )
      if (Result.isFailure(fresh)) {
        revert()
        return yield* Result.fail(fresh.failure)
      }
      if (member.supertype !== undefined) {
        const parent = state.types[member.supertype]
        if (parent === undefined || parent.final) {
          revert()
          return yield* Result.fail(
            invalidInput({
              operation,
              message: 'A declared supertype must exist and be non-final',
              input: member.supertype,
            }),
          )
        }
        if (!Subtype.compositeMatches(state, member.composite, parent.composite)) {
          revert()
          return yield* Result.fail(
            invalidInput({
              operation,
              message: 'A declared supertype must structurally match the definition',
              input: position,
            }),
          )
        }
      }
    }

    state.typeHandles.push(...provisional)
    state.typeKeys.set(key, start)
    state.recGroups.push({ start, length: count })
    return Object.freeze(provisional)
  })

class TypeKeyAbort {
  constructor(readonly error: WasmError) {}
}

/** @internal */
const single = (
  builder: Builder.Builder,
  operation: string,
  definition: Definition,
): Effect.Effect<Type, WasmError> =>
  ModuleState.mutate(builder, operation, (state, owner) =>
    Result.gen(function* () {
      const handles = yield* defineGroup(state, owner, 1, () => [definition], operation)
      const handle = handles[0]
      if (handle === undefined) {
        return yield* Result.fail(
          invalidState({ operation, message: 'Type group produced no handle', state: undefined }),
        )
      }
      return handle
    }),
  )

/**
 * Interns the function type `(params) -> (results)` and returns its handle.
 *
 * **When to use**
 *
 * Use for every function signature, including tag payload types. There is no cost to requesting
 * the same signature repeatedly, so callers do not need to cache handles themselves.
 *
 * **Details**
 *
 * Types are interned structurally: two calls with the same parameter and result sequences return
 * the same handle, and the emitted type section contains one entry per distinct definition.
 *
 * **Gotchas**
 *
 * Because interning is by structure, two signatures that happen to agree are the same type and
 * share one handle. The name of the first definition wins: passing a different `name` for an
 * already-interned structure succeeds, returns the existing handle, and discards the new name
 * rather than reporting a conflict.
 *
 * **Example** (An `(i32, i32) -> i32` signature)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silklang/wasm/Builder'
 * import * as Type from '@silklang/wasm/Type'
 * import * as ValType from '@silklang/wasm/ValType'
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
  options: Options = {},
): Effect.fn.Return<Type, WasmError> {
  return yield* single(builder, 'Type.func', { kind: 'func', params, results, ...options })
})

/**
 * Interns a struct type with the given fields.
 *
 * **Details**
 *
 * Fields are positional; `struct.get`/`struct.set` address them by index in declaration order.
 * A field is immutable unless `mutable` is set, and packed `'i8'`/`'i16'` storage is read back
 * through the signed or unsigned accessor rather than directly.
 *
 * **Gotchas**
 *
 * A subtype may append fields but never reorder or retype the ones it inherits, so field order
 * is part of the contract with every future subtype. To be subtyped at all, a struct must be
 * declared with `final: false`.
 *
 * **Example** (A mutable point)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silklang/wasm/Builder'
 * import * as Type from '@silklang/wasm/Type'
 * import * as ValType from '@silklang/wasm/ValType'
 *
 * const program = Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   return yield* Type.struct(builder, [
 *     { storage: ValType.f64, mutable: true },
 *     { storage: ValType.f64, mutable: true },
 *   ], { name: 'point' })
 * })
 * ```
 *
 * @category types
 * @since 0.0.0
 */
export const struct = Effect.fn('Type.struct')(function* (
  builder: Builder.Builder,
  fields: ReadonlyArray<Field>,
  options: Options = {},
): Effect.fn.Return<Type, WasmError> {
  return yield* single(builder, 'Type.struct', { kind: 'struct', fields, ...options })
})

/**
 * Interns an array type with the given element field.
 *
 * **Details**
 *
 * One field describes every element. Arrays are dynamically sized — the length belongs to each
 * allocation, not to the type — and `array.len` reads it back. As with struct fields, packed
 * `'i8'`/`'i16'` storage is read through the signed or unsigned accessor.
 *
 * **Gotchas**
 *
 * A mutable element makes the array invariant: a mutable array type has no array subtypes other
 * than itself, because its element would have to vary in both directions at once.
 *
 * @category types
 * @since 0.0.0
 */
export const array = Effect.fn('Type.array')(function* (
  builder: Builder.Builder,
  field: Field,
  options: Options = {},
): Effect.fn.Return<Type, WasmError> {
  return yield* single(builder, 'Type.array', { kind: 'array', field, ...options })
})

/**
 * Defines a recursive group of mutually referring types.
 *
 * **When to use**
 *
 * Use when definitions must refer to each other — a node type holding a reference to itself, or
 * a pair of types that name each other. A type that only refers to types already defined does
 * not need a group; use the singleton constructors instead.
 *
 * **Details**
 *
 * The callback receives `count` forward handles usable inside the definitions it returns (for
 * example, a struct field referencing a sibling member). The group commits atomically: it is
 * canonicalized as a unit, a structurally equivalent existing group is reused, and on reuse or
 * failure the forward handles are invalidated so no half-defined type escapes.
 *
 * **Gotchas**
 *
 * Use only the handles this function returns. The forward handles passed to the callback are
 * provisional: when the group is reused or rejected they are invalidated, so one captured outside
 * the callback and used later fails as a stale handle. The returned array is the group's real
 * identity, and equivalence is judged over the whole group — a group is reused only when every
 * member matches in order.
 *
 * **Example** (A linked-list node referring to itself)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silklang/wasm/Builder'
 * import * as Type from '@silklang/wasm/Type'
 * import * as ValType from '@silklang/wasm/ValType'
 *
 * const program = Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   const members = yield* Type.rec(builder, 1, (group) =>
 *     group.map((self): Type.Definition => ({
 *       kind: 'struct',
 *       name: 'node',
 *       fields: [
 *         { storage: ValType.i32 },
 *         { storage: ValType.refNull(self), mutable: true },
 *       ],
 *     })),
 *   )
 *   return members[0]
 * })
 * ```
 *
 * @category types
 * @since 0.0.0
 */
export const rec = Effect.fn('Type.rec')(function* (
  builder: Builder.Builder,
  count: number,
  define: (members: ReadonlyArray<Type>) => ReadonlyArray<Definition>,
): Effect.fn.Return<ReadonlyArray<Type>, WasmError> {
  return yield* ModuleState.mutate(builder, 'Type.rec', (state, owner) =>
    defineGroup(state, owner, count, define, 'Type.rec'),
  )
})

/**
 * Reads back the parameter and result sequences of a function type.
 *
 * **When to use**
 *
 * Use when generated code must inspect a signature it was handed — computing a call's argument
 * count, or deriving a wrapper's own type.
 *
 * **Gotchas**
 *
 * Only function types have a signature: a struct or array handle fails here rather than
 * returning empty sequences.
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
      const funcType = ModuleState.funcTypeAt(state.types, index)
      if (funcType === undefined) {
        return Result.fail(
          invalidState({
            operation: 'Type.signature',
            message: 'The type is not a function type',
            state: index,
          }),
        )
      }
      return Result.succeed(funcType)
    }),
  )
})
