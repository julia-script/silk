/**
 * WebAssembly value types as plain immutable data.
 *
 * Value types are context-free values, not builder-owned handles — except that a reference type
 * may point at a concrete type through its builder-owned `Type` handle. Reference types are
 * parameterized: `ref null? <heaptype>`, where the heap type is an abstract kind or a concrete
 * type handle. The classic shorthands (`funcref`, `externref`, `exnref`, `anyref`, …) are
 * ordinary values of that general form.
 *
 * @since 0.0.0
 */
import type * as Type from './Type.js'

/**
 * A WebAssembly number type.
 *
 * @category value types
 * @since 0.0.0
 */
export type NumType =
  | { readonly _tag: 'I32' }
  | { readonly _tag: 'I64' }
  | { readonly _tag: 'F32' }
  | { readonly _tag: 'F64' }

/**
 * A WebAssembly vector type.
 *
 * @category value types
 * @since 0.0.0
 */
export interface VecType {
  readonly _tag: 'V128'
}

/**
 * The abstract heap types, including the bottom types of each hierarchy.
 *
 * @category value types
 * @since 0.0.0
 */
export type AbstractHeapKind =
  | 'any'
  | 'eq'
  | 'i31'
  | 'struct'
  | 'array'
  | 'func'
  | 'extern'
  | 'exn'
  | 'none'
  | 'nofunc'
  | 'noextern'
  | 'noexn'

/**
 * A heap type: an abstract kind or a concrete builder-owned type.
 *
 * @category value types
 * @since 0.0.0
 */
export type HeapType =
  | { readonly _tag: 'Abstract'; readonly kind: AbstractHeapKind }
  | { readonly _tag: 'Concrete'; readonly type: Type.Type }

/**
 * Shorthand inputs accepted wherever a heap type is expected: an abstract kind name or a
 * concrete type handle.
 *
 * @category value types
 * @since 0.0.0
 */
export type HeapTypeInput = HeapType | AbstractHeapKind | Type.Type

/**
 * A parameterized WebAssembly reference type.
 *
 * @category value types
 * @since 0.0.0
 */
export interface RefType {
  readonly _tag: 'Ref'
  readonly nullable: boolean
  readonly heapType: HeapType
}

/**
 * Any WebAssembly value type.
 *
 * @category value types
 * @since 0.0.0
 */
export type ValType = NumType | VecType | RefType

/** @internal */
export const heapType = (input: HeapTypeInput): HeapType => {
  if (typeof input === 'string') return Object.freeze({ _tag: 'Abstract', kind: input })
  if (input._tag === 'Abstract' || input._tag === 'Concrete') return input
  return Object.freeze({ _tag: 'Concrete', type: input })
}

/**
 * Constructs a non-nullable reference type `(ref <heaptype>)`.
 *
 * **When to use**
 *
 * Use where a null would be a bug and you want the validator to enforce it — a `call_ref` target,
 * or a cast's target type. `ref.cast` and `ref.test` require a target at or below the operand's
 * type, so a non-null operand needs a non-null target here.
 *
 * **Gotchas**
 *
 * A non-nullable reference has no default value, so it cannot be used as a function local. Keep
 * such a value on the stack, or declare the local with {@link refNull}.
 *
 * @see {@link refNull} for the nullable form.
 * @category value types
 * @since 0.0.0
 */
export const ref = (input: HeapTypeInput): RefType =>
  Object.freeze({ _tag: 'Ref', nullable: false, heapType: heapType(input) })

/**
 * Constructs a nullable reference type `(ref null <heaptype>)`.
 *
 * **When to use**
 *
 * Use for locals, for mutable struct fields that start empty, and for any reference that can
 * legitimately be absent. This is the form the classic shorthands take, so `refNull('func')` and
 * {@link funcref} are the same type.
 *
 * **Details**
 *
 * A nullable reference is a supertype of its non-nullable counterpart, so a `(ref $t)` value
 * flows into a `(ref null $t)` slot but not the reverse. `ref.as_non_null` or `br_on_non_null`
 * makes the crossing explicit.
 *
 * @see {@link ref} for the non-nullable form.
 * @category value types
 * @since 0.0.0
 */
export const refNull = (input: HeapTypeInput): RefType =>
  Object.freeze({ _tag: 'Ref', nullable: true, heapType: heapType(input) })

/**
 * The 32-bit integer type `i32`.
 *
 * @category value types
 * @since 0.0.0
 */
export const i32: ValType = Object.freeze({ _tag: 'I32' })

/**
 * The 64-bit integer type `i64`.
 *
 * @category value types
 * @since 0.0.0
 */
export const i64: ValType = Object.freeze({ _tag: 'I64' })

/**
 * The 32-bit float type `f32`.
 *
 * @category value types
 * @since 0.0.0
 */
export const f32: ValType = Object.freeze({ _tag: 'F32' })

/**
 * The 64-bit float type `f64`.
 *
 * @category value types
 * @since 0.0.0
 */
export const f64: ValType = Object.freeze({ _tag: 'F64' })

/**
 * The 128-bit vector type `v128`.
 *
 * @category value types
 * @since 0.0.0
 */
export const v128: ValType = Object.freeze({ _tag: 'V128' })

/**
 * The shorthand `funcref`, i.e. `(ref null func)`.
 *
 * @category value types
 * @since 0.0.0
 */
export const funcref: RefType = refNull('func')

/**
 * The shorthand `externref`, i.e. `(ref null extern)`.
 *
 * @category value types
 * @since 0.0.0
 */
export const externref: RefType = refNull('extern')

/**
 * The shorthand `exnref`, i.e. `(ref null exn)`.
 *
 * @category value types
 * @since 0.0.0
 */
export const exnref: RefType = refNull('exn')

/**
 * The shorthand `anyref`, i.e. `(ref null any)`.
 *
 * @category value types
 * @since 0.0.0
 */
export const anyref: RefType = refNull('any')

/**
 * The shorthand `eqref`, i.e. `(ref null eq)`.
 *
 * @category value types
 * @since 0.0.0
 */
export const eqref: RefType = refNull('eq')

/**
 * The shorthand `i31ref`, i.e. `(ref null i31)`.
 *
 * @category value types
 * @since 0.0.0
 */
export const i31ref: RefType = refNull('i31')

/**
 * The shorthand `structref`, i.e. `(ref null struct)`.
 *
 * @category value types
 * @since 0.0.0
 */
export const structref: RefType = refNull('struct')

/**
 * The shorthand `arrayref`, i.e. `(ref null array)`.
 *
 * @category value types
 * @since 0.0.0
 */
export const arrayref: RefType = refNull('array')

/** @internal */
export const heapTypeEquals = (self: HeapType, that: HeapType): boolean => {
  if (self._tag === 'Abstract') return that._tag === 'Abstract' && self.kind === that.kind
  return that._tag === 'Concrete' && self.type === that.type
}

/**
 * Compares two value types structurally. Concrete heap types compare by handle identity, which
 * canonicalization makes structural.
 *
 * **Gotchas**
 *
 * This is equality, not assignability. Types related by subtyping — `(ref $t)` and
 * `(ref null $t)`, or `eqref` and `anyref` — are not equal, so this answers "are these the same
 * type", never "does a value of one fit where the other is expected". The validator applies the
 * subtype judgment; callers reproducing its decisions from equality alone will be too strict.
 *
 * @category value types
 * @since 0.0.0
 */
export const equals = (self: ValType, that: ValType): boolean => {
  if (self._tag === 'Ref') {
    return (
      that._tag === 'Ref' &&
      self.nullable === that.nullable &&
      heapTypeEquals(self.heapType, that.heapType)
    )
  }
  return self._tag === that._tag
}

/**
 * Tests whether a value type is a reference type.
 *
 * @category value types
 * @since 0.0.0
 */
export const isRefType = (self: ValType): self is RefType => self._tag === 'Ref'

/**
 * Tests whether a value type has a default value (all types except non-nullable references).
 *
 * **When to use**
 *
 * Use before declaring a local from a computed type. Non-defaultable locals are rejected by
 * `Func.define`, so checking here turns a validation failure into a decision you control.
 *
 * @category value types
 * @since 0.0.0
 */
export const isDefaultable = (self: ValType): boolean => self._tag !== 'Ref' || self.nullable

/** The shorthand keyword for each nullable abstract reference, per the text format. */
const shorthandNames: Record<AbstractHeapKind, string> = {
  any: 'anyref',
  eq: 'eqref',
  i31: 'i31ref',
  struct: 'structref',
  array: 'arrayref',
  func: 'funcref',
  extern: 'externref',
  exn: 'exnref',
  none: 'nullref',
  nofunc: 'nullfuncref',
  noextern: 'nullexternref',
  noexn: 'nullexnref',
}

/**
 * Renders a value type as its text-format keyword or general reference form.
 *
 * **Gotchas**
 *
 * A reference to a concrete type renders a placeholder, because the type's index or name is
 * only known to a rendering context; the text emitter renders the real identifier.
 *
 * @category value types
 * @since 0.0.0
 */
export const text = (self: ValType): string => {
  switch (self._tag) {
    case 'I32':
      return 'i32'
    case 'I64':
      return 'i64'
    case 'F32':
      return 'f32'
    case 'F64':
      return 'f64'
    case 'V128':
      return 'v128'
    case 'Ref': {
      if (self.heapType._tag === 'Abstract') {
        if (self.nullable) return shorthandNames[self.heapType.kind]
        return `(ref ${self.heapType.kind})`
      }
      return self.nullable ? '(ref null <type>)' : '(ref <type>)'
    }
  }
}

/** The single-byte encoding of each abstract heap type. */
const abstractBytes: Record<AbstractHeapKind, number> = {
  none: 0x71,
  noextern: 0x72,
  nofunc: 0x73,
  noexn: 0x74,
  func: 0x70,
  extern: 0x6f,
  any: 0x6e,
  eq: 0x6d,
  i31: 0x6c,
  struct: 0x6b,
  array: 0x6a,
  exn: 0x69,
}

/** @internal */
export const abstractByte = (kind: AbstractHeapKind): number => abstractBytes[kind]

/**
 * Encodes a numeric, vector, or shorthand (nullable abstract) reference type as its
 * binary-format byte.
 *
 * **Gotchas**
 *
 * Non-nullable and concrete reference types have multi-byte encodings that depend on emission
 * state; the binary emitter encodes them. This function returns `undefined` for those.
 *
 * @category value types
 * @since 0.0.0
 */
export const binary = (self: ValType): number | undefined => {
  switch (self._tag) {
    case 'I32':
      return 0x7f
    case 'I64':
      return 0x7e
    case 'F32':
      return 0x7d
    case 'F64':
      return 0x7c
    case 'V128':
      return 0x7b
    case 'Ref':
      return self.nullable && self.heapType._tag === 'Abstract'
        ? abstractBytes[self.heapType.kind]
        : undefined
  }
}
