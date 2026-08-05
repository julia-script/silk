/**
 * WebAssembly value types as plain immutable data.
 *
 * Value types are context-free values, not builder-owned handles: two builders share the same
 * `ValType.i32`. The tagged-union representation (rather than a string enum) leaves room for
 * parameterized reference types introduced by later feature proposals.
 *
 * @since 0.0.0
 */

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
 * A WebAssembly reference type.
 *
 * @category value types
 * @since 0.0.0
 */
export type RefType =
  | { readonly _tag: 'FuncRef' }
  | { readonly _tag: 'ExternRef' }
  | { readonly _tag: 'ExnRef' }

/**
 * Any WebAssembly value type.
 *
 * @category value types
 * @since 0.0.0
 */
export type ValType = NumType | VecType | RefType

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
 * The reference type `funcref`.
 *
 * @category value types
 * @since 0.0.0
 */
export const funcref: RefType = Object.freeze({ _tag: 'FuncRef' })

/**
 * The reference type `externref`.
 *
 * @category value types
 * @since 0.0.0
 */
export const externref: RefType = Object.freeze({ _tag: 'ExternRef' })

/**
 * The reference type `exnref`, holding caught exception references.
 *
 * @category value types
 * @since 0.0.0
 */
export const exnref: RefType = Object.freeze({ _tag: 'ExnRef' })

/**
 * Compares two value types structurally.
 *
 * @category value types
 * @since 0.0.0
 */
export const equals = (self: ValType, that: ValType): boolean => self._tag === that._tag

/**
 * Tests whether a value type is a reference type.
 *
 * @category value types
 * @since 0.0.0
 */
export const isRefType = (self: ValType): self is RefType =>
  self._tag === 'FuncRef' || self._tag === 'ExternRef' || self._tag === 'ExnRef'

/**
 * Renders a value type as its text-format keyword.
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
    case 'FuncRef':
      return 'funcref'
    case 'ExternRef':
      return 'externref'
    case 'ExnRef':
      return 'exnref'
  }
}

/**
 * Encodes a value type as its binary-format byte.
 *
 * @category value types
 * @since 0.0.0
 */
export const binary = (self: ValType): number => {
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
    case 'FuncRef':
      return 0x70
    case 'ExternRef':
      return 0x6f
    case 'ExnRef':
      return 0x69
  }
}
