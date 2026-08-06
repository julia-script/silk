/** The built-in scalar types implemented by the current executable bootstrap surface. */
export type Builtin = 'I32' | 'Bool'

/** One canonical nominal struct type, independent of import or source spelling. */
export interface Nominal {
  readonly _tag: 'NominalType'
  readonly module: string
  readonly name: string
}

/** One canonical inline fixed array whose length participates in structural identity. */
export interface FixedArray {
  readonly _tag: 'FixedArrayType'
  readonly element: Type
  readonly length: number
}

/** The closed semantic type vocabulary accepted by declaration analysis. */
export type Type = Builtin | Nominal | FixedArray

/** Constructs one immutable canonical nominal type. */
export const nominal = (module: string, name: string): Nominal =>
  Object.freeze({ _tag: 'NominalType', module, name })

/** Constructs one immutable canonical fixed-array type. */
export const fixedArray = (element: Type, length: number): FixedArray =>
  Object.freeze({ _tag: 'FixedArrayType', element, length })

/** Tests whether a semantic type is one of the executable built-in scalars. */
export const isBuiltin = (self: Type): self is Builtin => typeof self === 'string'

/** Tests whether a semantic type is a canonical nominal struct. */
export const isNominal = (self: Type): self is Nominal =>
  typeof self !== 'string' && self._tag === 'NominalType'

/** Tests whether a semantic type is a structural fixed array. */
export const isFixedArray = (self: Type): self is FixedArray =>
  typeof self !== 'string' && self._tag === 'FixedArrayType'

/** Returns the canonical deterministic key used for equality and ordering. */
export const key = (self: Type): string =>
  typeof self === 'string'
    ? `builtin:${self}`
    : self._tag === 'NominalType'
      ? `nominal:${self.module}.${self.name}`
      : `array:${self.length}<${key(self.element)}>`

/** Compares semantic types by canonical identity. */
export const equals = (left: Type, right: Type): boolean => key(left) === key(right)

/** Orders semantic types by canonical identity. */
export const compare = (left: Type, right: Type): number => key(left).localeCompare(key(right))

/** Encodes one type for deterministic compiler facts and diagnostics. */
export const encode = (self: Type): string =>
  typeof self === 'string'
    ? self
    : self._tag === 'NominalType'
      ? `${self.module}.${self.name}`
      : `Array<${encode(self.element)}, ${self.length}>`

/** Returns every canonical nominal nested in a type, in deterministic preorder. */
export const nominals = (self: Type): ReadonlyArray<Nominal> =>
  isNominal(self) ? Object.freeze([self]) : isFixedArray(self) ? nominals(self.element) : []
