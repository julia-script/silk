/** The built-in scalar types implemented by the current executable bootstrap surface. */
export type Builtin = 'I32' | 'Bool'

/** One canonical nominal struct type, independent of import or source spelling. */
export interface Nominal {
  readonly _tag: 'NominalType'
  readonly module: string
  readonly name: string
}

/** The closed semantic type vocabulary accepted by declaration analysis. */
export type Type = Builtin | Nominal

/** Constructs one immutable canonical nominal type. */
export const nominal = (module: string, name: string): Nominal =>
  Object.freeze({ _tag: 'NominalType', module, name })

/** Tests whether a semantic type is one of the executable built-in scalars. */
export const isBuiltin = (self: Type): self is Builtin => typeof self === 'string'

/** Tests whether a semantic type is a canonical nominal struct. */
export const isNominal = (self: Type): self is Nominal => typeof self !== 'string'

/** Returns the canonical deterministic key used for equality and ordering. */
export const key = (self: Type): string =>
  typeof self === 'string' ? `builtin:${self}` : `nominal:${self.module}.${self.name}`

/** Compares semantic types by canonical identity. */
export const equals = (left: Type, right: Type): boolean => key(left) === key(right)

/** Orders semantic types by canonical identity. */
export const compare = (left: Type, right: Type): number => key(left).localeCompare(key(right))

/** Encodes one type for deterministic compiler facts and diagnostics. */
export const encode = (self: Type): string =>
  typeof self === 'string' ? self : `${self.module}.${self.name}`
