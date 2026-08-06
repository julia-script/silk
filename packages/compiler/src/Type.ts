/** The built-in scalar types implemented by the current executable bootstrap surface. */
export type Builtin = 'I32' | 'Bool'

/** The empty structural union and uninhabited bottom type. */
export type Never = 'Never'

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

/** One normalized structural union with at least two canonical nominal members. */
const structuralUnionBrand: unique symbol = Symbol('StructuralUnion')
export interface StructuralUnion {
  readonly _tag: 'StructuralUnionType'
  readonly members: ReadonlyArray<Nominal>
  readonly [structuralUnionBrand]: true
}

/** The closed semantic type vocabulary accepted by declaration analysis. */
export type Type = Builtin | Never | Nominal | FixedArray | StructuralUnion

/** The typed result of attempting to normalize structural-union inputs. */
export type UnionNormalization =
  | { readonly _tag: 'Normalized'; readonly type: Type }
  | { readonly _tag: 'InvalidMembers'; readonly members: ReadonlyArray<Type> }

/** Constructs one immutable canonical nominal type. */
export const nominal = (module: string, name: string): Nominal =>
  Object.freeze({ _tag: 'NominalType', module, name })

/** Constructs one immutable canonical fixed-array type. */
export const fixedArray = (element: Type, length: number): FixedArray =>
  Object.freeze({ _tag: 'FixedArrayType', element, length })

const compareText = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0

/** Normalizes a finite union without permitting non-nominal leaves. */
export const union = (inputs: ReadonlyArray<Type>): UnionNormalization => {
  const members = new Map<string, Nominal>()
  const invalid: Array<Type> = []
  const visit = (input: Type): void => {
    if (input === 'Never') return
    if (isNominal(input)) {
      members.set(key(input), input)
      return
    }
    if (isUnion(input)) {
      for (const member of input.members) visit(member)
      return
    }
    invalid.push(input)
  }
  for (const input of inputs) visit(input)
  if (invalid.length > 0)
    return Object.freeze({ _tag: 'InvalidMembers', members: Object.freeze(invalid) })
  const normalized = Object.freeze([...members.values()].sort(compare))
  if (normalized.length === 0) return Object.freeze({ _tag: 'Normalized', type: 'Never' })
  const singleton = normalized.at(0)
  if (normalized.length === 1 && singleton !== undefined)
    return Object.freeze({ _tag: 'Normalized', type: singleton })
  return Object.freeze({
    _tag: 'Normalized',
    type: Object.freeze({
      _tag: 'StructuralUnionType',
      members: normalized,
      [structuralUnionBrand]: true as const,
    }),
  })
}

/** Tests whether a semantic type is one of the executable built-in scalars. */
export const isBuiltin = (self: Type): self is Builtin => self === 'I32' || self === 'Bool'

/** Tests whether a semantic type is the empty structural union. */
export const isNever = (self: Type): self is Never => self === 'Never'

/** Tests whether a semantic type is a canonical nominal struct. */
export const isNominal = (self: Type): self is Nominal =>
  typeof self !== 'string' && self._tag === 'NominalType'

/** Tests whether a semantic type is a structural fixed array. */
export const isFixedArray = (self: Type): self is FixedArray =>
  typeof self !== 'string' && self._tag === 'FixedArrayType'

/** Tests whether a semantic type is a normalized multi-member structural union. */
export const isUnion = (self: Type): self is StructuralUnion =>
  typeof self !== 'string' && self._tag === 'StructuralUnionType'

/** Returns the canonical deterministic key used for equality and ordering. */
export const key = (self: Type): string => {
  if (isBuiltin(self)) return `builtin:${self}`
  if (isNever(self)) return 'union:'
  if (isNominal(self)) return `nominal:${self.module}.${self.name}`
  if (isFixedArray(self)) return `array:${self.length}<${key(self.element)}>`
  return `union:${self.members.map(key).join('|')}`
}

/** Compares semantic types by canonical identity. */
export const equals = (left: Type, right: Type): boolean => key(left) === key(right)

/** Orders semantic types by canonical identity. */
export const compare = (left: Type, right: Type): number => compareText(key(left), key(right))

/** Encodes one type for deterministic compiler facts and diagnostics. */
export const encode = (self: Type): string => {
  if (typeof self === 'string') return self
  if (isNominal(self)) return `${self.module}.${self.name}`
  if (isFixedArray(self)) return `Array<${encode(self.element)}, ${self.length}>`
  return self.members.map(encode).join(' | ')
}

/** Returns every canonical nominal nested in a type, in deterministic preorder. */
export const nominals = (self: Type): ReadonlyArray<Nominal> =>
  isNominal(self)
    ? Object.freeze([self])
    : isFixedArray(self)
      ? nominals(self.element)
      : isUnion(self)
        ? self.members
        : []
