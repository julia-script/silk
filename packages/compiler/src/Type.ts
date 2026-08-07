/** The built-in scalar types implemented by the current executable bootstrap surface. */
export type Builtin = 'I32' | 'Usize' | 'Bool'

/** The empty structural union and uninhabited bottom type. */
export type Never = 'Never'

/** One canonical nominal struct type, independent of import or source spelling. */
export interface Nominal {
  readonly _tag: 'NominalType'
  readonly module: string
  readonly name: string
  readonly arguments: ReadonlyArray<Type>
}

/** One declaration-owned generic type parameter. Names are provenance, not identity. */
export interface Parameter {
  readonly _tag: 'TypeParameter'
  readonly owner: {
    readonly module: string
    readonly name: string
  }
  readonly ordinal: number
  readonly name: string
}

/** One canonical inline fixed array whose length participates in structural identity. */
export interface FixedArray {
  readonly _tag: 'FixedArrayType'
  readonly element: Type
  readonly length: number
}

/** A lexical runtime-length view whose access permission is checked statically. */
export interface Slice {
  readonly _tag: 'SliceType'
  readonly access: 'Shared' | 'Exclusive'
  readonly element: Type
}

/** A compiler-private lazy flow contract. Flow values never cross the executable ABI. */
export interface Flow {
  readonly _tag: 'FlowType'
  readonly success: Type
  readonly failures: ReadonlyArray<Nominal>
  readonly access: 'Shared' | 'Exclusive' | 'Take'
}

/** One normalized structural union with at least two canonical nominal members. */
const structuralUnionBrand: unique symbol = Symbol('StructuralUnion')
export interface StructuralUnion {
  readonly _tag: 'StructuralUnionType'
  readonly members: ReadonlyArray<Nominal>
  readonly [structuralUnionBrand]: true
}

/** The closed semantic type vocabulary accepted by declaration analysis. */
export type Type =
  | Builtin
  | Never
  | Nominal
  | Parameter
  | FixedArray
  | Slice
  | Flow
  | StructuralUnion

/** The typed result of attempting to normalize structural-union inputs. */
export type UnionNormalization =
  | { readonly _tag: 'Normalized'; readonly type: Type }
  | { readonly _tag: 'InvalidMembers'; readonly members: ReadonlyArray<Type> }

/** Constructs one immutable canonical nominal type. */
export const nominal = (
  module: string,
  name: string,
  arguments_: ReadonlyArray<Type> = [],
): Nominal =>
  Object.freeze({
    _tag: 'NominalType',
    module,
    name,
    arguments: Object.freeze(Array.from(arguments_)),
  })

/** Constructs one declaration-owned generic type parameter. */
export const parameter = (
  owner: { readonly module: string; readonly name: string },
  ordinal: number,
  name: string,
): Parameter =>
  Object.freeze({
    _tag: 'TypeParameter',
    owner: Object.freeze({ module: owner.module, name: owner.name }),
    ordinal,
    name,
  })

/** Constructs one immutable canonical fixed-array type. */
export const fixedArray = (element: Type, length: number): FixedArray =>
  Object.freeze({ _tag: 'FixedArrayType', element, length })

/** Constructs one canonical lexical slice type. */
export const slice = (access: Slice['access'], element: Type): Slice =>
  Object.freeze({ _tag: 'SliceType', access, element })

/** Constructs one normalized compiler-private lazy flow contract. */
export const flow = (
  success: Type,
  failures: ReadonlyArray<Nominal>,
  access: Flow['access'] = 'Shared',
): Flow => {
  const normalized = new Map(failures.map((failure) => [key(failure), failure] as const))
  return Object.freeze({
    _tag: 'FlowType',
    success,
    failures: Object.freeze([...normalized.values()].sort(compare)),
    access,
  })
}

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
export const isBuiltin = (self: Type): self is Builtin =>
  self === 'I32' || self === 'Usize' || self === 'Bool'

/** Tests whether a semantic type is the empty structural union. */
export const isNever = (self: Type): self is Never => self === 'Never'

/** Tests whether a semantic type is a canonical nominal struct. */
export const isNominal = (self: Type): self is Nominal =>
  typeof self !== 'string' && self._tag === 'NominalType'

/** Tests whether a semantic type is a declaration-owned generic parameter. */
export const isParameter = (self: Type): self is Parameter =>
  typeof self !== 'string' && self._tag === 'TypeParameter'

/** Tests whether a semantic type is a structural fixed array. */
export const isFixedArray = (self: Type): self is FixedArray =>
  typeof self !== 'string' && self._tag === 'FixedArrayType'

/** Tests whether a semantic type is a lexical runtime slice. */
export const isSlice = (self: Type): self is Slice =>
  typeof self !== 'string' && self._tag === 'SliceType'

/** Tests whether a semantic type is a compiler-private lazy flow contract. */
export const isFlow = (self: Type): self is Flow =>
  typeof self !== 'string' && self._tag === 'FlowType'

/** Tests whether a semantic type is a normalized multi-member structural union. */
export const isUnion = (self: Type): self is StructuralUnion =>
  typeof self !== 'string' && self._tag === 'StructuralUnionType'

/** Returns the canonical deterministic key used for equality and ordering. */
export const key = (self: Type): string => {
  if (isBuiltin(self)) return `builtin:${self}`
  if (isNever(self)) return 'union:'
  if (isNominal(self))
    return `nominal:${self.module}.${self.name}<${self.arguments.map(key).join(',')}>`
  if (isParameter(self)) return `parameter:${self.owner.module}.${self.owner.name}:${self.ordinal}`
  if (isFixedArray(self)) return `array:${self.length}<${key(self.element)}>`
  if (isSlice(self)) return `slice:${self.access}<${key(self.element)}>`
  if (isFlow(self))
    return `flow:${self.access}<${key(self.success)}!${self.failures.map(key).join('|')}>`
  return `union:${self.members.map(key).join('|')}`
}

/** Compares semantic types by canonical identity. */
export const equals = (left: Type, right: Type): boolean => key(left) === key(right)

/** Orders semantic types by canonical identity. */
export const compare = (left: Type, right: Type): number => compareText(key(left), key(right))

/** Encodes one type for deterministic compiler facts and diagnostics. */
export const encode = (self: Type): string => {
  if (typeof self === 'string') return self
  if (isNominal(self)) {
    const arguments_ =
      self.arguments.length === 0 ? '' : `<${self.arguments.map(encode).join(', ')}>`
    return `${self.module}.${self.name}${arguments_}`
  }
  if (isParameter(self)) return self.name
  if (isFixedArray(self)) return `Array<${encode(self.element)}, ${self.length}>`
  if (isSlice(self))
    return `${self.access === 'Exclusive' ? '&mut ' : '&'}[${encode(self.element)}]`
  if (isFlow(self)) {
    const row = self.failures.length === 0 ? '' : ` ! ${self.failures.map(encode).join(' | ')}`
    return `Flow<${encode(self.success)}${row}>`
  }
  return self.members.map(encode).join(' | ')
}

/** Returns every canonical nominal nested in a type, in deterministic preorder. */
export const nominals = (self: Type): ReadonlyArray<Nominal> =>
  isNominal(self)
    ? Object.freeze([self, ...self.arguments.flatMap(nominals)])
    : isFixedArray(self)
      ? nominals(self.element)
      : isSlice(self)
        ? nominals(self.element)
        : isFlow(self)
          ? Object.freeze([...nominals(self.success), ...self.failures.flatMap(nominals)])
          : isUnion(self)
            ? Object.freeze(self.members.flatMap(nominals))
            : []

/** Returns every declaration-owned parameter nested in a type, without duplicates. */
export const parameters = (self: Type): ReadonlyArray<Parameter> => {
  const found = new Map<string, Parameter>()
  const visit = (type: Type): void => {
    if (isParameter(type)) {
      found.set(key(type), type)
      return
    }
    if (isNominal(type)) {
      for (const argument of type.arguments) visit(argument)
      return
    }
    if (isFixedArray(type) || isSlice(type)) visit(type.element)
    else if (isFlow(type)) {
      visit(type.success)
      for (const failure of type.failures) visit(failure)
    } else if (isUnion(type)) for (const member of type.members) visit(member)
  }
  visit(self)
  return Object.freeze([...found.values()].sort(compare))
}

/** Tests whether a type contains no open generic parameters. */
export const isConcrete = (self: Type): boolean => parameters(self).length === 0

/** Tests whether a type contains a lexical borrow at any depth. */
export const containsBorrow = (self: Type): boolean => {
  if (isSlice(self)) return true
  if (isNominal(self)) return self.arguments.some(containsBorrow)
  if (isFixedArray(self)) return containsBorrow(self.element)
  if (isFlow(self)) return containsBorrow(self.success) || self.failures.some(containsBorrow)
  if (isUnion(self)) return self.members.some(containsBorrow)
  return false
}

/** Replaces declaration-owned parameters recursively through one canonical type. */
export const substitute = (self: Type, substitution: ReadonlyMap<string, Type>): Type => {
  if (isParameter(self)) return substitution.get(key(self)) ?? self
  if (isNominal(self))
    return nominal(
      self.module,
      self.name,
      self.arguments.map((argument) => substitute(argument, substitution)),
    )
  if (isFixedArray(self)) return fixedArray(substitute(self.element, substitution), self.length)
  if (isSlice(self)) return slice(self.access, substitute(self.element, substitution))
  if (isFlow(self)) {
    const success = substitute(self.success, substitution)
    const failures = self.failures.map((failure) => substitute(failure, substitution))
    return flow(
      success,
      failures.filter((failure): failure is Nominal => isNominal(failure)),
      self.access,
    )
  }
  if (isUnion(self)) {
    const normalized = union(self.members.map((member) => substitute(member, substitution)))
    return normalized._tag === 'Normalized' ? normalized.type : self
  }
  return self
}

/** Adds structural constraints from one declared type pattern to one supplied concrete type. */
export const infer = (pattern: Type, actual: Type, inferred: Map<string, Type>): boolean => {
  if (isParameter(pattern)) {
    const identity = key(pattern)
    const existing = inferred.get(identity)
    if (existing === undefined) {
      inferred.set(identity, actual)
      return true
    }
    return equals(existing, actual)
  }
  if (isNominal(pattern) && isNominal(actual)) {
    if (
      pattern.module !== actual.module ||
      pattern.name !== actual.name ||
      pattern.arguments.length !== actual.arguments.length
    )
      return false
    return pattern.arguments.every((argument, index) => {
      const supplied = actual.arguments.at(index)
      return supplied !== undefined && infer(argument, supplied, inferred)
    })
  }
  if (isFixedArray(pattern) && isFixedArray(actual)) {
    return pattern.length === actual.length && infer(pattern.element, actual.element, inferred)
  }
  if (isSlice(pattern) && isSlice(actual)) {
    return pattern.access === actual.access && infer(pattern.element, actual.element, inferred)
  }
  if (isFlow(pattern) && isFlow(actual)) {
    return (
      pattern.access === actual.access &&
      pattern.failures.length === actual.failures.length &&
      infer(pattern.success, actual.success, inferred) &&
      pattern.failures.every((failure, index) => {
        const supplied = actual.failures.at(index)
        return supplied !== undefined && infer(failure, supplied, inferred)
      })
    )
  }
  return equals(pattern, actual)
}

/** Builds a substitution from ordered parameters and arguments when their arities match. */
export const substitution = (
  declared: ReadonlyArray<Parameter>,
  arguments_: ReadonlyArray<Type>,
): ReadonlyMap<string, Type> | undefined => {
  if (declared.length !== arguments_.length) return undefined
  const result = new Map<string, Type>()
  for (const [index, parameter_] of declared.entries()) {
    const argument = arguments_.at(index)
    if (argument === undefined) return undefined
    result.set(key(parameter_), argument)
  }
  return result
}
