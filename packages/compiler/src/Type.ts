import * as Scalar from './Scalar.js'

/** The built-in scalar types implemented by the current executable bootstrap surface. */
export type Builtin = Scalar.Spelling

/** The empty structural union and uninhabited bottom type. */
export type Bottom = 'never'

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

/** A lexical borrow of one complete value. Unlike a Slice, it carries no runtime length. */
export interface Reference {
  readonly _tag: 'ReferenceType'
  readonly access: 'Shared' | 'Exclusive'
  readonly target: Type
}

/** How a callable environment may be accessed by one invocation. */
export type CallableMode = 'Shared' | 'Exclusive' | 'Take'

/** One canonical structural callable contract independent of its hidden concrete environment. */
export interface Callable {
  readonly _tag: 'CallableType'
  readonly parameters: ReadonlyArray<Type>
  readonly result: Type
  readonly mode: CallableMode
}

/** One compile-time capability requirement. Roles select slots and have no runtime value. */
export interface Requirement {
  readonly capability: Nominal
  readonly role: string
  readonly access: 'Shared' | 'Exclusive'
}

/** A compiler-private lazy effect contract. Effect values never cross the executable ABI. */
export interface Effect {
  readonly _tag: 'EffectType'
  readonly success: Type
  readonly failures: ReadonlyArray<Nominal>
  readonly requirements: ReadonlyArray<Requirement>
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
  | Bottom
  | Nominal
  | Parameter
  | FixedArray
  | Slice
  | Reference
  | Callable
  | Effect
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

/** Canonical allocation-free failure used by every allocator implementation. */
export const outOfMemory: Nominal = nominal('silk/core', 'OutOfMemory')
export const layout: Nominal = nominal('silk/core', 'Layout')
export const invalidAlignment: Nominal = nominal('silk/core', 'InvalidAlignment')
export const layoutOverflow: Nominal = nominal('silk/core', 'LayoutOverflow')
/** The implementation-erased allocation capability requested by allocation Effects. */
export const allocator: Nominal = nominal('silk/core', 'Allocator')
/** Explicit host capability for complete stdout and stderr byte writes. */
export const standardStreams: Nominal = nominal('silk/core', 'StandardStreams')
/** Allocation-free typed failure returned when a host cannot commit a complete write. */
export const streamWriteFailure: Nominal = nominal('silk/core', 'StreamWriteFailure')
/** A self-contained affine owner carrying one private active reclaim ticket. */
export const allocation: Nominal = nominal('silk/core', 'Allocation')
/** Compiler-sealed cleanup capability used only by restricted impl declarations. */
export const dropCapability: Nominal = nominal('silk/core', 'Drop')
/** Compiler-sealed marker authorizing a canonical terminal failure report. */
export const reportCapability: Nominal = nominal('silk/core', 'Report')
/** The nominal system-backed implementation of the Allocator capability. */
export const systemAllocator: Nominal = nominal('silk/core', 'SystemAllocator')
/** The canonical empty success value used by effect-free cleanup operations. */
export const unit: Nominal = nominal('silk/core', 'Unit')
/** Compiler-checked typed raw storage owned independently from its allocator provider. */
export const rawBuffer = (element: Type): Nominal => nominal('silk/core', 'RawBuffer', [element])
/** A lexical exclusive projection into one RawBuffer element. */
export const slot = (element: Type): Nominal => nominal('silk/core', 'Slot', [element])
/** Canonical recoverable success and failure members shipped by silk/option. */
export const some = (element: Type): Nominal => nominal('silk/option', 'Some', [element])
export const none: Nominal = nominal('silk/option', 'None')

/** Canonical transparent Option<T> identity, represented as the ordinary structural union. */
export const option = (element: Type): Type => {
  const normalized = union([some(element), none])
  return normalized._tag === 'Normalized' ? normalized.type : 'never'
}

export const isRawBuffer = (
  self: Type,
): self is Nominal & {
  readonly module: 'silk/core'
  readonly name: 'RawBuffer'
  readonly arguments: readonly [Type]
} =>
  isNominal(self) &&
  self.module === 'silk/core' &&
  self.name === 'RawBuffer' &&
  self.arguments.length === 1

export const isSlot = (
  self: Type,
): self is Nominal & {
  readonly module: 'silk/core'
  readonly name: 'Slot'
  readonly arguments: readonly [Type]
} =>
  isNominal(self) &&
  self.module === 'silk/core' &&
  self.name === 'Slot' &&
  self.arguments.length === 1

export const intrinsicNominals: ReadonlyMap<string, Nominal> = new Map([
  [outOfMemory.name, outOfMemory],
  [layout.name, layout],
  [invalidAlignment.name, invalidAlignment],
  [layoutOverflow.name, layoutOverflow],
  [allocator.name, allocator],
  [standardStreams.name, standardStreams],
  [streamWriteFailure.name, streamWriteFailure],
  [allocation.name, allocation],
  [dropCapability.name, dropCapability],
  [reportCapability.name, reportCapability],
  [systemAllocator.name, systemAllocator],
  ['RawBuffer', nominal('silk/core', 'RawBuffer')],
  ['Slot', nominal('silk/core', 'Slot')],
])

/** Returns the compiler-known generic arity of an intrinsic nominal actor. */
export const intrinsicNominalArity = (self: Nominal): number =>
  self.module === 'silk/core' && (self.name === 'RawBuffer' || self.name === 'Slot') ? 1 : 0
export const intrinsicNominalOrdinal = (self: Nominal): number =>
  [...intrinsicNominals.values()].findIndex(
    (candidate) => candidate.module === self.module && candidate.name === self.name,
  )

/** Compiler-shipped nominal capability witnesses; user declarations extend this in the index. */
export const intrinsicConformances: ReadonlyMap<string, ReadonlySet<string>> = new Map([
  ['nominal:silk/core.SystemAllocator<>', new Set(['nominal:silk/core.Allocator<>'])],
  ['nominal:silk/core.StreamWriteFailure<>', new Set(['nominal:silk/core.Report<>'])],
])

/** Tests one compiler-shipped nominal capability witness without inspecting provider kinds. */
export const intrinsicallyConforms = (provider: Type, capability: Nominal): boolean =>
  isNominal(provider) && (intrinsicConformances.get(key(provider))?.has(key(capability)) ?? false)

/** Tests the one compiler-sealed allocation exhaustion payload. */
export const isOutOfMemory = (self: Type): self is Nominal => equals(self, outOfMemory)
export const isIntrinsicNominal = (self: Type): boolean =>
  isNominal(self) && self.module === 'silk/core' && intrinsicNominals.get(self.name) !== undefined

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

/** Constructs one canonical lexical whole-value reference. */
export const reference = (access: Reference['access'], target: Type): Reference =>
  Object.freeze({ _tag: 'ReferenceType', access, target })

/** Constructs one immutable canonical callable contract. */
export const callable = (
  parameters_: ReadonlyArray<Type>,
  result: Type,
  mode: CallableMode = 'Shared',
): Callable =>
  Object.freeze({
    _tag: 'CallableType',
    parameters: Object.freeze(Array.from(parameters_)),
    result,
    mode,
  })

/** Constructs one normalized compiler-private lazy effect contract. */
export const effect = (
  success: Type,
  failures: ReadonlyArray<Nominal>,
  access: Effect['access'] = 'Shared',
  requirements: ReadonlyArray<Requirement> = [],
): Effect => {
  const normalized = new Map(failures.map((failure) => [key(failure), failure] as const))
  const normalizedRequirements = new Map<string, Requirement>()
  for (const requirement of requirements) {
    const identity = `${key(requirement.capability)}@${requirement.role}`
    const existing = normalizedRequirements.get(identity)
    normalizedRequirements.set(
      identity,
      Object.freeze({
        capability: requirement.capability,
        role: requirement.role,
        access:
          existing?.access === 'Exclusive' || requirement.access === 'Exclusive'
            ? 'Exclusive'
            : 'Shared',
      }),
    )
  }
  return Object.freeze({
    _tag: 'EffectType',
    success,
    failures: Object.freeze([...normalized.values()].sort(compare)),
    requirements: Object.freeze(
      [...normalizedRequirements.values()].sort((left, right) =>
        compareText(
          `${key(left.capability)}@${left.role}`,
          `${key(right.capability)}@${right.role}`,
        ),
      ),
    ),
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
    if (input === 'never') return
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
  if (normalized.length === 0) return Object.freeze({ _tag: 'Normalized', type: 'never' })
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
export const isBuiltin = (self: unknown): self is Builtin => Scalar.isSpelling(self)

/** Tests whether a semantic type is the empty structural union. */
export const isNever = (self: Type): self is Bottom => self === 'never'

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

/** Tests whether a semantic type is a lexical whole-value reference. */
export const isReference = (self: Type): self is Reference =>
  typeof self !== 'string' && self._tag === 'ReferenceType'

/** Tests whether a semantic type is a structural callable contract. */
export const isCallable = (self: Type): self is Callable =>
  typeof self !== 'string' && self._tag === 'CallableType'

/** Tests whether a semantic type is a compiler-private lazy effect contract. */
export const isEffect = (self: Type): self is Effect =>
  typeof self !== 'string' && self._tag === 'EffectType'

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
  if (isReference(self)) return `reference:${self.access}<${key(self.target)}>`
  if (isCallable(self))
    return `callable:${self.mode}<(${self.parameters.map(key).join(',')})->${key(self.result)}>`
  if (isEffect(self))
    return `effect:${self.access}<${key(self.success)}!${self.failures.map(key).join('|')}?${self.requirements.map((requirement) => `${requirement.access}:${key(requirement.capability)}@${requirement.role}`).join('|')}>`
  return `union:${self.members.map(key).join('|')}`
}

/** Compares semantic types by canonical identity. */
export const equals = (left: Type, right: Type): boolean => key(left) === key(right)

/** Orders semantic types by canonical identity. */
export const compare = (left: Type, right: Type): number => compareText(key(left), key(right))

/** Encodes one type for deterministic compiler facts and diagnostics. */
export const encode = (self: Type): string => {
  if (typeof self === 'string') return self
  if (equals(self, unit)) return '()'
  if (isNominal(self)) {
    const arguments_ =
      self.arguments.length === 0 ? '' : `<${self.arguments.map(encode).join(', ')}>`
    return `${self.module}.${self.name}${arguments_}`
  }
  if (isParameter(self)) return self.name
  if (isFixedArray(self)) return `Array<${encode(self.element)}, ${self.length}>`
  if (isSlice(self))
    return `${self.access === 'Exclusive' ? '&mut ' : '&'}[${encode(self.element)}]`
  if (isReference(self))
    return `${self.access === 'Exclusive' ? '&mut ' : '&'}${encode(self.target)}`
  if (isCallable(self)) {
    const mode = self.mode === 'Exclusive' ? 'mut ' : self.mode === 'Take' ? 'once ' : ''
    return `${mode}fn(${self.parameters.map(encode).join(', ')}) -> ${encode(self.result)}`
  }
  if (isEffect(self)) {
    const row = self.failures.length === 0 ? '' : ` ! ${self.failures.map(encode).join(' | ')}`
    const requirements =
      self.requirements.length === 0
        ? ''
        : ` ? ${self.requirements
            .map(
              (requirement) =>
                `${requirement.access === 'Exclusive' ? '&mut ' : '&'}${encode(requirement.capability)}${requirement.role === 'DefaultRole' ? '' : `@${requirement.role}`}`,
            )
            .join(' | ')}`
    return `Effect<${encode(self.success)}${row}${requirements}>`
  }
  const someMember = self.members.find(
    (member) =>
      member.module === 'silk/option' && member.name === 'Some' && member.arguments.length === 1,
  )
  const noneMember = self.members.find(
    (member) => member.module === 'silk/option' && member.name === 'None',
  )
  if (self.members.length === 2 && someMember !== undefined && noneMember !== undefined)
    return `Option<${encode(someMember.arguments.at(0) ?? 'never')}>`
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
        : isReference(self)
          ? nominals(self.target)
          : isCallable(self)
            ? Object.freeze([...self.parameters.flatMap(nominals), ...nominals(self.result)])
            : isEffect(self)
              ? Object.freeze([
                  ...nominals(self.success),
                  ...self.failures.flatMap(nominals),
                  ...self.requirements.flatMap((requirement) => nominals(requirement.capability)),
                ])
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
    else if (isReference(type)) visit(type.target)
    else if (isCallable(type)) {
      for (const parameter_ of type.parameters) visit(parameter_)
      visit(type.result)
    } else if (isEffect(type)) {
      visit(type.success)
      for (const failure of type.failures) visit(failure)
      for (const requirement of type.requirements) visit(requirement.capability)
    } else if (isUnion(type)) for (const member of type.members) visit(member)
  }
  visit(self)
  return Object.freeze([...found.values()].sort(compare))
}

/** Tests whether a type contains no open generic parameters. */
export const isConcrete = (self: Type): boolean => parameters(self).length === 0

/** Tests whether a type contains a lexical borrow at any depth. */
export const containsBorrow = (self: Type): boolean => {
  if (isSlice(self) || isReference(self)) return true
  if (isSlot(self)) return true
  if (isNominal(self)) return self.arguments.some(containsBorrow)
  if (isFixedArray(self)) return containsBorrow(self.element)
  if (isCallable(self)) return self.parameters.some(containsBorrow) || containsBorrow(self.result)
  if (isEffect(self)) return containsBorrow(self.success) || self.failures.some(containsBorrow)
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
  if (isReference(self)) return reference(self.access, substitute(self.target, substitution))
  if (isCallable(self))
    return callable(
      self.parameters.map((parameter_) => substitute(parameter_, substitution)),
      substitute(self.result, substitution),
      self.mode,
    )
  if (isEffect(self)) {
    const success = substitute(self.success, substitution)
    const failures = self.failures.map((failure) => substitute(failure, substitution))
    return effect(
      success,
      failures.filter((failure): failure is Nominal => isNominal(failure)),
      self.access,
      self.requirements.flatMap((requirement) => {
        const capability = substitute(requirement.capability, substitution)
        return isNominal(capability) ? [Object.freeze({ ...requirement, capability })] : []
      }),
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
  if (isReference(pattern) && isReference(actual)) {
    return pattern.access === actual.access && infer(pattern.target, actual.target, inferred)
  }
  if (isCallable(pattern) && isCallable(actual)) {
    return (
      pattern.mode === actual.mode &&
      pattern.parameters.length === actual.parameters.length &&
      pattern.parameters.every((parameter_, index) => {
        const supplied = actual.parameters.at(index)
        return supplied !== undefined && infer(parameter_, supplied, inferred)
      }) &&
      infer(pattern.result, actual.result, inferred)
    )
  }
  if (isEffect(pattern) && isEffect(actual)) {
    return (
      pattern.access === actual.access &&
      pattern.failures.length === actual.failures.length &&
      pattern.requirements.length === actual.requirements.length &&
      infer(pattern.success, actual.success, inferred) &&
      pattern.failures.every((failure, index) => {
        const supplied = actual.failures.at(index)
        return supplied !== undefined && infer(failure, supplied, inferred)
      }) &&
      pattern.requirements.every((requirement, index) => {
        const supplied = actual.requirements.at(index)
        return (
          supplied !== undefined &&
          requirement.access === supplied.access &&
          requirement.role === supplied.role &&
          infer(requirement.capability, supplied.capability, inferred)
        )
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
