import * as Scalar from './Scalar.js'

/** The built-in scalar types implemented by the current executable bootstrap surface. */
export type Builtin = Scalar.Spelling

/** The canonical immutable valid-UTF-8 view, distinct from every scalar and byte slice. */
export type String = 'string'

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
export type ParameterKind = 'Value' | 'FailureRow' | 'RequirementRow'

export interface Parameter {
  readonly _tag: 'TypeParameter'
  readonly owner: {
    readonly module: string
    readonly name: string
  }
  readonly ordinal: number
  readonly name: string
  readonly kind: ParameterKind
}

/** A compile-time projection of one failure row into its ordinary structural value sum. */
export interface FailureProjection {
  readonly _tag: 'FailureProjectionType'
  readonly parameter: Parameter
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
  readonly capability: Nominal | Parameter
  readonly role: string
  readonly access: 'Shared' | 'Exclusive'
}

/** One normalized failure-row argument, which may forward an enclosing open row. */
export interface FailureRowArgument {
  readonly _tag: 'FailureRowArgument'
  readonly failures: ReadonlyArray<Nominal>
  readonly parameters: ReadonlyArray<Parameter>
}

/** One concrete normalized requirement-row argument supplied to a requirement-row parameter. */
export interface RequirementRowArgument {
  readonly _tag: 'RequirementRowArgument'
  readonly requirements: ReadonlyArray<Requirement>
  readonly parameters: ReadonlyArray<Parameter>
}

/** One compiler-only hidden Effect construction identity used for monomorphic specialization. */
export interface EffectIdentityArgument {
  readonly _tag: 'EffectIdentityArgument'
  readonly identity: string
}

/** One compiler-only hidden callable identity used for monomorphic higher-order lowering. */
export interface CallableIdentityArgument {
  readonly _tag: 'CallableIdentityArgument'
  readonly identity: string
  readonly target:
    | { readonly _tag: 'Declaration'; readonly module: string; readonly name: string }
    | {
        readonly _tag: 'Builtin'
        readonly actor: string
        readonly operation: string
        readonly intrinsic: { readonly actor: string; readonly name: string }
      }
  readonly typeArguments: ReadonlyArray<GenericArgument>
  readonly environment?: string
}

/** One erased generic argument, including the two compiler-only contract-row kinds. */
export type GenericArgument =
  | Type
  | FailureRowArgument
  | RequirementRowArgument
  | EffectIdentityArgument
  | CallableIdentityArgument

/** One declaration-parameter identity to concrete erased argument mapping. */
export type Substitution = ReadonlyMap<string, GenericArgument>

/** A row-specific explanation for one failed generic decomposition. */
export type RowInferenceFailure =
  | { readonly _tag: 'AbsentFailureMember'; readonly member: string }
  | {
      readonly _tag: 'AmbiguousFailureRemainder'
      readonly parameters: ReadonlyArray<string>
    }
  | {
      readonly _tag: 'AbsentRequirementMember'
      readonly capability: string
      readonly role: string
      readonly access: Requirement['access']
    }
  | {
      readonly _tag: 'IncompatibleRequirementRole'
      readonly capability: string
      readonly expected: string
      readonly actual: ReadonlyArray<string>
    }
  | {
      readonly _tag: 'IncompatibleRequirementAccess'
      readonly capability: string
      readonly role: string
      readonly expected: Requirement['access']
      readonly actual: ReadonlyArray<Requirement['access']>
    }
  | {
      readonly _tag: 'AmbiguousRequirementRemainder'
      readonly parameters: ReadonlyArray<string>
    }
  | { readonly _tag: 'NonFiniteFailureRow' }
  | { readonly _tag: 'NonFiniteRequirementRow' }

/** A compiler-private lazy effect contract. Effect values never cross the executable ABI. */
export interface Effect {
  readonly _tag: 'EffectType'
  readonly success: Type
  readonly failures: ReadonlyArray<Nominal>
  readonly failureParameters: ReadonlyArray<Parameter>
  readonly requirements: ReadonlyArray<Requirement>
  readonly requirementParameters: ReadonlyArray<Parameter>
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
  | String
  | Bottom
  | Nominal
  | Parameter
  | FailureProjection
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

/** The canonical lowercase string identity used by source and every compiler phase. */
export const string: String = 'string'

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
export const layout: Nominal = nominal('silk/layout', 'Layout')
export const invalidAlignment: Nominal = nominal('silk/layout', 'InvalidAlignment')
export const layoutOverflow: Nominal = nominal('silk/layout', 'LayoutOverflow')
/** The implementation-erased allocation capability requested by allocation Effects. */
export const allocator: Nominal = nominal('silk/core', 'Allocator')
/** Explicit host capability for complete stdout and stderr byte writes. */
export const standardStreams: Nominal = nominal('silk/core', 'StandardStreams')
/** Allocation-free typed failure returned when a host cannot commit a complete write. */
export const streamWriteFailure: Nominal = nominal('silk/core', 'StreamWriteFailure')
/** A self-contained affine owner carrying one private active reclaim ticket. */
export const allocation: Nominal = nominal('silk/core', 'Allocation')
/** Opaque affine native file-or-directory handle used only by unsafe OS intrinsics. */
export const osHandle: Nominal = nominal('silk/core', 'OsHandle')
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

/** Canonical completed Effect outcome data shipped by silk/result. */
export const resultSuccess = (value: Type): Nominal => nominal('silk/result', 'Success', [value])
export const resultFailure = (error: Type): Nominal => nominal('silk/result', 'Failure', [error])
export const result = (value: Type, error: Type): Nominal =>
  nominal('silk/result', 'Result', [value, error])

/** Projects a closed normalized failure row to its ordinary runtime value sum. */
export const failureValue = (failures: ReadonlyArray<Nominal>): Type => {
  const normalized = union(failures)
  return normalized._tag === 'Normalized' ? normalized.type : 'never'
}

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
  [allocation.name, allocation],
  [osHandle.name, osHandle],
  [dropCapability.name, dropCapability],
  [reportCapability.name, reportCapability],
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
export const intrinsicConformances: ReadonlyMap<string, ReadonlySet<string>> = new Map([])

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
  kind: ParameterKind = 'Value',
): Parameter =>
  Object.freeze({
    _tag: 'TypeParameter',
    owner: Object.freeze({ module: owner.module, name: owner.name }),
    ordinal,
    name,
    kind,
  })

/** Projects a failure-row parameter into the value type formed by its normalized members. */
export const failureProjection = (parameter_: Parameter): FailureProjection =>
  Object.freeze({ _tag: 'FailureProjectionType', parameter: parameter_ })

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
  failureParameters: ReadonlyArray<Parameter> = [],
  requirementParameters: ReadonlyArray<Parameter> = [],
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
    failureParameters: Object.freeze(
      [
        ...new Map(failureParameters.map((parameter_) => [key(parameter_), parameter_])).values(),
      ].sort(compare),
    ),
    requirements: Object.freeze(
      [...normalizedRequirements.values()].sort((left, right) =>
        compareText(
          `${key(left.capability)}@${left.role}`,
          `${key(right.capability)}@${right.role}`,
        ),
      ),
    ),
    requirementParameters: Object.freeze(
      [
        ...new Map(
          requirementParameters.map((parameter_) => [key(parameter_), parameter_]),
        ).values(),
      ].sort(compare),
    ),
    access,
  })
}

/** Constructs one normalized failure-row generic argument. */
export const failureRowArgument = (
  failures: ReadonlyArray<Nominal>,
  parameters: ReadonlyArray<Parameter> = [],
): FailureRowArgument =>
  Object.freeze({
    _tag: 'FailureRowArgument',
    failures: effect('never', failures, 'Shared', [], parameters).failures,
    parameters: effect('never', failures, 'Shared', [], parameters).failureParameters,
  })

/** Constructs one normalized concrete requirement-row generic argument. */
export const requirementRowArgument = (
  requirements: ReadonlyArray<Requirement>,
  parameters: ReadonlyArray<Parameter> = [],
): RequirementRowArgument =>
  Object.freeze({
    _tag: 'RequirementRowArgument',
    requirements: effect('never', [], 'Shared', requirements, [], parameters).requirements,
    parameters: effect('never', [], 'Shared', requirements, [], parameters).requirementParameters,
  })

export const effectIdentityArgument = (identity: string): EffectIdentityArgument =>
  Object.freeze({ _tag: 'EffectIdentityArgument', identity })

export const callableIdentityArgument = (
  identity: string,
  target: CallableIdentityArgument['target'],
  typeArguments: ReadonlyArray<GenericArgument> = [],
  environment?: string,
): CallableIdentityArgument =>
  Object.freeze({
    _tag: 'CallableIdentityArgument',
    identity,
    target: Object.freeze(target),
    typeArguments: Object.freeze(Array.from(typeArguments)),
    ...(environment === undefined ? {} : { environment }),
  })

export const isFailureRowArgument = (self: GenericArgument): self is FailureRowArgument =>
  typeof self !== 'string' && self._tag === 'FailureRowArgument'

export const isRequirementRowArgument = (self: GenericArgument): self is RequirementRowArgument =>
  typeof self !== 'string' && self._tag === 'RequirementRowArgument'

export const isEffectIdentityArgument = (self: GenericArgument): self is EffectIdentityArgument =>
  typeof self !== 'string' && self._tag === 'EffectIdentityArgument'

export const isCallableIdentityArgument = (
  self: GenericArgument,
): self is CallableIdentityArgument =>
  typeof self !== 'string' && self._tag === 'CallableIdentityArgument'

export const isHiddenIdentityArgument = (
  self: GenericArgument,
): self is EffectIdentityArgument | CallableIdentityArgument =>
  isEffectIdentityArgument(self) || isCallableIdentityArgument(self)

export const isTypeArgument = (self: GenericArgument): self is Type =>
  !isFailureRowArgument(self) && !isRequirementRowArgument(self) && !isHiddenIdentityArgument(self)

/** Returns the canonical deterministic identity of any erased generic argument. */
export const genericArgumentKey = (self: GenericArgument): string =>
  isEffectIdentityArgument(self)
    ? `effect-identity:${self.identity}`
    : isCallableIdentityArgument(self)
      ? `callable-identity:${self.identity}`
      : isFailureRowArgument(self)
        ? `failure-row:${self.failures.map(key).join('|')};${self.parameters.map(key).join('|')}`
        : isRequirementRowArgument(self)
          ? `requirement-row:${self.requirements
              .map(
                (requirement) =>
                  `${requirement.access}:${key(requirement.capability)}@${requirement.role}`,
              )
              .join('|')};${self.parameters.map(key).join('|')}`
          : key(self)

/** Encodes any erased generic argument for semantic presentation and artifact inspection. */
export const encodeGenericArgument = (self: GenericArgument): string =>
  isEffectIdentityArgument(self)
    ? `effect@${self.identity}`
    : isCallableIdentityArgument(self)
      ? `callable@${self.identity}`
      : isFailureRowArgument(self)
        ? `! ${
            [
              ...self.failures.map(encode),
              ...self.parameters.map((parameter_) => parameter_.name),
            ].join(' | ') || 'never'
          }`
        : isRequirementRowArgument(self)
          ? `? ${self.requirements
              .map(
                (requirement) =>
                  `${requirement.access === 'Exclusive' ? '&mut ' : '&'}${encode(requirement.capability)}${requirement.role === 'DefaultRole' ? '' : `@${requirement.role}`}`,
              )
              .concat(self.parameters.map((parameter_) => parameter_.name))
              .join(' | ')}`
          : encode(self)

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

/** Tests whether a semantic type is the canonical immutable UTF-8 string view. */
export const isString = (self: unknown): self is String => self === string

/** Tests whether a semantic type is the empty structural union. */
export const isNever = (self: Type): self is Bottom => self === 'never'

/** Tests whether a semantic type is a canonical nominal struct. */
export const isNominal = (self: Type): self is Nominal =>
  typeof self !== 'string' && self._tag === 'NominalType'

/** Tests whether a semantic type is a declaration-owned generic parameter. */
export const isParameter = (self: Type): self is Parameter =>
  typeof self !== 'string' && self._tag === 'TypeParameter'

export const isFailureProjection = (self: Type): self is FailureProjection =>
  typeof self !== 'string' && self._tag === 'FailureProjectionType'

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

const keyCache = new WeakMap<Exclude<Type, string>, string>()

/** Returns the canonical deterministic key used for equality and ordering. */
export const key = (self: Type): string => {
  if (typeof self === 'string') return computeKey(self)
  let cached = keyCache.get(self)
  if (cached === undefined) {
    cached = computeKey(self)
    keyCache.set(self, cached)
  }
  return cached
}

const computeKey = (self: Type): string => {
  if (isString(self)) return 'string'
  if (isBuiltin(self)) return `builtin:${self}`
  if (isNever(self)) return 'union:'
  if (isNominal(self))
    return `nominal:${self.module}.${self.name}<${self.arguments.map(key).join(',')}>`
  if (isParameter(self))
    return `parameter:${self.kind}:${self.owner.module}.${self.owner.name}:${self.ordinal}`
  if (isFailureProjection(self)) return `failure-projection:${key(self.parameter)}`
  if (isFixedArray(self)) return `array:${self.length}<${key(self.element)}>`
  if (isSlice(self)) return `slice:${self.access}<${key(self.element)}>`
  if (isReference(self)) return `reference:${self.access}<${key(self.target)}>`
  if (isCallable(self))
    return `callable:${self.mode}<(${self.parameters.map(key).join(',')})->${key(self.result)}>`
  if (isEffect(self))
    return `effect:${self.access}<${key(self.success)}!${[
      ...self.failures.map(key),
      ...self.failureParameters.map(key),
    ].join('|')}?${[
      ...self.requirements.map(
        (requirement) => `${requirement.access}:${key(requirement.capability)}@${requirement.role}`,
      ),
      ...self.requirementParameters.map(key),
    ].join('|')}>`
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
  if (isFailureProjection(self)) return `Row<! ${self.parameter.name}>`
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
    const access = self.access === 'Exclusive' ? 'mut ' : self.access === 'Take' ? 'once ' : ''
    const failureMembers = [
      ...self.failures.map(encode),
      ...self.failureParameters.map((parameter_) => parameter_.name),
    ]
    const row = failureMembers.length === 0 ? '' : ` ! ${failureMembers.join(' | ')}`
    const requirements =
      self.requirements.length === 0 && self.requirementParameters.length === 0
        ? ''
        : ` ? ${[
            ...self.requirements.map(
              (requirement) =>
                `${requirement.access === 'Exclusive' ? '&mut ' : '&'}${encode(requirement.capability)}${requirement.role === 'DefaultRole' ? '' : `@${requirement.role}`}`,
            ),
            ...self.requirementParameters.map((parameter_) => parameter_.name),
          ].join(' | ')}`
    return `${access}Effect<${encode(self.success)}${row}${requirements}>`
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
    } else if (isFailureProjection(type)) {
      found.set(key(type.parameter), type.parameter)
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
      for (const parameter_ of type.failureParameters) visit(parameter_)
      for (const requirement of type.requirements) visit(requirement.capability)
      for (const parameter_ of type.requirementParameters) visit(parameter_)
    } else if (isUnion(type)) for (const member of type.members) visit(member)
  }
  visit(self)
  return Object.freeze([...found.values()].sort(compare))
}

/** Tests whether a type contains no open generic parameters. */
export const isConcrete = (self: Type): boolean => parameters(self).length === 0

/** Tests whether an erased generic argument contains no open value or row parameters. */
export const isConcreteGenericArgument = (self: GenericArgument): boolean =>
  isEffectIdentityArgument(self)
    ? true
    : isCallableIdentityArgument(self)
      ? self.typeArguments.every(isConcreteGenericArgument)
      : isFailureRowArgument(self)
        ? self.parameters.length === 0 && self.failures.every(isConcrete)
        : isRequirementRowArgument(self)
          ? self.parameters.length === 0 &&
            self.requirements.every((requirement) => isConcrete(requirement.capability))
          : isConcrete(self)

/** Tests whether a type contains a lexical borrow at any depth. */
export const containsBorrow = (self: Type): boolean => {
  if (isString(self)) return true
  if (isSlice(self) || isReference(self)) return true
  if (isSlot(self)) return true
  if (isFailureProjection(self)) return false
  if (isNominal(self)) return self.arguments.some(containsBorrow)
  if (isFixedArray(self)) return containsBorrow(self.element)
  if (isCallable(self)) return self.parameters.some(containsBorrow) || containsBorrow(self.result)
  if (isEffect(self)) return containsBorrow(self.success) || self.failures.some(containsBorrow)
  if (isUnion(self)) return self.members.some(containsBorrow)
  return false
}

/** Tests whether a value may carry a lexical immutable view through data or control flow. */
export const containsViewBorrow = (self: Type): boolean => {
  if (isString(self) || isSlice(self)) return true
  if (isNominal(self)) return self.arguments.some(containsViewBorrow)
  if (isFixedArray(self)) return containsViewBorrow(self.element)
  if (isCallable(self))
    return self.parameters.some(containsViewBorrow) || containsViewBorrow(self.result)
  if (isEffect(self))
    return containsViewBorrow(self.success) || self.failures.some(containsViewBorrow)
  if (isUnion(self)) return self.members.some(containsViewBorrow)
  return false
}

/** Tests for explicit borrow wrappers forbidden inside ordinary type positions. */
export const containsPositionRestrictedBorrow = (self: Type): boolean => {
  if (isString(self)) return false
  if (isSlice(self) || isReference(self) || isSlot(self)) return true
  if (isNominal(self)) return self.arguments.some(containsPositionRestrictedBorrow)
  if (isFixedArray(self)) return containsPositionRestrictedBorrow(self.element)
  if (isCallable(self))
    return (
      self.parameters.some(containsPositionRestrictedBorrow) ||
      containsPositionRestrictedBorrow(self.result)
    )
  if (isEffect(self))
    return (
      containsPositionRestrictedBorrow(self.success) ||
      self.failures.some(containsPositionRestrictedBorrow)
    )
  if (isUnion(self)) return self.members.some(containsPositionRestrictedBorrow)
  return false
}

/** Replaces declaration-owned parameters recursively through one canonical type. */
export const substitute = (self: Type, substitution: Substitution): Type => {
  if (isParameter(self)) {
    const replacement = substitution.get(key(self))
    return replacement !== undefined && isTypeArgument(replacement) ? replacement : self
  }
  if (isFailureProjection(self)) {
    const replacement = substitution.get(key(self.parameter))
    if (replacement === undefined || !isFailureRowArgument(replacement)) return self
    const forwarded = replacement.parameters.at(0)
    if (replacement.failures.length === 0 && replacement.parameters.length === 1 && forwarded)
      return failureProjection(forwarded)
    const normalized = union(replacement.failures)
    return normalized._tag === 'Normalized' ? normalized.type : self
  }
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
    const expandedFailures: Array<Nominal> = []
    const failureParameters: Array<Parameter> = []
    for (const parameter_ of self.failureParameters) {
      const replacement = substitution.get(key(parameter_))
      if (replacement !== undefined && isFailureRowArgument(replacement)) {
        expandedFailures.push(...replacement.failures)
        failureParameters.push(...replacement.parameters)
      } else failureParameters.push(parameter_)
    }
    const expandedRequirements: Array<Requirement> = []
    const requirementParameters: Array<Parameter> = []
    for (const parameter_ of self.requirementParameters) {
      const replacement = substitution.get(key(parameter_))
      if (replacement !== undefined && isRequirementRowArgument(replacement)) {
        expandedRequirements.push(...replacement.requirements)
        requirementParameters.push(...replacement.parameters)
      } else requirementParameters.push(parameter_)
    }
    return effect(
      success,
      [
        ...failures.filter((failure): failure is Nominal => isNominal(failure)),
        ...expandedFailures,
      ],
      self.access,
      [
        ...self.requirements.flatMap((requirement) => {
          const capability = substitute(requirement.capability, substitution)
          return isNominal(capability) || isParameter(capability)
            ? [Object.freeze({ ...requirement, capability })]
            : []
        }),
        ...expandedRequirements,
      ],
      failureParameters,
      requirementParameters,
    )
  }
  if (isUnion(self)) {
    const normalized = union(self.members.map((member) => substitute(member, substitution)))
    return normalized._tag === 'Normalized' ? normalized.type : self
  }
  return self
}

/** Substitutes nested value parameters inside any erased generic argument. */
export const substituteGenericArgument = (
  self: GenericArgument,
  substitution: Substitution,
): GenericArgument =>
  isEffectIdentityArgument(self)
    ? self
    : isCallableIdentityArgument(self)
      ? callableIdentityArgument(
          self.identity,
          self.target,
          self.typeArguments.map((argument) => substituteGenericArgument(argument, substitution)),
          self.environment,
        )
      : isFailureRowArgument(self)
        ? failureRowArgument(
            [
              ...self.failures.flatMap((failure) => {
                const specialized = substitute(failure, substitution)
                return isNominal(specialized) ? [specialized] : []
              }),
              ...self.parameters.flatMap((parameter_) => {
                const replacement = substitution.get(key(parameter_))
                return replacement !== undefined && isFailureRowArgument(replacement)
                  ? replacement.failures
                  : []
              }),
            ],
            self.parameters.flatMap((parameter_) => {
              const replacement = substitution.get(key(parameter_))
              return replacement !== undefined && isFailureRowArgument(replacement)
                ? replacement.parameters
                : [parameter_]
            }),
          )
        : isRequirementRowArgument(self)
          ? requirementRowArgument(
              [
                ...self.requirements.flatMap((requirement) => {
                  const capability = substitute(requirement.capability, substitution)
                  return isNominal(capability) || isParameter(capability)
                    ? [Object.freeze({ ...requirement, capability })]
                    : []
                }),
                ...self.parameters.flatMap((parameter_) => {
                  const replacement = substitution.get(key(parameter_))
                  return replacement !== undefined && isRequirementRowArgument(replacement)
                    ? replacement.requirements
                    : []
                }),
              ],
              self.parameters.flatMap((parameter_) => {
                const replacement = substitution.get(key(parameter_))
                return replacement !== undefined && isRequirementRowArgument(replacement)
                  ? replacement.parameters
                  : [parameter_]
              }),
            )
          : substitute(self, substitution)

/** Adds structural constraints from one declared type pattern to one supplied concrete type. */
const bindGenericArgument = (
  parameter_: Parameter,
  actual: GenericArgument,
  inferred: Map<string, GenericArgument>,
): boolean => {
  const identity = key(parameter_)
  const existing = inferred.get(identity)
  if (existing === undefined) {
    inferred.set(identity, actual)
    return true
  }
  return genericArgumentKey(existing) === genericArgumentKey(actual)
}

const commitInference = (
  target: Map<string, GenericArgument>,
  source: ReadonlyMap<string, GenericArgument>,
): void => {
  target.clear()
  for (const [identity, argument] of source) target.set(identity, argument)
}

const inferFailureRows = (
  pattern: Effect,
  actual: Effect,
  inferred: Map<string, GenericArgument>,
): boolean => {
  const remaining = [...actual.failures]
  for (const failure of pattern.failures) {
    let matched = false
    for (const [index, supplied] of remaining.entries()) {
      const trial = new Map(inferred)
      if (!infer(failure, supplied, trial)) continue
      remaining.splice(index, 1)
      commitInference(inferred, trial)
      matched = true
      break
    }
    if (!matched) return false
  }
  if (pattern.failureParameters.length === 0) return remaining.length === 0
  const remainder = failureRowArgument(remaining, actual.failureParameters)
  const parameter_ = pattern.failureParameters.at(0)
  return (
    pattern.failureParameters.length === 1 &&
    parameter_ !== undefined &&
    bindGenericArgument(parameter_, remainder, inferred)
  )
}

const inferRequirementRows = (
  pattern: Effect,
  actual: Effect,
  inferred: Map<string, GenericArgument>,
): boolean => {
  const remaining = [...actual.requirements]
  for (const requirement of pattern.requirements) {
    let matched = false
    for (const [index, supplied] of remaining.entries()) {
      if (
        (requirement.access !== supplied.access && requirement.access !== 'Exclusive') ||
        requirement.role !== supplied.role
      )
        continue
      const trial = new Map(inferred)
      if (!infer(requirement.capability, supplied.capability, trial)) continue
      remaining.splice(index, 1)
      commitInference(inferred, trial)
      matched = true
      break
    }
    if (!matched) return false
  }
  if (pattern.requirementParameters.length === 0) return remaining.length === 0
  const remainder = requirementRowArgument(remaining, actual.requirementParameters)
  const parameter_ = pattern.requirementParameters.at(0)
  return (
    pattern.requirementParameters.length === 1 &&
    parameter_ !== undefined &&
    bindGenericArgument(parameter_, remainder, inferred)
  )
}

/** Explains a failed Effect-row decomposition without replacing ordinary type diagnostics. */
export const rowInferenceFailure = (
  pattern: Type,
  actual: Type,
): RowInferenceFailure | undefined => {
  if (isNominal(pattern) && isNominal(actual)) {
    if (
      pattern.module !== actual.module ||
      pattern.name !== actual.name ||
      pattern.arguments.length !== actual.arguments.length
    )
      return undefined
    for (const [index, argument] of pattern.arguments.entries()) {
      const supplied = actual.arguments.at(index)
      if (supplied === undefined) continue
      const failure = rowInferenceFailure(argument, supplied)
      if (failure !== undefined) return failure
    }
    return undefined
  }
  if (isFixedArray(pattern) && isFixedArray(actual))
    return rowInferenceFailure(pattern.element, actual.element)
  if (isSlice(pattern) && isSlice(actual))
    return rowInferenceFailure(pattern.element, actual.element)
  if (isReference(pattern) && isReference(actual))
    return rowInferenceFailure(pattern.target, actual.target)
  if (isCallable(pattern) && isCallable(actual)) {
    for (const [index, parameter_] of pattern.parameters.entries()) {
      const supplied = actual.parameters.at(index)
      if (supplied === undefined) continue
      const failure = rowInferenceFailure(parameter_, supplied)
      if (failure !== undefined) return failure
    }
    return rowInferenceFailure(pattern.result, actual.result)
  }
  if (!isEffect(pattern) || !isEffect(actual)) return undefined
  if (actual.failureParameters.length !== 0) return Object.freeze({ _tag: 'NonFiniteFailureRow' })
  if (actual.requirementParameters.length !== 0)
    return Object.freeze({ _tag: 'NonFiniteRequirementRow' })
  if (pattern.failureParameters.length > 1)
    return Object.freeze({
      _tag: 'AmbiguousFailureRemainder',
      parameters: Object.freeze(pattern.failureParameters.map((parameter_) => parameter_.name)),
    })
  for (const failure of pattern.failures) {
    if (!actual.failures.some((supplied) => infer(failure, supplied, new Map())))
      return Object.freeze({ _tag: 'AbsentFailureMember', member: encode(failure) })
  }
  if (pattern.requirementParameters.length > 1)
    return Object.freeze({
      _tag: 'AmbiguousRequirementRemainder',
      parameters: Object.freeze(pattern.requirementParameters.map((parameter_) => parameter_.name)),
    })
  for (const requirement of pattern.requirements) {
    const capabilityMatches = actual.requirements.filter((supplied) =>
      infer(requirement.capability, supplied.capability, new Map()),
    )
    if (capabilityMatches.length === 0)
      return Object.freeze({
        _tag: 'AbsentRequirementMember',
        capability: encode(requirement.capability),
        role: requirement.role,
        access: requirement.access,
      })
    const roleMatches = capabilityMatches.filter((supplied) => supplied.role === requirement.role)
    if (roleMatches.length === 0)
      return Object.freeze({
        _tag: 'IncompatibleRequirementRole',
        capability: encode(requirement.capability),
        expected: requirement.role,
        actual: Object.freeze(
          [...new Set(capabilityMatches.map((supplied) => supplied.role))].sort(),
        ),
      })
    if (
      !roleMatches.some(
        (supplied) => supplied.access === requirement.access || requirement.access === 'Exclusive',
      )
    )
      return Object.freeze({
        _tag: 'IncompatibleRequirementAccess',
        capability: encode(requirement.capability),
        role: requirement.role,
        expected: requirement.access,
        actual: Object.freeze([...new Set(roleMatches.map((supplied) => supplied.access))].sort()),
      })
  }
  return undefined
}

export const infer = (
  pattern: Type,
  actual: Type,
  inferred: Map<string, GenericArgument>,
): boolean => {
  if (isParameter(pattern)) {
    return pattern.kind === 'Value' && bindGenericArgument(pattern, actual, inferred)
  }
  if (isFailureProjection(pattern)) {
    const failures = isNever(actual)
      ? Object.freeze([])
      : isNominal(actual)
        ? Object.freeze([actual])
        : isUnion(actual)
          ? actual.members
          : undefined
    return (
      (failures !== undefined &&
        bindGenericArgument(pattern.parameter, failureRowArgument(failures), inferred)) ||
      (isFailureProjection(actual) &&
        bindGenericArgument(
          pattern.parameter,
          failureRowArgument([], [actual.parameter]),
          inferred,
        ))
    )
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
    const patternRank = pattern.mode === 'Shared' ? 0 : pattern.mode === 'Exclusive' ? 1 : 2
    const actualRank = actual.mode === 'Shared' ? 0 : actual.mode === 'Exclusive' ? 1 : 2
    return (
      actualRank <= patternRank &&
      pattern.parameters.length === actual.parameters.length &&
      pattern.parameters.every((parameter_, index) => {
        const supplied = actual.parameters.at(index)
        return supplied !== undefined && infer(parameter_, supplied, inferred)
      }) &&
      infer(pattern.result, actual.result, inferred)
    )
  }
  if (isEffect(pattern) && isEffect(actual)) {
    const patternRank = pattern.access === 'Shared' ? 0 : pattern.access === 'Exclusive' ? 1 : 2
    const actualRank = actual.access === 'Shared' ? 0 : actual.access === 'Exclusive' ? 1 : 2
    return (
      actualRank <= patternRank &&
      infer(pattern.success, actual.success, inferred) &&
      inferFailureRows(pattern, actual, inferred) &&
      inferRequirementRows(pattern, actual, inferred)
    )
  }
  return equals(pattern, actual)
}

/** Builds a substitution from ordered parameters and arguments when their arities match. */
export const substitution = (
  declared: ReadonlyArray<Parameter>,
  arguments_: ReadonlyArray<GenericArgument>,
): Substitution | undefined => {
  if (declared.length !== arguments_.length) return undefined
  const result = new Map<string, GenericArgument>()
  for (const [index, parameter_] of declared.entries()) {
    const argument = arguments_.at(index)
    if (argument === undefined) return undefined
    if (
      (parameter_.kind === 'Value' && !isTypeArgument(argument)) ||
      (parameter_.kind === 'FailureRow' && !isFailureRowArgument(argument)) ||
      (parameter_.kind === 'RequirementRow' && !isRequirementRowArgument(argument))
    )
      return undefined
    result.set(key(parameter_), argument)
  }
  return result
}
