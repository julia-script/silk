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
  readonly arguments: ReadonlyArray<GenericArgument>
}

/** One declaration-owned generic type parameter. Names are provenance, not identity. */
export type ParameterKind =
  | 'Value'
  | 'FailureRow'
  | 'RequirementRow'
  | 'CallableRepresentation'
  | 'EffectRepresentation'

export interface Parameter {
  readonly _tag: 'TypeParameter'
  readonly owner: {
    readonly module: string
    readonly name: string
  }
  readonly ordinal: number
  readonly name: string
  readonly kind: ParameterKind
  readonly representationBound?: RepresentationBound
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

/** A structural contract that may bound one statically known executable representation. */
export type RepresentationBound = Callable | Effect

/** An open reference to one declaration-owned representation parameter. */
export interface RepresentationParameterArgument {
  readonly _tag: 'RepresentationParameterArgument'
  readonly parameter: Parameter
}

/** One exact callable or Effect construction together with its intrinsic contract. */
export interface ExactRepresentationArgument {
  readonly _tag: 'ExactRepresentationArgument'
  readonly identity: EffectIdentityArgument | CallableIdentityArgument
  readonly contract: RepresentationBound
}

/** A statically known representation supplied to a representation parameter. */
export type RepresentationArgument = RepresentationParameterArgument | ExactRepresentationArgument

/** A deterministic recovery placeholder that never reaches specialization or runtime phases. */
export interface UnavailableGenericArgument {
  readonly _tag: 'UnavailableGenericArgument'
  readonly expectedKind: ParameterKind
  readonly reason: string
}

/** Evidence that one intrinsic representation contract is admissible at a required bound. */
export type RepresentationAdmissibility =
  | { readonly _tag: 'Open' }
  | { readonly _tag: 'Admitted' }
  | { readonly _tag: 'Unavailable'; readonly reason: string }

/** One use of an executable representation under a declaration-owned required bound. */
export interface RepresentationUse {
  readonly requiredBound: RepresentationBound
  readonly argument: RepresentationArgument
  readonly admissibility: RepresentationAdmissibility
}

/** A callable or Effect value whose exact representation participates in static type identity. */
export interface Represented {
  readonly _tag: 'RepresentedType'
  readonly contract: RepresentationBound
  readonly representation: RepresentationUse
}

/** One erased generic argument, including the two compiler-only contract-row kinds. */
export type GenericArgument =
  | Type
  | FailureRowArgument
  | RequirementRowArgument
  | EffectIdentityArgument
  | CallableIdentityArgument
  | RepresentationArgument
  | UnavailableGenericArgument

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
  | Represented
  | StructuralUnion

/** A semantic type admissible in an ordinary type-parameter argument slot. */
export type OrdinaryType = Exclude<Type, Represented>

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
  arguments_: ReadonlyArray<GenericArgument> = [],
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
} => {
  if (!isNominal(self) || self.module !== 'silk/core' || self.name !== 'RawBuffer') return false
  const argument = self.arguments.at(0)
  return self.arguments.length === 1 && argument !== undefined && isTypeArgument(argument)
}

export const isSlot = (
  self: Type,
): self is Nominal & {
  readonly module: 'silk/core'
  readonly name: 'Slot'
  readonly arguments: readonly [Type]
} => {
  if (!isNominal(self) || self.module !== 'silk/core' || self.name !== 'Slot') return false
  const argument = self.arguments.at(0)
  return self.arguments.length === 1 && argument !== undefined && isTypeArgument(argument)
}

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
  representationBound?: RepresentationBound,
): Parameter =>
  Object.freeze({
    _tag: 'TypeParameter',
    owner: Object.freeze({ module: owner.module, name: owner.name }),
    ordinal,
    name,
    kind,
    ...(representationBound === undefined ? {} : { representationBound }),
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

/** Constructs an open representation argument owned by one representation parameter. */
export const representationParameterArgument = (
  parameter_: Parameter,
): RepresentationParameterArgument =>
  Object.freeze({ _tag: 'RepresentationParameterArgument', parameter: parameter_ })

/** Constructs one exact representation argument without mixing its identity with a use bound. */
export const exactRepresentationArgument = (
  identity: EffectIdentityArgument | CallableIdentityArgument,
  contract: RepresentationBound,
): ExactRepresentationArgument =>
  Object.freeze({ _tag: 'ExactRepresentationArgument', identity, contract })

const accessRank = (access: CallableMode | Effect['access']): number =>
  access === 'Shared' ? 0 : access === 'Exclusive' ? 1 : 2

/**
 * Intersects two uses of one representation contract. The result keeps the most restrictive
 * access while rejecting structurally unrelated callable or Effect contracts.
 */
export const intersectRepresentationBounds = (
  left: RepresentationBound,
  right: RepresentationBound,
): RepresentationBound | undefined => {
  if (left._tag !== right._tag) return undefined
  const access =
    accessRank(left._tag === 'CallableType' ? left.mode : left.access) <=
    accessRank(right._tag === 'CallableType' ? right.mode : right.access)
      ? left._tag === 'CallableType'
        ? left.mode
        : left.access
      : right._tag === 'CallableType'
        ? right.mode
        : right.access
  if (left._tag === 'CallableType' && right._tag === 'CallableType') {
    const leftShape = callable(left.parameters, left.result)
    const rightShape = callable(right.parameters, right.result)
    return equals(leftShape, rightShape)
      ? callable(left.parameters, left.result, access)
      : undefined
  }
  if (left._tag === 'EffectType' && right._tag === 'EffectType') {
    const leftShape = effect(
      left.success,
      left.failures,
      'Shared',
      left.requirements,
      left.failureParameters,
      left.requirementParameters,
    )
    const rightShape = effect(
      right.success,
      right.failures,
      'Shared',
      right.requirements,
      right.failureParameters,
      right.requirementParameters,
    )
    return equals(leftShape, rightShape)
      ? effect(
          left.success,
          left.failures,
          access,
          left.requirements,
          left.failureParameters,
          left.requirementParameters,
        )
      : undefined
  }
  return undefined
}

/** Checks structural contract equality and the shared/exclusive/take admissibility ordering. */
export const representationAdmissibility = (
  contract: RepresentationBound,
  requiredBound: RepresentationBound,
): RepresentationAdmissibility => {
  if (contract._tag !== requiredBound._tag)
    return Object.freeze({ _tag: 'Unavailable', reason: 'representation kind mismatch' })
  const structuralContract =
    contract._tag === 'CallableType' && requiredBound._tag === 'CallableType'
      ? callable(contract.parameters, contract.result, requiredBound.mode)
      : contract._tag === 'EffectType' && requiredBound._tag === 'EffectType'
        ? effect(
            contract.success,
            contract.failures,
            requiredBound.access,
            contract.requirements,
            contract.failureParameters,
            contract.requirementParameters,
          )
        : undefined
  const requiredAccess =
    requiredBound._tag === 'CallableType' ? requiredBound.mode : requiredBound.access
  const actualAccess = contract._tag === 'CallableType' ? contract.mode : contract.access
  return structuralContract !== undefined &&
    equals(structuralContract, requiredBound) &&
    accessRank(actualAccess) <= accessRank(requiredAccess)
    ? Object.freeze({ _tag: 'Admitted' })
    : Object.freeze({ _tag: 'Unavailable', reason: 'representation contract mismatch' })
}

/** Constructs a represented callable or Effect value at one required use bound. */
export const represented = (
  contract: RepresentationBound,
  requiredBound: RepresentationBound,
  argument: RepresentationArgument,
): Represented => {
  const admissibility = representationAdmissibility(contract, requiredBound)
  return Object.freeze({
    _tag: 'RepresentedType',
    contract,
    representation: Object.freeze({
      requiredBound,
      argument,
      admissibility:
        argument._tag === 'RepresentationParameterArgument'
          ? admissibility._tag === 'Admitted'
            ? Object.freeze({ _tag: 'Open' as const })
            : admissibility
          : admissibility,
    }),
  })
}

/** Constructs a kinded recovery placeholder for damaged or unresolved generic syntax. */
export const unavailableGenericArgument = (
  expectedKind: ParameterKind,
  reason: string,
): UnavailableGenericArgument =>
  Object.freeze({ _tag: 'UnavailableGenericArgument', expectedKind, reason })

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

export const isRepresentationParameterArgument = (
  self: GenericArgument,
): self is RepresentationParameterArgument =>
  typeof self !== 'string' && self._tag === 'RepresentationParameterArgument'

export const isExactRepresentationArgument = (
  self: GenericArgument,
): self is ExactRepresentationArgument =>
  typeof self !== 'string' && self._tag === 'ExactRepresentationArgument'

export const isRepresentationArgument = (self: GenericArgument): self is RepresentationArgument =>
  isRepresentationParameterArgument(self) || isExactRepresentationArgument(self)

export const isUnavailableGenericArgument = (
  self: GenericArgument,
): self is UnavailableGenericArgument =>
  typeof self !== 'string' && self._tag === 'UnavailableGenericArgument'

/** Returns the callable/Effect generic kind carried by one representation argument. */
export const representationArgumentKind = (
  self: RepresentationArgument,
): 'CallableRepresentation' | 'EffectRepresentation' =>
  self._tag === 'RepresentationParameterArgument'
    ? self.parameter.kind === 'EffectRepresentation'
      ? 'EffectRepresentation'
      : 'CallableRepresentation'
    : self.contract._tag === 'EffectType'
      ? 'EffectRepresentation'
      : 'CallableRepresentation'

const representationArgumentContract = (
  self: RepresentationArgument,
): RepresentationBound | undefined =>
  self._tag === 'ExactRepresentationArgument' ? self.contract : self.parameter.representationBound

export const isHiddenIdentityArgument = (
  self: GenericArgument,
): self is EffectIdentityArgument | CallableIdentityArgument =>
  isEffectIdentityArgument(self) || isCallableIdentityArgument(self)

export const isTypeArgument = (self: GenericArgument): self is OrdinaryType =>
  !isFailureRowArgument(self) &&
  !isRequirementRowArgument(self) &&
  !isHiddenIdentityArgument(self) &&
  !isRepresentationArgument(self) &&
  !isUnavailableGenericArgument(self) &&
  !(isParameter(self) && self.kind !== 'Value') &&
  !(typeof self !== 'string' && self._tag === 'RepresentedType')

/** Reads one ordinary type argument without erasing the other generic argument kinds. */
export const typeArgumentAt = (self: Nominal, ordinal: number): Type | undefined => {
  const argument = self.arguments.at(ordinal)
  return argument !== undefined && isTypeArgument(argument) ? argument : undefined
}

/** Returns the canonical deterministic identity of any erased generic argument. */
const callableIdentityKey = (self: CallableIdentityArgument): string =>
  [
    'callable-identity:',
    self.identity,
    ':target=',
    self.target._tag === 'Declaration'
      ? `declaration:${self.target.module}.${self.target.name}`
      : `builtin:${self.target.actor}.${self.target.operation}:${self.target.intrinsic.actor}.${self.target.intrinsic.name}`,
    ':arguments=<',
    self.typeArguments.map(genericArgumentKey).join(','),
    '>:environment=',
    self.environment ?? '',
  ].join('')

export const genericArgumentKey = (self: GenericArgument): string =>
  isUnavailableGenericArgument(self)
    ? `unavailable:${self.expectedKind}:${self.reason}`
    : isRepresentationParameterArgument(self)
      ? `representation-parameter:${key(self.parameter)}`
      : isExactRepresentationArgument(self)
        ? `exact-representation:${genericArgumentKey(self.identity)}:${key(self.contract)}`
        : isEffectIdentityArgument(self)
          ? `effect-identity:${self.identity}`
          : isCallableIdentityArgument(self)
            ? callableIdentityKey(self)
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
  isUnavailableGenericArgument(self)
    ? `<unavailable ${self.expectedKind}: ${self.reason}>`
    : isRepresentationParameterArgument(self)
      ? self.parameter.name
      : isExactRepresentationArgument(self)
        ? `typeof(${encodeRepresentationOrigin(self.identity)})`
        : isEffectIdentityArgument(self)
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

const encodeRepresentationOrigin = (
  self: EffectIdentityArgument | CallableIdentityArgument,
): string =>
  self._tag === 'EffectIdentityArgument'
    ? self.identity
    : `${
        self.target._tag === 'Declaration'
          ? `${self.target.module}.${self.target.name}`
          : `${self.target.actor}.${self.target.operation}`
      }${
        self.environment === undefined
          ? ''
          : `@${self.environment.split('\u0001').at(0) ?? self.environment}`
      }`

const compareText = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0

/** Compares any kinded generic arguments by canonical structural identity. */
export const compareGenericArgument = (left: GenericArgument, right: GenericArgument): number =>
  compareText(genericArgumentKey(left), genericArgumentKey(right))

/** Tests canonical structural equality across all generic argument kinds. */
export const equalsGenericArgument = (left: GenericArgument, right: GenericArgument): boolean =>
  genericArgumentKey(left) === genericArgumentKey(right)

/** Computes one deterministic unsigned FNV-1a hash of a canonical generic argument key. */
export const hashGenericArgument = (self: GenericArgument): number => {
  const value = genericArgumentKey(self)
  let hash = 0x811c9dc5
  for (let index = 0; index < value.length; index += 1) {
    hash ^= value.charCodeAt(index)
    hash = Math.imul(hash, 0x01000193)
  }
  return hash >>> 0
}

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

/** Tests whether a value type carries a statically known executable representation. */
export const isRepresented = (self: Type): self is Represented =>
  typeof self !== 'string' && self._tag === 'RepresentedType'

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
    return `nominal:${self.module}.${self.name}<${self.arguments.map(genericArgumentKey).join(',')}>`
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
  if (isRepresented(self))
    return `represented:${key(self.contract)}:${genericArgumentKey(self.representation.argument)}`
  return `union:${self.members.map(key).join('|')}`
}

/** Compares semantic types by canonical identity. */
export const equals = (left: Type, right: Type): boolean => key(left) === key(right)

export interface RepresentationDivergence {
  readonly left: RepresentationArgument
  readonly right: RepresentationArgument
}

const genericArgumentRepresentationDivergence = (
  left: GenericArgument,
  right: GenericArgument,
): RepresentationDivergence | undefined => {
  if (isRepresentationArgument(left) || isRepresentationArgument(right))
    return isRepresentationArgument(left) &&
      isRepresentationArgument(right) &&
      equalsGenericArgument(left, right)
      ? undefined
      : isRepresentationArgument(left) && isRepresentationArgument(right)
        ? Object.freeze({ left, right })
        : undefined
  if (isFailureRowArgument(left) && isFailureRowArgument(right)) {
    for (
      let ordinal = 0;
      ordinal < Math.min(left.failures.length, right.failures.length);
      ordinal += 1
    ) {
      const leftFailure = left.failures.at(ordinal)
      const rightFailure = right.failures.at(ordinal)
      if (leftFailure === undefined || rightFailure === undefined) continue
      const divergence = firstRepresentationDivergence(leftFailure, rightFailure)
      if (divergence !== undefined) return divergence
    }
    return undefined
  }
  if (isRequirementRowArgument(left) && isRequirementRowArgument(right)) {
    for (
      let ordinal = 0;
      ordinal < Math.min(left.requirements.length, right.requirements.length);
      ordinal += 1
    ) {
      const leftRequirement = left.requirements.at(ordinal)
      const rightRequirement = right.requirements.at(ordinal)
      if (leftRequirement === undefined || rightRequirement === undefined) continue
      const divergence = firstRepresentationDivergence(
        leftRequirement.capability,
        rightRequirement.capability,
      )
      if (divergence !== undefined) return divergence
    }
    return undefined
  }
  return isTypeArgument(left) && isTypeArgument(right)
    ? firstRepresentationDivergence(left, right)
    : undefined
}

/** Finds the first source-independent representation mismatch in structural type order. */
export const firstRepresentationDivergence = (
  left: Type,
  right: Type,
): RepresentationDivergence | undefined => {
  if (isRepresented(left) && isRepresented(right))
    return genericArgumentRepresentationDivergence(
      left.representation.argument,
      right.representation.argument,
    )
  if (isNominal(left) && isNominal(right)) {
    if (left.module !== right.module || left.name !== right.name) return undefined
    for (
      let ordinal = 0;
      ordinal < Math.min(left.arguments.length, right.arguments.length);
      ordinal += 1
    ) {
      const leftArgument = left.arguments.at(ordinal)
      const rightArgument = right.arguments.at(ordinal)
      if (leftArgument === undefined || rightArgument === undefined) continue
      const divergence = genericArgumentRepresentationDivergence(leftArgument, rightArgument)
      if (divergence !== undefined) return divergence
    }
    return undefined
  }
  if (isFixedArray(left) && isFixedArray(right))
    return firstRepresentationDivergence(left.element, right.element)
  if (isSlice(left) && isSlice(right))
    return firstRepresentationDivergence(left.element, right.element)
  if (isReference(left) && isReference(right))
    return firstRepresentationDivergence(left.target, right.target)
  if (isCallable(left) && isCallable(right)) {
    for (
      let ordinal = 0;
      ordinal < Math.min(left.parameters.length, right.parameters.length);
      ordinal += 1
    ) {
      const leftParameter = left.parameters.at(ordinal)
      const rightParameter = right.parameters.at(ordinal)
      if (leftParameter === undefined || rightParameter === undefined) continue
      const divergence = firstRepresentationDivergence(leftParameter, rightParameter)
      if (divergence !== undefined) return divergence
    }
    return firstRepresentationDivergence(left.result, right.result)
  }
  if (isEffect(left) && isEffect(right)) {
    const success = firstRepresentationDivergence(left.success, right.success)
    if (success !== undefined) return success
    for (
      let ordinal = 0;
      ordinal < Math.min(left.failures.length, right.failures.length);
      ordinal += 1
    ) {
      const leftFailure = left.failures.at(ordinal)
      const rightFailure = right.failures.at(ordinal)
      if (leftFailure === undefined || rightFailure === undefined) continue
      const divergence = firstRepresentationDivergence(leftFailure, rightFailure)
      if (divergence !== undefined) return divergence
    }
    for (
      let ordinal = 0;
      ordinal < Math.min(left.requirements.length, right.requirements.length);
      ordinal += 1
    ) {
      const leftRequirement = left.requirements.at(ordinal)
      const rightRequirement = right.requirements.at(ordinal)
      if (leftRequirement === undefined || rightRequirement === undefined) continue
      const divergence = firstRepresentationDivergence(
        leftRequirement.capability,
        rightRequirement.capability,
      )
      if (divergence !== undefined) return divergence
    }
    return undefined
  }
  if (isUnion(left) && isUnion(right)) {
    for (
      let ordinal = 0;
      ordinal < Math.min(left.members.length, right.members.length);
      ordinal += 1
    ) {
      const leftMember = left.members.at(ordinal)
      const rightMember = right.members.at(ordinal)
      if (leftMember === undefined || rightMember === undefined) continue
      const divergence = firstRepresentationDivergence(leftMember, rightMember)
      if (divergence !== undefined) return divergence
    }
  }
  return undefined
}

/** Orders semantic types by canonical identity. */
export const compare = (left: Type, right: Type): number => compareText(key(left), key(right))

/** Encodes one type for deterministic compiler facts and diagnostics. */
export const encode = (self: Type): string => {
  if (typeof self === 'string') return self
  if (equals(self, unit)) return '()'
  if (isNominal(self)) {
    const arguments_ =
      self.arguments.length === 0 ? '' : `<${self.arguments.map(encodeGenericArgument).join(', ')}>`
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
  if (isRepresented(self)) return encode(self.contract)
  const someMember = self.members.find(
    (member) =>
      member.module === 'silk/option' && member.name === 'Some' && member.arguments.length === 1,
  )
  const noneMember = self.members.find(
    (member) => member.module === 'silk/option' && member.name === 'None',
  )
  const someArgument = someMember?.arguments.at(0)
  if (
    self.members.length === 2 &&
    someMember !== undefined &&
    noneMember !== undefined &&
    someArgument !== undefined &&
    isTypeArgument(someArgument)
  )
    return `Option<${encode(someArgument)}>`
  return self.members.map(encode).join(' | ')
}

/** Returns every canonical nominal nested in a type, in deterministic preorder. */
/** One declaration named by an exact representation carried inside a type. */
export interface ExactRepresentationDeclaration {
  readonly module: string
  readonly name: string
}

/**
 * Names every declaration whose exact representation one type carries, in encounter order.
 *
 * The walk mirrors `nominals` so the two agree about which positions a type reaches. An exact
 * representation is reported before descending into its structural contract, because the contract
 * alone does not name the construction the representation fixed.
 */
export const exactRepresentationDeclarations = (
  self: Type,
): ReadonlyArray<ExactRepresentationDeclaration> =>
  isRepresented(self)
    ? Object.freeze([
        ...(isExactRepresentationArgument(self.representation.argument) &&
        isCallableIdentityArgument(self.representation.argument.identity) &&
        self.representation.argument.identity.target._tag === 'Declaration'
          ? [
              Object.freeze({
                module: self.representation.argument.identity.target.module,
                name: self.representation.argument.identity.target.name,
              }),
            ]
          : []),
        ...exactRepresentationDeclarations(self.contract),
      ])
    : isNominal(self)
      ? Object.freeze(
          self.arguments.filter(isTypeArgument).flatMap(exactRepresentationDeclarations),
        )
      : isFixedArray(self)
        ? exactRepresentationDeclarations(self.element)
        : isSlice(self)
          ? exactRepresentationDeclarations(self.element)
          : isReference(self)
            ? exactRepresentationDeclarations(self.target)
            : isCallable(self)
              ? Object.freeze([
                  ...self.parameters.flatMap(exactRepresentationDeclarations),
                  ...exactRepresentationDeclarations(self.result),
                ])
              : isEffect(self)
                ? Object.freeze([
                    ...exactRepresentationDeclarations(self.success),
                    ...self.failures.flatMap(exactRepresentationDeclarations),
                    ...self.requirements.flatMap((requirement) =>
                      exactRepresentationDeclarations(requirement.capability),
                    ),
                  ])
                : isUnion(self)
                  ? Object.freeze(self.members.flatMap(exactRepresentationDeclarations))
                  : []

export const nominals = (self: Type): ReadonlyArray<Nominal> =>
  isNominal(self)
    ? Object.freeze([self, ...self.arguments.filter(isTypeArgument).flatMap(nominals)])
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
              : isRepresented(self)
                ? nominals(self.contract)
                : isUnion(self)
                  ? Object.freeze(self.members.flatMap(nominals))
                  : []

/** Returns every declaration-owned parameter nested in a type, without duplicates. */
export const parameters = (self: Type): ReadonlyArray<Parameter> => {
  const found = new Map<string, Parameter>()
  const visitArgument = (argument: GenericArgument): void => {
    if (isTypeArgument(argument)) visit(argument)
    else if (isRepresentationParameterArgument(argument))
      found.set(key(argument.parameter), argument.parameter)
    else if (isExactRepresentationArgument(argument)) {
      visit(argument.contract)
      visitArgument(argument.identity)
    } else if (isCallableIdentityArgument(argument))
      for (const typeArgument of argument.typeArguments) visitArgument(typeArgument)
    else if (isFailureRowArgument(argument)) {
      for (const failure of argument.failures) visit(failure)
      for (const parameter_ of argument.parameters) found.set(key(parameter_), parameter_)
    } else if (isRequirementRowArgument(argument)) {
      for (const requirement of argument.requirements) visit(requirement.capability)
      for (const parameter_ of argument.parameters) found.set(key(parameter_), parameter_)
    }
  }
  const visit = (type: Type): void => {
    if (isParameter(type)) {
      found.set(key(type), type)
    } else if (isFailureProjection(type)) {
      found.set(key(type.parameter), type.parameter)
      return
    }
    if (isNominal(type)) {
      for (const argument of type.arguments) visitArgument(argument)
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
    } else if (isRepresented(type)) {
      visit(type.contract)
      visitArgument(type.representation.argument)
    } else if (isUnion(type)) for (const member of type.members) visit(member)
  }
  visit(self)
  return Object.freeze([...found.values()].sort(compare))
}

/** Tests whether a type contains no open generic parameters. */
export const isConcrete = (self: Type): boolean => parameters(self).length === 0

/** Tests whether an erased generic argument contains no open value or row parameters. */
export const isConcreteGenericArgument = (self: GenericArgument): boolean =>
  isUnavailableGenericArgument(self)
    ? false
    : isRepresentationParameterArgument(self)
      ? false
      : isExactRepresentationArgument(self)
        ? isConcrete(self.contract) && isConcreteGenericArgument(self.identity)
        : isEffectIdentityArgument(self)
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
  if (isNominal(self)) return self.arguments.filter(isTypeArgument).some(containsBorrow)
  if (isFixedArray(self)) return containsBorrow(self.element)
  if (isCallable(self)) return self.parameters.some(containsBorrow) || containsBorrow(self.result)
  if (isEffect(self)) return containsBorrow(self.success) || self.failures.some(containsBorrow)
  if (isRepresented(self)) return containsBorrow(self.contract)
  if (isUnion(self)) return self.members.some(containsBorrow)
  return false
}

/** Tests whether a value may carry a lexical immutable view through data or control flow. */
export const containsViewBorrow = (self: Type): boolean => {
  if (isString(self) || isSlice(self)) return true
  if (isNominal(self)) return self.arguments.filter(isTypeArgument).some(containsViewBorrow)
  if (isFixedArray(self)) return containsViewBorrow(self.element)
  if (isCallable(self))
    return self.parameters.some(containsViewBorrow) || containsViewBorrow(self.result)
  if (isEffect(self))
    return containsViewBorrow(self.success) || self.failures.some(containsViewBorrow)
  if (isRepresented(self)) return containsViewBorrow(self.contract)
  if (isUnion(self)) return self.members.some(containsViewBorrow)
  return false
}

/** Tests for explicit borrow wrappers forbidden inside ordinary type positions. */
export const containsPositionRestrictedBorrow = (self: Type): boolean => {
  if (isString(self)) return false
  if (isSlice(self) || isReference(self) || isSlot(self)) return true
  if (isNominal(self))
    return self.arguments.filter(isTypeArgument).some(containsPositionRestrictedBorrow)
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
  if (isRepresented(self)) return containsPositionRestrictedBorrow(self.contract)
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
      self.arguments.map((argument) => substituteGenericArgument(argument, substitution)),
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
  if (isRepresented(self)) {
    const requiredBound = substitute(self.representation.requiredBound, substitution)
    const contextualContract = substitute(self.contract, substitution)
    if (!isCallable(requiredBound) && !isEffect(requiredBound)) return self
    const open = self.representation.argument
    const replacement =
      open._tag === 'RepresentationParameterArgument'
        ? substitution.get(key(open.parameter))
        : undefined
    const argument =
      replacement !== undefined && isRepresentationArgument(replacement)
        ? replacement
        : substituteGenericArgument(open, substitution)
    if (!isRepresentationArgument(argument)) return self
    const intrinsicContract =
      argument._tag === 'ExactRepresentationArgument'
        ? argument.contract
        : argument.parameter.representationBound
    const contract = intrinsicContract ?? contextualContract
    if (!isCallable(contract) && !isEffect(contract)) return self
    return represented(contract, requiredBound, argument)
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
  isUnavailableGenericArgument(self)
    ? self
    : isRepresentationParameterArgument(self)
      ? (substitution.get(key(self.parameter)) ?? self)
      : isExactRepresentationArgument(self)
        ? (() => {
            const contract = substitute(self.contract, substitution)
            if (!isCallable(contract) && !isEffect(contract)) return self
            const identity = isCallableIdentityArgument(self.identity)
              ? substituteGenericArgument(self.identity, substitution)
              : self.identity
            return isCallableIdentityArgument(identity) || isEffectIdentityArgument(identity)
              ? exactRepresentationArgument(identity, contract)
              : self
          })()
        : isEffectIdentityArgument(self)
          ? self
          : isCallableIdentityArgument(self)
            ? callableIdentityArgument(
                self.identity,
                self.target,
                self.typeArguments.map((argument) =>
                  substituteGenericArgument(argument, substitution),
                ),
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

const inferGenericArgument = (
  pattern: GenericArgument,
  actual: GenericArgument,
  inferred: Map<string, GenericArgument>,
): boolean => {
  if (isRepresentationParameterArgument(pattern))
    return (
      isRepresentationArgument(actual) && bindGenericArgument(pattern.parameter, actual, inferred)
    )
  if (isTypeArgument(pattern) && isTypeArgument(actual)) return infer(pattern, actual, inferred)
  return genericArgumentKey(pattern) === genericArgumentKey(actual)
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
      if (!isTypeArgument(argument) || !isTypeArgument(supplied)) continue
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
      return supplied !== undefined && inferGenericArgument(argument, supplied, inferred)
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
  if (isRepresented(pattern) && isRepresented(actual)) {
    if (!infer(pattern.contract, actual.contract, inferred)) return false
    return inferGenericArgument(
      pattern.representation.argument,
      actual.representation.argument,
      inferred,
    )
  }
  return equals(pattern, actual)
}

/**
 * Builds a substitution from a leading run of parameters, binding only the parameters an argument
 * was supplied for. The parameters past the prefix stay open, so inference can determine them
 * afterwards; the result is undefined when an argument's kind does not match its parameter, or
 * when more arguments were supplied than the declaration has parameters.
 */
export const prefixSubstitution = (
  declared: ReadonlyArray<Parameter>,
  arguments_: ReadonlyArray<GenericArgument>,
): Substitution | undefined => {
  if (arguments_.length > declared.length) return undefined
  const result = new Map<string, GenericArgument>()
  for (const [index, argument] of arguments_.entries()) {
    const parameter_ = declared.at(index)
    if (parameter_ === undefined) return undefined
    const rawRepresentationContract = isRepresentationArgument(argument)
      ? representationArgumentContract(argument)
      : undefined
    const substitutedRepresentationContract =
      rawRepresentationContract === undefined
        ? undefined
        : substitute(rawRepresentationContract, result)
    const representationContract =
      substitutedRepresentationContract !== undefined &&
      (isCallable(substitutedRepresentationContract) || isEffect(substitutedRepresentationContract))
        ? substitutedRepresentationContract
        : undefined
    const substitutedRepresentationBound =
      parameter_.representationBound === undefined
        ? undefined
        : substitute(parameter_.representationBound, result)
    const requiredRepresentationBound =
      substitutedRepresentationBound !== undefined &&
      (isCallable(substitutedRepresentationBound) || isEffect(substitutedRepresentationBound))
        ? substitutedRepresentationBound
        : undefined
    if (
      (parameter_.kind === 'Value' && !isTypeArgument(argument)) ||
      (parameter_.kind === 'FailureRow' && !isFailureRowArgument(argument)) ||
      (parameter_.kind === 'RequirementRow' && !isRequirementRowArgument(argument)) ||
      ((parameter_.kind === 'CallableRepresentation' ||
        parameter_.kind === 'EffectRepresentation') &&
        (!isRepresentationArgument(argument) ||
          representationArgumentKind(argument) !== parameter_.kind ||
          requiredRepresentationBound === undefined ||
          representationContract === undefined ||
          representationAdmissibility(representationContract, requiredRepresentationBound)._tag !==
            'Admitted'))
    )
      return undefined
    result.set(key(parameter_), argument)
  }
  return result
}

/** Builds a substitution from ordered parameters and arguments when their arities match. */
export const substitution = (
  declared: ReadonlyArray<Parameter>,
  arguments_: ReadonlyArray<GenericArgument>,
): Substitution | undefined =>
  declared.length !== arguments_.length ? undefined : prefixSubstitution(declared, arguments_)
