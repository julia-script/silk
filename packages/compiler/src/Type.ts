import type * as CallableContract from './CallableContract.js'
import type * as Constraint from './Constraint.js'
import * as FiniteRow from './FiniteRow.js'
import * as Canonical from './internal/Canonical.js'
import * as RequirementRow from './RequirementRow.js'
import * as RowAlgebra from './RowAlgebra.js'
import * as Scalar from './Scalar.js'
import * as SourceSpan from './SourceSpan.js'

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
  /** Compiler-minted provenance for sealed nominal identities unavailable to source declarations. */
  readonly sealed?: 'Intrinsic.SharedCore'
}

/** One declaration-owned generic type parameter. Names are provenance, not identity. */
export type ParameterKind =
  | 'Value'
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

/** Access carried by a stored environment lane; copy adds no ownership dependency. */
export type CaptureAccess = 'Copy' | CallableMode

/**
 * Compile-time-only obligations retained by a partially applied generic callable.
 *
 * The binders are nested under the callable value: they are not free parameters of the enclosing
 * function or instance. Constraint and evidence keys keep the type layer independent from the
 * solver implementation while the structured values remain available to a later static call.
 * Origins are diagnostic provenance and deliberately do not participate in semantic identity.
 */
export interface CallableSchema {
  readonly contract: CallableContract.CallableContract
  readonly binders: ReadonlyArray<Parameter>
  readonly constraints: ReadonlyArray<Constraint.Constraint>
  readonly evidence: ReadonlyArray<Constraint.ConstraintEvidence>
  readonly substitution: Substitution
  readonly contractKey: string
  readonly constraintKeys: ReadonlyArray<string>
  readonly evidenceKeys: ReadonlyArray<string>
  readonly origins: ReadonlyArray<SourceSpan.SourceSpan>
}

/**
 * Lets the constraint layer specialize schema-owned metadata without introducing a Type ->
 * Constraint runtime cycle. Both callbacks are the same recursive owner-specialization walk used
 * for the callable's ordinary parameter and result types.
 */
export type CallableSchemaOwnerSpecializer = (
  schema: CallableSchema,
  specializeType: (type: Type) => Type,
  specializeArgument: (argument: GenericArgument) => GenericArgument,
) => CallableSchema

/** One canonical structural callable contract independent of its hidden concrete environment. */
export interface Callable {
  readonly _tag: 'CallableType'
  readonly unsafe: boolean
  readonly parameters: ReadonlyArray<Type>
  readonly result: Type
  readonly mode: CallableMode
  readonly schema?: CallableSchema
}

/** One compile-time capability requirement. Roles select slots and have no runtime value. */
export interface Requirement extends RequirementRow.Member<Nominal | Parameter> {}

/** One open nominal member lifted into a failure row. */
export interface FailureMemberShape {
  readonly parameter: Parameter
}

/** One open capability key with a retained access demand lifted into a requirement row. */
export interface RequirementMemberShape {
  readonly capability: Parameter
  readonly access: Requirement['access']
  readonly role: RequirementRow.Role
}

export type FailureRow = RowAlgebra.Row<Type, Parameter, FailureMemberShape>
export type RequirementsRow = RowAlgebra.Row<Requirement, Parameter, RequirementMemberShape>

/** One concrete normalized requirement-row argument supplied to a requirement-row parameter. */
export interface RequirementRowArgument {
  readonly _tag: 'RequirementRowArgument'
  readonly row: RequirementsRow
}

/** The complete enclosing executable specialization retained by a source construction identity. */
export interface ExecutableSpecializationOwner {
  readonly declaration: { readonly module: string; readonly name: string }
  readonly typeArguments: ReadonlyArray<GenericArgument>
}

/** One compiler-only hidden Effect construction identity used for monomorphic specialization. */
export interface EffectIdentityArgument {
  readonly _tag: 'EffectIdentityArgument'
  readonly identity: string
  /** Present for a source site whose runner depends on its enclosing generic specialization. */
  readonly owner?: ExecutableSpecializationOwner
}

/** The path- and span-independent construction site of one callable capture environment. */
export type CallableEnvironmentSite =
  | {
      readonly _tag: 'DeclaredCallableEnvironmentSite'
      readonly declaration: { readonly module: string; readonly name: string }
      readonly ordinal: number
    }
  | {
      readonly _tag: 'RecoveredCallableEnvironmentSite'
      readonly functionOrdinal: number
      readonly ordinal: number
    }

/** The complete specialized identity of one callable capture environment. */
export interface CallableEnvironmentIdentity {
  readonly _tag: 'CallableEnvironmentIdentity'
  readonly site: CallableEnvironmentSite
  readonly owner: ExecutableSpecializationOwner
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
        readonly operation: BuiltinOperation
        readonly intrinsic: { readonly actor: string; readonly name: string }
      }
  readonly typeArguments: ReadonlyArray<GenericArgument>
  readonly environment?: CallableEnvironmentIdentity
}

const nonScalarBuiltinOperations = Object.freeze([
  'LayoutOf',
  'EffectSuspend',
  'StorageAcquire',
  'HostWrite',
  'RawBufferFrom',
  'RawBufferSlot',
  'RawBufferCount',
  'RawBufferRead',
  'RawBufferView',
  'RawBufferViewMut',
  'RawBufferCopy',
  'RawBufferFill',
  'SlotWrite',
  'SlotTake',
  'SlotCopy',
  'SlotDrop',
  'StringFromUtf8Unchecked',
  'StringUtf8Bytes',
  'StringByteLength',
  'StringEqualsExact',
  'OsFileOpen',
  'OsFileRead',
  'OsFileWrite',
  'OsDirectoryOpen',
  'OsDirectoryNext',
  'OsPathInspect',
  'OsDirectoryCreate',
  'OsDirectoryCreateUnique',
  'OsFileRemove',
  'OsDirectoryRemove',
  'OsHandleClose',
  'OsStandardInputRead',
  'OsProcessExecute',
  'OsProcessCapture',
  'OsHostArgumentCount',
  'OsHostArgument',
  'OsHostVariable',
  'OsHostWorkingDirectory',
] as const)

/** The closed operation vocabulary shared by semantic callable identities and HIR targets. */
export type BuiltinOperation = Scalar.OperationCode | (typeof nonScalarBuiltinOperations)[number]

const builtinOperations: ReadonlySet<string> = new Set([
  ...Scalar.all().flatMap((scalar) => scalar.operations.map((operation) => operation.code)),
  ...nonScalarBuiltinOperations,
])

/** Tests whether external text names one operation from the closed builtin vocabulary. */
export const isBuiltinOperation = (value: string): value is BuiltinOperation =>
  builtinOperations.has(value)

/** A structural contract that may bound one statically known executable representation. */
export type RepresentationBound = Callable | Effect

/** An open reference to one declaration-owned representation parameter. */
export interface RepresentationParameterArgument {
  readonly _tag: 'RepresentationParameterArgument'
  readonly parameter: Parameter
}

/** Stable source identity of one declaration-owned opaque representation family. */
export interface OpaqueFamilyKey {
  readonly _tag: 'OpaqueFamilyKey'
  readonly producer: { readonly module: string; readonly name: string }
  readonly binderOrdinal: number
}

/** One opaque family specialized over every enclosing generic argument. */
export interface OpaqueRepresentationArgument {
  readonly _tag: 'OpaqueRepresentationArgument'
  readonly family: OpaqueFamilyKey
  readonly contract: RepresentationBound
  readonly arguments: ReadonlyArray<GenericArgument>
}

/** One exact callable or Effect construction together with its intrinsic contract. */
export interface ExactRepresentationArgument {
  readonly _tag: 'ExactRepresentationArgument'
  readonly identity: EffectIdentityArgument | CallableIdentityArgument
  readonly contract: RepresentationBound
}

/** A closed finite set of exact Effect representations selected by source control flow. */
export interface CompositeEffectRepresentationArgument {
  readonly _tag: 'CompositeEffectRepresentationArgument'
  readonly contract: Effect
  readonly alternatives: ReadonlyArray<ExactRepresentationArgument>
}

/** A statically known representation supplied to a representation parameter. */
export type RepresentationArgument =
  | RepresentationParameterArgument
  | OpaqueRepresentationArgument
  | ExactRepresentationArgument
  | CompositeEffectRepresentationArgument

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

/** One erased generic argument, including the compiler-only requirement-row kind. */
export type GenericArgument =
  | Type
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
  | { readonly _tag: 'NonFiniteRequirementRow' }

/** A compiler-private lazy effect contract. Effect values never cross the executable ABI. */
export interface Effect {
  readonly _tag: 'EffectType'
  readonly success: Type
  readonly failureRow: FailureRow
  readonly requirementRow: RequirementsRow
  readonly access: 'Shared' | 'Exclusive' | 'Take'
}

/** One normalized structural union with at least two canonical ordinary members. */
const structuralUnionBrand: unique symbol = Symbol('StructuralUnion')
export interface StructuralUnion {
  readonly _tag: 'StructuralUnionType'
  readonly members: ReadonlyArray<Type>
  readonly [structuralUnionBrand]: true
}

/** The tag convention used by one concrete runtime carrier representation. */
export type FailureCarrierTagPolicy = 'ZeroBased' | 'OneBased'

/** The closed semantic type vocabulary accepted by declaration analysis. */
export type Type =
  | Builtin
  | String
  | Bottom
  | Nominal
  | Parameter
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

const sealedSharedCore = (arguments_: ReadonlyArray<GenericArgument>): Nominal =>
  Object.freeze({
    _tag: 'NominalType',
    module: 'Intrinsic',
    name: 'SharedCore',
    arguments: Object.freeze(Array.from(arguments_)),
    sealed: 'Intrinsic.SharedCore',
  })

/** Replaces one nominal's arguments while preserving compiler-minted sealed provenance. */
export const specializeNominal = (
  self: Nominal,
  arguments_: ReadonlyArray<GenericArgument>,
): Nominal =>
  self.sealed === 'Intrinsic.SharedCore'
    ? sealedSharedCore(arguments_)
    : nominal(self.module, self.name, arguments_)

/** Canonical allocation-free failure used by every allocator implementation. */
export const outOfMemoryError: Nominal = nominal('silk/core', 'OutOfMemoryError')
export const layout: Nominal = nominal('silk/layout', 'Layout')
export const invalidAlignment: Nominal = nominal('silk/layout', 'InvalidAlignment')
export const layoutOverflow: Nominal = nominal('silk/layout', 'LayoutOverflow')
/** The implementation-erased allocation capability requested by allocation Effects. */
export const allocator: Nominal = nominal('silk/core', 'Allocator')
/** Explicit host capability for complete stdout and stderr byte writes. */
export const standardStreams: Nominal = nominal('silk/core', 'StandardStreams')
/** Allocation-free typed failure returned when a host cannot commit a complete write. */
export const streamWriteFailure: Nominal = nominal('silk/core', 'StreamWriteError')
/** A self-contained affine owner carrying one private active reclaim ticket. */
export const allocation: Nominal = nominal('silk/core', 'Allocation')
/** Opaque affine native file-or-directory handle used only by unsafe OS intrinsics. */
export const osHandle: Nominal = nominal('silk/core', 'OsHandle')
/** Compiler-sealed cleanup capability used only by restricted impl declarations. */
export const dropCapability: Nominal = nominal('silk/core', 'Drop')
/** Compiler-sealed zero-operation property proving that values duplicate without user code. */
export const copyCapability: Nominal = nominal('silk/core', 'Copy')
/** The nominal system-backed implementation of the Allocator capability. */
export const systemAllocator: Nominal = nominal('silk/core', 'SystemAllocator')
/** The canonical empty success value used by effect-free cleanup operations. */
export const unit: Nominal = nominal('silk/core', 'Unit')
/** Compiler-checked typed raw storage owned independently from its allocator provider. */
export const rawBuffer = (element: Type): Nominal => nominal('silk/core', 'RawBuffer', [element])
/** A lexical exclusive projection into one RawBuffer element. */
export const slot = (element: Type): Nominal => nominal('silk/core', 'Slot', [element])
/** The compiler-sealed local strong handle identity. Its representation is intentionally opaque. */
export const sharedCore = (element: Type): Nominal => sealedSharedCore([element])
/** Canonical recoverable success and failure members shipped by silk/option. */
export const some = (element: Type): Nominal => nominal('silk/option', 'Some', [element])
export const none: Nominal = nominal('silk/option', 'None')

/** Canonical completed Effect outcome data shipped by silk/result. */
export const resultSuccess = (value: Type): Nominal => nominal('silk/result', 'Success', [value])
export const resultFailure = (error: Type): Nominal => nominal('silk/result', 'Failure', [error])
export const result = (value: Type, error: Type): Nominal =>
  nominal('silk/result', 'Result', [value, error])

/** Normalizes one or more ordinary failure types to their runtime value union. */
export const failureValue = (failures: ReadonlyArray<Type>): Type => {
  const only = failures.at(0)
  if (failures.length === 1 && only !== undefined) return only
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

/** Tests the canonical sealed local-shared core identity without consulting source spelling. */
export const isSharedCore = (
  self: Type,
): self is Nominal & {
  readonly module: 'Intrinsic'
  readonly name: 'SharedCore'
  readonly arguments: readonly [Type]
} => {
  if (
    !isNominal(self) ||
    self.module !== 'Intrinsic' ||
    self.name !== 'SharedCore' ||
    self.sealed !== 'Intrinsic.SharedCore'
  )
    return false
  const argument = self.arguments.at(0)
  return self.arguments.length === 1 && argument !== undefined && isTypeArgument(argument)
}

export const intrinsicNominals: ReadonlyMap<string, Nominal> = new Map([
  [allocation.name, allocation],
  [osHandle.name, osHandle],
  [copyCapability.name, copyCapability],
  [dropCapability.name, dropCapability],
  ['RawBuffer', nominal('silk/core', 'RawBuffer')],
  ['Slot', nominal('silk/core', 'Slot')],
  ['Intrinsic.SharedCore', sealedSharedCore([])],
])

/** Returns the compiler-known generic arity of an intrinsic nominal actor. */
export const intrinsicNominalArity = (self: Nominal): number =>
  (self.module === 'silk/core' && (self.name === 'RawBuffer' || self.name === 'Slot')) ||
  self.sealed === 'Intrinsic.SharedCore'
    ? 1
    : 0
export const intrinsicNominalOrdinal = (self: Nominal): number =>
  [...intrinsicNominals.values()].findIndex(
    (candidate) =>
      candidate.module === self.module &&
      candidate.name === self.name &&
      candidate.sealed === self.sealed,
  )

export const isIntrinsicNominal = (self: Type): boolean =>
  isNominal(self) &&
  [...intrinsicNominals.values()].some(
    (candidate) =>
      candidate.module === self.module &&
      candidate.name === self.name &&
      candidate.sealed === self.sealed,
  )

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
  schema?: CallableSchema,
  unsafe = false,
): Callable =>
  Object.freeze({
    _tag: 'CallableType',
    unsafe,
    parameters: Object.freeze(Array.from(parameters_)),
    result,
    mode,
    ...(schema === undefined
      ? {}
      : {
          schema: Object.freeze({
            ...schema,
            binders: Object.freeze(Array.from(schema.binders)),
            constraints: Object.freeze(Array.from(schema.constraints)),
            evidence: Object.freeze(Array.from(schema.evidence)),
            substitution: new Map(schema.substitution),
            constraintKeys: Object.freeze(Array.from(schema.constraintKeys)),
            evidenceKeys: Object.freeze(Array.from(schema.evidenceKeys)),
            origins: Object.freeze(Array.from(schema.origins)),
          }),
        }),
  })

const implicitRowOrigin: SourceSpan.SourceSpan = (() => {
  const span = SourceSpan.fromOffsets('$implicit-row', 0, 0)
  if (span === undefined) throw new RangeError('implicit row span is invalid')
  return span
})()

/** Constructs one normalized compiler-private lazy effect contract. */
export const effect = (
  success: Type,
  failures: ReadonlyArray<Type>,
  access: Effect['access'] = 'Shared',
  requirements: ReadonlyArray<Requirement> = [],
  requirementParameters: ReadonlyArray<Parameter> = [],
): Effect => {
  const failureLeaves = failures.flatMap(
    (failure): ReadonlyArray<Type> =>
      isNever(failure) ? [] : isUnion(failure) ? failure.members : [failure],
  )
  const concreteFailures = failureLeaves.filter(
    (failure) => !(isParameter(failure) && failure.kind === 'Value'),
  )
  const symbolicFailures = failureLeaves.filter(
    (failure): failure is Parameter => isParameter(failure) && failure.kind === 'Value',
  )
  const normalized = FiniteRow.make<Type>(
    {
      collisionKey: key,
      memberKey: key,
      merge: (left) => left,
    },
    concreteFailures,
  )
  const concreteRequirements = requirements.filter((requirement) =>
    isNominal(requirement.capability),
  )
  const symbolicRequirements = requirements.filter((requirement) =>
    isParameter(requirement.capability),
  )
  const normalizedRequirements = FiniteRow.make<Requirement>(
    RequirementRow.policy<Nominal | Parameter>(key),
    concreteRequirements,
  )
  const normalizedRequirementParameters = Object.freeze(
    [
      ...new Map(requirementParameters.map((parameter_) => [key(parameter_), parameter_])).values(),
    ].sort(compare),
  )
  const failureRow = symbolicFailures.reduce<FailureRow>(
    (row, failure) =>
      RowAlgebra.union(
        failureRowPolicy(),
        row,
        RowAlgebra.singleton(failureRowPolicy(), failureMemberShape(failure), implicitRowOrigin),
      ),
    RowAlgebra.concrete(failureRowPolicy(), normalized.members),
  )
  const parameterizedRequirementRow = symbolicRequirements.reduce<RequirementsRow>(
    (row, requirement) =>
      isParameter(requirement.capability)
        ? RowAlgebra.union(
            requirementRowPolicy(),
            row,
            RowAlgebra.singleton(
              requirementRowPolicy(),
              requirementMemberShape(requirement.capability, requirement.access, requirement.role),
              implicitRowOrigin,
            ),
          )
        : row,
    RowAlgebra.concrete(requirementRowPolicy(), normalizedRequirements.members),
  )
  const requirementRow = normalizedRequirementParameters.reduce<RequirementsRow>(
    (row, parameter_) =>
      RowAlgebra.union(
        requirementRowPolicy(),
        row,
        RowAlgebra.parameter<Requirement, Parameter, RequirementMemberShape>(parameter_),
      ),
    parameterizedRequirementRow,
  )
  return Object.freeze({
    _tag: 'EffectType',
    success,
    failureRow,
    requirementRow,
    access,
  })
}

/** Symbolic failure-row domain policy. */
export function failureRowPolicy(): RowAlgebra.Policy<
  Type,
  Parameter,
  FailureMemberShape,
  Parameter
> {
  return Object.freeze({
    finite: Object.freeze({
      collisionKey: key,
      memberKey: key,
      merge: (left: Type) => left,
    }),
    concreteMemberMaySpecialize: typeMaySpecialize,
    rowParameterKey: key,
    symbolicMemberKey: (member: FailureMemberShape) => key(member.parameter),
    symbolicMemberParameters: (member: FailureMemberShape) => Object.freeze([member.parameter]),
    memberParameterKey: key,
    memberWellFormedKey: (member: FailureMemberShape) =>
      Canonical.record('FailureMemberWellFormed', [key(member.parameter)]),
    allowsSetCancellation: true,
  })
}

/** Symbolic requirement-row domain policy with fixed access and role. */
export function requirementRowPolicy(): RowAlgebra.Policy<
  Requirement,
  Parameter,
  RequirementMemberShape,
  Parameter
> {
  return Object.freeze({
    finite: RequirementRow.policy<Nominal | Parameter>(key),
    concreteMemberMaySpecialize: (member: Requirement) => typeMaySpecialize(member.capability),
    rowParameterKey: key,
    symbolicMemberKey: (member: RequirementMemberShape) =>
      Canonical.record('RequirementMemberShape', [
        member.access,
        RequirementRow.roleKey(member.role),
        key(member.capability),
      ]),
    symbolicMemberParameters: (member: RequirementMemberShape) =>
      Object.freeze([member.capability]),
    memberParameterKey: key,
    memberWellFormedKey: (member: RequirementMemberShape) =>
      Canonical.record('RequirementMemberWellFormed', [
        member.access,
        RequirementRow.roleKey(member.role),
        key(member.capability),
      ]),
    allowsSetCancellation: false,
  })
}

export const failureMemberShape = (parameter_: Parameter): FailureMemberShape =>
  Object.freeze({ parameter: parameter_ })

export const requirementMemberShape = (
  capability: Parameter,
  access: Requirement['access'],
  role: RequirementRow.Role,
): RequirementMemberShape => Object.freeze({ capability, access, role })

/** Constructs an Effect directly from symbolic channel rows. */
export const effectWithRows = (
  success: Type,
  failureRow: FailureRow,
  access: Effect['access'] = 'Shared',
  requirementRow: RequirementsRow = RowAlgebra.concrete(requirementRowPolicy(), []),
): Effect => {
  const concreteFailures = RowAlgebra.concretize(failureRowPolicy(), failureRow)
  const concreteRequirements = RowAlgebra.concretize(requirementRowPolicy(), requirementRow)
  const requirementParameters = RowAlgebra.parameters(requirementRowPolicy(), requirementRow).rows
  const base = effect(
    success,
    concreteFailures._tag === 'Concrete' ? concreteFailures.row.members : [],
    access,
    concreteRequirements._tag === 'Concrete' ? concreteRequirements.row.members : [],
    requirementParameters,
  )
  return Object.freeze({ ...base, failureRow, requirementRow })
}

/** Constructs one normalized concrete requirement-row generic argument. */
export const requirementRowArgument = (
  requirements: ReadonlyArray<Requirement>,
  parameters: ReadonlyArray<Parameter> = [],
): RequirementRowArgument => {
  const row = parameters.reduce<RequirementsRow>(
    (current, parameter_) =>
      RowAlgebra.union(
        requirementRowPolicy(),
        current,
        RowAlgebra.parameter<Requirement, Parameter, RequirementMemberShape>(parameter_),
      ),
    RowAlgebra.concrete(requirementRowPolicy(), requirements),
  )
  return requirementRowArgumentFromRow(row)
}

/** Constructs one requirement-row argument without flattening computed row expressions. */
export const requirementRowArgumentFromRow = (row: RequirementsRow): RequirementRowArgument => {
  return Object.freeze({
    _tag: 'RequirementRowArgument',
    row,
  })
}

/** Concrete members projected from one symbolic failure row. */
const failureRowOf = (self: Effect | FailureRow): FailureRow =>
  'failureRow' in self ? self.failureRow : self

export const failureMembers = (self: Effect | FailureRow): ReadonlyArray<Type> =>
  RowAlgebra.concreteMembers(failureRowPolicy(), failureRowOf(self))

const isConcreteFailureCarrierMember = (self: Type): boolean => isRuntimeConcrete(self)

/** Selects one ordinary failure member under the carrier's explicit runtime tag convention. */
export const failureCarrierMember = (
  self: Type,
  tag: number,
  policy: FailureCarrierTagPolicy,
): Type | undefined => {
  if (!Number.isSafeInteger(tag)) return undefined
  const ordinal = policy === 'ZeroBased' ? tag : tag - 1
  if (ordinal < 0) return undefined
  if (isUnion(self))
    return policy === 'ZeroBased' && self.members.every(isConcreteFailureCarrierMember)
      ? self.members.at(ordinal)
      : undefined
  if (isEffect(self)) {
    if (policy !== 'OneBased' || !isRuntimeConcrete(self)) return undefined
    const failures = RowAlgebra.concretize(failureRowPolicy(), self.failureRow)
    return failures._tag === 'Concrete' &&
      failures.row.members.every(isConcreteFailureCarrierMember)
      ? failures.row.members.at(ordinal)
      : undefined
  }
  return policy === 'ZeroBased' && ordinal === 0 && isConcreteFailureCarrierMember(self)
    ? self
    : undefined
}

/** Ordinary type parameters used as symbolic members of one failure union. */
export const failureMemberParameters = (self: Effect | FailureRow): ReadonlyArray<Parameter> =>
  RowAlgebra.parameters(failureRowPolicy(), failureRowOf(self)).members

/** Presents one failure union as the ordinary value type carried by its outcome channel. */
export const failureType = (self: Effect | FailureRow): Type => {
  const concrete = failureMembers(self)
  const symbolic = failureMemberParameters(self)
  if (concrete.length === 0 && symbolic.length === 1) return symbolic[0] ?? 'never'
  const normalized = union([...concrete, ...symbolic])
  return normalized._tag === 'Normalized' ? normalized.type : 'never'
}

/** Concrete members projected from one symbolic requirement row. */
export const requirementMembers = (
  self: Effect | RequirementRowArgument,
): ReadonlyArray<Requirement> =>
  RowAlgebra.concreteMembers(
    requirementRowPolicy(),
    self._tag === 'EffectType' ? self.requirementRow : self.row,
  )

/** Whole-row parameters projected from one symbolic requirement row. */
export const requirementRowParameters = (
  self: Effect | RequirementRowArgument,
): ReadonlyArray<Parameter> =>
  RowAlgebra.parameters(
    requirementRowPolicy(),
    self._tag === 'EffectType' ? self.requirementRow : self.row,
  ).rows

export const effectIdentityArgument = (
  identity: string,
  owner?: ExecutableSpecializationOwner,
): EffectIdentityArgument =>
  Object.freeze({
    _tag: 'EffectIdentityArgument',
    identity,
    ...(owner === undefined
      ? {}
      : {
          owner: Object.freeze({
            declaration: Object.freeze({ ...owner.declaration }),
            typeArguments: Object.freeze(Array.from(owner.typeArguments)),
          }),
        }),
  })

/** Constructs the stable structural site of one callable capture environment. */
export const callableEnvironmentSite = (
  declaration: { readonly module: string; readonly name: string } | undefined,
  functionOrdinal: number,
  ordinal: number,
): CallableEnvironmentSite =>
  declaration === undefined
    ? Object.freeze({
        _tag: 'RecoveredCallableEnvironmentSite',
        functionOrdinal,
        ordinal,
      })
    : Object.freeze({
        _tag: 'DeclaredCallableEnvironmentSite',
        declaration: Object.freeze({ ...declaration }),
        ordinal,
      })

/** Constructs the complete specialization identity of one callable capture environment. */
export const callableEnvironmentIdentity = (
  site: CallableEnvironmentSite,
  owner: CallableEnvironmentIdentity['owner'],
): CallableEnvironmentIdentity =>
  Object.freeze({
    _tag: 'CallableEnvironmentIdentity',
    site,
    owner: Object.freeze({
      declaration: Object.freeze({ ...owner.declaration }),
      typeArguments: Object.freeze(Array.from(owner.typeArguments)),
    }),
  })

export const callableIdentityArgument = (
  identity: string,
  target: CallableIdentityArgument['target'],
  typeArguments: ReadonlyArray<GenericArgument> = [],
  environment?: CallableEnvironmentIdentity,
): CallableIdentityArgument =>
  Object.freeze({
    _tag: 'CallableIdentityArgument',
    identity: environment === undefined ? identity : callableEnvironmentKey(environment),
    target: Object.freeze(target),
    typeArguments: Object.freeze(Array.from(typeArguments)),
    ...(environment === undefined ? {} : { environment }),
  })

/** Constructs an open representation argument owned by one representation parameter. */
export const representationParameterArgument = (
  parameter_: Parameter,
): RepresentationParameterArgument =>
  Object.freeze({ _tag: 'RepresentationParameterArgument', parameter: parameter_ })

/** Constructs one opaque family instance from canonical producer and enclosing arguments. */
export const opaqueRepresentationArgument = (
  family: OpaqueFamilyKey,
  contract: RepresentationBound,
  arguments_: ReadonlyArray<GenericArgument>,
): OpaqueRepresentationArgument =>
  Object.freeze({
    _tag: 'OpaqueRepresentationArgument',
    family: Object.freeze({
      _tag: 'OpaqueFamilyKey',
      producer: Object.freeze({ ...family.producer }),
      binderOrdinal: family.binderOrdinal,
    }),
    contract,
    arguments: Object.freeze(Array.from(arguments_)),
  })

/** Constructs one exact representation argument without mixing its identity with a use bound. */
export const exactRepresentationArgument = (
  identity: EffectIdentityArgument | CallableIdentityArgument,
  contract: RepresentationBound,
): ExactRepresentationArgument =>
  Object.freeze({ _tag: 'ExactRepresentationArgument', identity, contract })

/** Constructs one canonical finite Effect representation from exact alternatives. */
export const compositeEffectRepresentationArgument = (
  contract: Effect,
  alternatives: ReadonlyArray<ExactRepresentationArgument>,
): CompositeEffectRepresentationArgument =>
  Object.freeze({
    _tag: 'CompositeEffectRepresentationArgument',
    contract,
    alternatives: Object.freeze(
      [
        ...new Map(
          alternatives.map((alternative) => [genericArgumentKey(alternative), alternative]),
        ).values(),
      ].sort((left, right) => genericArgumentKey(left).localeCompare(genericArgumentKey(right))),
    ),
  })

/** Reifies one declaration parameter as an open generic argument of the same kind. */
export const parameterArgument = (self: Parameter): GenericArgument => {
  switch (self.kind) {
    case 'Value':
      return self
    case 'RequirementRow':
      return requirementRowArgument([], [self])
    case 'CallableRepresentation':
    case 'EffectRepresentation':
      return representationParameterArgument(self)
  }
}

/** Ranks access modes: Shared(0) < Exclusive(1) < Take(2). */
export const accessRank = (access: CallableMode | Effect['access']): number =>
  access === 'Shared' ? 0 : access === 'Exclusive' ? 1 : 2

/** True when the supplied access is at least as strong as the required one. */
export const compareAccess = (
  supplied: CallableMode | Effect['access'],
  required: CallableMode | Effect['access'],
): boolean => accessRank(supplied) >= accessRank(required)

/** True when one requirement is satisfied by a supplied requirement with compatible access. */
export const requirementSatisfies = (
  supplied: { readonly access: 'Shared' | 'Exclusive' | 'Take' },
  required: { readonly access: 'Shared' | 'Exclusive' },
): boolean => compareAccess(supplied.access, required.access)

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
    const leftShape = callable(left.parameters, left.result, 'Shared', left.schema, left.unsafe)
    const rightShape = callable(
      right.parameters,
      right.result,
      'Shared',
      right.schema,
      right.unsafe,
    )
    return equals(leftShape, rightShape)
      ? callable(left.parameters, left.result, access, left.schema, left.unsafe)
      : undefined
  }
  if (left._tag === 'EffectType' && right._tag === 'EffectType') {
    const leftShape = effectWithRows(left.success, left.failureRow, 'Shared', left.requirementRow)
    const rightShape = effectWithRows(
      right.success,
      right.failureRow,
      'Shared',
      right.requirementRow,
    )
    return equals(leftShape, rightShape)
      ? effectWithRows(left.success, left.failureRow, access, left.requirementRow)
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
      ? callable(
          contract.parameters,
          contract.result,
          requiredBound.mode,
          contract.schema,
          contract.unsafe,
        )
      : contract._tag === 'EffectType' && requiredBound._tag === 'EffectType'
        ? effectWithRows(
            contract.success,
            contract.failureRow,
            requiredBound.access,
            contract.requirementRow,
          )
        : undefined
  const requiredAccess =
    requiredBound._tag === 'CallableType' ? requiredBound.mode : requiredBound.access
  const actualAccess = contract._tag === 'CallableType' ? contract.mode : contract.access
  return structuralContract !== undefined &&
    equals(structuralContract, requiredBound) &&
    compareAccess(requiredAccess, actualAccess)
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

export const isOpaqueRepresentationArgument = (
  self: GenericArgument,
): self is OpaqueRepresentationArgument =>
  typeof self !== 'string' && self._tag === 'OpaqueRepresentationArgument'

/** Returns the canonical source identity shared by every specialization of one opaque family. */
export const opaqueFamilyKey = (self: OpaqueFamilyKey): string =>
  Canonical.record('OpaqueFamily', [
    Canonical.record('Producer', [self.producer.module, self.producer.name]),
    String(self.binderOrdinal),
  ])

/** Tests family identity without consulting a realization or any source location. */
export const equalsOpaqueFamily = (left: OpaqueFamilyKey, right: OpaqueFamilyKey): boolean =>
  opaqueFamilyKey(left) === opaqueFamilyKey(right)

export const isExactRepresentationArgument = (
  self: GenericArgument,
): self is ExactRepresentationArgument =>
  typeof self !== 'string' && self._tag === 'ExactRepresentationArgument'

export const isCompositeEffectRepresentationArgument = (
  self: GenericArgument,
): self is CompositeEffectRepresentationArgument =>
  typeof self !== 'string' && self._tag === 'CompositeEffectRepresentationArgument'

export const isRepresentationArgument = (self: GenericArgument): self is RepresentationArgument =>
  isRepresentationParameterArgument(self) ||
  isOpaqueRepresentationArgument(self) ||
  isExactRepresentationArgument(self) ||
  isCompositeEffectRepresentationArgument(self)

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
  self._tag === 'RepresentationParameterArgument'
    ? self.parameter.representationBound
    : self.contract

export const isHiddenIdentityArgument = (
  self: GenericArgument,
): self is EffectIdentityArgument | CallableIdentityArgument =>
  isEffectIdentityArgument(self) || isCallableIdentityArgument(self)

export const isTypeArgument = (self: GenericArgument): self is OrdinaryType =>
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
const callableEnvironmentSiteKey = (self: CallableEnvironmentSite): string =>
  self._tag === 'DeclaredCallableEnvironmentSite'
    ? `declaration:${self.declaration.module}.${self.declaration.name}:site:${self.ordinal}`
    : `recovered:${self.functionOrdinal}:site:${self.ordinal}`

/** Returns the deterministic identity of one specialized callable capture environment. */
export const callableEnvironmentKey = (self: CallableEnvironmentIdentity): string =>
  `${callableEnvironmentSiteKey(self.site)}:owner=${self.owner.declaration.module}.${self.owner.declaration.name}<${self.owner.typeArguments.map(genericArgumentKey).join(',')}>`

/** Tests complete callable-environment specialization identity. */
export const equalsCallableEnvironmentIdentity = (
  left: CallableEnvironmentIdentity,
  right: CallableEnvironmentIdentity,
): boolean => callableEnvironmentKey(left) === callableEnvironmentKey(right)

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
    self.environment === undefined ? '' : callableEnvironmentKey(self.environment),
  ].join('')

export const genericArgumentKey = (self: GenericArgument): string =>
  isUnavailableGenericArgument(self)
    ? `unavailable:${self.expectedKind}:${self.reason}`
    : isRepresentationParameterArgument(self)
      ? `representation-parameter:${key(self.parameter)}`
      : isOpaqueRepresentationArgument(self)
        ? Canonical.record('OpaqueRepresentation', [
            opaqueFamilyKey(self.family),
            Canonical.array(self.arguments.map(genericArgumentKey)),
            key(self.contract),
          ])
        : isExactRepresentationArgument(self)
          ? `exact-representation:${genericArgumentKey(self.identity)}:${key(self.contract)}`
          : isCompositeEffectRepresentationArgument(self)
            ? Canonical.record('CompositeEffectRepresentation', [
                key(self.contract),
                Canonical.array(self.alternatives.map(genericArgumentKey)),
              ])
            : isEffectIdentityArgument(self)
              ? self.owner === undefined
                ? `effect-identity:${self.identity}`
                : `effect-identity:${self.identity}:owner=${self.owner.declaration.module}.${self.owner.declaration.name}<${self.owner.typeArguments.map(genericArgumentKey).join(',')}>`
              : isCallableIdentityArgument(self)
                ? callableIdentityKey(self)
                : isRequirementRowArgument(self)
                  ? `requirement-row:${RowAlgebra.key(requirementRowPolicy(), self.row)}`
                  : key(self)

/** Encodes any erased generic argument for semantic presentation and artifact inspection. */
export const encodeGenericArgument = (self: GenericArgument): string =>
  isUnavailableGenericArgument(self)
    ? `<unavailable ${self.expectedKind}: ${self.reason}>`
    : isRepresentationParameterArgument(self)
      ? self.parameter.name
      : isOpaqueRepresentationArgument(self)
        ? `some(${self.family.producer.module}.${self.family.producer.name}#${self.family.binderOrdinal})`
        : isExactRepresentationArgument(self)
          ? `typeof(${encodeRepresentationOrigin(self.identity)})`
          : isCompositeEffectRepresentationArgument(self)
            ? `oneof(${self.alternatives.map(encodeGenericArgument).join(', ')})`
            : isEffectIdentityArgument(self)
              ? `effect@${self.identity}`
              : isCallableIdentityArgument(self)
                ? `callable@${self.identity}`
                : isRequirementRowArgument(self)
                  ? `? ${RowAlgebra.encode(
                      requirementRowPolicy(),
                      self.row,
                      (requirement) =>
                        `${requirement.access === 'Exclusive' ? '&mut ' : '&'}${encode(requirement.capability)}${requirement.role === RequirementRow.defaultRole ? '' : ` at ${RequirementRow.roleName(requirement.role)}`}`,
                      (parameter_) => parameter_.name,
                      (member) =>
                        `${member.access === 'Exclusive' ? '&mut ' : '&'}${member.capability.name}${member.role === RequirementRow.defaultRole ? '' : ` at ${RequirementRow.roleName(member.role)}`}`,
                    )}`
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
      }${self.environment === undefined ? '' : `@${callableEnvironmentKey(self.environment)}`}`

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

/** Tests whether one ordinary value type can be stored without a lexical borrow wrapper. */
export const isDetachedUnionMember = (self: Type): boolean => {
  if (isSlice(self) || isReference(self) || isSlot(self)) return false
  if (isCallable(self) || isEffect(self)) return false
  if (isRepresented(self)) return self.representation.admissibility._tag !== 'Unavailable'
  if (isFixedArray(self)) return isDetachedUnionMember(self.element)
  // A nominal member has its own finite identity. Whether one concrete application can be stored
  // depends on its declared fields and representation evidence, so layout/ownership must retain
  // that later diagnostic boundary instead of rejecting the type argument here.
  if (isNominal(self)) return true
  if (isUnion(self)) return self.members.every(isDetachedUnionMember)
  return true
}

/** Normalizes a finite union of detached ordinary type leaves. */
export const union = (inputs: ReadonlyArray<Type>): UnionNormalization => {
  const members: Array<Type> = []
  const invalid: Array<Type> = []
  const visit = (input: Type): void => {
    if (input === 'never') return
    if (isUnion(input)) {
      for (const member of input.members) visit(member)
      return
    }
    if (!isDetachedUnionMember(input)) {
      invalid.push(input)
      return
    }
    members.push(input)
  }
  for (const input of inputs) visit(input)
  if (invalid.length > 0)
    return Object.freeze({ _tag: 'InvalidMembers', members: Object.freeze(invalid) })
  const normalized = FiniteRow.make<Type>(
    { collisionKey: key, memberKey: key, merge: (left) => left },
    members,
  ).members
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

/** The canonical identity of one provider's application of an interface. */
export const conformanceKey = (capability: Nominal, provider: Type): string =>
  `${key(capability)}\u0000${key(provider)}`

const computeKey = (self: Type): string => {
  if (isString(self)) return 'string'
  if (isBuiltin(self)) return `builtin:${self}`
  if (isNever(self)) return 'union:'
  if (isNominal(self))
    return `${self.sealed === undefined ? 'nominal' : `sealed:${self.sealed}`}:${self.module}.${self.name}<${self.arguments.map(genericArgumentKey).join(',')}>`
  if (isParameter(self))
    return `parameter:${self.kind}:${self.owner.module}.${self.owner.name}:${self.ordinal}`
  if (isFixedArray(self)) return `array:${self.length}<${key(self.element)}>`
  if (isSlice(self)) return `slice:${self.access}<${key(self.element)}>`
  if (isReference(self)) return `reference:${self.access}<${key(self.target)}>`
  if (isCallable(self)) {
    const schema = self.schema
    const schemaKey =
      schema === undefined
        ? ''
        : Canonical.record('QuantifiedCallableSchema', [
            schema.contractKey,
            Canonical.array(schema.constraintKeys),
            Canonical.array(schema.evidenceKeys),
            Canonical.array(
              [...schema.substitution.entries()]
                .sort(([left], [right]) => compareText(left, right))
                .map(([parameter_, argument]) =>
                  Canonical.record('SubstitutionEntry', [parameter_, genericArgumentKey(argument)]),
                ),
            ),
          ])
    return `callable:${self.unsafe ? 'unsafe:' : 'safe:'}${self.mode}<(${self.parameters.map(key).join(',')})->${key(self.result)}>${schemaKey}`
  }
  if (isEffect(self))
    return `effect:${self.access}<${key(self.success)}!${RowAlgebra.key(
      failureRowPolicy(),
      self.failureRow,
    )}?${RowAlgebra.key(requirementRowPolicy(), self.requirementRow)}>`
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
  if (isRequirementRowArgument(left) && isRequirementRowArgument(right)) {
    for (
      let ordinal = 0;
      ordinal < Math.min(requirementMembers(left).length, requirementMembers(right).length);
      ordinal += 1
    ) {
      const leftRequirement = requirementMembers(left).at(ordinal)
      const rightRequirement = requirementMembers(right).at(ordinal)
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
      ordinal < Math.min(failureMembers(left).length, failureMembers(right).length);
      ordinal += 1
    ) {
      const leftFailure = failureMembers(left).at(ordinal)
      const rightFailure = failureMembers(right).at(ordinal)
      if (leftFailure === undefined || rightFailure === undefined) continue
      const divergence = firstRepresentationDivergence(leftFailure, rightFailure)
      if (divergence !== undefined) return divergence
    }
    for (
      let ordinal = 0;
      ordinal < Math.min(requirementMembers(left).length, requirementMembers(right).length);
      ordinal += 1
    ) {
      const leftRequirement = requirementMembers(left).at(ordinal)
      const rightRequirement = requirementMembers(right).at(ordinal)
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

const genericArgumentsHaveSameRepresentationShape = (
  left: GenericArgument,
  right: GenericArgument,
): boolean => {
  if (isRepresentationArgument(left) || isRepresentationArgument(right)) {
    if (!isRepresentationArgument(left) || !isRepresentationArgument(right)) return false
    const leftContract = representationArgumentContract(left)
    const rightContract = representationArgumentContract(right)
    return (
      leftContract !== undefined &&
      rightContract !== undefined &&
      haveSameRepresentationShape(leftContract, rightContract)
    )
  }
  if (isRequirementRowArgument(left) || isRequirementRowArgument(right)) {
    return (
      isRequirementRowArgument(left) &&
      isRequirementRowArgument(right) &&
      requirementMembers(left).length === requirementMembers(right).length &&
      requirementRowParameters(left).length === requirementRowParameters(right).length &&
      requirementMembers(left).every((requirement, ordinal) => {
        const compared = requirementMembers(right).at(ordinal)
        return (
          compared !== undefined &&
          requirement.role === compared.role &&
          requirementSatisfies(compared, requirement) &&
          haveSameRepresentationShape(requirement.capability, compared.capability)
        )
      }) &&
      requirementRowParameters(left).every((parameter_, ordinal) => {
        const compared = requirementRowParameters(right).at(ordinal)
        return compared !== undefined && equals(parameter_, compared)
      })
    )
  }
  return isTypeArgument(left) && isTypeArgument(right)
    ? haveSameRepresentationShape(left, right)
    : equalsGenericArgument(left, right)
}

/**
 * Compares the complete value shape of two types while deliberately ignoring concrete executable
 * identities. Producer return checking uses this relation before its opaque-realization pass
 * unifies those identities; ordinary type equality remains identity-sensitive.
 */
export const haveSameRepresentationShape = (left: Type, right: Type): boolean => {
  if (isRepresented(left)) return haveSameRepresentationShape(left.contract, right)
  if (isRepresented(right)) return haveSameRepresentationShape(left, right.contract)
  if (typeof left === 'string' || typeof right === 'string') return left === right
  if (isParameter(left) || isParameter(right))
    return isParameter(left) && isParameter(right) && equals(left, right)
  if (isNominal(left) || isNominal(right))
    return (
      isNominal(left) &&
      isNominal(right) &&
      left.module === right.module &&
      left.name === right.name &&
      left.arguments.length === right.arguments.length &&
      left.arguments.every((argument, ordinal) => {
        const compared = right.arguments.at(ordinal)
        return (
          compared !== undefined && genericArgumentsHaveSameRepresentationShape(argument, compared)
        )
      })
    )
  if (isFixedArray(left) || isFixedArray(right))
    return (
      isFixedArray(left) &&
      isFixedArray(right) &&
      left.length === right.length &&
      haveSameRepresentationShape(left.element, right.element)
    )
  if (isSlice(left) || isSlice(right))
    return (
      isSlice(left) &&
      isSlice(right) &&
      left.access === right.access &&
      haveSameRepresentationShape(left.element, right.element)
    )
  if (isReference(left) || isReference(right))
    return (
      isReference(left) &&
      isReference(right) &&
      left.access === right.access &&
      haveSameRepresentationShape(left.target, right.target)
    )
  if (isCallable(left) || isCallable(right)) {
    if (!isCallable(left) || !isCallable(right)) return false
    return (
      (!left.unsafe || right.unsafe) &&
      compareAccess(right.mode, left.mode) &&
      left.parameters.length === right.parameters.length &&
      left.parameters.every((parameter_, ordinal) => {
        const compared = right.parameters.at(ordinal)
        return compared !== undefined && haveSameRepresentationShape(parameter_, compared)
      }) &&
      haveSameRepresentationShape(left.result, right.result)
    )
  }
  if (isEffect(left) || isEffect(right)) {
    if (!isEffect(left) || !isEffect(right)) return false
    return (
      compareAccess(right.access, left.access) &&
      haveSameRepresentationShape(left.success, right.success) &&
      haveSameRepresentationShape(failureType(left), failureType(right)) &&
      requirementMembers(left).length === requirementMembers(right).length &&
      requirementMembers(left).every((requirement, ordinal) => {
        const compared = requirementMembers(right).at(ordinal)
        return (
          compared !== undefined &&
          requirement.role === compared.role &&
          requirementSatisfies(compared, requirement) &&
          haveSameRepresentationShape(requirement.capability, compared.capability)
        )
      }) &&
      requirementRowParameters(left).length === requirementRowParameters(right).length &&
      requirementRowParameters(left).every((parameter_, ordinal) => {
        const compared = requirementRowParameters(right).at(ordinal)
        return compared !== undefined && equals(parameter_, compared)
      })
    )
  }
  if (isUnion(left) || isUnion(right))
    return (
      isUnion(left) &&
      isUnion(right) &&
      left.members.length === right.members.length &&
      left.members.every((member, ordinal) => {
        const compared = right.members.at(ordinal)
        return compared !== undefined && haveSameRepresentationShape(member, compared)
      })
    )
  return false
}

const opaqueEvidenceInGenericArguments = (
  actual: GenericArgument,
  expected: GenericArgument,
  family: OpaqueFamilyKey,
): ReadonlyArray<RepresentationArgument> => {
  if (isOpaqueRepresentationArgument(expected) && equalsOpaqueFamily(expected.family, family))
    return isRepresentationArgument(actual) ? Object.freeze([actual]) : Object.freeze([])
  if (isTypeArgument(actual) && isTypeArgument(expected))
    return opaqueRepresentationEvidence(actual, expected, family)
  if (isRequirementRowArgument(actual) && isRequirementRowArgument(expected))
    return Object.freeze(
      requirementMembers(expected).flatMap((requirement, ordinal) => {
        const supplied = requirementMembers(actual).at(ordinal)
        return supplied === undefined
          ? []
          : opaqueRepresentationEvidence(supplied.capability, requirement.capability, family)
      }),
    )
  return Object.freeze([])
}

/**
 * Extracts concrete or dependent representation evidence from the positions occupied by one
 * opaque family in an expected producer result.
 */
export const opaqueRepresentationEvidence = (
  actual: Type,
  expected: Type,
  family: OpaqueFamilyKey,
): ReadonlyArray<RepresentationArgument> => {
  if (
    isRepresented(expected) &&
    isOpaqueRepresentationArgument(expected.representation.argument) &&
    equalsOpaqueFamily(expected.representation.argument.family, family)
  )
    return isRepresented(actual)
      ? Object.freeze([actual.representation.argument])
      : Object.freeze([])
  if (isRepresented(actual)) return opaqueRepresentationEvidence(actual.contract, expected, family)
  if (isRepresented(expected))
    return opaqueRepresentationEvidence(actual, expected.contract, family)
  if (isNominal(actual) && isNominal(expected))
    return Object.freeze(
      expected.arguments.flatMap((argument, ordinal) => {
        const supplied = actual.arguments.at(ordinal)
        return supplied === undefined
          ? []
          : opaqueEvidenceInGenericArguments(supplied, argument, family)
      }),
    )
  if (isFixedArray(actual) && isFixedArray(expected))
    return opaqueRepresentationEvidence(actual.element, expected.element, family)
  if (isSlice(actual) && isSlice(expected))
    return opaqueRepresentationEvidence(actual.element, expected.element, family)
  if (isReference(actual) && isReference(expected))
    return opaqueRepresentationEvidence(actual.target, expected.target, family)
  if (isCallable(actual) && isCallable(expected))
    return Object.freeze([
      ...expected.parameters.flatMap((parameter_, ordinal) => {
        const supplied = actual.parameters.at(ordinal)
        return supplied === undefined
          ? []
          : opaqueRepresentationEvidence(supplied, parameter_, family)
      }),
      ...opaqueRepresentationEvidence(actual.result, expected.result, family),
    ])
  if (isEffect(actual) && isEffect(expected))
    return Object.freeze([
      ...opaqueRepresentationEvidence(actual.success, expected.success, family),
      ...failureMembers(expected).flatMap((failure, ordinal) => {
        const supplied = failureMembers(actual).at(ordinal)
        return supplied === undefined ? [] : opaqueRepresentationEvidence(supplied, failure, family)
      }),
      ...requirementMembers(expected).flatMap((requirement, ordinal) => {
        const supplied = requirementMembers(actual).at(ordinal)
        return supplied === undefined
          ? []
          : opaqueRepresentationEvidence(supplied.capability, requirement.capability, family)
      }),
    ])
  if (isUnion(actual) && isUnion(expected))
    return Object.freeze(
      expected.members.flatMap((member, ordinal) => {
        const supplied = actual.members.at(ordinal)
        return supplied === undefined ? [] : opaqueRepresentationEvidence(supplied, member, family)
      }),
    )
  return Object.freeze([])
}

interface FoldVisitor<A> {
  readonly type?: (self: Type) => A | undefined
  readonly argument?: (self: GenericArgument) => A | undefined
}

/**
 * Folds every semantic type and erased generic argument in deterministic preorder.
 *
 * This is the single structural walk used by Type-owned collectors. Adding a new type or generic
 * argument kind therefore has one exhaustiveness point instead of several subtly different walks.
 */
const fold = <A>(self: Type, visitor: FoldVisitor<A>): ReadonlyArray<A> => {
  const found: Array<A> = []
  const append = (value: A | undefined): void => {
    if (value !== undefined) found.push(value)
  }
  const visitArgument = (argument: GenericArgument): void => {
    append(visitor.argument?.(argument))
    if (isTypeArgument(argument)) visitType(argument)
    else if (isRepresentationParameterArgument(argument)) visitType(argument.parameter)
    else if (isOpaqueRepresentationArgument(argument)) {
      visitType(argument.contract)
      for (const enclosing of argument.arguments) visitArgument(enclosing)
    } else if (isExactRepresentationArgument(argument)) {
      visitArgument(argument.identity)
      visitType(argument.contract)
    } else if (isCompositeEffectRepresentationArgument(argument)) {
      visitType(argument.contract)
      for (const alternative of argument.alternatives) visitArgument(alternative)
    } else if (isEffectIdentityArgument(argument)) {
      for (const typeArgument of argument.owner?.typeArguments ?? []) visitArgument(typeArgument)
    } else if (isCallableIdentityArgument(argument)) {
      for (const typeArgument of argument.typeArguments) visitArgument(typeArgument)
      for (const typeArgument of argument.environment?.owner.typeArguments ?? [])
        visitArgument(typeArgument)
    } else if (isRequirementRowArgument(argument)) {
      for (const requirement of RowAlgebra.concreteMembers(requirementRowPolicy(), argument.row))
        visitType(requirement.capability)
      const parameters_ = RowAlgebra.parameters(requirementRowPolicy(), argument.row)
      for (const parameter_ of [...parameters_.rows, ...parameters_.members]) visitType(parameter_)
    }
  }
  const visitFailureRow = (row: FailureRow): void => {
    for (const failure of RowAlgebra.concreteMembers(failureRowPolicy(), row)) visitType(failure)
    const parameters_ = RowAlgebra.parameters(failureRowPolicy(), row)
    for (const parameter_ of [...parameters_.rows, ...parameters_.members]) visitType(parameter_)
  }
  const visitRequirementRow = (row: RequirementsRow): void => {
    for (const requirement of RowAlgebra.concreteMembers(requirementRowPolicy(), row))
      visitType(requirement.capability)
    const parameters_ = RowAlgebra.parameters(requirementRowPolicy(), row)
    for (const parameter_ of [...parameters_.rows, ...parameters_.members]) visitType(parameter_)
  }
  const visitConstraint = (constraint: Constraint.Constraint): void => {
    switch (constraint._tag) {
      case 'NominalMemberConstraint':
        visitType(constraint.selected)
        visitFailureRow(constraint.source)
        break
      case 'FailureSubsetConstraint':
        visitFailureRow(constraint.selected)
        visitFailureRow(constraint.source)
        break
      case 'RequirementSubsetConstraint':
        visitRequirementRow(constraint.selected)
        visitRequirementRow(constraint.source)
        break
      case 'ProviderSelectionConstraint':
        visitType(constraint.provider)
        visitRequirementRow(constraint.selected)
        visitRequirementRow(constraint.source)
        break
    }
  }
  const visitEvidence = (evidence: Constraint.ConstraintEvidence): void => {
    switch (evidence._tag) {
      case 'Assumed':
        visitConstraint(evidence.wanted)
        for (const argument of evidence.substitution.values()) visitArgument(argument)
        break
      case 'Member':
        visitType(evidence.selected)
        visitFailureRow(evidence.source)
        break
      case 'FailureSubset':
        visitFailureRow(evidence.selected)
        visitFailureRow(evidence.source)
        break
      case 'RequirementSubset':
        visitRequirementRow(evidence.selected)
        visitRequirementRow(evidence.source)
        break
      case 'RequirementSelection':
        visitConstraint(evidence.wanted)
        visitType(evidence.selected.capability)
        visitType(evidence.provider)
        if (evidence.providerMatch._tag === 'Conformance')
          for (const argument of evidence.providerMatch.witness.typeArguments)
            visitArgument(argument)
        break
    }
  }
  const visitContract = (contract: CallableContract.CallableContract): void => {
    for (const binder of contract.binders) {
      visitType(binder)
      if (binder.representationBound !== undefined) visitType(binder.representationBound)
    }
    for (const parameter_ of contract.parameters) visitType(parameter_.type)
    visitType(contract.result)
    for (const constraint of contract.constraints) visitConstraint(constraint)
  }
  const visitType = (type: Type): void => {
    append(visitor.type?.(type))
    if (isNominal(type)) {
      for (const argument of type.arguments) visitArgument(argument)
    } else if (isFixedArray(type) || isSlice(type)) visitType(type.element)
    else if (isReference(type)) visitType(type.target)
    else if (isCallable(type)) {
      for (const parameter_ of type.parameters) visitType(parameter_)
      visitType(type.result)
      if (type.schema !== undefined) {
        visitContract(type.schema.contract)
        for (const binder of type.schema.binders) {
          visitType(binder)
          if (binder.representationBound !== undefined) visitType(binder.representationBound)
        }
        for (const constraint of type.schema.constraints) visitConstraint(constraint)
        for (const evidence of type.schema.evidence) visitEvidence(evidence)
        for (const argument of type.schema.substitution.values()) visitArgument(argument)
      }
    } else if (isEffect(type)) {
      visitType(type.success)
      visitFailureRow(type.failureRow)
      visitRequirementRow(type.requirementRow)
    } else if (isRepresented(type)) {
      visitArgument(type.representation.argument)
      visitType(type.contract)
    } else if (isUnion(type)) {
      for (const member of type.members) visitType(member)
    }
  }
  visitType(self)
  return Object.freeze(found)
}

const typeMaySpecialize = (self: Type): boolean =>
  parameters(self).length > 0 ||
  fold(self, {
    argument: (argument) =>
      (isEffectIdentityArgument(argument) && argument.owner !== undefined) ||
      (isCallableIdentityArgument(argument) && argument.environment !== undefined)
        ? true
        : undefined,
  }).length > 0

/** Returns every opaque family instance nested in one semantic type. */
export const opaqueRepresentationArguments = (
  self: Type,
): ReadonlyArray<OpaqueRepresentationArgument> =>
  fold(self, {
    argument: (argument) => (isOpaqueRepresentationArgument(argument) ? argument : undefined),
  })

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
  if (isFixedArray(self)) return `Array<${encode(self.element)}, ${self.length}>`
  if (isSlice(self))
    return `${self.access === 'Exclusive' ? '&mut ' : '&'}[${encode(self.element)}]`
  if (isReference(self))
    return `${self.access === 'Exclusive' ? '&mut ' : '&'}${encode(self.target)}`
  if (isCallable(self)) {
    const mode = self.mode === 'Exclusive' ? 'mut ' : self.mode === 'Take' ? 'once ' : ''
    return `${self.unsafe ? 'unsafe ' : ''}${mode}fn(${self.parameters.map(encode).join(', ')}) -> ${encode(self.result)}`
  }
  if (isEffect(self)) {
    const access = self.access === 'Exclusive' ? 'mut ' : self.access === 'Take' ? 'once ' : ''
    const failureMembers = RowAlgebra.encode(
      failureRowPolicy(),
      self.failureRow,
      encode,
      (parameter_) => parameter_.name,
      (member) => member.parameter.name,
    )
    const row = failureMembers.length === 0 ? '' : ` ! ${failureMembers}`
    const requirementMembers = RowAlgebra.encode(
      requirementRowPolicy(),
      self.requirementRow,
      (requirement) =>
        `${requirement.access === 'Exclusive' ? '&mut ' : '&'}${encode(requirement.capability)}${requirement.role === RequirementRow.defaultRole ? '' : ` at ${RequirementRow.roleName(requirement.role)}`}`,
      (parameter_) => parameter_.name,
      (member) =>
        `${member.access === 'Exclusive' ? '&mut ' : '&'}${member.capability.name}${member.role === RequirementRow.defaultRole ? '' : ` at ${RequirementRow.roleName(member.role)}`}`,
    )
    const requirements = requirementMembers.length === 0 ? '' : ` ? ${requirementMembers}`
    return `${access}Effect<${encode(self.success)}${row}${requirements}>`
  }
  if (isRepresented(self)) return encode(self.contract)
  const someMember = self.members.find(
    (member): member is Nominal =>
      isNominal(member) &&
      member.module === 'silk/option' &&
      member.name === 'Some' &&
      member.arguments.length === 1,
  )
  const noneMember = self.members.find(
    (member): member is Nominal =>
      isNominal(member) && member.module === 'silk/option' && member.name === 'None',
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

/** Renders one normalized requirement member with its access demand and optional nominal role. */
export const encodeRequirement = (
  self: Requirement,
  encodeCapability: (capability: Type) => string = encode,
): string =>
  `${self.access === 'Exclusive' ? '&mut ' : '&'}${encodeCapability(self.capability)}${self.role === RequirementRow.defaultRole ? '' : ` at ${RequirementRow.roleName(self.role)}`}`

/** One declaration named by an exact representation carried inside a type. */
export interface ExactRepresentationDeclaration {
  readonly module: string
  readonly name: string
}

/**
 * Names every declaration whose exact representation one type carries, in encounter order.
 *
 * An exact representation is reported before descending into its identity arguments and structural
 * contract, because the contract alone does not name the construction the representation fixed.
 */
export const exactRepresentationDeclarations = (
  self: Type,
): ReadonlyArray<ExactRepresentationDeclaration> =>
  fold(self, {
    argument: (argument) =>
      isExactRepresentationArgument(argument) &&
      isCallableIdentityArgument(argument.identity) &&
      argument.identity.target._tag === 'Declaration'
        ? Object.freeze({
            module: argument.identity.target.module,
            name: argument.identity.target.name,
          })
        : undefined,
  })

/** Returns every canonical nominal nested in a type, in deterministic preorder. */
export const nominals = (self: Type): ReadonlyArray<Nominal> =>
  fold(self, { type: (type) => (isNominal(type) ? type : undefined) })

/** Visits every structural type occurrence in deterministic pre-order. */
export const visit = (self: Type, visitor: (type: Type) => void): void => {
  fold(self, { type: (type) => visitor(type) })
}

/** Reports whether one type occurs strictly inside another type's structural representation. */
export const isStrictStructuralSubterm = (candidate: Type, whole: Type): boolean => {
  if (equals(candidate, whole)) return false
  let found = false
  visit(whole, (type) => {
    if (equals(candidate, type)) found = true
  })
  return found
}

/** Returns every declaration-owned parameter nested in a type, without duplicates. */
export const parameters = (self: Type): ReadonlyArray<Parameter> => {
  const found = new Map<string, Parameter>()
  const nested = new Set<string>()
  fold(self, {
    type: (type) => {
      if (isCallable(type)) for (const binder of type.schema?.binders ?? []) nested.add(key(binder))
      else if (isParameter(type)) found.set(key(type), type)
    },
    argument: (argument) => {
      if (isRepresentationParameterArgument(argument))
        found.set(key(argument.parameter), argument.parameter)
    },
  })
  return Object.freeze(
    [...found.values()].filter((parameter_) => !nested.has(key(parameter_))).sort(compare),
  )
}

/** Tests whether a type contains no open generic parameters. */
export const isConcrete = (self: Type): boolean => parameters(self).length === 0

const runtimeAvailableFailureRow = (self: FailureRow): boolean => {
  const concrete = RowAlgebra.concretize(failureRowPolicy(), self)
  const parameters_ = RowAlgebra.parameters(failureRowPolicy(), self)
  return (
    concrete._tag === 'Concrete' &&
    concrete.row.members.every(runtimeAvailable) &&
    parameters_.rows.every(runtimeAvailable) &&
    parameters_.members.every(runtimeAvailable)
  )
}

const runtimeAvailableRequirementRow = (self: RequirementsRow): boolean => {
  const concrete = RowAlgebra.concretize(requirementRowPolicy(), self)
  const parameters_ = RowAlgebra.parameters(requirementRowPolicy(), self)
  return (
    concrete._tag === 'Concrete' &&
    concrete.row.members.every((requirement) => runtimeAvailable(requirement.capability)) &&
    parameters_.rows.every(runtimeAvailable) &&
    parameters_.members.every(runtimeAvailable)
  )
}

const runtimeAvailableGenericArgument = (self: GenericArgument): boolean => {
  if (isUnavailableGenericArgument(self)) return false
  if (isRepresentationParameterArgument(self)) return true
  if (isOpaqueRepresentationArgument(self))
    return runtimeAvailable(self.contract) && self.arguments.every(runtimeAvailableGenericArgument)
  if (isExactRepresentationArgument(self))
    return runtimeAvailable(self.contract) && runtimeAvailableGenericArgument(self.identity)
  if (isCompositeEffectRepresentationArgument(self))
    return (
      runtimeAvailable(self.contract) &&
      self.alternatives.length > 0 &&
      self.alternatives.every(runtimeAvailableGenericArgument)
    )
  if (isEffectIdentityArgument(self))
    return self.owner?.typeArguments.every(runtimeAvailableGenericArgument) ?? true
  if (isCallableIdentityArgument(self))
    return (
      self.typeArguments.every(runtimeAvailableGenericArgument) &&
      (self.environment?.owner.typeArguments.every(runtimeAvailableGenericArgument) ?? true)
    )
  if (isRequirementRowArgument(self)) return runtimeAvailableRequirementRow(self.row)
  return runtimeAvailable(self)
}

const runtimeAvailableConstraint = (constraint: Constraint.Constraint): boolean => {
  switch (constraint._tag) {
    case 'NominalMemberConstraint':
      return runtimeAvailable(constraint.selected) && runtimeAvailableFailureRow(constraint.source)
    case 'FailureSubsetConstraint':
      return (
        runtimeAvailableFailureRow(constraint.selected) &&
        runtimeAvailableFailureRow(constraint.source)
      )
    case 'RequirementSubsetConstraint':
      return (
        runtimeAvailableRequirementRow(constraint.selected) &&
        runtimeAvailableRequirementRow(constraint.source)
      )
    case 'ProviderSelectionConstraint':
      return (
        runtimeAvailable(constraint.provider) &&
        runtimeAvailableRequirementRow(constraint.selected) &&
        runtimeAvailableRequirementRow(constraint.source)
      )
  }
}

/** Checks one blueprint constraint after closing it through the schema substitution. */
const runtimeAvailableConstraintUnder = (
  constraint: Constraint.Constraint,
  substitution: Substitution,
): boolean => {
  switch (constraint._tag) {
    case 'NominalMemberConstraint':
      return runtimeAvailableConstraint(
        Object.freeze({
          ...constraint,
          selected: substitute(constraint.selected, substitution),
          source: substituteFailureRow(constraint.source, substitution),
        }),
      )
    case 'FailureSubsetConstraint':
      return runtimeAvailableConstraint(
        Object.freeze({
          ...constraint,
          selected: substituteFailureRow(constraint.selected, substitution),
          source: substituteFailureRow(constraint.source, substitution),
        }),
      )
    case 'RequirementSubsetConstraint':
      return runtimeAvailableConstraint(
        Object.freeze({
          ...constraint,
          selected: substituteRequirementsRow(constraint.selected, substitution),
          source: substituteRequirementsRow(constraint.source, substitution),
        }),
      )
    case 'ProviderSelectionConstraint':
      return runtimeAvailableConstraint(
        Object.freeze({
          ...constraint,
          provider: substitute(constraint.provider, substitution),
          selected: substituteRequirementsRow(constraint.selected, substitution),
          source: substituteRequirementsRow(constraint.source, substitution),
        }),
      )
  }
}

const runtimeAvailableEvidence = (evidence: Constraint.ConstraintEvidence): boolean => {
  switch (evidence._tag) {
    case 'Assumed':
      return (
        runtimeAvailableConstraint(evidence.wanted) &&
        [...evidence.substitution.values()].every(runtimeAvailableGenericArgument)
      )
    case 'Member':
      return runtimeAvailable(evidence.selected) && runtimeAvailableFailureRow(evidence.source)
    case 'FailureSubset':
      return (
        runtimeAvailableFailureRow(evidence.selected) && runtimeAvailableFailureRow(evidence.source)
      )
    case 'RequirementSubset':
      return (
        runtimeAvailableRequirementRow(evidence.selected) &&
        runtimeAvailableRequirementRow(evidence.source)
      )
    case 'RequirementSelection':
      return (
        runtimeAvailableConstraint(evidence.wanted) &&
        runtimeAvailable(evidence.selected.capability) &&
        runtimeAvailable(evidence.provider) &&
        (evidence.providerMatch._tag === 'Conformance'
          ? evidence.providerMatch.witness.typeArguments.every(runtimeAvailableGenericArgument)
          : true)
      )
  }
}

function runtimeAvailable(self: Type): boolean {
  if (typeof self === 'string' || isParameter(self)) return true
  if (isNominal(self)) return self.arguments.every(runtimeAvailableGenericArgument)
  if (isFixedArray(self) || isSlice(self)) return runtimeAvailable(self.element)
  if (isReference(self)) return runtimeAvailable(self.target)
  if (isCallable(self))
    return (
      self.parameters.every(runtimeAvailable) &&
      runtimeAvailable(self.result) &&
      // The schema contract is the generic declaration blueprint; its binders, rows, and
      // constraints are symbolic by construction and close through schema.substitution. Runtime
      // availability is therefore decided on the substituted constraints, the evidence, and the
      // substitution itself — never on the raw blueprint.
      (self.schema === undefined ||
        (self.schema.binders.every(runtimeAvailable) &&
          self.schema.constraints.every((constraint) =>
            runtimeAvailableConstraintUnder(constraint, self.schema?.substitution ?? new Map()),
          ) &&
          self.schema.evidence.every(runtimeAvailableEvidence) &&
          [...self.schema.substitution.values()].every(runtimeAvailableGenericArgument)))
    )
  if (isEffect(self))
    return (
      runtimeAvailable(self.success) &&
      runtimeAvailableFailureRow(self.failureRow) &&
      runtimeAvailableRequirementRow(self.requirementRow)
    )
  if (isRepresented(self))
    return (
      self.representation.admissibility._tag !== 'Unavailable' &&
      runtimeAvailable(self.contract) &&
      runtimeAvailable(self.representation.requiredBound) &&
      runtimeAvailableGenericArgument(self.representation.argument)
    )
  return self.members.every(runtimeAvailable)
}

/** Tests whether a type is closed, fully available, and safe to expose to runtime consumers. */
export const isRuntimeConcrete = (self: Type): boolean => isConcrete(self) && runtimeAvailable(self)

const isClosedGenericArgument = (self: GenericArgument): boolean => {
  if (isUnavailableGenericArgument(self) || isRepresentationParameterArgument(self)) return false
  if (isOpaqueRepresentationArgument(self))
    return isConcrete(self.contract) && self.arguments.every(isClosedGenericArgument)
  if (isExactRepresentationArgument(self))
    return isConcrete(self.contract) && isClosedGenericArgument(self.identity)
  if (isCompositeEffectRepresentationArgument(self))
    return (
      isConcrete(self.contract) &&
      self.alternatives.length > 0 &&
      self.alternatives.every(isClosedGenericArgument)
    )
  if (isEffectIdentityArgument(self))
    return self.owner?.typeArguments.every(isClosedGenericArgument) ?? true
  if (isCallableIdentityArgument(self))
    return (
      self.typeArguments.every(isClosedGenericArgument) &&
      (self.environment?.owner.typeArguments.every(isClosedGenericArgument) ?? true)
    )
  if (isRequirementRowArgument(self))
    return (
      RowAlgebra.concretize(requirementRowPolicy(), self.row)._tag === 'Concrete' &&
      requirementMembers(self).every((requirement) => isConcrete(requirement.capability))
    )
  return isConcrete(self)
}

/** Tests whether one erased argument is fully closed and contains no unavailable recovery value. */
export const isRuntimeConcreteGenericArgument = (self: GenericArgument): boolean =>
  isClosedGenericArgument(self) && runtimeAvailableGenericArgument(self)

/** True when any nested Type satisfies the predicate (including self). */
export const someSubterm = (self: Type, predicate: (type: Type) => boolean): boolean => {
  return fold(self, { type: (type) => (predicate(type) ? true : undefined) }).length > 0
}

/** Tests whether a type contains a lexical borrow at any depth. */
export const containsBorrow = (self: Type): boolean =>
  someSubterm(self, (type) => isString(type) || isSlice(type) || isReference(type) || isSlot(type))

/** Tests whether a value may carry a lexical immutable view through data or control flow. */
export const containsViewBorrow = (self: Type): boolean =>
  someSubterm(self, (type) => isString(type) || isSlice(type))

/**
 * Tests whether a type stores one statically known callable environment anywhere inside it.
 *
 * A nominal that stores a callable names it in its own arguments, so the whole environment — and
 * every borrow it captured — travels with any value of that type. Ownership uses this to keep a
 * stored capture's loan alive for as long as the enclosing value holds the callable, exactly as it
 * does for a callable bound directly.
 */
export const containsCallableRepresentation = (self: Type): boolean =>
  someSubterm(
    self,
    (type) =>
      (isRepresented(type) && isCallable(type.contract)) ||
      (isNominal(type) &&
        type.arguments.some(
          (argument) =>
            isExactRepresentationArgument(argument) &&
            isCallableIdentityArgument(argument.identity),
        )),
  )

/** Tests whether a value stores one statically known Effect environment anywhere inside it. */
export const containsEffectRepresentation = (self: Type): boolean =>
  someSubterm(
    self,
    (type) =>
      (isRepresented(type) && isEffect(type.contract)) ||
      (isNominal(type) &&
        type.arguments.some(
          (argument) =>
            (isExactRepresentationArgument(argument) &&
              isEffectIdentityArgument(argument.identity)) ||
            isCompositeEffectRepresentationArgument(argument),
        )),
  )

/** Tests for either concrete executable environment carried through an enclosing value. */
export const containsExecutableRepresentation = (self: Type): boolean =>
  containsCallableRepresentation(self) || containsEffectRepresentation(self)

/** Tests for explicit borrow wrappers forbidden inside ordinary type positions. */
export const containsPositionRestrictedBorrow = (self: Type): boolean =>
  someSubterm(self, (type) => isSlice(type) || isReference(type) || isSlot(type))

/** Applies one substitution while preserving invalid lifted-member specialization as data. */
export const specializeFailureRow = (
  self: FailureRow,
  substitution: Substitution,
): RowAlgebra.SubstitutionResult<Type, Parameter, FailureMemberShape> => {
  const concrete = RowAlgebra.mapConcreteMembers(failureRowPolicy(), self, (failure) => {
    return substitute(failure, substitution)
  })
  const result = RowAlgebra.substitute(failureRowPolicy(), concrete, {
    row: (parameter_) => {
      void parameter_
      return undefined
    },
    member: (member) => {
      const replacement = substitution.get(key(member.parameter))
      if (replacement === undefined) return Object.freeze({ _tag: 'Residual', member })
      if (isTypeArgument(replacement) && isParameter(replacement) && replacement.kind === 'Value')
        return Object.freeze({
          _tag: 'Residual',
          member: failureMemberShape(replacement),
        })
      if (isTypeArgument(replacement) && !isUnion(replacement) && !isNever(replacement))
        return Object.freeze({ _tag: 'Concrete', member: replacement })
      if (isTypeArgument(replacement) && isUnion(replacement))
        return Object.freeze({ _tag: 'ConcreteRow', members: replacement.members })
      if (isTypeArgument(replacement) && isNever(replacement))
        return Object.freeze({ _tag: 'ConcreteRow', members: Object.freeze([]) })
      return Object.freeze({
        _tag: 'InvalidSingleton',
        reason: `failure member ${member.parameter.name} did not specialize to an ordinary type`,
      })
    },
  })
  return result
}

export const substituteFailureRow = (self: FailureRow, substitution: Substitution): FailureRow => {
  const result = specializeFailureRow(self, substitution)
  return result._tag === 'Substituted' ? result.row : self
}

/** Applies one substitution while preserving invalid lifted capability specialization as data. */
export const specializeRequirementsRow = (
  self: RequirementsRow,
  substitution: Substitution,
): RowAlgebra.SubstitutionResult<Requirement, Parameter, RequirementMemberShape> => {
  const concrete = RowAlgebra.mapConcreteMembers(requirementRowPolicy(), self, (requirement) => {
    const capability = substitute(requirement.capability, substitution)
    return isNominal(capability) || isParameter(capability)
      ? Object.freeze({ ...requirement, capability })
      : requirement
  })
  const result = RowAlgebra.substitute(requirementRowPolicy(), concrete, {
    row: (parameter_) => {
      const replacement = substitution.get(key(parameter_))
      if (replacement === undefined || !isRequirementRowArgument(replacement)) return undefined
      return replacement.row
    },
    member: (member) => {
      const replacement = substitution.get(key(member.capability))
      if (replacement === undefined) return Object.freeze({ _tag: 'Residual', member })
      if (isTypeArgument(replacement) && isNominal(replacement))
        return Object.freeze({
          _tag: 'Concrete',
          member: Object.freeze({
            capability: replacement,
            access: member.access,
            role: member.role,
          }),
        })
      if (isTypeArgument(replacement) && isParameter(replacement) && replacement.kind === 'Value')
        return Object.freeze({
          _tag: 'Residual',
          member: requirementMemberShape(replacement, member.access, member.role),
        })
      return Object.freeze({
        _tag: 'InvalidSingleton',
        reason: `requirement capability ${member.capability.name} did not specialize to one nominal`,
      })
    },
  })
  return result
}

export const substituteRequirementsRow = (
  self: RequirementsRow,
  substitution: Substitution,
): RequirementsRow => {
  const result = specializeRequirementsRow(self, substitution)
  return result._tag === 'Substituted' ? result.row : self
}

/** Replaces declaration-owned parameters recursively through one canonical type. */
export const substitute = (self: Type, substitution: Substitution): Type => {
  if (isParameter(self)) {
    const replacement = substitution.get(key(self))
    return replacement !== undefined && isTypeArgument(replacement) ? replacement : self
  }
  if (isNominal(self))
    return specializeNominal(
      self,
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
      self.schema === undefined
        ? undefined
        : Object.freeze({
            ...self.schema,
            substitution: new Map([
              ...[...self.schema.substitution.entries()].map(
                ([parameter_, argument]) =>
                  [parameter_, substituteGenericArgument(argument, substitution)] as const,
              ),
              ...self.schema.binders.flatMap((binder) => {
                const replacement = substitution.get(key(binder))
                return replacement === undefined ? [] : ([[key(binder), replacement]] as const)
              }),
            ]),
          }),
      self.unsafe,
    )
  if (isEffect(self)) {
    const success = substitute(self.success, substitution)
    return effectWithRows(
      success,
      substituteFailureRow(self.failureRow, substitution),
      self.access,
      substituteRequirementsRow(self.requirementRow, substitution),
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
      argument._tag === 'RepresentationParameterArgument'
        ? argument.parameter.representationBound
        : argument.contract
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
      : isOpaqueRepresentationArgument(self)
        ? (() => {
            const contract = substitute(self.contract, substitution)
            if (!isCallable(contract) && !isEffect(contract)) return self
            return opaqueRepresentationArgument(
              self.family,
              contract,
              self.arguments.map((argument) => substituteGenericArgument(argument, substitution)),
            )
          })()
        : isCompositeEffectRepresentationArgument(self)
          ? (() => {
              const contract = substitute(self.contract, substitution)
              if (!isEffect(contract)) return self
              const alternatives = self.alternatives.flatMap((alternative) => {
                const specialized = substituteGenericArgument(alternative, substitution)
                return isExactRepresentationArgument(specialized) &&
                  isEffect(specialized.contract) &&
                  isEffectIdentityArgument(specialized.identity)
                  ? [specialized]
                  : []
              })
              return alternatives.length === self.alternatives.length
                ? compositeEffectRepresentationArgument(contract, alternatives)
                : self
            })()
          : isExactRepresentationArgument(self)
            ? (() => {
                const contract = substitute(self.contract, substitution)
                if (!isCallable(contract) && !isEffect(contract)) return self
                const identity = substituteGenericArgument(self.identity, substitution)
                return isCallableIdentityArgument(identity) || isEffectIdentityArgument(identity)
                  ? exactRepresentationArgument(identity, contract)
                  : self
              })()
            : isEffectIdentityArgument(self)
              ? effectIdentityArgument(
                  self.identity,
                  self.owner === undefined
                    ? undefined
                    : {
                        declaration: self.owner.declaration,
                        typeArguments: self.owner.typeArguments.map((argument) =>
                          substituteGenericArgument(argument, substitution),
                        ),
                      },
                )
              : isCallableIdentityArgument(self)
                ? callableIdentityArgument(
                    self.identity,
                    self.target,
                    self.typeArguments.map((argument) =>
                      substituteGenericArgument(argument, substitution),
                    ),
                    self.environment === undefined
                      ? undefined
                      : callableEnvironmentIdentity(self.environment.site, {
                          declaration: self.environment.owner.declaration,
                          typeArguments: self.environment.owner.typeArguments.map((argument) =>
                            substituteGenericArgument(argument, substitution),
                          ),
                        }),
                  )
                : isRequirementRowArgument(self)
                  ? requirementRowArgumentFromRow(substituteRequirementsRow(self.row, substitution))
                  : substitute(self, substitution)

const sameExecutableOwnerDeclaration = (
  left: ExecutableSpecializationOwner,
  right: ExecutableSpecializationOwner,
): boolean =>
  left.declaration.module === right.declaration.module &&
  left.declaration.name === right.declaration.name

/**
 * Replaces a source executable owner's open specialization with one complete discovered instance.
 * This stays a semantic type transformation: it neither inspects construction syntax nor creates a
 * second representation identity.
 */
export const specializeExecutableOwner = (
  self: Type,
  owner: ExecutableSpecializationOwner,
  specializeSchema?: CallableSchemaOwnerSpecializer,
): Type => {
  const specializeOwner = (
    current: ExecutableSpecializationOwner,
  ): ExecutableSpecializationOwner =>
    sameExecutableOwnerDeclaration(current, owner)
      ? owner
      : Object.freeze({
          declaration: current.declaration,
          typeArguments: Object.freeze(current.typeArguments.map(specializeArgument)),
        })
  const specializeArgument = (argument: GenericArgument): GenericArgument => {
    if (isUnavailableGenericArgument(argument) || isRepresentationParameterArgument(argument))
      return argument
    if (isOpaqueRepresentationArgument(argument)) {
      const contract = specializeType(argument.contract)
      return isCallable(contract) || isEffect(contract)
        ? opaqueRepresentationArgument(
            argument.family,
            contract,
            argument.arguments.map(specializeArgument),
          )
        : argument
    }
    if (isExactRepresentationArgument(argument)) {
      const contract = specializeType(argument.contract)
      const identity = specializeArgument(argument.identity)
      return (isCallableIdentityArgument(identity) || isEffectIdentityArgument(identity)) &&
        (isCallable(contract) || isEffect(contract))
        ? exactRepresentationArgument(identity, contract)
        : argument
    }
    if (isCompositeEffectRepresentationArgument(argument)) {
      const contract = specializeType(argument.contract)
      if (!isEffect(contract)) return argument
      const alternatives = argument.alternatives.flatMap((alternative) => {
        const specialized = specializeArgument(alternative)
        return isExactRepresentationArgument(specialized) &&
          isEffect(specialized.contract) &&
          isEffectIdentityArgument(specialized.identity)
          ? [specialized]
          : []
      })
      return alternatives.length === argument.alternatives.length
        ? compositeEffectRepresentationArgument(contract, alternatives)
        : argument
    }
    if (isEffectIdentityArgument(argument))
      return effectIdentityArgument(
        argument.identity,
        argument.owner === undefined ? undefined : specializeOwner(argument.owner),
      )
    if (isCallableIdentityArgument(argument))
      return callableIdentityArgument(
        argument.identity,
        argument.target,
        argument.typeArguments.map(specializeArgument),
        argument.environment === undefined
          ? undefined
          : callableEnvironmentIdentity(
              argument.environment.site,
              specializeOwner(argument.environment.owner),
            ),
      )
    if (isRequirementRowArgument(argument))
      return requirementRowArgumentFromRow(
        RowAlgebra.mapConcreteMembers(requirementRowPolicy(), argument.row, (requirement) => {
          const capability = specializeType(requirement.capability)
          return Object.freeze({
            ...requirement,
            capability:
              isNominal(capability) || isParameter(capability)
                ? capability
                : requirement.capability,
          })
        }),
      )
    return specializeType(argument)
  }
  const specializeType = (type: Type): Type => {
    if (isNominal(type)) return specializeNominal(type, type.arguments.map(specializeArgument))
    if (isFixedArray(type)) return fixedArray(specializeType(type.element), type.length)
    if (isSlice(type)) return slice(type.access, specializeType(type.element))
    if (isReference(type)) return reference(type.access, specializeType(type.target))
    if (isCallable(type))
      return callable(
        type.parameters.map(specializeType),
        specializeType(type.result),
        type.mode,
        type.schema === undefined
          ? undefined
          : (specializeSchema?.(type.schema, specializeType, specializeArgument) ?? type.schema),
        type.unsafe,
      )
    if (isEffect(type))
      return effectWithRows(
        specializeType(type.success),
        RowAlgebra.mapConcreteMembers(failureRowPolicy(), type.failureRow, (failure) => {
          const specialized = specializeType(failure)
          return isNominal(specialized) ? specialized : failure
        }),
        type.access,
        RowAlgebra.mapConcreteMembers(
          requirementRowPolicy(),
          type.requirementRow,
          (requirement) => {
            const capability = specializeType(requirement.capability)
            return Object.freeze({
              ...requirement,
              capability:
                isNominal(capability) || isParameter(capability)
                  ? capability
                  : requirement.capability,
            })
          },
        ),
      )
    if (isRepresented(type)) {
      const contract = specializeType(type.contract)
      const requiredBound = specializeType(type.representation.requiredBound)
      const argument = specializeArgument(type.representation.argument)
      return (isCallable(contract) || isEffect(contract)) &&
        (isCallable(requiredBound) || isEffect(requiredBound)) &&
        isRepresentationArgument(argument)
        ? represented(contract, requiredBound, argument)
        : type
    }
    if (isUnion(type)) {
      const normalized = union(type.members.map(specializeType))
      return normalized._tag === 'Normalized' ? normalized.type : type
    }
    return type
  }
  return specializeType(self)
}
