import * as CleanupPlan from './CleanupPlan.js'
import * as ConformanceProof from './ConformanceProof.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as ExecutionPackage from './ExecutionPackage.js'
import * as FieldRealization from './FieldRealization.js'
import * as Hir from './Hir.js'
import * as InstanceDiagnostics from './InstanceDiagnostics.js'
import * as Instances from './Instances.js'
import { alignUp } from './internal/Align.js'
import type {
  AddressScalar,
  CallingLane,
  CallingScalar,
  CallingShape,
  CallingShapeNode,
  Selector,
} from './internal/CallingShape.js'
import * as Packing from './internal/Packing.js'
import * as TypeInference from './internal/TypeInference.js'
import * as LocalSharedAllocationProvenance from './LocalSharedAllocationProvenance.js'
import * as LocalSharedControlBlock from './LocalSharedControlBlock.js'
import * as Match from './Match.js'
import * as OpaqueRealization from './OpaqueRealization.js'
import * as RepresentationField from './RepresentationField.js'
import * as RowAlgebra from './RowAlgebra.js'
import * as Scalar from './Scalar.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as StaticText from './StaticText.js'
import * as SuspensionMode from './SuspensionMode.js'
import type * as Target from './Target.js'
import * as Type from './Type.js'

const compareRuntimeTypes = (left: Type.Type, right: Type.Type): number => {
  const leftKey = Type.runtimeKey(left)
  const rightKey = Type.runtimeKey(right)
  if (leftKey < rightKey) return -1
  if (leftKey > rightKey) return 1
  return 0
}

/** Physical placement shared by every aggregate and hidden-environment field. */
export interface PlacedField extends Packing.PlacedField {}

/** One declaration-ordered physical field within an aggregate representation. */
export interface Field extends PlacedField {
  readonly _tag: 'LayoutField'
  readonly id: DeclarationFacts.FieldId
  readonly name: string
  readonly type: DeclarationFacts.SemanticType
}

/** Static cleanup hook required before structural cleanup; contributes no ABI bytes. */
export interface CleanupHook {
  readonly hook: DeclarationFacts.CanonicalId
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
}

/** The initial closed representation vocabulary for concrete runtime types. */
export type Representation =
  | { readonly _tag: 'SignedInteger'; readonly bits: Scalar.FixedBits }
  | { readonly _tag: 'UnsignedInteger'; readonly bits: Scalar.FixedBits }
  | {
      readonly _tag: 'ScalarEnum'
      readonly enum: DeclarationFacts.CanonicalId
      readonly scalar: Scalar.EnumRepresentationSpelling
      readonly bits: Scalar.FixedBits
      readonly signedness: 'Signed' | 'Unsigned'
      readonly members: ReadonlyArray<{
        readonly member: DeclarationFacts.CanonicalEnumMemberId
        readonly discriminant: bigint
      }>
    }
  | { readonly _tag: 'Floating'; readonly bits: 32 | 64; readonly ieee: true }
  | { readonly _tag: 'Boolean'; readonly bits: 32; readonly falseValue: 0; readonly trueValue: 1 }
  | {
      readonly _tag: 'Aggregate'
      readonly fields: ReadonlyArray<Field>
      readonly tailPadding: number
      readonly cleanupHook?: CleanupHook
    }
  | {
      readonly _tag: 'CallableEnvironment'
      readonly realization: FieldRealization.CallableRealization
      readonly fields: ReadonlyArray<CallableEnvironmentField>
      readonly tailPadding: number
    }
  | {
      readonly _tag: 'StoredEffectEnvironment'
      readonly realization: FieldRealization.EffectRealization
      readonly fields: ReadonlyArray<StoredEffectEnvironmentField>
      readonly tailPadding: number
    }
  | {
      readonly _tag: 'Repeated'
      readonly element: DeclarationFacts.SemanticType
      readonly length: number
      readonly stride: number
    }
  | {
      readonly _tag: 'Slice'
      readonly element: DeclarationFacts.SemanticType
      readonly address: {
        readonly bits: 32 | 64
        readonly offset: 0
        readonly size: 4 | 8
        readonly alignment: 4 | 8
      }
      readonly length: {
        readonly type: 'usize'
        readonly offset: number
        readonly size: 4 | 8
      }
      readonly addressPadding: number
      readonly tailPadding: number
      readonly stride: number
    }
  | {
      readonly _tag: 'String'
      readonly storage: {
        readonly provenance: 'Utf8'
        readonly bits: 32 | 64
        readonly offset: 0
        readonly size: 4 | 8
        readonly alignment: 4 | 8
      }
      readonly byteLength: {
        readonly type: 'usize'
        readonly offset: number
        readonly size: 4 | 8
      }
      readonly storagePadding: number
      readonly tailPadding: number
    }
  | {
      readonly _tag: 'Reference'
      readonly target: DeclarationFacts.SemanticType
      readonly address: {
        readonly bits: 32 | 64
        readonly offset: 0
        readonly size: 4 | 8
        readonly alignment: 4 | 8
      }
    }
  | {
      readonly _tag: 'Union'
      readonly tag: { readonly bits: 32; readonly size: 4 }
      readonly members: ReadonlyArray<{
        readonly type: Type.Type
        readonly ordinal: number
        readonly size: number
        readonly alignment: number
      }>
      readonly payloadOffset: number
      readonly payloadSize: number
      readonly payloadAlignment: number
      readonly tagPadding: number
      readonly tailPadding: number
    }
  | {
      readonly _tag: 'NominalUnion'
      readonly union: DeclarationFacts.CanonicalId
      readonly tag: { readonly bits: 32; readonly size: 4 }
      readonly variants: ReadonlyArray<{
        readonly variant: DeclarationFacts.CanonicalUnionVariantId
        readonly ordinal: number
        readonly fields: ReadonlyArray<Field>
        readonly size: number
        readonly alignment: number
        readonly tailPadding: number
      }>
      readonly payloadOffset: number
      readonly payloadSize: number
      readonly payloadAlignment: number
      readonly tagPadding: number
      readonly tailPadding: number
      readonly cleanupHook?: CleanupHook
    }

/** Canonical struct-like storage used transiently for one selected nominal-union variant. */
export interface NominalUnionMaterialization {
  readonly payloadOffset: number
  readonly payloadSize: number
  readonly payloadAlignment: number
  readonly size: number
  readonly alignment: number
}

export const nominalUnionMaterialization = (
  representation: Extract<Representation, { readonly _tag: 'NominalUnion' }>,
): NominalUnionMaterialization => {
  const payloadSize = representation.variants.reduce(
    (maximum, variant) => Math.max(maximum, variant.size),
    0,
  )
  const payloadAlignment = representation.variants.reduce(
    (maximum, variant) => Math.max(maximum, variant.alignment),
    1,
  )
  const payloadOffset = alignUp(4, payloadAlignment)
  const alignment = Math.max(4, payloadAlignment)
  return Object.freeze({
    payloadOffset,
    payloadSize,
    payloadAlignment,
    size: alignUp(payloadOffset + payloadSize, alignment),
    alignment,
  })
}

/** One compiler-owned concrete layout entry. */
export interface Entry {
  readonly _tag: 'LayoutEntry'
  readonly type: DeclarationFacts.SemanticType
  /** Concrete sealed Copy evidence carried unchanged into MIR and every backend. */
  readonly copy: boolean
  readonly size: number
  readonly alignment: number
  readonly representation: Representation
  /** Compiler-private inline lanes for one exact executable value with no structural ABI. */
  readonly executable?: {
    readonly _tag: 'Callable' | 'Effect'
    readonly fields: ReadonlyArray<
      PlacedField & {
        readonly capture: number
        readonly type: DeclarationFacts.SemanticType
        readonly access: Type.CaptureAccess
        readonly representation: 'Value' | 'Borrow' | 'Callable'
        readonly offset: number
        readonly size: number
        readonly alignment: number
        readonly padding: number
        readonly effectIdentity?: string
        readonly callableIdentity?: Type.CallableIdentityArgument
      }
    >
  }
}

/** Why one nominal declaration cannot have a concrete physical representation. */
export type UnavailableReason =
  | { readonly _tag: 'InvalidDeclaration'; readonly detail: string }
  | {
      readonly _tag: 'UnavailableField'
      readonly field?: DeclarationFacts.FieldId
      readonly detail: string
    }
  | { readonly _tag: 'UnavailableDependency'; readonly dependency: DeclarationFacts.SemanticType }

/** One retained nominal layout failure that does not prevent unrelated layouts. */
export interface UnavailableEntry {
  readonly _tag: 'UnavailableLayoutEntry'
  readonly type: DeclarationFacts.SemanticType
  readonly dependencies: ReadonlyArray<Type.Nominal>
  readonly reason: UnavailableReason
  readonly cause?: Diagnostic.Identity
}

export type CatalogEntry = Entry | UnavailableEntry

/** One valid target-word constant awaiting the selected target's exact range verdict. */
export interface WordConstantLiteral {
  readonly type: WordType
  readonly value: bigint
  readonly span: SourceSpan.SourceSpan
}

/** The integer types whose width is the selected target's word. */
export type WordType = 'usize' | 'isize'

export const isWordType = (type: unknown): type is WordType => type === 'usize' || type === 'isize'

/** The inclusive exact range one word type spans on a target of the given width. */
export const wordRange = (
  type: WordType,
  bits: 32 | 64,
): { readonly minimum: bigint; readonly maximum: bigint } =>
  type === 'usize'
    ? { minimum: 0n, maximum: (1n << BigInt(bits)) - 1n }
    : { minimum: -(1n << BigInt(bits - 1)), maximum: (1n << BigInt(bits - 1)) - 1n }

/** Every canonical nominal declaration laid out for one selected target. */
export interface Catalog {
  readonly _tag: 'LayoutCatalog'
  readonly target: Target.Target
  readonly entries: ReadonlyArray<CatalogEntry>
  readonly wordConstants: ReadonlyArray<WordConstantLiteral>
}

/** The concrete layouts reached by one target-aware MIR program. */
export interface Plan {
  readonly _tag: 'LayoutPlan'
  readonly target: Target.Target
  readonly entries: ReadonlyArray<Entry>
  readonly effectEnvironments: ReadonlyArray<EffectEnvironment>
  readonly callableEnvironments: ReadonlyArray<CallableEnvironment>
  readonly callingShapes: ReadonlyArray<CallingShape>
  readonly staticData?: ReadonlyArray<StaticDataPlacement>
  readonly literalVerdicts: ReadonlyArray<WordLiteralVerdict>
  readonly localSharedAllocationProvenance: LocalSharedAllocationProvenance.Plan
  readonly executionPackages: ExecutionPackage.Module
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

/** Target placement facts for compiler-owned immutable literal bytes. */
export interface StaticDataPlacement {
  readonly _tag: 'StaticDataPlacement'
  readonly data: StaticText.Data
  readonly alignment: 1
  readonly addressBits: 32 | 64
  readonly lengthBits: 32 | 64
}

/** Target-owned storage for one monomorphized hidden Effect closure environment. */
export type EffectEnvironment =
  | {
      readonly _tag: 'EffectEnvironment'
      readonly instance: Instances.InstanceKey
      readonly site: Hir.EffectSiteId
      readonly effect: Type.Effect
      readonly successEffectIdentity?: string
      readonly fields: ReadonlyArray<EffectEnvironmentField>
      readonly size: number
      readonly alignment: number
      readonly tailPadding: number
    }
  | {
      readonly _tag: 'UnavailableEffectEnvironment'
      readonly instance: Instances.InstanceKey
      readonly site: Hir.EffectSiteId
      readonly effect: Type.Effect
      readonly reason: string
    }

/**
 * Whether one shared or exclusive capture stores a pointer to its source slot. Slice and
 * reference values are already stable borrow descriptors, so a capture of one stores the
 * descriptor inline: the loan it carries survives the short-lived slot it was read from, and the
 * hidden body receives the reference itself rather than a pointer to it.
 */
const borrowedCapture = (access: Type.CaptureAccess, type: Type.Type): boolean =>
  (access === 'Shared' || access === 'Exclusive') && !Type.isSlice(type) && !Type.isReference(type)

const sameExactOwner = (
  left: Instances.InstanceKey,
  right: Type.ExecutableSpecializationOwner,
): boolean =>
  left.declaration.module === right.declaration.module &&
  left.declaration.name === right.declaration.name &&
  left.typeArguments.length === right.typeArguments.length &&
  left.typeArguments.every((argument, ordinal) => {
    const expected = right.typeArguments.at(ordinal)
    return (
      expected !== undefined &&
      Type.runtimeGenericArgumentKey(argument) === Type.runtimeGenericArgumentKey(expected)
    )
  })

const sameVisibleOwner = (
  left: Instances.InstanceKey,
  right: Type.ExecutableSpecializationOwner,
): boolean => {
  if (
    left.declaration.module !== right.declaration.module ||
    left.declaration.name !== right.declaration.name
  )
    return false
  const leftVisible = left.typeArguments.filter(
    (argument) => !Type.isHiddenExecutableArgument(argument),
  )
  const rightVisible = right.typeArguments.filter(
    (argument) => !Type.isHiddenExecutableArgument(argument),
  )
  return (
    leftVisible.length === rightVisible.length &&
    leftVisible.every((argument, ordinal) => {
      const expected = rightVisible.at(ordinal)
      return (
        expected !== undefined &&
        Type.runtimeGenericArgumentKey(argument) === Type.runtimeGenericArgumentKey(expected)
      )
    })
  )
}

const effectInstanceByIdentity = (
  discovery: Instances.Discovery,
  identity: string,
): Instances.EffectInstance | undefined => {
  const exact = discovery.effects.filter((candidate) => candidate.identity === identity)
  if (exact.length === 1) return exact.at(0)
  const represented = discovery.effects.filter(
    (candidate) => candidate.representationIdentity === identity,
  )
  return represented.length === 1 ? represented.at(0) : undefined
}

/** Resolves one exact represented Effect environment without arbitrary owner fallback. */
export const effectEnvironmentByIdentity = (
  environments: ReadonlyArray<EffectEnvironment>,
  identity: Type.EffectIdentityArgument,
): Extract<EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> | undefined => {
  const available = environments.filter(
    (candidate): candidate is Extract<EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> =>
      candidate._tag === 'EffectEnvironment',
  )
  const concrete = available.filter(
    (candidate) =>
      Instances.effectIdentity(candidate.instance, candidate.site) === identity.identity ||
      candidate.successEffectIdentity === identity.identity,
  )
  if (concrete.length === 1) return concrete.at(0)
  const represented = available.filter(
    (candidate) => Hir.effectRepresentationIdentity(candidate.site) === identity.identity,
  )
  const owner = identity.owner
  if (owner === undefined) return represented.length === 1 ? represented.at(0) : undefined
  const exact = represented.filter((candidate) => sameExactOwner(candidate.instance, owner))
  if (exact.length === 1) return exact.at(0)
  const visible = represented.filter((candidate) => sameVisibleOwner(candidate.instance, owner))
  return visible.length === 1 ? visible.at(0) : undefined
}

export interface EffectEnvironmentField extends PlacedField {
  readonly source: 'Binding' | 'Parameter' | 'Pattern'
  readonly ordinal: number
  readonly access: Type.CaptureAccess
  readonly type: DeclarationFacts.SemanticType
  readonly representation: 'Value' | 'Borrow' | 'Callable'
  readonly effectIdentity?: string
  readonly callableIdentity?: Type.CallableIdentityArgument
  readonly providedRequirement?: NonNullable<
    FieldRealization.EffectEnvironmentSlot['providedRequirement']
  >
}

/** One realized Effect slot after target placement inside its enclosing nominal field. */
export interface StoredEffectEnvironmentField extends EffectEnvironmentField {
  readonly capture: number
}

/** Target-owned storage and call-scoped view for one concrete callable section identity. */
export type CallableEnvironment =
  | {
      readonly _tag: 'CallableEnvironment'
      readonly callable: Instances.CallableInstance
      readonly fields: ReadonlyArray<CallableEnvironmentField>
      readonly size: number
      readonly alignment: number
      readonly tailPadding: number
      readonly view: CallableView
    }
  | {
      readonly _tag: 'UnavailableCallableEnvironment'
      readonly callable: Instances.CallableInstance
      readonly reason: string
      readonly view: CallableView
    }

export interface CallableEnvironmentField extends PlacedField {
  readonly ordinal: number
  readonly parameterOrdinal: number
  readonly access: Type.CaptureAccess
  readonly type: DeclarationFacts.SemanticType
  readonly representation: 'Value' | 'Borrow' | 'Callable'
  readonly callableIdentity?: Type.CallableIdentityArgument
}

/** The ephemeral target-local pair passed at indirect callable application. */
export interface CallableView {
  readonly codeOffset: 0
  readonly environmentOffset: number
  readonly size: number
  readonly alignment: number
  readonly pointerBits: 32 | 64
}

/** A target-owned verdict for one reachable exact contextual `usize` or `isize` literal. */
export type WordLiteralVerdict =
  | {
      readonly _tag: 'AvailableWordLiteral'
      readonly type: WordType
      readonly value: bigint
      readonly bits: 32 | 64
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'UnavailableWordLiteral'
      readonly type: WordType
      readonly value: bigint
      readonly bits: 32 | 64
      readonly span: SourceSpan.SourceSpan
      readonly cause: Diagnostic.Identity
    }

export type { AddressScalar, CallingLane, CallingScalar, CallingShape, CallingShapeNode, Selector }

/** One member-specific lane transfer between two failure payload carriers. */
export interface FailurePayloadLane {
  readonly sourceOrdinal: number
  readonly source: CallingLane
  readonly member: CallingLane
  readonly targetOrdinal: number
  readonly target: CallingLane
}

/** The exact lanes occupied by one failure member while it moves between carrier rows. */
export interface FailurePayloadRepacking {
  readonly member: Type.Type
  readonly targetPayloadLanes: ReadonlyArray<CallingLane>
  readonly lanes: ReadonlyArray<FailurePayloadLane>
}

/** One deterministic explanation of malformed layout facts. */
export interface Violation {
  readonly _tag: 'LayoutViolation'
  readonly rule:
    | 'NonCanonicalTarget'
    | 'DuplicateType'
    | 'NonCanonicalOrder'
    | 'InvalidScalar'
    | 'InvalidAggregate'
    | 'InvalidCLayout'
    | 'InvalidCallingShape'
    | 'InvalidLiteralVerdict'
    | 'CatalogMismatch'
  readonly type?: DeclarationFacts.SemanticType
  readonly detail: string
}

export const scalarEntry = (target: Target.Target, type: Type.Builtin): Entry => {
  const scalar = Scalar.find(type)
  if (scalar === undefined) throw new RangeError(`Layout lost scalar catalog entry for ${type}`)
  const layout = Scalar.resolveLayout(scalar, target.pointerSize, target.pointerAlignment)
  const bits = Scalar.bits(scalar, target.pointerSize === 4 ? 32 : 64)
  let representation: Representation
  if (scalar.category === 'Boolean') {
    representation = Object.freeze({ _tag: 'Boolean', bits: 32, falseValue: 0, trueValue: 1 })
  } else if (scalar.category === 'Floating') {
    representation = Object.freeze({ _tag: 'Floating', bits: bits as 32 | 64, ieee: true })
  } else if (scalar.signedness === 'Signed') {
    representation = Object.freeze({ _tag: 'SignedInteger', bits })
  } else {
    representation = Object.freeze({ _tag: 'UnsignedInteger', bits })
  }
  return Object.freeze({
    _tag: 'LayoutEntry',
    type,
    copy: true,
    size: layout.size,
    alignment: layout.alignment,
    representation,
  })
}

const repeatedEntry = (type: Type.FixedArray, element: Entry): Entry | undefined => {
  const stride = alignUp(element.size, element.alignment)
  const size = stride * type.length
  if (!Number.isSafeInteger(stride) || !Number.isSafeInteger(size)) return undefined
  return Object.freeze({
    _tag: 'LayoutEntry',
    type,
    copy: element.copy,
    size,
    alignment: element.alignment,
    representation: Object.freeze({
      _tag: 'Repeated',
      element: type.element,
      length: type.length,
      stride,
    }),
  })
}

export const sliceEntry = (target: Target.Target, type: Type.Slice, element: Entry): Entry => {
  const addressBits: 32 | 64 = target.pointerSize === 4 ? 32 : 64
  const lengthOffset = alignUp(target.pointerSize, target.pointerAlignment)
  const alignment = target.pointerAlignment
  const contentSize = lengthOffset + target.pointerSize
  const size = alignUp(contentSize, alignment)
  return Object.freeze({
    _tag: 'LayoutEntry',
    type,
    copy: type.access === 'Shared',
    size,
    alignment,
    representation: Object.freeze({
      _tag: 'Slice',
      element: type.element,
      address: Object.freeze({
        bits: addressBits,
        offset: 0,
        size: target.pointerSize,
        alignment: target.pointerAlignment,
      }),
      length: Object.freeze({ type: 'usize', offset: lengthOffset, size: target.pointerSize }),
      addressPadding: lengthOffset - target.pointerSize,
      tailPadding: size - contentSize,
      stride: alignUp(element.size, element.alignment),
    }),
  })
}

export const stringEntry = (target: Target.Target, type: Type.String): Entry => {
  const addressBits: 32 | 64 = target.pointerSize === 4 ? 32 : 64
  const byteLengthOffset = alignUp(target.pointerSize, target.pointerAlignment)
  const alignment = target.pointerAlignment
  const contentSize = byteLengthOffset + target.pointerSize
  const size = alignUp(contentSize, alignment)
  return Object.freeze({
    _tag: 'LayoutEntry',
    type,
    copy: true,
    size,
    alignment,
    representation: Object.freeze({
      _tag: 'String',
      storage: Object.freeze({
        provenance: 'Utf8',
        bits: addressBits,
        offset: 0,
        size: target.pointerSize,
        alignment: target.pointerAlignment,
      }),
      byteLength: Object.freeze({
        type: 'usize',
        offset: byteLengthOffset,
        size: target.pointerSize,
      }),
      storagePadding: byteLengthOffset - target.pointerSize,
      tailPadding: size - contentSize,
    }),
  })
}

export const referenceEntry = (target: Target.Target, type: Type.Reference): Entry =>
  Object.freeze({
    _tag: 'LayoutEntry',
    type,
    copy: type.access === 'Shared',
    size: target.pointerSize,
    alignment: target.pointerAlignment,
    representation: Object.freeze({
      _tag: 'Reference',
      target: type.target,
      address: Object.freeze({
        bits: target.pointerSize === 4 ? 32 : 64,
        offset: 0,
        size: target.pointerSize,
        alignment: target.pointerAlignment,
      }),
    }),
  })

/** A raw pointer is one Copy address lane; the pointee's layout is never embedded. */
export const pointerEntry = (target: Target.Target, type: Type.Pointer): Entry =>
  Object.freeze({
    _tag: 'LayoutEntry',
    type,
    copy: true,
    size: target.pointerSize,
    alignment: target.pointerAlignment,
    representation: Object.freeze({
      _tag: 'Reference',
      target: type.pointee,
      address: Object.freeze({
        bits: target.pointerSize === 4 ? 32 : 64,
        offset: 0,
        size: target.pointerSize,
        alignment: target.pointerAlignment,
      }),
    }),
  })

/** A C function pointer is one Copy address lane with no embedded pointee layout. */
export const foreignFunctionEntry = (target: Target.Target, type: Type.ForeignFunction): Entry =>
  Object.freeze({
    _tag: 'LayoutEntry',
    type,
    copy: true,
    size: target.pointerSize,
    alignment: target.pointerAlignment,
    representation: Object.freeze({
      _tag: 'Reference',
      target: type,
      address: Object.freeze({
        bits: target.pointerSize === 4 ? 32 : 64,
        offset: 0,
        size: target.pointerSize,
        alignment: target.pointerAlignment,
      }),
    }),
  })

export const unionEntry = (type: Type.StructuralUnion, members: ReadonlyArray<Entry>): Entry => {
  const payloadAlignment = members.reduce(
    (maximum, member) => Math.max(maximum, member.alignment),
    1,
  )
  const payloadSize = members.reduce((maximum, member) => Math.max(maximum, member.size), 0)
  const payloadOffset = alignUp(4, payloadAlignment)
  const alignment = Math.max(4, payloadAlignment)
  const contentSize = payloadOffset + payloadSize
  const size = alignUp(contentSize, alignment)
  return Object.freeze({
    _tag: 'LayoutEntry',
    type,
    copy: members.every((member) => member.copy),
    size,
    alignment,
    representation: Object.freeze({
      _tag: 'Union',
      tag: Object.freeze({ bits: 32, size: 4 }),
      members: Object.freeze(
        type.members.map((member, ordinal) => {
          const layout = members.at(ordinal)
          return Object.freeze({
            type: member,
            ordinal,
            size: layout?.size ?? 0,
            alignment: layout?.alignment ?? 1,
          })
        }),
      ),
      payloadOffset,
      payloadSize,
      payloadAlignment,
      tagPadding: payloadOffset - 4,
      tailPadding: size - contentSize,
    }),
  })
}

// `never` has no values or calling lanes, but generic aggregates still need a compositional
// physical fact for impossible fields such as `Failure<never>`. This entry is never materialized
// as a value; it only lets the enclosing representation remain well-defined.
export const neverEntry = (): Entry =>
  Object.freeze({
    _tag: 'LayoutEntry',
    type: 'never',
    copy: true,
    size: 0,
    alignment: 1,
    representation: Object.freeze({
      _tag: 'Aggregate',
      fields: Object.freeze([]),
      tailPadding: 0,
    }),
  })

const nominalOf = (
  declaration: DeclarationFacts.StructFact | DeclarationFacts.UnionFact | DeclarationFacts.EnumFact,
): Type.Nominal | undefined =>
  declaration.canonical._tag === 'Canonical'
    ? Type.nominal(declaration.canonical.id.module, declaration.canonical.id.name)
    : undefined

export const scalarEnumEntry = (
  target: Target.Target,
  declaration: DeclarationFacts.EnumFact,
): Entry | undefined => {
  if (
    declaration.canonical._tag !== 'Canonical' ||
    declaration.validity._tag !== 'Valid' ||
    declaration.representation._tag !== 'Available'
  )
    return undefined
  const scalar = declaration.representation.scalar
  const layout = Scalar.resolveLayout(scalar, target.pointerSize, target.pointerAlignment)
  const members = declaration.members.flatMap((member) =>
    member.canonical._tag === 'Canonical' && member.discriminant._tag === 'Available'
      ? [
          Object.freeze({
            member: member.canonical.id,
            discriminant: member.discriminant.value,
          }),
        ]
      : [],
  )
  if (members.length !== declaration.members.length) return undefined
  return Object.freeze({
    _tag: 'LayoutEntry',
    type: Type.nominal(declaration.canonical.id.module, declaration.canonical.id.name),
    copy: true,
    size: layout.size,
    alignment: layout.alignment,
    representation: Object.freeze({
      _tag: 'ScalarEnum',
      enum: declaration.canonical.id,
      scalar: scalar.spelling,
      bits: Scalar.bits(scalar, target.pointerSize === 4 ? 32 : 64),
      signedness: scalar.signedness,
      members: Object.freeze(members),
    }),
  })
}

const dependenciesOf = (
  aggregate: DeclarationFacts.StructFact | DeclarationFacts.UnionFact,
  substitution: Type.Substitution = new Map(),
): ReadonlyArray<Type.Nominal> => {
  const dependencies = new Map<string, Type.Nominal>()
  const fields =
    aggregate._tag === 'StructDeclaration'
      ? aggregate.fields
      : aggregate.variants.flatMap((variant) => variant.fields)
  for (const field of fields) {
    let types: ReadonlyArray<Type.Nominal> = []
    if (field.declaredType._tag === 'Resolved') {
      types = Type.nominals(Type.substitute(field.declaredType.type, substitution))
    } else if (
      field.declaredType._tag === 'Unresolved' &&
      field.declaredType.candidate !== undefined
    ) {
      types = [field.declaredType.candidate]
    }
    for (const type of types) dependencies.set(Type.runtimeKey(type), type)
  }
  return Object.freeze([...dependencies.values()].sort(compareRuntimeTypes))
}

const unavailable = (
  type: DeclarationFacts.SemanticType,
  dependencies: ReadonlyArray<Type.Nominal>,
  reason: UnavailableReason,
  cause?: Diagnostic.Identity,
): UnavailableEntry =>
  Object.freeze({
    _tag: 'UnavailableLayoutEntry',
    type,
    dependencies,
    reason: Object.freeze(reason),
    ...(cause === undefined ? {} : { cause }),
  })

/** Computes every canonical nominal layout before runtime reachability or backend work. */
export const catalog = (
  target: Target.Target,
  index: DeclarationIndex.Index,
  discovery?: Instances.Discovery,
  opaqueRealizations?: OpaqueRealization.Catalog,
): Catalog => {
  const declarations = index.modules
    .flatMap((module) => module.structs)
    .flatMap((struct) => {
      const type = nominalOf(struct)
      return type === undefined ? [] : [Object.freeze({ struct, type })]
    })
    .sort((left, right) => compareRuntimeTypes(left.type, right.type))
  const generatedDeclarations = [...index.generatedAggregates.values()]
    .flatMap((struct) => {
      const type = nominalOf(struct)
      return type === undefined ? [] : [Object.freeze({ struct, type })]
    })
    .sort((left, right) => compareRuntimeTypes(left.type, right.type))
  const enumDeclarations = index.modules
    .flatMap((module) => module.enums)
    .flatMap((enum_) => {
      const type = nominalOf(enum_)
      return type === undefined ? [] : [Object.freeze({ enum_, type })]
    })
    .sort((left, right) => compareRuntimeTypes(left.type, right.type))
  const unionDeclarations = index.modules
    .flatMap((module) => module.unions)
    .flatMap((union) => {
      const type = nominalOf(union)
      return type === undefined ? [] : [Object.freeze({ union, type })]
    })
    .sort((left, right) => compareRuntimeTypes(left.type, right.type))
  const byType = new Map(
    [...declarations, ...generatedDeclarations].map((declaration) => [
      `${declaration.type.module}\u0000${declaration.type.name}`,
      declaration,
    ]),
  )
  const unionByType = new Map(
    unionDeclarations.map((declaration) => [
      `${declaration.type.module}\u0000${declaration.type.name}`,
      declaration,
    ]),
  )
  const completed = new Map<string, CatalogEntry>()
  for (const declaration of enumDeclarations) {
    const entry = scalarEnumEntry(target, declaration.enum_)
    let cause: Diagnostic.Identity | undefined
    if (declaration.enum_.validity._tag === 'Invalid') {
      cause = declaration.enum_.validity.causes.at(0)
    } else if (declaration.enum_.representation._tag === 'Unavailable') {
      cause = declaration.enum_.representation.cause
    }
    completed.set(
      Type.runtimeKey(declaration.type),
      entry ??
        unavailable(
          declaration.type,
          Object.freeze([]),
          {
            _tag: 'InvalidDeclaration',
            detail: `scalar enum ${Type.encode(declaration.type)} has no valid fixed-width representation plan`,
          },
          cause,
        ),
    )
  }
  const visiting = new Set<string>()
  const callableRealizations =
    discovery === undefined
      ? undefined
      : InstanceDiagnostics.callableFieldRealizations(discovery, index)

  interface InlineEnvironmentLayout {
    readonly fields: ReadonlyArray<StoredEffectEnvironmentField>
    readonly copy: boolean
    readonly size: number
    readonly alignment: number
    readonly tailPadding: number
  }

  const layoutEffectSlots = (
    slots: ReadonlyArray<FieldRealization.EffectEnvironmentSlot>,
    active: ReadonlySet<string>,
  ): InlineEnvironmentLayout | undefined => {
    let copy = true
    const fieldInputs: Array<
      Packing.Input<Omit<StoredEffectEnvironmentField, keyof Packing.PlacedField>>
    > = []
    for (const slot of slots) {
      const nestedEffect =
        slot.effectIdentity === undefined || discovery === undefined
          ? undefined
          : effectInstanceByIdentity(discovery, slot.effectIdentity)
      const callableIdentity = slot.callableIdentity
      const nestedCallable =
        callableIdentity === undefined
          ? undefined
          : discovery?.callables.find((candidate) =>
              FieldRealization.matchesIdentity(callableIdentity, candidate),
            )
      const borrowed =
        borrowedCapture(slot.access, slot.type) &&
        nestedEffect === undefined &&
        nestedCallable === undefined
      let nestedLayout:
        | { readonly size: number; readonly alignment: number; readonly copy: boolean }
        | undefined
      if (nestedEffect !== undefined) {
        if (active.has(nestedEffect.identity)) return undefined
        nestedLayout = layoutEffectSlots(
          FieldRealization.effectEnvironmentOf(nestedEffect),
          new Set([...active, nestedEffect.identity]),
        )
      } else if (nestedCallable !== undefined) {
        let callableCopy = true
        const captureInputs: Array<Packing.Input<undefined>> = []
        for (const capture of nestedCallable.captures) {
          const captureBorrowed = borrowedCapture(capture.access, capture.type)
          const captureLayout = captureBorrowed ? undefined : layoutType(capture.type)
          if (captureLayout?._tag === 'UnavailableLayoutEntry') return undefined
          const size = captureBorrowed ? target.pointerSize : (captureLayout?.size ?? 0)
          const alignment = captureBorrowed
            ? target.pointerAlignment
            : (captureLayout?.alignment ?? 1)
          captureInputs.push({ value: undefined, size, alignment })
          callableCopy =
            callableCopy &&
            capture.access !== 'Exclusive' &&
            (capture.access === 'Copy' ||
              capture.access === 'Shared' ||
              captureLayout?.copy === true)
        }
        const packed = Packing.pack(captureInputs)
        nestedLayout = Object.freeze({
          size: packed.size,
          alignment: packed.alignment,
          copy: callableCopy,
        })
      } else if (!borrowed) {
        const candidate = layoutType(slot.type)
        if (candidate._tag === 'UnavailableLayoutEntry') return undefined
        nestedLayout = candidate
      }
      const size = borrowed ? target.pointerSize : (nestedLayout?.size ?? 0)
      const alignment = borrowed ? target.pointerAlignment : (nestedLayout?.alignment ?? 1)
      copy =
        copy &&
        slot.access !== 'Exclusive' &&
        (slot.access === 'Copy' ||
          (slot.access === 'Shared' && borrowed) ||
          nestedLayout?.copy === true)
      let representation: 'Borrow' | 'Callable' | 'Value' = 'Value'
      if (borrowed) representation = 'Borrow'
      else if (nestedCallable !== undefined) representation = 'Callable'
      fieldInputs.push({
        value: Object.freeze({
          capture: slot.ordinal,
          source: slot.source,
          ordinal: slot.sourceOrdinal,
          access: slot.access,
          type: nestedEffect?.type ?? slot.type,
          representation,
          ...(slot.effectIdentity === undefined ? {} : { effectIdentity: slot.effectIdentity }),
          ...(slot.callableIdentity === undefined
            ? {}
            : { callableIdentity: slot.callableIdentity }),
          ...(slot.providedRequirement === undefined
            ? {}
            : { providedRequirement: slot.providedRequirement }),
        }),
        size,
        alignment,
      })
    }
    const packed = Packing.pack(fieldInputs)
    return Object.freeze({
      fields: Object.freeze(
        packed.fields.map(({ value, offset, size, alignment, padding }) =>
          Object.freeze({ ...value, offset, size, alignment, padding }),
        ),
      ),
      copy,
      size: packed.size,
      alignment: packed.alignment,
      tailPadding: packed.tailPadding,
    })
  }

  const layoutRepresentedCallable = (
    type: Type.Represented,
    realization: FieldRealization.CallableRealization,
  ): CatalogEntry => {
    const key = Type.runtimeKey(type)
    const existing = completed.get(key)
    if (existing !== undefined) return existing
    let copy = true
    const inputs: Array<Packing.Input<Omit<CallableEnvironmentField, keyof Packing.PlacedField>>> =
      []
    for (const capture of realization.captures) {
      const borrowed = borrowedCapture(capture.access, capture.type)
      const valueLayout = borrowed ? undefined : layoutType(capture.type)
      if (valueLayout?._tag === 'UnavailableLayoutEntry') {
        const result = unavailable(
          type,
          Object.freeze(Type.nominals(capture.type)),
          { _tag: 'UnavailableDependency', dependency: capture.type },
          valueLayout?.cause,
        )
        completed.set(key, result)
        return result
      }
      const size = borrowed ? target.pointerSize : (valueLayout?.size ?? 0)
      const alignment = borrowed ? target.pointerAlignment : (valueLayout?.alignment ?? 1)
      copy =
        copy &&
        capture.access !== 'Exclusive' &&
        (capture.access === 'Copy' || capture.access === 'Shared' || valueLayout?.copy === true)
      inputs.push(
        Object.freeze({
          value: Object.freeze({
            ordinal: capture.ordinal,
            parameterOrdinal: capture.parameterOrdinal,
            access: capture.access,
            type: capture.type,
            representation: borrowed ? ('Borrow' as const) : ('Value' as const),
          }),
          size,
          alignment,
        }),
      )
    }
    const packed = Packing.pack(inputs)
    const fields = packed.fields.map(({ value, ...placement }) =>
      Object.freeze({ ...value, ...placement }),
    )
    const result: Entry = Object.freeze({
      _tag: 'LayoutEntry',
      type,
      copy,
      size: packed.size,
      alignment: packed.alignment,
      representation: Object.freeze({
        _tag: 'CallableEnvironment',
        realization,
        fields: Object.freeze(fields),
        tailPadding: packed.tailPadding,
      }),
    })
    completed.set(key, result)
    return result
  }

  const layoutRepresentedEffect = (
    type: Type.Represented,
    realization: FieldRealization.EffectRealization,
  ): CatalogEntry => {
    const key = Type.runtimeKey(type)
    const existing = completed.get(key)
    if (existing !== undefined) return existing
    const environment = layoutEffectSlots(
      realization.environment,
      new Set([realization.runnerIdentity]),
    )
    if (environment === undefined) {
      const result = unavailable(type, Object.freeze(Type.nominals(type)), {
        _tag: 'InvalidDeclaration',
        detail: 'stored Effect environment has an unavailable or recursive capture layout',
      })
      completed.set(key, result)
      return result
    }
    const result: Entry = Object.freeze({
      _tag: 'LayoutEntry',
      type,
      copy: environment.copy,
      size: environment.size,
      alignment: environment.alignment,
      representation: Object.freeze({
        _tag: 'StoredEffectEnvironment',
        realization,
        fields: environment.fields,
        tailPadding: environment.tailPadding,
      }),
    })
    completed.set(key, result)
    return result
  }

  const layoutNominal = (type: Type.Nominal): CatalogEntry => {
    const key = Type.runtimeKey(type)
    const existing = completed.get(key)
    if (existing !== undefined) return existing
    if (Type.isSharedCore(type) || Type.isExecution(type) || Type.isWake(type)) {
      const result: Entry = Object.freeze({
        _tag: 'LayoutEntry',
        type,
        copy: false,
        size: target.pointerSize,
        alignment: target.pointerAlignment,
        representation: Object.freeze({
          _tag: 'Reference',
          target: type,
          address: Object.freeze({
            bits: target.pointerSize === 4 ? 32 : 64,
            offset: 0,
            size: target.pointerSize,
            alignment: target.pointerAlignment,
          }),
        }),
      })
      completed.set(key, result)
      return result
    }
    if (Type.isIntrinsicNominal(type) || Type.equals(type, Type.unit)) {
      const ordinal = Type.equals(type, Type.unit)
        ? Type.intrinsicNominals.size
        : Type.intrinsicNominalOrdinal(type)
      const structId: DeclarationFacts.DeclarationId = Object.freeze({
        _tag: 'DeclarationId',
        sourceId: type.module,
        ordinal,
      })
      let fieldTypes: ReadonlyArray<readonly [string, Type.Type]> = Object.freeze([])
      if (Type.equals(type, Type.layout)) {
        fieldTypes = Object.freeze([
          Object.freeze(['bytes', 'usize'] as const),
          Object.freeze(['alignment', 'usize'] as const),
        ])
      } else if (Type.equals(type, Type.invalidAlignment)) {
        fieldTypes = Object.freeze([Object.freeze(['alignment', 'usize'] as const)])
      } else if (Type.equals(type, Type.allocation)) {
        fieldTypes = Object.freeze([
          Object.freeze(['$base', 'usize'] as const),
          Object.freeze(['$bytes', 'usize'] as const),
          Object.freeze(['$alignment', 'usize'] as const),
          Object.freeze(['$reclaim', 'usize'] as const),
          Object.freeze(['$context', 'usize'] as const),
          Object.freeze(['$active', 'usize'] as const),
        ])
      } else if (Type.equals(type, Type.osHandle)) {
        fieldTypes = Object.freeze([
          Object.freeze(['$identity', 'usize'] as const),
          Object.freeze(['$kind', 'i32'] as const),
          Object.freeze(['$active', 'i32'] as const),
        ])
      } else if (Type.isRawBuffer(type)) {
        fieldTypes = Object.freeze([
          Object.freeze(['$allocation', Type.allocation] as const),
          Object.freeze(['count', 'usize'] as const),
        ])
      } else if (Type.isSlot(type)) {
        fieldTypes = Object.freeze([Object.freeze(['$address', 'usize'] as const)])
      }
      const inputs: Array<Packing.Input<Omit<Field, keyof Packing.PlacedField>>> = []
      for (const [fieldOrdinal, [name, fieldType]] of fieldTypes.entries()) {
        let fieldLayout: CatalogEntry | undefined
        if (Type.isBuiltin(fieldType)) {
          fieldLayout = scalarEntry(target, fieldType)
        } else if (Type.isNominal(fieldType)) {
          fieldLayout = layoutNominal(fieldType)
        }
        if (fieldLayout === undefined || fieldLayout._tag === 'UnavailableLayoutEntry') {
          const result = unavailable(
            type,
            Object.freeze(Type.nominals(fieldType)),
            { _tag: 'UnavailableDependency', dependency: fieldType },
            fieldLayout?.cause,
          )
          completed.set(key, result)
          return result
        }
        inputs.push(
          Object.freeze({
            value: Object.freeze({
              _tag: 'LayoutField' as const,
              id: Object.freeze({
                _tag: 'FieldId' as const,
                owner: Object.freeze({
                  _tag: 'StructFieldOwnerId' as const,
                  declaration: structId,
                }),
                ordinal: fieldOrdinal,
              }),
              name,
              type: fieldType,
            }),
            size: fieldLayout.size,
            alignment: fieldLayout.alignment,
          }),
        )
      }
      const packed = Packing.pack(inputs)
      const fields = packed.fields.map(({ value, ...placement }) =>
        Object.freeze({ ...value, ...placement }),
      )
      const entry: Entry = Object.freeze({
        _tag: 'LayoutEntry',
        type,
        copy: Type.equals(type, Type.unit),
        size: packed.size,
        alignment: packed.alignment,
        representation: Object.freeze({
          _tag: 'Aggregate',
          fields: Object.freeze(fields),
          tailPadding: packed.tailPadding,
        }),
      })
      completed.set(key, entry)
      return entry
    }
    const layoutAggregateField = (
      fieldType: DeclarationFacts.SemanticType,
      fieldId: DeclarationFacts.FieldId,
    ): CatalogEntry => {
      const representationPlans = RepresentationField.plansOf(index, type).filter((plan) =>
        RepresentationField.belongsTo(plan.id, fieldId),
      )
      let representationOrdinal = 0
      const visit = (candidate: DeclarationFacts.SemanticType): CatalogEntry => {
        if (Type.isRepresented(candidate)) {
          const plan = representationPlans.at(representationOrdinal)
          representationOrdinal += 1
          const realization =
            plan === undefined || callableRealizations === undefined
              ? undefined
              : FieldRealization.realizationOf(callableRealizations, type, plan.id)
          if (realization === undefined) {
            return unavailable(candidate, Object.freeze(Type.nominals(candidate)), {
              _tag: 'InvalidDeclaration',
              detail: 'represented executable values remain unavailable to layout',
            })
          }
          return FieldRealization.isCallableRealization(realization)
            ? layoutRepresentedCallable(candidate, realization)
            : layoutRepresentedEffect(candidate, realization)
        }
        if (Type.isFixedArray(candidate)) {
          const element = visit(candidate.element)
          if (element._tag === 'UnavailableLayoutEntry') return element
          return (
            repeatedEntry(candidate, element) ??
            unavailable(candidate, Object.freeze(Type.nominals(candidate.element)), {
              _tag: 'InvalidDeclaration',
              detail: `array layout overflows for ${Type.encode(candidate)}`,
            })
          )
        }
        if (Type.isSlice(candidate)) {
          const element = visit(candidate.element)
          return element._tag === 'UnavailableLayoutEntry'
            ? element
            : sliceEntry(target, candidate, element)
        }
        return layoutType(candidate)
      }
      return visit(fieldType)
    }
    const unionDeclaration = unionByType.get(`${type.module}\u0000${type.name}`)
    if (unionDeclaration !== undefined) {
      const union = unionDeclaration.union
      if (union.canonical._tag !== 'Canonical') {
        const result = unavailable(type, Object.freeze([]), {
          _tag: 'InvalidDeclaration',
          detail: `canonical identity is unavailable for ${Type.encode(type)}`,
        })
        completed.set(key, result)
        return result
      }
      const parameters = union.typeParameters.map((parameter) => parameter.type)
      const substitution = TypeInference.substitution(parameters, type.arguments)
      const dependencies =
        substitution === undefined ? Object.freeze([]) : dependenciesOf(union, substitution)
      if (substitution === undefined) {
        return unavailable(type, dependencies, {
          _tag: 'InvalidDeclaration',
          detail: `${Type.encode(type)} has ${type.arguments.length} type arguments; expected ${parameters.length}`,
        })
      }
      if (visiting.has(key)) {
        const result = unavailable(type, dependencies, {
          _tag: 'InvalidDeclaration',
          detail: `recursive dependency for ${Type.encode(type)} was not rejected during declaration analysis`,
        })
        completed.set(key, result)
        return result
      }
      if (union.validity._tag !== 'Valid' || union.dependency._tag === 'Unavailable') {
        let cause: Diagnostic.Identity | undefined
        if (union.validity._tag === 'Invalid') cause = union.validity.causes.at(0)
        else if (union.dependency._tag === 'Unavailable') cause = union.dependency.cause
        const result = unavailable(
          type,
          dependencies,
          { _tag: 'InvalidDeclaration', detail: `declaration dependencies are unavailable` },
          cause,
        )
        completed.set(key, result)
        return result
      }

      visiting.add(key)
      let fieldsCopy = true
      let failure: UnavailableEntry | undefined
      const variants: Array<
        Extract<Representation, { readonly _tag: 'NominalUnion' }>['variants'][number]
      > = []
      for (const variant of union.variants) {
        if (variant.canonical._tag !== 'Canonical') {
          failure = unavailable(type, dependencies, {
            _tag: 'InvalidDeclaration',
            detail: `variant identity is unavailable for ${Type.encode(type)}`,
          })
          break
        }
        const inputs: Array<Packing.Input<Omit<Field, keyof Packing.PlacedField>>> = []
        for (const field of variant.fields) {
          if (
            field.state._tag !== 'Unique' ||
            field.name._tag !== 'Present' ||
            field.declaredType._tag !== 'Resolved' ||
            field.declaredType.exposureCause !== undefined
          ) {
            let cause: Diagnostic.Identity | undefined
            if (field.state._tag === 'Duplicate') cause = field.state.cause
            else if (field.declaredType._tag === 'Unresolved') {
              cause = field.declaredType.cause
            } else if (field.declaredType._tag === 'Resolved') {
              cause = field.declaredType.exposureCause
            }
            failure = unavailable(
              type,
              dependencies,
              { _tag: 'UnavailableField', field: field.id, detail: 'field is unavailable' },
              cause,
            )
            break
          }
          const fieldType = Type.substitute(field.declaredType.type, substitution)
          const fieldLayout = layoutAggregateField(fieldType, field.id)
          if (fieldLayout._tag === 'UnavailableLayoutEntry') {
            failure = unavailable(
              type,
              dependencies,
              { _tag: 'UnavailableDependency', dependency: fieldType },
              fieldLayout.cause,
            )
            break
          }
          fieldsCopy = fieldsCopy && fieldLayout.copy
          inputs.push(
            Object.freeze({
              value: Object.freeze({
                _tag: 'LayoutField' as const,
                id: field.id,
                name: field.name.spelling,
                type: fieldType,
              }),
              size: fieldLayout.size,
              alignment: fieldLayout.alignment,
            }),
          )
        }
        if (failure !== undefined) break
        const packed = Packing.pack(inputs)
        variants.push(
          Object.freeze({
            variant: variant.canonical.id,
            ordinal: variant.id.ordinal,
            fields: Object.freeze(
              packed.fields.map(({ value, ...placement }) =>
                Object.freeze({ ...value, ...placement }),
              ),
            ),
            size: packed.size,
            alignment: packed.alignment,
            tailPadding: packed.tailPadding,
          }),
        )
      }
      visiting.delete(key)
      if (failure !== undefined) {
        completed.set(key, failure)
        return failure
      }
      const callingEntries = new Map(
        [...completed].flatMap(([entryKey, candidate]) =>
          candidate._tag === 'LayoutEntry' ? [[entryKey, candidate] as const] : [],
        ),
      )
      const callingContext = Object.freeze({
        target,
        entries: callingEntries,
        effectEnvironments: Object.freeze([]),
        callableEnvironments: Object.freeze([]),
        active: new Set<string>(),
      })
      const variantShapes = variants.map((variant): CallingShapeNode => {
        const fields = Object.freeze(
          variant.fields.map((field) =>
            Object.freeze({ field: field.id, shape: shapeNode(field.type, callingContext) }),
          ),
        )
        return Object.freeze({
          _tag: 'ProductShape',
          type,
          fields,
          laneCount: fields.reduce((total, field) => total + field.shape.laneCount, 0),
        })
      })
      const payloadTypes = unifyPayloadTypes(variantShapes, target)
      const payload = Packing.pack(
        payloadTypes.map((payloadType) => {
          const scalar = scalarEntry(target, payloadType)
          return Object.freeze({
            value: payloadType,
            size: scalar.size,
            alignment: scalar.alignment,
          })
        }),
      )
      const payloadAlignment = payload.alignment
      const payloadSize = payload.size
      const payloadOffset = alignUp(4, payloadAlignment)
      const alignment = Math.max(4, payloadAlignment)
      const size = alignUp(payloadOffset + payloadSize, alignment)
      const cleanup = CleanupPlan.cleanupPlan(index, type)
      const entry: Entry = Object.freeze({
        _tag: 'LayoutEntry',
        type,
        copy:
          ConformanceProof.hasCopyDeclaration(index, type) &&
          fieldsCopy &&
          cleanup._tag !== 'HookCleanup',
        size,
        alignment,
        representation: Object.freeze({
          _tag: 'NominalUnion',
          union: union.canonical.id,
          tag: Object.freeze({ bits: 32, size: 4 }),
          variants: Object.freeze(variants),
          payloadOffset,
          payloadSize,
          payloadAlignment,
          tagPadding: payloadOffset - 4,
          tailPadding: size - (payloadOffset + payloadSize),
          ...(cleanup._tag === 'HookCleanup'
            ? {
                cleanupHook: Object.freeze({
                  hook: cleanup.hook,
                  typeArguments: cleanup.typeArguments,
                }),
              }
            : {}),
        }),
      })
      completed.set(key, entry)
      return entry
    }
    const declaration = byType.get(`${type.module}\u0000${type.name}`)
    if (declaration === undefined) {
      return unavailable(type, Object.freeze([]), {
        _tag: 'InvalidDeclaration',
        detail: `missing canonical declaration for ${Type.encode(type)}`,
      })
    }
    const parameters = declaration.struct.typeParameters.map((parameter) => parameter.type)
    const substitution = TypeInference.substitution(parameters, type.arguments)
    if (substitution === undefined) {
      return unavailable(type, Object.freeze([]), {
        _tag: 'InvalidDeclaration',
        detail: `${Type.encode(type)} has ${type.arguments.length} type arguments; expected ${parameters.length}`,
      })
    }
    const dependencies = dependenciesOf(declaration.struct, substitution)
    if (visiting.has(key)) {
      const result = unavailable(type, dependencies, {
        _tag: 'InvalidDeclaration',
        detail: `recursive dependency for ${Type.encode(type)} was not rejected during declaration analysis`,
      })
      completed.set(key, result)
      return result
    }
    if (declaration.struct.dependency._tag === 'Unavailable') {
      const result = unavailable(
        type,
        dependencies,
        { _tag: 'InvalidDeclaration', detail: `declaration dependencies are unavailable` },
        declaration.struct.dependency.cause,
      )
      completed.set(key, result)
      return result
    }

    visiting.add(key)
    const inputs: Array<Packing.Input<Omit<Field, keyof Packing.PlacedField>>> = []
    let fieldsCopy = true
    let failure: UnavailableEntry | undefined
    for (const field of declaration.struct.fields) {
      if (
        field.state._tag !== 'Unique' ||
        (field.name._tag !== 'Present' && field.member._tag !== 'OrdinalAggregateMember')
      ) {
        failure = unavailable(
          type,
          dependencies,
          {
            _tag: 'UnavailableField',
            field: field.id,
            detail: 'field identity is unavailable',
          },
          field.state._tag === 'Duplicate' ? field.state.cause : undefined,
        )
        break
      }
      if (
        field.declaredType._tag !== 'Resolved' ||
        field.declaredType.exposureCause !== undefined
      ) {
        let cause: Diagnostic.Identity | undefined
        if (field.declaredType._tag === 'Unresolved') cause = field.declaredType.cause
        else if (field.declaredType._tag === 'Resolved') cause = field.declaredType.exposureCause
        failure = unavailable(
          type,
          dependencies,
          {
            _tag: 'UnavailableField',
            field: field.id,
            detail: 'field type is unavailable',
          },
          cause,
        )
        break
      }
      const fieldType = Type.substitute(field.declaredType.type, substitution)
      const fieldLayout = layoutAggregateField(fieldType, field.id)
      if (fieldLayout._tag === 'UnavailableLayoutEntry') {
        failure = unavailable(
          type,
          dependencies,
          { _tag: 'UnavailableDependency', dependency: fieldType },
          fieldLayout.cause,
        )
        break
      }
      fieldsCopy = fieldsCopy && fieldLayout.copy
      let fieldName = ''
      if (field.name._tag === 'Present') fieldName = field.name.spelling
      else if (field.member._tag === 'OrdinalAggregateMember') fieldName = `${field.member.ordinal}`
      inputs.push(
        Object.freeze({
          value: Object.freeze({
            _tag: 'LayoutField' as const,
            id: field.id,
            name: fieldName,
            type: fieldType,
          }),
          size: fieldLayout.size,
          alignment: fieldLayout.alignment,
        }),
      )
    }
    visiting.delete(key)
    if (failure !== undefined) {
      completed.set(key, failure)
      return failure
    }
    const packed = Packing.pack(inputs)
    const fields = packed.fields.map(({ value, ...placement }) =>
      Object.freeze({ ...value, ...placement }),
    )
    const cleanup = CleanupPlan.cleanupPlan(index, type)
    const entry: Entry = Object.freeze({
      _tag: 'LayoutEntry',
      type,
      copy:
        ConformanceProof.hasCopyDeclaration(index, type) &&
        fieldsCopy &&
        cleanup._tag !== 'HookCleanup',
      size: packed.size,
      alignment: packed.alignment,
      representation: Object.freeze({
        _tag: 'Aggregate',
        fields: Object.freeze(fields),
        tailPadding: packed.tailPadding,
        ...(cleanup._tag === 'HookCleanup'
          ? {
              cleanupHook: Object.freeze({
                hook: cleanup.hook,
                typeArguments: cleanup.typeArguments,
              }),
            }
          : {}),
      }),
    })
    completed.set(key, entry)
    return entry
  }

  const layoutDirectRepresented = (
    type: Type.Represented,
    active = new Set<string>(),
  ): CatalogEntry => {
    const typeKey = Type.runtimeKey(type)
    const existing = completed.get(typeKey)
    if (existing?._tag === 'LayoutEntry') return existing
    if (active.has(typeKey))
      return unavailable(type, Object.freeze(Type.nominals(type)), {
        _tag: 'InvalidDeclaration',
        detail: 'recursive executable union representation has no finite inline layout',
      })
    const next = new Set(active).add(typeKey)
    const argument = type.representation.argument
    if (Type.isOpaqueRepresentationArgument(argument)) {
      const definition =
        opaqueRealizations === undefined
          ? undefined
          : OpaqueRealization.definitionOf(opaqueRealizations, argument)
      const realization = definition?.realization
      if (realization === undefined)
        return unavailable(type, Object.freeze(Type.nominals(type)), {
          _tag: 'InvalidDeclaration',
          detail: 'opaque executable union member has no finite realization',
        })
      const realized = layoutDirectRepresented(
        Type.represented(type.contract, type.representation.requiredBound, realization),
        next,
      )
      if (realized._tag === 'UnavailableLayoutEntry') return realized
      const result: Entry = Object.freeze({ ...realized, type })
      completed.set(typeKey, result)
      return result
    }
    if (Type.isCompositeEffectRepresentationArgument(argument)) {
      const alternatives = argument.alternatives.map((alternative) =>
        layoutDirectRepresented(
          Type.represented(type.contract, type.representation.requiredBound, alternative),
          next,
        ),
      )
      const unavailableAlternative = alternatives.find(
        (alternative): alternative is UnavailableEntry =>
          alternative._tag === 'UnavailableLayoutEntry',
      )
      if (unavailableAlternative !== undefined) return unavailableAlternative
      const entries = alternatives.flatMap((alternative) =>
        alternative._tag === 'LayoutEntry' ? [alternative] : [],
      )
      const payloadAlignment = entries.reduce(
        (maximum, alternative) => Math.max(maximum, alternative.alignment),
        1,
      )
      const payloadSize = entries.reduce(
        (maximum, alternative) => Math.max(maximum, alternative.size),
        0,
      )
      const payloadOffset = alignUp(4, payloadAlignment)
      const alignment = Math.max(4, payloadAlignment)
      const size = alignUp(payloadOffset + payloadSize, alignment)
      const result: Entry = Object.freeze({
        _tag: 'LayoutEntry',
        type,
        copy: entries.every((entry) => entry.copy),
        size,
        alignment,
        representation: Object.freeze({
          _tag: 'Aggregate',
          fields: Object.freeze([]),
          tailPadding: size,
        }),
      })
      completed.set(typeKey, result)
      return result
    }
    if (!Type.isExactRepresentationArgument(argument))
      return unavailable(type, Object.freeze(Type.nominals(type)), {
        _tag: 'InvalidDeclaration',
        detail: 'open executable union member has no finite realization',
      })
    if (Type.isCallable(type.contract) && Type.isCallableIdentityArgument(argument.identity)) {
      const identity = argument.identity
      const callable =
        identity.environment === undefined
          ? undefined
          : discovery?.callables.find((candidate) =>
              FieldRealization.matchesIdentity(identity, candidate),
            )
      if (identity.environment !== undefined && callable === undefined)
        return unavailable(type, Object.freeze(Type.nominals(type)), {
          _tag: 'InvalidDeclaration',
          detail: 'callable union member has no finite environment',
        })
      let copy = true
      const fieldInputs = (callable?.captures ?? []).flatMap((capture) => {
        const borrowed = borrowedCapture(capture.access, capture.type)
        const valueLayout = borrowed ? undefined : layoutType(capture.type)
        if (valueLayout?._tag === 'UnavailableLayoutEntry') return []
        const fieldSize = borrowed ? target.pointerSize : (valueLayout?.size ?? 0)
        const fieldAlignment = borrowed ? target.pointerAlignment : (valueLayout?.alignment ?? 1)
        copy =
          copy &&
          capture.access !== 'Exclusive' &&
          (capture.access === 'Copy' || capture.access === 'Shared' || valueLayout?.copy === true)
        return [
          Object.freeze({
            value: Object.freeze({
              capture: capture.ordinal,
              type: capture.type,
              access: capture.access,
              representation: borrowed ? ('Borrow' as const) : ('Value' as const),
            }),
            size: fieldSize,
            alignment: fieldAlignment,
          }),
        ]
      })
      if ((callable?.captures.length ?? 0) !== fieldInputs.length)
        return unavailable(type, Object.freeze(Type.nominals(type)), {
          _tag: 'InvalidDeclaration',
          detail: 'callable union member captures a value without finite layout',
        })
      const packed = Packing.pack(fieldInputs)
      const fields = packed.fields.map(({ value, offset, size, alignment, padding }) =>
        Object.freeze({ ...value, offset, size, alignment, padding }),
      )
      const result: Entry = Object.freeze({
        _tag: 'LayoutEntry',
        type,
        copy,
        size: packed.size,
        alignment: packed.alignment,
        representation: Object.freeze({
          _tag: 'Aggregate',
          fields: Object.freeze([]),
          tailPadding: packed.size,
        }),
        executable: Object.freeze({
          _tag: 'Callable',
          fields: Object.freeze(fields),
        }),
      })
      completed.set(typeKey, result)
      return result
    }
    if (Type.isEffect(type.contract) && Type.isEffectIdentityArgument(argument.identity)) {
      const identity = argument.identity
      const effect =
        discovery === undefined ? undefined : Instances.representedEffectOf(discovery, identity)
      const environment =
        effect === undefined
          ? undefined
          : layoutEffectSlots(
              FieldRealization.effectEnvironmentOf(effect),
              new Set([effect.identity]),
            )
      if (environment === undefined)
        return unavailable(type, Object.freeze(Type.nominals(type)), {
          _tag: 'InvalidDeclaration',
          detail: 'Effect union member has no finite environment',
        })
      const result: Entry = Object.freeze({
        _tag: 'LayoutEntry',
        type,
        copy: environment.copy,
        size: environment.size,
        alignment: environment.alignment,
        representation: Object.freeze({
          _tag: 'Aggregate',
          fields: Object.freeze([]),
          tailPadding: environment.size,
        }),
        executable: Object.freeze({
          _tag: 'Effect',
          fields: Object.freeze(
            environment.fields.map((field) =>
              Object.freeze({
                capture: field.capture,
                type: field.type,
                access: field.access,
                representation: field.representation,
                offset: field.offset,
                size: field.size,
                alignment: field.alignment,
                padding: field.padding,
                ...(field.effectIdentity === undefined
                  ? {}
                  : { effectIdentity: field.effectIdentity }),
                ...(field.callableIdentity === undefined
                  ? {}
                  : { callableIdentity: field.callableIdentity }),
              }),
            ),
          ),
        }),
      })
      completed.set(typeKey, result)
      return result
    }
    return unavailable(type, Object.freeze(Type.nominals(type)), {
      _tag: 'InvalidDeclaration',
      detail: 'executable union member representation does not match its contract',
    })
  }

  const layoutType = (type: DeclarationFacts.SemanticType): CatalogEntry => {
    if (Type.isBuiltin(type)) return scalarEntry(target, type)
    if (Type.isString(type)) {
      const result = stringEntry(target, type)
      completed.set(Type.runtimeKey(type), result)
      return result
    }
    if (Type.isNever(type)) {
      const result = neverEntry()
      completed.set(Type.runtimeKey(type), result)
      return result
    }
    if (Type.isParameter(type)) {
      return unavailable(type, Object.freeze([]), {
        _tag: 'InvalidDeclaration',
        detail: `open generic parameter ${Type.encode(type)} has no target layout`,
      })
    }
    if (Type.isNominal(type)) return layoutNominal(type)
    if (Type.isSlice(type)) {
      const key = Type.runtimeKey(type)
      const existing = completed.get(key)
      if (existing !== undefined) return existing
      const element = layoutType(type.element)
      if (element._tag === 'UnavailableLayoutEntry') {
        const result = unavailable(
          type,
          Object.freeze(Type.nominals(type.element)),
          { _tag: 'UnavailableDependency', dependency: type.element },
          element.cause,
        )
        completed.set(key, result)
        return result
      }
      const result = sliceEntry(target, type, element)
      completed.set(key, result)
      return result
    }
    if (Type.isReference(type)) {
      const result = referenceEntry(target, type)
      completed.set(Type.runtimeKey(type), result)
      return result
    }
    if (Type.isPointer(type)) {
      const result = pointerEntry(target, type)
      completed.set(Type.runtimeKey(type), result)
      return result
    }
    if (Type.isForeignFunction(type)) {
      const result = foreignFunctionEntry(target, type)
      completed.set(Type.runtimeKey(type), result)
      return result
    }
    const key = Type.runtimeKey(type)
    const existing = completed.get(key)
    if (existing !== undefined) return existing
    if (Type.isUnion(type)) {
      const members: Array<Entry> = []
      for (const member of type.members) {
        const memberLayout = layoutType(member)
        if (memberLayout._tag === 'UnavailableLayoutEntry') {
          const result = unavailable(
            type,
            Object.freeze(type.members.flatMap(Type.nominals)),
            { _tag: 'UnavailableDependency', dependency: member },
            memberLayout.cause,
          )
          completed.set(key, result)
          return result
        }
        members.push(memberLayout)
      }
      const result = unionEntry(type, Object.freeze(members))
      completed.set(key, result)
      return result
    }
    if (Type.isEffect(type)) {
      const result = unavailable(type, Object.freeze(Type.nominals(type)), {
        _tag: 'InvalidDeclaration',
        detail: 'compiler-private effect values have no target layout',
      })
      completed.set(key, result)
      return result
    }
    if (Type.isCallable(type)) {
      const result = unavailable(type, Object.freeze(Type.nominals(type)), {
        _tag: 'InvalidDeclaration',
        detail: 'callable environment layout is planned from its hidden concrete identity',
      })
      completed.set(key, result)
      return result
    }
    if (Type.isRepresented(type)) {
      return layoutDirectRepresented(type)
    }
    const element = layoutType(type.element)
    const dependencies = Object.freeze(Type.nominals(type.element))
    if (element._tag === 'UnavailableLayoutEntry') {
      const result = unavailable(
        type,
        dependencies,
        { _tag: 'UnavailableDependency', dependency: type.element },
        element.cause,
      )
      completed.set(key, result)
      return result
    }
    const entry = repeatedEntry(type, element)
    if (entry === undefined) {
      const result = unavailable(type, dependencies, {
        _tag: 'InvalidDeclaration',
        detail: `array layout overflows for ${Type.encode(type)}`,
      })
      completed.set(key, result)
      return result
    }
    completed.set(key, entry)
    return entry
  }

  const referenced = new Map<string, DeclarationFacts.SemanticType>()
  const addReferenced = (type: DeclarationFacts.SemanticType): void => {
    if (!Type.isRuntimeConcrete(type)) return
    const key = Type.runtimeKey(type)
    if (referenced.has(key)) return
    referenced.set(key, type)
    if (Type.isNominal(type)) {
      for (const argument of type.arguments)
        if (Type.isTypeArgument(argument)) addReferenced(argument)
    }
    if (Type.isFixedArray(type)) addReferenced(type.element)
    if (Type.isSlice(type)) addReferenced(type.element)
    else if (Type.isReference(type)) addReferenced(type.target)
    else if (Type.isPointer(type)) addReferenced(type.pointee)
    if (Type.isUnion(type)) for (const member of type.members) addReferenced(member)
    if (Type.isEffect(type)) {
      addReferenced(type.success)
      for (const failure of Type.failureMembers(type)) addReferenced(failure)
    }
    if (Type.isRepresented(type) && Type.isEffect(type.contract)) {
      addReferenced(type.contract.success)
      for (const failure of Type.failureMembers(type.contract)) addReferenced(failure)
    }
  }
  for (const module of index.modules) {
    for (const member of module.members) {
      if (member._tag === 'FunctionDeclaration') {
        for (const parameter of member.parameters) {
          if (parameter.declaredType._tag === 'Resolved') addReferenced(parameter.declaredType.type)
        }
        if (member.returnType._tag === 'Resolved') addReferenced(member.returnType.type)
      } else if (member._tag === 'StructDeclaration') {
        for (const field of member.fields) {
          if (field.declaredType._tag === 'Resolved') addReferenced(field.declaredType.type)
        }
      } else if (member._tag === 'UnionDeclaration') {
        for (const variant of member.variants) {
          for (const field of variant.fields) {
            if (field.declaredType._tag === 'Resolved') addReferenced(field.declaredType.type)
          }
        }
      } else if (member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration') {
        for (const operation of member.operations) {
          for (const parameter of operation.parameters)
            if (parameter.declaredType._tag === 'Resolved')
              addReferenced(parameter.declaredType.type)
          if (operation.returnType._tag === 'Resolved') addReferenced(operation.returnType.type)
        }
      } else if (
        (member._tag === 'ConstantDeclaration' || member._tag === 'ForeignStaticDeclaration') &&
        member.declaredType._tag === 'Resolved'
      ) {
        addReferenced(member.declaredType.type)
      }
    }
  }
  for (const declaration of declarations) {
    if (declaration.struct.typeParameters.length === 0) layoutNominal(declaration.type)
  }
  for (const declaration of unionDeclarations) {
    if (declaration.union.typeParameters.length === 0) layoutNominal(declaration.type)
  }
  for (const instance of discovery?.instances ?? []) {
    const substitution = instance.substitution
    if (instance.function.contract._tag === 'Contract') {
      for (const parameter of instance.specialization.parameters) addReferenced(parameter)
      addReferenced(instance.specialization.result)
      for (const failure of RowAlgebra.concreteMembers(
        Type.failureRowPolicy(),
        instance.specialization.failureRow ?? RowAlgebra.concrete(Type.failureRowPolicy(), []),
      ))
        addReferenced(failure)
      for (const requirement of RowAlgebra.concreteMembers(
        Type.requirementRowPolicy(),
        instance.specialization.requirementRow ??
          RowAlgebra.concrete(Type.requirementRowPolicy(), []),
      ))
        addReferenced(requirement.capability)
    }
    const addSpecializedExpression = (expression: Hir.Expression): void => {
      if (expression._tag === 'Unavailable') return
      addReferenced(Type.substitute(expression.type, substitution))
      if (expression._tag === 'UnionConvert')
        addReferenced(Type.substitute(expression.sourceType, substitution))
      for (const child of Hir.expressionTree(expression).slice(1)) {
        if (child._tag !== 'Unavailable') addReferenced(Type.substitute(child.type, substitution))
      }
      for (const child of Hir.expressionTree(expression)) {
        if (child._tag === 'BuiltinCall') {
          if (Scalar.isCheckedOperation(child.operation)) addReferenced('bool')
          for (const argument of child.typeArguments) {
            const specialized = Type.substituteGenericArgument(argument, substitution)
            if (Type.isTypeArgument(specialized)) addReferenced(specialized)
            else {
              const represented = Type.representedType(specialized)
              if (represented !== undefined) addReferenced(represented)
            }
          }
        }
        if (child._tag === 'EffectCatch' && child.protected._tag !== 'Unavailable') {
          const protected_ = Type.substitute(child.protected.type, substitution)
          if (Type.isEffect(protected_)) {
            addReferenced('bool')
            addReferenced(protected_.success)
            addReferenced(Type.failureValue(Type.failureMembers(protected_)))
          }
        }
      }
    }
    const addPatternStatementTypes = (statement: Hir.Statement): void => {
      if (statement._tag === 'PatternBind' || statement._tag === 'IfLet') {
        addReferenced('bool')
        for (const member of statement.selection.members)
          addReferenced(Type.substitute(Match.sourceType(member), substitution))
        for (const binding of statement.selection.bindings)
          addReferenced(Type.substitute(binding.type, substitution))
      }
      if (statement._tag === 'Unsafe')
        for (const nested of statement.statements) addPatternStatementTypes(nested)
      if (statement._tag === 'If' || statement._tag === 'IfLet') {
        for (const nested of statement.taken) addPatternStatementTypes(nested)
        for (const nested of statement.otherwise) addPatternStatementTypes(nested)
      }
      if (statement._tag === 'While')
        for (const nested of statement.body) addPatternStatementTypes(nested)
    }
    for (const statement of instance.function.statements) {
      for (const expression of Hir.statementExpressions(statement))
        addSpecializedExpression(expression)
      addPatternStatementTypes(statement)
    }
  }
  for (const effect of discovery?.effects ?? []) addReferenced(effect.type)
  let completedSize = -1
  let referencedSize = -1
  while (completedSize !== completed.size || referencedSize !== referenced.size) {
    completedSize = completed.size
    referencedSize = referenced.size
    for (const entry of completed.values()) addReferenced(entry.type)
    for (const type of referenced.values()) {
      if (!Type.isBuiltin(type)) layoutType(type)
    }
  }

  return Object.freeze({
    _tag: 'LayoutCatalog',
    target,
    entries: Object.freeze(
      [...completed.values()].sort((left, right) => compareRuntimeTypes(left.type, right.type)),
    ),
    wordConstants: Object.freeze(
      index.modules.flatMap((module) =>
        module.constants.flatMap((constant) => {
          if (constant.declaredType._tag !== 'Resolved' || !isWordType(constant.declaredType.type))
            return []
          const literal = constant.literal
          if (literal._tag !== 'IntegerLiteral') return []
          return [
            Object.freeze({
              type: constant.declaredType.type,
              value: literal.value,
              span: literal.token.span,
            }),
          ]
        }),
      ),
    ),
  })
}

const addExpressionTypes = (
  types: Map<string, DeclarationFacts.SemanticType>,
  expression: Hir.Expression,
  substitution: Type.Substitution = new Map(),
): void => {
  if (expression._tag === 'Unavailable') return
  const specialized = Type.substitute(expression.type, substitution)
  types.set(Type.runtimeKey(specialized), specialized)
  if (expression._tag === 'BuiltinCall') {
    if (Scalar.isCheckedOperation(expression.operation)) types.set(Type.runtimeKey('bool'), 'bool')
    for (const argument of expression.typeArguments) {
      const specialized = Type.substituteGenericArgument(argument, substitution)
      const type = Type.isTypeArgument(specialized)
        ? specialized
        : Type.representedType(specialized)
      if (type !== undefined) types.set(Type.runtimeKey(type), type)
    }
  }
  if (expression._tag === 'Move') addExpressionTypes(types, expression.subject, substitution)
  if (expression._tag === 'RuntimeStringView')
    addExpressionTypes(types, expression.source, substitution)
  if (expression._tag === 'ShortCircuit') {
    addExpressionTypes(types, expression.left, substitution)
    addExpressionTypes(types, expression.right, substitution)
  }
  if (expression._tag === 'StringEquality' || expression._tag === 'EnumEquality') {
    addExpressionTypes(types, expression.left, substitution)
    addExpressionTypes(types, expression.right, substitution)
  }
  if (expression._tag === 'EnumValue') addExpressionTypes(types, expression.value, substitution)
  if (expression._tag === 'UnionConvert') {
    const sourceType = Type.substitute(expression.sourceType, substitution)
    types.set(Type.runtimeKey(sourceType), sourceType)
    addExpressionTypes(types, expression.source, substitution)
  }
  if (expression._tag === 'Project') addExpressionTypes(types, expression.subject, substitution)
  if (expression._tag === 'IndexPlace') {
    addExpressionTypes(types, expression.subject, substitution)
    addExpressionTypes(types, expression.index, substitution)
  }
  if (expression._tag === 'SliceLength') {
    addExpressionTypes(types, expression.slice, substitution)
  }
  if (expression._tag === 'SliceIndexPlace') {
    addExpressionTypes(types, expression.slice, substitution)
    addExpressionTypes(types, expression.index, substitution)
  }
  if (
    (expression._tag === 'SliceBorrow' || expression._tag === 'ValueBorrow') &&
    expression.root._tag === 'TemporarySliceRoot'
  ) {
    addExpressionTypes(types, expression.root.value, substitution)
  }
  if (expression._tag === 'Construct' || expression._tag === 'ConstructUnionVariant') {
    for (const field of expression.fields) addExpressionTypes(types, field.value, substitution)
  }
  if (expression._tag === 'ArrayConstruct') {
    for (const element of expression.elements) addExpressionTypes(types, element, substitution)
  }
  if (
    expression._tag === 'Call' ||
    expression._tag === 'EffectConstruct' ||
    expression._tag === 'ServiceEffectConstruct' ||
    expression._tag === 'BuiltinCall' ||
    expression._tag === 'InterfaceOperationCall'
  ) {
    for (const argument of expression.arguments) addExpressionTypes(types, argument, substitution)
    let contract: DeclarationFacts.InterfaceOperationApplicationFact | undefined
    if (expression._tag === 'InterfaceOperationCall') {
      contract = expression.contract
    } else if (expression._tag === 'BuiltinCall') {
      contract = expression.interfaceOperation?.contract
    }
    for (const operand of contract?.operands ?? []) {
      if (operand.type._tag !== 'Resolved') continue
      const type = Type.substitute(operand.type.type, substitution)
      types.set(Type.runtimeKey(type), type)
    }
  }
  if (expression._tag === 'CallableSection') {
    for (const capture of expression.captures) {
      addExpressionTypes(types, capture.value, substitution)
    }
  }
  if (expression._tag === 'CallableApply') {
    addExpressionTypes(types, expression.callee, substitution)
    for (const argument of expression.arguments) addExpressionTypes(types, argument, substitution)
  }
  if (expression._tag === 'EffectBlock') {
    addStatementTypes(types, expression.statements, substitution)
  }
  if (expression._tag === 'Run') addExpressionTypes(types, expression.subject, substitution)
  if (expression._tag === 'EffectBindRequirement') {
    addExpressionTypes(types, expression.protected, substitution)
    const provider = Type.substitute(expression.provider.providerType, substitution)
    if (Type.isNominal(provider)) {
      types.set(Type.runtimeKey(provider), provider)
      const reference = Type.reference(
        expression.provider.selectionAccess === 'Take'
          ? 'Exclusive'
          : expression.provider.selectionAccess,
        provider,
        Type.substituteLifetime(expression.type.environment, substitution),
      )
      types.set(Type.runtimeKey(reference), reference)
    }
  }
  if (expression._tag === 'EffectCatch') {
    types.set(Type.runtimeKey('never'), 'never')
    types.set(Type.runtimeKey('bool'), 'bool')
    addExpressionTypes(types, expression.protected, substitution)
    addExpressionTypes(types, expression.handler, substitution)
    if (expression.protected._tag !== 'Unavailable') {
      const protected_ = Type.substitute(expression.protected.type, substitution)
      if (Type.isEffect(protected_)) {
        types.set(Type.runtimeKey(protected_.success), protected_.success)
        const failure = Type.failureValue(Type.failureMembers(protected_))
        types.set(Type.runtimeKey(failure), failure)
      }
    }
  }
  if (expression._tag === 'Match') {
    addExpressionTypes(types, expression.scrutinee, substitution)
    for (const member of expression.members) {
      const type = Type.substitute(Match.sourceType(member), substitution)
      types.set(Type.runtimeKey(type), type)
    }
    for (const arm of expression.arms) {
      if (!arm.reachable) continue
      if (arm.member !== undefined) {
        const memberType = Match.sourceType(arm.member)
        types.set(Type.runtimeKey(memberType), memberType)
      }
      for (const binding of arm.bindings) types.set(Type.runtimeKey(binding.type), binding.type)
      if (arm.guard !== undefined) addExpressionTypes(types, arm.guard, substitution)
      if (arm.body._tag === 'Expression')
        addExpressionTypes(types, arm.body.expression, substitution)
      else addStatementTypes(types, arm.body.statements, substitution)
    }
  }
}

const addStatementTypes = (
  types: Map<string, DeclarationFacts.SemanticType>,
  statements: ReadonlyArray<Hir.Statement>,
  substitution: Type.Substitution = new Map(),
): void => {
  for (const statement of statements) {
    if (statement._tag === 'Unsafe') addStatementTypes(types, statement.statements, substitution)
    if (statement._tag === 'Bind') addExpressionTypes(types, statement.initializer, substitution)
    if (statement._tag === 'PatternBind') {
      types.set(Type.runtimeKey('bool'), 'bool')
      addExpressionTypes(types, statement.selection.subject, substitution)
      for (const member of statement.selection.members) {
        const type = Type.substitute(Match.sourceType(member), substitution)
        types.set(Type.runtimeKey(type), type)
      }
      for (const binding of statement.selection.bindings) {
        const type = Type.substitute(binding.type, substitution)
        types.set(Type.runtimeKey(type), type)
      }
    }
    if (statement._tag === 'Evaluate') addExpressionTypes(types, statement.expression, substitution)
    if (statement._tag === 'Return') addExpressionTypes(types, statement.expression, substitution)
    if (statement._tag === 'Fail' || statement._tag === 'Drop')
      addExpressionTypes(types, statement.expression, substitution)
    if (statement._tag === 'If') {
      addExpressionTypes(types, statement.condition, substitution)
      addStatementTypes(types, statement.taken, substitution)
      addStatementTypes(types, statement.otherwise, substitution)
    }
    if (statement._tag === 'IfLet') {
      types.set(Type.runtimeKey('bool'), 'bool')
      addExpressionTypes(types, statement.selection.subject, substitution)
      for (const member of statement.selection.members) {
        const type = Type.substitute(Match.sourceType(member), substitution)
        types.set(Type.runtimeKey(type), type)
      }
      for (const binding of statement.selection.bindings) {
        const type = Type.substitute(binding.type, substitution)
        types.set(Type.runtimeKey(type), type)
      }
      addStatementTypes(types, statement.taken, substitution)
      addStatementTypes(types, statement.otherwise, substitution)
    }
    if (statement._tag === 'Write') {
      addExpressionTypes(types, statement.value, substitution)
      for (const selector of statement.place.selectors) {
        if (selector._tag === 'Index' || selector._tag === 'SliceIndex') {
          addExpressionTypes(types, selector.index, substitution)
        }
      }
    }
    if (statement._tag === 'While') {
      addExpressionTypes(types, statement.condition, substitution)
      addStatementTypes(types, statement.body, substitution)
    }
  }
}

const addFunctionTypes = (
  types: Map<string, DeclarationFacts.SemanticType>,
  instance: Instances.Instance,
): void => {
  const fn = instance.function
  const substitution = instance.substitution
  for (const parameter of fn.declaration.parameters) {
    if (parameter.declaredType._tag === 'Resolved') {
      const type = Type.substitute(parameter.declaredType.type, substitution)
      types.set(Type.runtimeKey(type), type)
    }
  }
  if (fn.declaration.returnType._tag === 'Resolved') {
    const type = Type.substitute(fn.declaration.returnType.type, substitution)
    types.set(Type.runtimeKey(type), type)
    if (fn.declaration.functionKind === 'Effect') {
      const failures = fn.declaration.failureRow.failures.flatMap((failure) => {
        const specialized = Type.substitute(failure, substitution)
        return Type.isNominal(specialized) ? [specialized] : []
      })
      const requirements = fn.declaration.requirementRow.requirements.flatMap((requirement) => {
        const capability = Type.substitute(requirement.capability, substitution)
        return Type.isNominal(capability) ? [Object.freeze({ ...requirement, capability })] : []
      })
      const outcome = Type.effect(
        type,
        failures,
        { ...DeclarationFacts.executableLifetimes(fn.declaration), lifetimeBinders: [] },
        'Shared',
        requirements,
      )
      types.set(Type.runtimeKey(outcome), outcome)
    }
  }
  addStatementTypes(types, fn.statements, substitution)
}

const effectEnvironments = (
  target: Target.Target,
  entries: ReadonlyArray<Entry>,
  discovery: Instances.Discovery,
  callablePlans: ReadonlyArray<CallableEnvironment>,
): ReadonlyArray<EffectEnvironment> => {
  const layouts = new Map(
    entries.map((candidate) => [Type.runtimeKey(candidate.type), candidate] as const),
  )
  const environments: Array<EffectEnvironment> = []
  type EffectFieldDraft = Omit<EffectEnvironmentField, keyof Packing.PlacedField>
  const placeEffectFields = (inputs: ReadonlyArray<Packing.Input<EffectFieldDraft>>) => {
    const packed = Packing.pack(inputs)
    return Object.freeze({
      ...packed,
      fields: Object.freeze(
        packed.fields.map(({ value, offset, size, alignment, padding }) =>
          Object.freeze({ ...value, offset, size, alignment, padding }),
        ),
      ),
    })
  }

  // Effect parameters capture concrete environments supplied elsewhere in the instance graph.
  // Resolve those dependencies to a fixed point: breadth-first discovery is deterministic but is
  // not a topological order once combinators both consume and produce Effects.
  for (let pass = 0; pass <= discovery.instances.length; pass += 1) {
    const availableBefore = new Set(
      environments.flatMap((environment) =>
        environment._tag === 'EffectEnvironment'
          ? [Instances.effectIdentity(environment.instance, environment.site)]
          : [],
      ),
    ).size
    for (const instance of [...discovery.instances].reverse()) {
      const bindingTypes = new Map<number, DeclarationFacts.SemanticType>()
      const patternTypes = new Map<string, DeclarationFacts.SemanticType>()
      const patternKey = (id: Match.BindingId): string =>
        `${id.arm.match.function.sourceId}:${id.arm.match.function.ordinal}:${id.arm.match.span.start}:${id.arm.ordinal}:${id.ordinal}`
      const collectPatterns = (bindings: ReadonlyArray<Hir.PatternBinding>): void => {
        for (const binding of bindings) patternTypes.set(patternKey(binding.id), binding.type)
      }
      const collectBindings = (statements: ReadonlyArray<Hir.Statement>): void => {
        for (const statement of statements) {
          if (statement._tag === 'PatternBind' || statement._tag === 'IfLet')
            collectPatterns(statement.selection.bindings)
          if (statement._tag === 'Bind' && statement.initializer._tag !== 'Unavailable') {
            bindingTypes.set(
              statement.binding.ordinal,
              Type.substitute(statement.initializer.type, instance.substitution),
            )
          } else if (statement._tag === 'If' || statement._tag === 'IfLet') {
            collectBindings(statement.taken)
            collectBindings(statement.otherwise)
          } else if (statement._tag === 'While') collectBindings(statement.body)
          else if (statement._tag === 'Unsafe') collectBindings(statement.statements)
          for (const expression of Hir.statementExpressions(statement)) {
            for (const child of Hir.expressionTree(expression)) {
              if (child._tag === 'EffectBlock') collectBindings(child.statements)
              if (child._tag === 'Match') {
                for (const arm of child.arms) {
                  collectPatterns(arm.bindings)
                  if (arm.body._tag === 'Block') collectBindings(arm.body.statements)
                }
              }
            }
          }
        }
      }
      collectBindings(instance.function.statements)

      const blocks = instance.function.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .filter(
          (expression): expression is Extract<Hir.Expression, { readonly _tag: 'EffectBlock' }> =>
            expression._tag === 'EffectBlock',
        )
      const catchSites = instance.function.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .flatMap((expression) =>
          expression._tag !== 'EffectCatch'
            ? []
            : [
                Object.freeze({
                  site: Hir.effectCatchSite(
                    instance.function.declaration.id,
                    instance.key.declaration,
                    expression.span,
                  ),
                  type: expression.type,
                  captures: Object.freeze([
                    Object.freeze({
                      access: 'Take' as const,
                      pattern: undefined,
                      binding: undefined,
                      parameter: undefined,
                    }),
                    Object.freeze({
                      access: 'Take' as const,
                      pattern: undefined,
                      binding: undefined,
                      parameter: undefined,
                    }),
                  ]),
                }),
              ],
        )
      const builtinSites = instance.function.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .flatMap((expression) => {
          if (expression._tag !== 'BuiltinCall' || expression.witnessEffectSite !== undefined)
            return []
          const type = Type.substitute(expression.type, instance.substitution)
          if (!Type.isEffect(type)) return []
          return [
            Object.freeze({
              site: Hir.builtinEffectSite(
                instance.function.declaration.id,
                instance.key.declaration,
                expression.span,
              ),
              type: expression.type,
              captures: Object.freeze(
                expression.arguments.map((argument) => {
                  const specialized =
                    argument._tag === 'Unavailable'
                      ? undefined
                      : Type.substitute(argument.type, instance.substitution)
                  let access: 'Copy' | 'Shared' | 'Exclusive' | 'Take' = 'Take'
                  if (
                    specialized !== undefined &&
                    (Type.isReference(specialized) || Type.isSlice(specialized))
                  ) {
                    access = specialized.access
                  } else if (specialized !== undefined && Type.isCallable(specialized)) {
                    access = specialized.mode
                  }
                  return Object.freeze({
                    access,
                    pattern: undefined,
                    binding: undefined,
                    parameter: undefined,
                  })
                }),
              ),
            }),
          ]
        })
      const effectSites = Object.freeze([
        ...blocks.map((block) =>
          Object.freeze({ site: block.site, type: block.type, captures: block.captures }),
        ),
        ...catchSites,
        ...builtinSites,
      ])
      for (const block of effectSites) {
        const structuralEffect = Type.substitute(block.type, instance.substitution)
        if (!Type.isEffect(structuralEffect)) continue
        const effectInstance = discovery.effects.find(
          (candidate) => candidate.identity === Instances.effectIdentity(instance.key, block.site),
        )
        const realizedSlots =
          effectInstance === undefined
            ? Object.freeze([])
            : FieldRealization.effectEnvironmentOf(effectInstance)
        let effect = structuralEffect
        let unavailable: string | undefined
        const fieldInputs: Array<Packing.Input<EffectFieldDraft>> = []
        for (const [captureOrdinal, capture] of block.captures.entries()) {
          const realized = realizedSlots.find((slot) => slot.ordinal === captureOrdinal)
          let source = realized?.source
          if (source === undefined) {
            if (capture.pattern !== undefined) source = 'Pattern'
            else source = capture.binding === undefined ? 'Parameter' : 'Binding'
          }
          const ordinal =
            realized?.sourceOrdinal ??
            capture.pattern?.ordinal ??
            capture.binding?.ordinal ??
            capture.parameter?.ordinal
          let type = realized?.type
          if (type === undefined && capture.pattern !== undefined) {
            type = patternTypes.get(patternKey(capture.pattern))
          } else if (type === undefined && capture.binding === undefined) {
            if (instance.function.contract._tag === 'Contract' && ordinal !== undefined) {
              type = instance.function.contract.parameters.at(ordinal)
            }
          } else if (type === undefined && ordinal !== undefined) {
            type = bindingTypes.get(ordinal)
          }
          if (ordinal === undefined || type === undefined) {
            unavailable = `capture ${source.toLowerCase()} has no concrete type`
            break
          }
          const specialized = realized?.type ?? Type.substitute(type, instance.substitution)
          const representedEffect =
            Type.isRepresented(specialized) &&
            Type.isEffect(specialized.contract) &&
            Type.isExactRepresentationArgument(specialized.representation.argument) &&
            Type.isEffectIdentityArgument(specialized.representation.argument.identity)
          const parameterEffectRepresentation =
            source === 'Parameter'
              ? Instances.parameterEffectRepresentationArgument(
                  instance.function,
                  instance.key,
                  ordinal,
                )
              : undefined
          const capturedEffectIdentity =
            realized?.effectIdentity ??
            ((Type.isEffect(specialized) || representedEffect) &&
            parameterEffectRepresentation !== undefined &&
            Type.isEffectIdentityArgument(parameterEffectRepresentation)
              ? parameterEffectRepresentation.identity
              : undefined)
          const capturedEffectInstance =
            capturedEffectIdentity === undefined
              ? undefined
              : effectInstanceByIdentity(discovery, capturedEffectIdentity)
          const capturedEffectEnvironment =
            capturedEffectIdentity === undefined
              ? undefined
              : environments.find(
                  (
                    candidate,
                  ): candidate is Extract<
                    EffectEnvironment,
                    { readonly _tag: 'EffectEnvironment' }
                  > =>
                    candidate._tag === 'EffectEnvironment' &&
                    (Instances.effectIdentity(candidate.instance, candidate.site) ===
                      capturedEffectIdentity ||
                      candidate.successEffectIdentity === capturedEffectIdentity ||
                      (capturedEffectInstance !== undefined &&
                        Instances.effectIdentity(candidate.instance, candidate.site) ===
                          capturedEffectInstance.identity)),
                )
          const capturedCompositeRepresentation =
            parameterEffectRepresentation !== undefined &&
            Type.isCompositeEffectRepresentationArgument(parameterEffectRepresentation)
              ? parameterEffectRepresentation
              : undefined
          const capturedCompositeEnvironments = capturedCompositeRepresentation?.alternatives.map(
            (alternative) =>
              Type.isEffectIdentityArgument(alternative.identity)
                ? effectEnvironmentByIdentity(environments, alternative.identity)
                : undefined,
          )
          const capturedCompositeLayout = capturedCompositeEnvironments?.every(
            (
              candidate,
            ): candidate is Extract<EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> =>
              candidate !== undefined,
          )
            ? (() => {
                const payloadAlignment = capturedCompositeEnvironments.reduce(
                  (maximum, candidate) => Math.max(maximum, candidate.alignment),
                  1,
                )
                const payloadSize = capturedCompositeEnvironments.reduce(
                  (maximum, candidate) => Math.max(maximum, candidate.size),
                  0,
                )
                const alignment = Math.max(4, payloadAlignment)
                const payloadOffset = alignUp(4, payloadAlignment)
                return Object.freeze({
                  size: alignUp(payloadOffset + payloadSize, alignment),
                  alignment,
                })
              })()
            : undefined
          const capturedCallableIdentity =
            realized?.callableIdentity ??
            (Type.isCallable(specialized) && source === 'Parameter'
              ? Instances.parameterCallableIdentity(instance.function, instance.key, ordinal)
              : undefined)
          const capturedCallableEnvironment =
            capturedCallableIdentity?.environment === undefined
              ? undefined
              : callablePlans.find(
                  (
                    candidate,
                  ): candidate is Extract<
                    CallableEnvironment,
                    { readonly _tag: 'CallableEnvironment' }
                  > =>
                    candidate._tag === 'CallableEnvironment' &&
                    FieldRealization.matchesIdentity(capturedCallableIdentity, candidate.callable),
                )
          const fieldType =
            capturedEffectEnvironment?.effect ??
            (capturedCompositeRepresentation === undefined
              ? undefined
              : Type.represented(
                  capturedCompositeRepresentation.contract,
                  capturedCompositeRepresentation.contract,
                  capturedCompositeRepresentation,
                )) ??
            (capturedCallableEnvironment === undefined
              ? undefined
              : Object.freeze({
                  ...capturedCallableEnvironment.callable.type,
                  mode: capturedCallableEnvironment.callable.mode,
                })) ??
            (capturedCallableIdentity !== undefined && Type.isCallable(specialized)
              ? Object.freeze({ ...specialized, mode: 'Shared' as const })
              : specialized)
          const access =
            capturedEffectEnvironment?.effect.access ??
            capturedCallableEnvironment?.callable.mode ??
            (capturedCallableIdentity === undefined ? capture.access : 'Shared')
          const callable = capturedCallableIdentity !== undefined
          const borrowed =
            borrowedCapture(access, fieldType) &&
            capturedEffectEnvironment === undefined &&
            !callable
          const valueLayout =
            borrowed || callable
              ? undefined
              : (capturedEffectEnvironment ??
                capturedCompositeLayout ??
                layouts.get(Type.runtimeKey(fieldType)))
          if (!borrowed && !callable && valueLayout === undefined) {
            unavailable = `capture ${source.toLowerCase()} ${ordinal} has no value layout`
            break
          }
          let size = valueLayout?.size ?? 0
          let alignment = valueLayout?.alignment ?? 1
          if (borrowed) {
            size = target.pointerSize
            alignment = target.pointerAlignment
          } else if (callable) {
            size = capturedCallableEnvironment?.size ?? 0
            alignment = capturedCallableEnvironment?.alignment ?? 1
          }
          let representation: 'Borrow' | 'Callable' | 'Value' = 'Value'
          if (borrowed) representation = 'Borrow'
          else if (callable) representation = 'Callable'
          fieldInputs.push({
            value: Object.freeze({
              source,
              ordinal,
              access,
              type: fieldType,
              representation,
              ...(capturedEffectIdentity === undefined
                ? {}
                : { effectIdentity: capturedEffectIdentity }),
              ...(capturedCallableIdentity === undefined
                ? {}
                : { callableIdentity: capturedCallableIdentity }),
              ...(realized?.providedRequirement === undefined
                ? {}
                : { providedRequirement: realized.providedRequirement }),
            }),
            size,
            alignment,
          })
        }
        if (unavailable === undefined) {
          let access: Type.CallableMode = 'Shared'
          if (fieldInputs.some((field) => field.value.access === 'Take')) {
            access = 'Take'
          } else if (fieldInputs.some((field) => field.value.access === 'Exclusive')) {
            access = 'Exclusive'
          }
          effect = Type.effectWithRows(
            structuralEffect.success,
            structuralEffect.failureRow,
            structuralEffect,
            access,
            structuralEffect.requirementRow,
          )
        }
        if (unavailable !== undefined) {
          environments.push(
            Object.freeze({
              _tag: 'UnavailableEffectEnvironment',
              instance: instance.key,
              site: block.site,
              effect,
              reason: unavailable,
            }),
          )
          continue
        }
        const packed = placeEffectFields(fieldInputs)
        const successEffectIdentity = (instance.effectSuccesses ?? []).find((success) =>
          Hir.sameExecutableSite(success.site, block.site),
        )?.identity
        environments.push(
          Object.freeze({
            _tag: 'EffectEnvironment',
            instance: instance.key,
            site: block.site,
            effect,
            ...(successEffectIdentity === undefined ? {} : { successEffectIdentity }),
            fields: packed.fields,
            size: packed.size,
            alignment: packed.alignment,
            tailPadding: packed.tailPadding,
          }),
        )
      }

      const witnessEffects = instance.function.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .flatMap((expression) => {
          if (expression._tag !== 'InterfaceOperationCall' && expression._tag !== 'BuiltinCall')
            return []
          if (expression.witnessEffectSite === undefined) return []
          let contract: DeclarationFacts.InterfaceOperationApplicationFact | undefined
          if (expression._tag === 'InterfaceOperationCall') {
            contract = expression.contract
          } else if (expression._tag === 'BuiltinCall') {
            contract = expression.interfaceOperation?.contract
          }
          return contract === undefined
            ? []
            : [Object.freeze({ expression, contract, site: expression.witnessEffectSite })]
        })
      for (const witness of witnessEffects) {
        const structuralEffect = Type.substitute(witness.expression.type, instance.substitution)
        if (!Type.isEffect(structuralEffect)) continue
        let unavailable: string | undefined
        const fieldInputs: Array<Packing.Input<EffectFieldDraft>> = []
        for (const [ordinal, operand] of witness.contract.operands.entries()) {
          if (operand.type._tag !== 'Resolved') {
            unavailable = `interface operand ${ordinal} has no concrete type`
            break
          }
          const fieldType = Type.substitute(operand.type.type, instance.substitution)
          const valueLayout = layouts.get(Type.runtimeKey(fieldType))
          if (valueLayout === undefined) {
            unavailable = `interface operand ${ordinal} has no value layout`
            break
          }
          const access =
            Type.isReference(fieldType) || Type.isSlice(fieldType) ? fieldType.access : 'Take'
          fieldInputs.push({
            value: Object.freeze({
              source: 'Parameter',
              ordinal,
              access,
              type: fieldType,
              representation: 'Value',
            }),
            size: valueLayout.size,
            alignment: valueLayout.alignment,
          })
        }
        let access: Type.CallableMode = 'Shared'
        if (fieldInputs.some((field) => field.value.access === 'Take')) {
          access = 'Take'
        } else if (fieldInputs.some((field) => field.value.access === 'Exclusive')) {
          access = 'Exclusive'
        }
        const effect = Type.effectWithRows(
          structuralEffect.success,
          structuralEffect.failureRow,
          structuralEffect,
          access,
          structuralEffect.requirementRow,
        )
        if (unavailable !== undefined) {
          environments.push(
            Object.freeze({
              _tag: 'UnavailableEffectEnvironment',
              instance: instance.key,
              site: witness.site,
              effect,
              reason: unavailable,
            }),
          )
          continue
        }
        const packed = placeEffectFields(fieldInputs)
        environments.push(
          Object.freeze({
            _tag: 'EffectEnvironment',
            instance: instance.key,
            site: witness.site,
            effect,
            fields: packed.fields,
            size: packed.size,
            alignment: packed.alignment,
            tailPadding: packed.tailPadding,
          }),
        )
      }
    }
    const availableAfter = new Set(
      environments.flatMap((environment) =>
        environment._tag === 'EffectEnvironment'
          ? [Instances.effectIdentity(environment.instance, environment.site)]
          : [],
      ),
    ).size
    if (availableAfter === availableBefore) break
  }

  const resolved = new Map<string, EffectEnvironment>()
  for (const environment of environments) {
    const identity = Instances.effectIdentity(environment.instance, environment.site)
    const previous = resolved.get(identity)
    if (previous === undefined || environment._tag === 'EffectEnvironment')
      resolved.set(identity, environment)
  }
  return Object.freeze(
    [...resolved.values()].sort(
      (left, right) =>
        left.instance.declaration.module.localeCompare(right.instance.declaration.module) ||
        left.instance.declaration.name.localeCompare(right.instance.declaration.name) ||
        Hir.compareExecutableSites(left.site, right.site),
    ),
  )
}

const callableView = (target: Target.Target): CallableView =>
  Object.freeze({
    codeOffset: 0,
    environmentOffset: target.pointerSize,
    size: target.pointerSize * 2,
    alignment: target.pointerAlignment,
    pointerBits: target.pointerSize === 4 ? 32 : 64,
  })

const callableEnvironments = (
  target: Target.Target,
  entries: ReadonlyArray<Entry>,
  discovery: Instances.Discovery,
): ReadonlyArray<CallableEnvironment> => {
  const layouts = new Map(entries.map((entry) => [Type.runtimeKey(entry.type), entry] as const))
  const view = callableView(target)
  const planned = new Map<Instances.CallableInstance, CallableEnvironment>()
  const planning = new Set<Instances.CallableInstance>()
  const plan = (callable: Instances.CallableInstance): CallableEnvironment => {
    const cached = planned.get(callable)
    if (cached !== undefined) return cached
    if (planning.has(callable)) {
      return Object.freeze({
        _tag: 'UnavailableCallableEnvironment',
        callable,
        reason: 'recursive callable capture environment has no finite value layout',
        view,
      })
    }
    planning.add(callable)
    const unavailable = (reason: string): CallableEnvironment => {
      const result = Object.freeze({
        _tag: 'UnavailableCallableEnvironment' as const,
        callable,
        reason,
        view,
      })
      planning.delete(callable)
      planned.set(callable, result)
      return result
    }
    const inputs: Array<
      Packing.Input<Omit<CallableEnvironmentField, 'offset' | 'size' | 'alignment' | 'padding'>>
    > = []
    for (const capture of callable.captures) {
      const callableCapture = capture.callableIdentity !== undefined
      const borrowed = !callableCapture && borrowedCapture(capture.access, capture.type)
      const callableIdentity = capture.callableIdentity
      const nestedCallable =
        callableIdentity?.environment === undefined
          ? undefined
          : discovery.callables.find((candidate) =>
              FieldRealization.matchesIdentity(callableIdentity, candidate),
            )
      const nestedEnvironment = nestedCallable === undefined ? undefined : plan(nestedCallable)
      if (
        callableIdentity?.environment !== undefined &&
        nestedEnvironment?._tag !== 'CallableEnvironment'
      ) {
        return unavailable(`capture ${capture.ordinal} has no concrete callable environment`)
      }
      const valueLayout =
        borrowed || callableCapture ? undefined : layouts.get(Type.runtimeKey(capture.type))
      if (!borrowed && !callableCapture && valueLayout === undefined) {
        return unavailable(`capture ${capture.ordinal} has no concrete value layout`)
      }
      let size = valueLayout?.size ?? 0
      let alignment = valueLayout?.alignment ?? 1
      if (borrowed) {
        size = target.pointerSize
        alignment = target.pointerAlignment
      } else if (callableCapture) {
        size = nestedEnvironment?._tag === 'CallableEnvironment' ? nestedEnvironment.size : 0
        alignment =
          nestedEnvironment?._tag === 'CallableEnvironment' ? nestedEnvironment.alignment : 1
      }
      let representation: 'Borrow' | 'Callable' | 'Value' = 'Value'
      if (borrowed) representation = 'Borrow'
      else if (callableCapture) representation = 'Callable'
      inputs.push(
        Object.freeze({
          value: Object.freeze({
            ordinal: capture.ordinal,
            parameterOrdinal: capture.parameterOrdinal,
            access: capture.access,
            type: capture.type,
            representation,
            ...(callableIdentity === undefined ? {} : { callableIdentity }),
          }),
          size,
          alignment,
        }),
      )
    }
    const packed = Packing.pack(inputs)
    const fields: ReadonlyArray<CallableEnvironmentField> = Object.freeze(
      packed.fields.map((field) => Object.freeze({ ...field.value, ...field })),
    )
    const result: CallableEnvironment = Object.freeze({
      _tag: 'CallableEnvironment',
      callable,
      fields,
      size: packed.size,
      alignment: packed.alignment,
      tailPadding: packed.tailPadding,
      view,
    })
    planning.delete(callable)
    planned.set(callable, result)
    return result
  }
  return Object.freeze(discovery.callables.map(plan))
}

const wordLiteralVerdicts = (
  target: Target.Target,
  discovery: Instances.Discovery,
  constants: ReadonlyArray<WordConstantLiteral>,
): {
  readonly verdicts: ReadonlyArray<WordLiteralVerdict>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const bits: 32 | 64 = target.pointerSize === 4 ? 32 : 64
  const verdicts: Array<WordLiteralVerdict> = []
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const seen = new Set<string>()
  const add = (type: WordType, value: bigint, span: SourceSpan.SourceSpan): void => {
    const key = `${span.sourceId}:${span.start}:${span.end}:${type}:${value}`
    if (seen.has(key)) return
    seen.add(key)
    const range = wordRange(type, bits)
    if (value >= range.minimum && value <= range.maximum) {
      verdicts.push(
        Object.freeze({
          _tag: 'AvailableWordLiteral',
          type,
          value,
          bits,
          span,
        }),
      )
      return
    }
    const diagnostic = Diagnostic.wordLiteralTargetOutOfRange(
      type,
      value.toString(),
      target.id,
      bits,
      span,
    )
    diagnostics.push(diagnostic)
    verdicts.push(
      Object.freeze({
        _tag: 'UnavailableWordLiteral',
        type,
        value,
        bits,
        span,
        cause: Diagnostic.identity(diagnostic),
      }),
    )
  }
  for (const constant of constants) add(constant.type, constant.value, constant.span)
  for (const instance of discovery.instances) {
    const expressions = instance.function.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)
    for (const expression of expressions) {
      if (expression._tag !== 'IntegerLiteral' || expression.constant !== undefined) continue
      const type = Type.substitute(expression.type, instance.substitution)
      if (!isWordType(type)) continue
      add(type, BigInt(expression.value), expression.span)
    }
  }
  return Object.freeze({
    verdicts: Object.freeze(verdicts),
    diagnostics: Object.freeze(diagnostics),
  })
}

/** Selects runtime-reachable entries while reusing nominal decisions from the catalog. */
export const plan = (
  self: Catalog,
  discovery: Instances.Discovery,
  index: DeclarationIndex.Index,
): Plan => {
  const reached = new Map<string, DeclarationFacts.SemanticType>()
  for (const instance of discovery.instances) addFunctionTypes(reached, instance)
  for (const effect of discovery.effects) reached.set(Type.runtimeKey(effect.type), effect.type)
  for (const instance of discovery.instances) {
    for (const expression of instance.function.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)) {
      if (expression._tag === 'EffectCatch') reached.set(Type.runtimeKey('bool'), 'bool')
      if (
        expression._tag !== 'BuiltinCall' ||
        (expression.operation !== 'ExecutionLayout' &&
          expression.operation !== 'ExecutionFromAllocation')
      )
        continue
      const arguments_ = expression.typeArguments.map((argument) =>
        Instances.concreteEffectRepresentationArgument(
          instance.function,
          instance.key,
          Type.substituteGenericArgument(argument, instance.substitution),
        ),
      )
      for (const argument of [arguments_.at(0), arguments_.at(2)])
        if (argument !== undefined && Type.isTypeArgument(argument))
          reached.set(Type.runtimeKey(argument), argument)
      for (const argument of [arguments_.at(1), arguments_.at(3)]) {
        const represented = argument === undefined ? undefined : Type.representedType(argument)
        if (represented !== undefined) reached.set(Type.runtimeKey(represented), represented)
      }
    }
  }
  if (
    discovery.entry._tag === 'Resolved' &&
    (discovery.entry.kind === 'Effect' || discovery.entry.result === 'Unit')
  ) {
    reached.set(Type.runtimeKey('i32'), 'i32')
  }
  for (const callable of discovery.callables) {
    for (const capture of callable.captures)
      reached.set(Type.runtimeKey(capture.type), capture.type)
  }
  const entries = new Map<string, Entry>()
  const resolve = (type: DeclarationFacts.SemanticType): Entry | undefined => {
    if (Type.isBuiltin(type)) return scalarEntry(self.target, type)
    if (Type.isString(type)) return stringEntry(self.target, type)
    if (Type.isNever(type)) return neverEntry()
    const candidate = catalogEntry(self, type)
    if (candidate?._tag === 'LayoutEntry') return candidate
    if (Type.isSlice(type)) {
      if (candidate?._tag === 'UnavailableLayoutEntry') return undefined
      const element = resolve(type.element)
      return element === undefined ? undefined : sliceEntry(self.target, type, element)
    }
    if (Type.isReference(type)) return referenceEntry(self.target, type)
    if (Type.isPointer(type)) return pointerEntry(self.target, type)
    if (!Type.isFixedArray(type) || candidate?._tag === 'UnavailableLayoutEntry') return undefined
    const element = resolve(type.element)
    return element === undefined ? undefined : repeatedEntry(type, element)
  }
  const add = (type: DeclarationFacts.SemanticType): void => {
    const key = Type.runtimeKey(type)
    if (Type.isEffect(type)) {
      add(type.success)
      for (const failure of Type.failureMembers(type)) add(failure)
      return
    }
    if (entries.has(key)) return
    const candidate = resolve(type)
    if (candidate === undefined) return
    entries.set(key, candidate)
    if (Type.isSharedCore(type)) {
      const element = Type.typeArgumentAt(type, 0)
      if (element !== undefined) add(element)
      // The control block embeds the allocation that backs it, so releasing a shared core that
      // nothing in the program ever made (a union alternative, an absent Option) still needs it.
      add(Type.allocation)
    }
    if (
      Type.isRepresented(type) &&
      Type.isCompositeEffectRepresentationArgument(type.representation.argument)
    ) {
      for (const alternative of type.representation.argument.alternatives)
        add(Type.represented(type.contract, type.representation.requiredBound, alternative))
    }
    for (const field of candidate.executable?.fields ?? []) add(field.type)
    if (candidate.representation._tag === 'Aggregate') {
      for (const field of candidate.representation.fields) add(field.type)
    } else if (candidate.representation._tag === 'NominalUnion') {
      for (const variant of candidate.representation.variants)
        for (const field of variant.fields) add(field.type)
    } else if (
      candidate.representation._tag === 'CallableEnvironment' ||
      candidate.representation._tag === 'StoredEffectEnvironment'
    ) {
      for (const field of candidate.representation.fields) add(field.type)
    } else if (candidate.representation._tag === 'Repeated') {
      add(candidate.representation.element)
    } else if (candidate.representation._tag === 'Slice') {
      add(candidate.representation.element)
      add('usize')
    } else if (candidate.representation._tag === 'String') {
      add('usize')
    } else if (candidate.representation._tag === 'Reference') {
      add(candidate.representation.target)
    } else if (candidate.representation._tag === 'Union') {
      for (const member of candidate.representation.members) add(member.type)
    }
  }
  for (const type of reached.values()) add(type)
  const orderedEntries = Object.freeze(
    [...entries.values()].sort((left, right) => compareRuntimeTypes(left.type, right.type)),
  )
  const literals = wordLiteralVerdicts(self.target, discovery, self.wordConstants)
  const localSharedAllocationProvenance = LocalSharedAllocationProvenance.plan(discovery, index)
  const localSharedDiagnostics: Array<Diagnostic.Diagnostic> = []
  for (const instance of discovery.instances) {
    for (const expression of instance.function.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)) {
      if (expression._tag !== 'BuiltinCall' || expression.operation !== 'SharedLayout') continue
      const raw = expression.typeArguments.at(0)
      const element =
        raw !== undefined && Type.isTypeArgument(raw)
          ? Type.substitute(raw, instance.substitution)
          : undefined
      const elementLayout = element === undefined ? undefined : resolve(element)
      if (
        element !== undefined &&
        elementLayout !== undefined &&
        LocalSharedControlBlock.plan(self.target, element, elementLayout)._tag ===
          'LocalSharedControlBlockUnavailable'
      )
        localSharedDiagnostics.push(
          Diagnostic.intrinsicTargetUnavailable(
            'Intrinsic.sharedLayout',
            self.target.id,
            expression.span,
          ),
        )
    }
  }
  const shaped = new Map(
    orderedEntries.map((entry) => [Type.runtimeKey(entry.type), entry.type] as const),
  )
  for (const type of reached.values()) {
    if (
      Type.isRuntimeConcrete(type) &&
      (Type.isEffect(type) ||
        Type.isNever(type) ||
        (Type.isRepresented(type) &&
          Type.isCompositeEffectRepresentationArgument(type.representation.argument)))
    )
      shaped.set(Type.runtimeKey(type), type)
  }
  const shapeTypes = Object.freeze([...shaped.values()].sort(compareRuntimeTypes))
  const staticDataById = new Map<string, StaticText.Data>()
  for (const instance of discovery.instances) {
    const expressions = instance.function.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)
    for (const expression of expressions) {
      if (expression._tag === 'StaticStringLiteral' || expression._tag === 'StaticByteViewLiteral')
        staticDataById.set(expression.data.id, expression.data)
    }
  }
  const addressBits: 32 | 64 = self.target.pointerSize === 4 ? 32 : 64
  const staticData = Object.freeze(
    [...staticDataById.values()]
      .sort((left, right) => left.id.localeCompare(right.id))
      .map((data) =>
        Object.freeze({
          _tag: 'StaticDataPlacement' as const,
          data,
          alignment: 1 as const,
          addressBits,
          lengthBits: addressBits,
        }),
      ),
  )
  const callablePlans = callableEnvironments(self.target, orderedEntries, discovery)
  const effectPlans = effectEnvironments(self.target, orderedEntries, discovery, callablePlans)
  const executionPlanByKey = new Map<string, ExecutionPackage.Plan>()
  const executionUnavailableByKey = new Map<string, ExecutionPackage.Unavailable>()
  const executionDiagnostics: Array<Diagnostic.Diagnostic> = []
  const representedStorageLayout = (
    argument: Type.GenericArgument,
  ): { readonly size: number; readonly alignment: number } | undefined => {
    if (Type.isExactRepresentationArgument(argument)) {
      if (Type.isEffectIdentityArgument(argument.identity)) {
        const environment = effectEnvironmentByIdentity(effectPlans, argument.identity)
        return environment === undefined
          ? undefined
          : Object.freeze({ size: environment.size, alignment: environment.alignment })
      }
      if (!Type.isCallableIdentityArgument(argument.identity)) return undefined
      const callableIdentity = argument.identity
      const callableEnvironment = callableIdentity.environment
      if (callableEnvironment === undefined) return Object.freeze({ size: 0, alignment: 1 })
      const environment = callablePlans.find(
        (
          candidate,
        ): candidate is Extract<CallableEnvironment, { readonly _tag: 'CallableEnvironment' }> =>
          candidate._tag === 'CallableEnvironment' &&
          Type.runtimeCallableEnvironmentIdentityKey(
            Instances.callableEnvironmentIdentity(candidate.callable),
          ) === Type.runtimeCallableEnvironmentIdentityKey(callableEnvironment),
      )
      return environment === undefined
        ? undefined
        : Object.freeze({ size: environment.size, alignment: environment.alignment })
    }
    if (Type.isCompositeEffectRepresentationArgument(argument)) {
      const alternatives = argument.alternatives.map(representedStorageLayout)
      if (alternatives.some((alternative) => alternative === undefined)) return undefined
      const available = alternatives.filter(
        (alternative): alternative is { readonly size: number; readonly alignment: number } =>
          alternative !== undefined,
      )
      const payloadAlignment = available.reduce(
        (maximum, alternative) => Math.max(maximum, alternative.alignment),
        1,
      )
      const payloadSize = available.reduce(
        (maximum, alternative) => Math.max(maximum, alternative.size),
        0,
      )
      const alignment = Math.max(4, payloadAlignment)
      const payloadOffset = alignUp(4, payloadAlignment)
      const size = alignUp(payloadOffset + payloadSize, alignment)
      return Number.isSafeInteger(size) ? Object.freeze({ size, alignment }) : undefined
    }
    return undefined
  }
  const suspensionOf = (argument: Type.GenericArgument): SuspensionMode.Summary => {
    if (Type.isExactRepresentationArgument(argument))
      return Type.isEffectIdentityArgument(argument.identity)
        ? Instances.representedEffectSuspensionOf(discovery, argument.identity)
        : SuspensionMode.direct
    if (Type.isCompositeEffectRepresentationArgument(argument))
      return SuspensionMode.join(argument.alternatives.map(suspensionOf))
    return SuspensionMode.openExecutable(Object.freeze([]))
  }
  for (const instance of discovery.instances) {
    for (const expression of instance.function.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)) {
      if (
        expression._tag !== 'BuiltinCall' ||
        (expression.operation !== 'ExecutionLayout' &&
          expression.operation !== 'ExecutionFromAllocation')
      )
        continue
      const arguments_ = expression.typeArguments.map((argument) =>
        Instances.concreteEffectRepresentationArgument(
          instance.function,
          instance.key,
          Type.substituteGenericArgument(argument, instance.substitution),
        ),
      )
      const result = arguments_.at(0)
      const bodyArgument = arguments_.at(1)
      const endpoint = arguments_.at(2)
      const callbackArgument = arguments_.at(3)
      const body = bodyArgument === undefined ? undefined : Type.representedType(bodyArgument)
      const callback =
        callbackArgument === undefined ? undefined : Type.representedType(callbackArgument)
      if (
        result === undefined ||
        !Type.isTypeArgument(result) ||
        bodyArgument === undefined ||
        body === undefined ||
        endpoint === undefined ||
        !Type.isTypeArgument(endpoint) ||
        callback === undefined ||
        callbackArgument === undefined
      )
        continue
      const bodyLayout = representedStorageLayout(bodyArgument)
      const endpointLayout = resolve(endpoint)
      const callbackLayout = representedStorageLayout(callbackArgument)
      if (bodyLayout === undefined || endpointLayout === undefined || callbackLayout === undefined)
        continue
      const specialization: ExecutionPackage.Specialization = Object.freeze({
        result,
        body,
        endpoint,
        callback,
        suspension: suspensionOf(bodyArgument),
      })
      const planned = ExecutionPackage.plan(self.target, specialization, {
        body: bodyLayout,
        endpoint: endpointLayout,
        callback: callbackLayout,
      })
      const selected =
        planned._tag === 'ExecutionPackagePlan'
          ? Object.freeze({
              ...planned,
              cleanup: Object.freeze({
                body: CleanupPlan.cleanupPlan(index, body),
                endpoint: CleanupPlan.cleanupPlan(index, endpoint),
                callback: CleanupPlan.cleanupPlan(index, callback),
              }),
            })
          : planned
      const key = ExecutionPackage.specializationKey(specialization)
      if (selected._tag === 'ExecutionPackagePlan') executionPlanByKey.set(key, selected)
      else {
        executionUnavailableByKey.set(key, selected)
        executionDiagnostics.push(
          Diagnostic.intrinsicTargetUnavailable(
            `Intrinsic.${expression.operation === 'ExecutionLayout' ? 'executionLayout' : 'executionFromAllocation'}`,
            self.target.id,
            expression.span,
          ),
        )
      }
    }
  }
  const executionPackages: ExecutionPackage.Module = Object.freeze({
    _tag: 'ExecutionPackageModule',
    plans: Object.freeze(
      [...executionPlanByKey.values()].sort((left, right) =>
        left.provenance.localeCompare(right.provenance),
      ),
    ),
    unavailable: Object.freeze(
      [...executionUnavailableByKey.values()].sort((left, right) =>
        ExecutionPackage.specializationKey(left.specialization).localeCompare(
          ExecutionPackage.specializationKey(right.specialization),
        ),
      ),
    ),
  })
  const specializedShapeTypes = new Map(
    shapeTypes.map((type) => [Type.runtimeKey(type), type] as const),
  )
  for (const environment of effectPlans)
    specializedShapeTypes.set(Type.runtimeKey(environment.effect), environment.effect)
  return Object.freeze({
    _tag: 'LayoutPlan',
    target: self.target,
    entries: orderedEntries,
    effectEnvironments: effectPlans,
    callableEnvironments: callablePlans,
    callingShapes: callingShapes(
      self.target,
      orderedEntries,
      [...specializedShapeTypes.values()].sort(compareRuntimeTypes),
      effectPlans,
      callablePlans,
    ),
    staticData,
    literalVerdicts: literals.verdicts,
    localSharedAllocationProvenance,
    executionPackages,
    diagnostics: Diagnostic.merge([
      ...literals.diagnostics,
      ...localSharedDiagnostics,
      ...localSharedAllocationProvenance.diagnostics,
      ...executionDiagnostics,
    ]),
  })
}

/** Constructs a scalar plan for hand-built MIR samples and focused tests. */
export const make = (target: Target.Target, types: ReadonlyArray<Type.Builtin>): Plan => {
  const entries = new Map(types.map((type) => [Type.runtimeKey(type), scalarEntry(target, type)]))
  const orderedEntries = Object.freeze(
    [...entries.values()].sort((left, right) => compareRuntimeTypes(left.type, right.type)),
  )
  return Object.freeze({
    _tag: 'LayoutPlan',
    target,
    entries: orderedEntries,
    effectEnvironments: Object.freeze([]),
    callableEnvironments: Object.freeze([]),
    callingShapes: callingShapes(target, orderedEntries),
    staticData: Object.freeze([]),
    literalVerdicts: Object.freeze([]),
    localSharedAllocationProvenance: LocalSharedAllocationProvenance.empty(),
    executionPackages: ExecutionPackage.empty(),
    diagnostics: Object.freeze([]),
  })
}

interface ShapeContext {
  readonly target: Target.Target
  readonly entries: ReadonlyMap<string, Entry>
  readonly effectEnvironments: ReadonlyArray<EffectEnvironment>
  readonly callableEnvironments: ReadonlyArray<CallableEnvironment>
  readonly active: ReadonlySet<string>
}

const withActiveShape = (context: ShapeContext, identity: string): ShapeContext => {
  if (context.active.has(identity))
    throw new RangeError(`recursive executable environment ${identity} has no calling shape`)
  return Object.freeze({ ...context, active: new Set([...context.active, identity]) })
}

const borrowedShape = (
  context: ShapeContext,
  type: DeclarationFacts.SemanticType,
): Extract<CallingShapeNode, { readonly _tag: 'AddressShape' }> =>
  Object.freeze({
    _tag: 'AddressShape',
    type,
    address: Object.freeze({
      type: Object.freeze({
        _tag: 'Address',
        element: type,
        bits: context.target.pointerSize === 4 ? 32 : 64,
      }),
      lane: 0,
    }),
    laneCount: 1,
  })

const executableEnvironmentFieldShape = (
  context: ShapeContext,
  field: Pick<
    EffectEnvironmentField,
    'representation' | 'type' | 'callableIdentity' | 'effectIdentity'
  >,
): CallingShapeNode => {
  if (field.representation === 'Borrow') return borrowedShape(context, field.type)
  if (field.callableIdentity !== undefined) {
    const identity = field.callableIdentity
    if (identity.environment === undefined) {
      return Object.freeze({
        _tag: 'CallableEnvironmentShape',
        type: field.type,
        fields: Object.freeze([]),
        laneCount: 0,
      })
    }
    const environment = context.callableEnvironments.find(
      (
        candidate,
      ): candidate is Extract<CallableEnvironment, { readonly _tag: 'CallableEnvironment' }> =>
        candidate._tag === 'CallableEnvironment' &&
        FieldRealization.matchesIdentity(identity, candidate.callable),
    )
    if (environment === undefined)
      throw new RangeError(
        `callable environment ${Type.runtimeGenericArgumentKey(identity)} is unavailable to calling-shape planning`,
      )
    const nested = withActiveShape(context, `callable:${Type.runtimeGenericArgumentKey(identity)}`)
    const fields = environment.fields.map((capture) =>
      Object.freeze({
        capture: capture.ordinal,
        shape: executableEnvironmentFieldShape(nested, capture),
      }),
    )
    return Object.freeze({
      _tag: 'CallableEnvironmentShape',
      type: field.type,
      fields: Object.freeze(fields),
      laneCount: fields.reduce((total, capture) => total + capture.shape.laneCount, 0),
    })
  }
  if (field.effectIdentity !== undefined) {
    const environment = context.effectEnvironments.find(
      (
        candidate,
      ): candidate is Extract<EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> =>
        candidate._tag === 'EffectEnvironment' &&
        (Instances.effectIdentity(candidate.instance, candidate.site) === field.effectIdentity ||
          candidate.successEffectIdentity === field.effectIdentity),
    )
    if (environment === undefined)
      throw new RangeError(
        `Effect environment ${field.effectIdentity} is unavailable to calling-shape planning`,
      )
    const nested = withActiveShape(context, `effect:${field.effectIdentity}`)
    const fields = environment.fields.map((capture) =>
      Object.freeze({
        capture: capture.ordinal,
        shape: executableEnvironmentFieldShape(nested, capture),
      }),
    )
    return Object.freeze({
      _tag: 'EffectEnvironmentShape',
      type: field.type,
      fields: Object.freeze(fields),
      laneCount: fields.reduce((total, capture) => total + capture.shape.laneCount, 0),
    })
  }
  return shapeNode(field.type, context)
}

const shapeNode = (
  type: DeclarationFacts.SemanticType,
  context: ShapeContext,
): CallingShapeNode => {
  const { target, entries } = context
  if (Type.isBuiltin(type)) {
    return Object.freeze({ _tag: 'ScalarShape', type, laneCount: 1 })
  }
  const enumRepresentation = entries.get(Type.runtimeKey(type))?.representation
  if (Type.isNominal(type) && enumRepresentation?._tag === 'ScalarEnum') {
    return Object.freeze({
      _tag: 'ScalarEnumShape',
      type,
      lane: enumRepresentation.scalar,
      laneCount: 1,
    })
  }
  if (Type.isString(type)) {
    return Object.freeze({
      _tag: 'StringShape',
      type,
      storage: Object.freeze({
        type: Object.freeze({
          _tag: 'Address',
          element: type,
          bits: target.pointerSize === 4 ? 32 : 64,
        }),
        lane: 0,
      }),
      byteLength: Object.freeze({ type: 'usize', lane: 1 }),
      laneCount: 2,
    })
  }
  if (Type.isNever(type)) {
    return Object.freeze({ _tag: 'EmptyShape', type, laneCount: 0 })
  }
  if (Type.isParameter(type)) {
    throw new RangeError(`open generic parameter ${Type.encode(type)} has no calling shape`)
  }
  if (Type.isSlice(type)) {
    return Object.freeze({
      _tag: 'SliceShape',
      type,
      address: Object.freeze({
        type: Object.freeze({
          _tag: 'Address',
          element: type.element,
          bits: target.pointerSize === 4 ? 32 : 64,
        }),
        lane: 0,
      }),
      length: Object.freeze({ type: 'usize', lane: 1 }),
      laneCount: 2,
    })
  }
  if (Type.isReference(type)) {
    return Object.freeze({
      _tag: 'ReferenceShape',
      type,
      address: Object.freeze({
        type: Object.freeze({
          _tag: 'Address',
          element: type.target,
          bits: target.pointerSize === 4 ? 32 : 64,
        }),
        lane: 0,
      }),
      laneCount: 1,
    })
  }
  if (Type.isSharedCore(type) || Type.isExecution(type) || Type.isWake(type)) {
    return Object.freeze({
      _tag: 'AddressShape',
      type,
      address: Object.freeze({
        type: Object.freeze({
          _tag: 'Address',
          element: type,
          bits: target.pointerSize === 4 ? 32 : 64,
        }),
        lane: 0,
      }),
      laneCount: 1,
    })
  }
  if (Type.isPointer(type)) {
    return Object.freeze({
      _tag: 'AddressShape',
      type,
      address: Object.freeze({
        type: Object.freeze({
          _tag: 'Address',
          element: type.pointee,
          bits: target.pointerSize === 4 ? 32 : 64,
        }),
        lane: 0,
      }),
      laneCount: 1,
    })
  }
  if (Type.isForeignFunction(type)) return borrowedShape(context, type)
  if (Type.isCallable(type)) {
    throw new RangeError(
      `callable ${Type.encode(type)} needs a hidden concrete identity before calling-shape planning`,
    )
  }
  if (Type.isRepresented(type)) {
    const argument = type.representation.argument
    if (Type.isEffect(type.contract) && Type.isCompositeEffectRepresentationArgument(argument)) {
      const alternatives = argument.alternatives.map((alternative) => {
        if (!Type.isEffectIdentityArgument(alternative.identity))
          throw new RangeError('Effect composite retained a non-Effect alternative')
        const identity = alternative.identity
        const environment = effectEnvironmentByIdentity(context.effectEnvironments, identity)
        if (environment === undefined)
          throw new RangeError('Effect composite alternative has no concrete environment')
        const fields = environment.fields.map((field) =>
          Object.freeze({
            capture: field.ordinal,
            shape: executableEnvironmentFieldShape(context, field),
          }),
        )
        return Object.freeze({
          _tag: 'EffectEnvironmentShape' as const,
          type: environment.effect,
          fields: Object.freeze(fields),
          laneCount: fields.reduce((total, field) => total + field.shape.laneCount, 0),
        })
      })
      const alternativeLanes = alternatives.map((alternative) => materializeLanes(alternative))
      const payloadTypes = unifyPayloadTypes(alternatives, target)
      return Object.freeze({
        _tag: 'EffectCompositeShape',
        type,
        alternativeLaneCounts: Object.freeze(alternativeLanes.map((lanes) => lanes.length)),
        payloadTypes,
        laneCount: payloadTypes.length + 1,
      })
    }
    const entry = entries.get(Type.runtimeKey(type))
    const executable = entry?.executable
    const stored = entry?.representation
    const storedCallable = stored?._tag === 'CallableEnvironment' ? stored : undefined
    const storedEffect = stored?._tag === 'StoredEffectEnvironment' ? stored : undefined
    if (executable === undefined && storedCallable === undefined && storedEffect === undefined) {
      throw new RangeError(
        `represented executable ${Type.encode(type)} is unavailable to calling-shape planning`,
      )
    }
    const kind = executable?._tag ?? (storedCallable === undefined ? 'Effect' : 'Callable')
    let fields: ReadonlyArray<{ readonly capture: number; readonly shape: CallingShapeNode }>
    if (executable !== undefined) {
      fields = executable.fields.map((field) => {
        const shape =
          executable._tag === 'Callable' && field.representation !== 'Borrow'
            ? shapeNode(field.type, context)
            : executableEnvironmentFieldShape(context, field)
        return Object.freeze({ capture: field.capture, shape })
      })
    } else if (storedCallable !== undefined) {
      fields = storedCallable.fields.map((field) =>
        Object.freeze({
          capture: field.ordinal,
          shape: executableEnvironmentFieldShape(context, field),
        }),
      )
    } else {
      fields = (storedEffect?.fields ?? []).map((field) =>
        Object.freeze({
          capture: field.capture,
          shape: executableEnvironmentFieldShape(context, field),
        }),
      )
    }
    return Object.freeze({
      _tag:
        kind === 'Callable'
          ? ('CallableEnvironmentShape' as const)
          : ('EffectEnvironmentShape' as const),
      type,
      fields: Object.freeze(fields),
      laneCount: fields.reduce((total, field) => total + field.shape.laneCount, 0),
    })
  }
  const candidate = entries.get(Type.runtimeKey(type))
  if (Type.isNominal(type) && candidate?.representation._tag === 'NominalUnion') {
    const variants = Object.freeze(
      candidate.representation.variants.map((variant) => {
        const fields = Object.freeze(
          variant.fields.map((field) =>
            Object.freeze({ field: field.id, shape: shapeNode(field.type, context) }),
          ),
        )
        const shape: CallingShapeNode = Object.freeze({
          _tag: 'ProductShape',
          type,
          fields,
          laneCount: fields.reduce((total, field) => total + field.shape.laneCount, 0),
        })
        return Object.freeze({
          variant: variant.variant,
          ordinal: variant.ordinal,
          shape,
          payloadSlots: Object.freeze(Array.from({ length: shape.laneCount }, (_, slot) => slot)),
        })
      }),
    )
    const payloadLaneCount = variants.reduce(
      (maximum, variant) => Math.max(maximum, variant.shape.laneCount),
      0,
    )
    return Object.freeze({
      _tag: 'NominalUnionShape',
      type,
      tag: Object.freeze({ type: 'i32', lane: 0 }),
      payloadLaneCount,
      payloadTypes: unifyPayloadTypes(
        variants.map((variant) => variant.shape),
        target,
      ),
      zeroFill: true,
      variants,
      laneCount: 1 + payloadLaneCount,
    })
  }
  if (Type.isFixedArray(type)) {
    const element = shapeNode(type.element, context)
    const laneCount = element.laneCount * type.length
    if (!Number.isSafeInteger(laneCount)) {
      throw new RangeError(`Calling shape lane count overflows for ${Type.encode(type)}`)
    }
    return Object.freeze({
      _tag: 'RepeatedShape',
      type,
      length: type.length,
      element,
      laneCount,
    })
  }
  if (Type.isUnion(type)) {
    const members = Object.freeze(
      type.members.map((member, ordinal) => {
        const shape = shapeNode(member, context)
        return Object.freeze({
          member,
          ordinal,
          shape,
          payloadSlots: Object.freeze(Array.from({ length: shape.laneCount }, (_, slot) => slot)),
        })
      }),
    )
    const payloadLaneCount = members.reduce(
      (maximum, member) => Math.max(maximum, member.shape.laneCount),
      0,
    )
    const payloadTypes = unifyPayloadTypes(
      members.map((member) => member.shape),
      target,
    )
    return Object.freeze({
      _tag: 'SumShape',
      type,
      tag: Object.freeze({ type: 'i32', lane: 0 }),
      payloadLaneCount,
      payloadTypes,
      zeroFill: true,
      members,
      laneCount: 1 + payloadLaneCount,
    })
  }
  if (Type.isEffect(type)) {
    const success = shapeNode(type.success, context)
    const failures = Type.failureMembers(type).map((failure, index) =>
      Object.freeze({
        type: failure,
        tag: index + 1,
        shape: shapeNode(failure, context),
      }),
    )
    const variants = [success, ...failures.map((failure) => failure.shape)]
    const payloadLaneCount = variants.reduce(
      (maximum, variant) => Math.max(maximum, variant.laneCount),
      0,
    )
    const payloadTypes = unifyPayloadTypes(variants, target)
    return Object.freeze({
      _tag: 'OutcomeShape',
      type,
      success,
      failures: Object.freeze(failures),
      payloadLaneCount,
      payloadTypes,
      laneCount: 1 + payloadLaneCount,
    })
  }
  const fields =
    candidate?.representation._tag === 'Aggregate'
      ? candidate.representation.fields.map((field) =>
          Object.freeze({ field: field.id, shape: shapeNode(field.type, context) }),
        )
      : []
  return Object.freeze({
    _tag: 'ProductShape',
    type,
    fields: Object.freeze(fields),
    laneCount: fields.reduce((total, field) => total + field.shape.laneCount, 0),
  })
}

/** Chooses one deterministic scalar carrier for each payload lane across tagged variants. */
export const unifyPayloadTypes = (
  variants: ReadonlyArray<CallingShapeNode>,
  target: Target.Target,
): ReadonlyArray<Type.Builtin> => {
  const payloadLaneCount = variants.reduce(
    (maximum, variant) => Math.max(maximum, variant.laneCount),
    0,
  )
  return Object.freeze(
    Array.from({ length: payloadLaneCount }, (_, slot): Type.Builtin => {
      const candidates = variants.flatMap((variant) => {
        const lane = materializeLanes(variant).at(slot)
        if (lane === undefined) return []
        return [typeof lane.type === 'string' ? lane.type : ('usize' as const)]
      })
      return (
        candidates
          .sort((left, right) => {
            const leftScalar = Scalar.find(left)
            const rightScalar = Scalar.find(right)
            const pointerBits = target.pointerSize === 4 ? 32 : 64
            const leftBits = leftScalar === undefined ? 32 : Scalar.bits(leftScalar, pointerBits)
            const rightBits = rightScalar === undefined ? 32 : Scalar.bits(rightScalar, pointerBits)
            return rightBits - leftBits || compareRuntimeTypes(left, right)
          })
          .at(0) ?? 'i32'
      )
    }),
  )
}

const materializeLanes = (
  node: CallingShapeNode,
  path: ReadonlyArray<Selector> = Object.freeze([]),
): ReadonlyArray<CallingLane> => {
  if (node._tag === 'EmptyShape') return Object.freeze([])
  if (node._tag === 'ScalarShape') {
    return Object.freeze([Object.freeze({ _tag: 'CallingLane', path, type: node.type })])
  }
  if (node._tag === 'ScalarEnumShape') {
    return Object.freeze([Object.freeze({ _tag: 'CallingLane', path, type: node.lane })])
  }
  if (node._tag === 'SliceShape') {
    return Object.freeze([
      Object.freeze({
        _tag: 'CallingLane',
        path: Object.freeze([...path, Object.freeze({ _tag: 'SliceAddressSelector' })]),
        type: node.address.type,
      }),
      Object.freeze({
        _tag: 'CallingLane',
        path: Object.freeze([...path, Object.freeze({ _tag: 'SliceLengthSelector' })]),
        type: 'usize',
      }),
    ])
  }
  if (node._tag === 'StringShape') {
    return Object.freeze([
      Object.freeze({
        _tag: 'CallingLane',
        path: Object.freeze([...path, Object.freeze({ _tag: 'StringStorageSelector' })]),
        type: node.storage.type,
      }),
      Object.freeze({
        _tag: 'CallingLane',
        path: Object.freeze([...path, Object.freeze({ _tag: 'StringByteLengthSelector' })]),
        type: 'usize',
      }),
    ])
  }
  if (node._tag === 'ReferenceShape' || node._tag === 'AddressShape') {
    return Object.freeze([
      Object.freeze({
        _tag: 'CallingLane',
        path: Object.freeze([...path, Object.freeze({ _tag: 'ReferenceAddressSelector' })]),
        type: node.address.type,
      }),
    ])
  }
  if (node._tag === 'ProductShape') {
    return Object.freeze(
      node.fields.flatMap((field) =>
        materializeLanes(field.shape, Object.freeze([...path, field.field])),
      ),
    )
  }
  if (node._tag === 'CallableEnvironmentShape' || node._tag === 'EffectEnvironmentShape') {
    const selectorTag =
      node._tag === 'CallableEnvironmentShape'
        ? ('CallableCaptureSelector' as const)
        : ('EffectCaptureSelector' as const)
    return Object.freeze(
      node.fields.flatMap((field) =>
        materializeLanes(
          field.shape,
          Object.freeze([...path, Object.freeze({ _tag: selectorTag, ordinal: field.capture })]),
        ),
      ),
    )
  }
  if (node._tag === 'SumShape') {
    return Object.freeze([
      Object.freeze({
        _tag: 'CallingLane' as const,
        path: Object.freeze([...path, Object.freeze({ _tag: 'UnionTagSelector' as const })]),
        type: 'i32' as const,
      }),
      ...Array.from({ length: node.payloadLaneCount }, (_, slot) =>
        Object.freeze({
          _tag: 'CallingLane' as const,
          path: Object.freeze([
            ...path,
            Object.freeze({ _tag: 'UnionPayloadSelector' as const, slot }),
          ]),
          type: node.payloadTypes.at(slot) ?? ('i32' as const),
        }),
      ),
    ])
  }
  if (node._tag === 'NominalUnionShape') {
    return Object.freeze([
      Object.freeze({
        _tag: 'CallingLane' as const,
        path: Object.freeze([...path, Object.freeze({ _tag: 'NominalUnionTagSelector' as const })]),
        type: 'i32' as const,
      }),
      ...Array.from({ length: node.payloadLaneCount }, (_, slot) =>
        Object.freeze({
          _tag: 'CallingLane' as const,
          path: Object.freeze([
            ...path,
            Object.freeze({ _tag: 'NominalUnionPayloadSelector' as const, slot }),
          ]),
          type: node.payloadTypes.at(slot) ?? ('i32' as const),
        }),
      ),
    ])
  }
  if (node._tag === 'OutcomeShape') {
    return Object.freeze([
      Object.freeze({
        _tag: 'CallingLane' as const,
        path: Object.freeze([...path, Object.freeze({ _tag: 'UnionTagSelector' as const })]),
        type: 'i32' as const,
      }),
      ...Array.from({ length: node.payloadLaneCount }, (_, slot) =>
        Object.freeze({
          _tag: 'CallingLane' as const,
          path: Object.freeze([
            ...path,
            Object.freeze({ _tag: 'UnionPayloadSelector' as const, slot }),
          ]),
          type: node.payloadTypes.at(slot) ?? ('i32' as const),
        }),
      ),
    ])
  }
  if (node._tag === 'EffectCompositeShape') {
    return Object.freeze([
      Object.freeze({
        _tag: 'CallingLane' as const,
        path: Object.freeze([...path, Object.freeze({ _tag: 'UnionTagSelector' as const })]),
        type: 'i32' as const,
      }),
      ...node.payloadTypes.map((type, slot) =>
        Object.freeze({
          _tag: 'CallingLane' as const,
          path: Object.freeze([
            ...path,
            Object.freeze({ _tag: 'UnionPayloadSelector' as const, slot }),
          ]),
          type,
        }),
      ),
    ])
  }
  const lanes: Array<CallingLane> = []
  for (let index = 0; index < node.length; index += 1) {
    const selector: Selector = Object.freeze({ _tag: 'ElementSelector', index })
    lanes.push(...materializeLanes(node.element, Object.freeze([...path, selector])))
  }
  return Object.freeze(lanes)
}

const shapeOf = (
  target: Target.Target,
  type: DeclarationFacts.SemanticType,
  entries: ReadonlyMap<string, Entry>,
  effectEnvironments: ReadonlyArray<EffectEnvironment>,
  callableEnvironments: ReadonlyArray<CallableEnvironment>,
): CallingShape => {
  const tree = shapeNode(
    type,
    Object.freeze({
      target,
      entries,
      effectEnvironments,
      callableEnvironments,
      active: new Set<string>(),
    }),
  )
  let materialized: ReadonlyArray<CallingLane> | undefined
  return Object.freeze({
    _tag: 'CallingShape' as const,
    type,
    tree,
    laneCount: tree.laneCount,
    get lanes(): ReadonlyArray<CallingLane> {
      materialized ??= materializeLanes(tree)
      return materialized
    },
  })
}

export const callingShapes = (
  target: Target.Target,
  entries: ReadonlyArray<Entry>,
  types: ReadonlyArray<DeclarationFacts.SemanticType> = entries.map((entry) => entry.type),
  effectEnvironments: ReadonlyArray<EffectEnvironment> = Object.freeze([]),
  callableEnvironments: ReadonlyArray<CallableEnvironment> = Object.freeze([]),
): ReadonlyArray<CallingShape> => {
  const byType = new Map(entries.map((candidate) => [Type.runtimeKey(candidate.type), candidate]))
  return Object.freeze(
    types.map((type) => shapeOf(target, type, byType, effectEnvironments, callableEnvironments)),
  )
}

/**
 * Both lookups run once per lowered operation. Their physical indexes erase lifetime proof
 * arguments while retaining the original semantic types on entries for inspection.
 */
const entryIndexCache = new WeakMap<ReadonlyArray<Entry>, Map<string, Entry>>()
const callingShapeIndexCache = new WeakMap<ReadonlyArray<CallingShape>, Map<string, CallingShape>>()

const indexByTypeKey = <A extends { readonly type: DeclarationFacts.SemanticType }>(
  cache: WeakMap<ReadonlyArray<A>, Map<string, A>>,
  values: ReadonlyArray<A>,
): Map<string, A> => {
  let index = cache.get(values)
  if (index === undefined) {
    index = new Map()
    for (const value of values) {
      const key = Type.runtimeKey(value.type)
      if (!index.has(key)) index.set(key, value)
    }
    cache.set(values, index)
  }
  return index
}

/** Looks up one canonical runtime-plan entry. */
export const entry = (self: Plan, type: DeclarationFacts.SemanticType): Entry | undefined =>
  indexByTypeKey(entryIndexCache, self.entries).get(Type.runtimeKey(type))

/** Looks up one compiler-owned calling shape by logical type. */
export const callingShape = (
  self: Plan,
  type: DeclarationFacts.SemanticType,
): CallingShape | undefined =>
  indexByTypeKey(callingShapeIndexCache, self.callingShapes).get(Type.runtimeKey(type))

/**
 * Plans the bit-exact movement of one nominal failure payload between two tagged carriers.
 *
 * Carrier slots are deliberately not treated as the member's value type: a row containing an
 * `f64` member can make the slot wider than an `i32` member which occupies that same slot. The
 * member lane is therefore retained as the normalization point between the source and target
 * carriers, and lanes outside this member's shape are omitted so consumers zero-fill them.
 */
export const failurePayloadRepacking = (
  self: Plan,
  sourceType: DeclarationFacts.SemanticType,
  sourceTag: number,
  targetType: Type.Effect,
  targetTag: number,
): FailurePayloadRepacking | undefined => {
  const sourceMember = Type.failureCarrierMember(
    sourceType,
    sourceTag,
    Type.isEffect(sourceType) ? 'OneBased' : 'ZeroBased',
  )
  const targetMember = Type.failureCarrierMember(targetType, targetTag, 'OneBased')
  if (sourceMember === undefined || targetMember === undefined) return undefined
  const sourceShape = callingShape(self, sourceType)
  const targetShape = callingShape(self, targetType)
  if (sourceShape === undefined || targetShape?.tree._tag !== 'OutcomeShape') return undefined
  if (!(Type.runtimeKey(sourceMember) === Type.runtimeKey(targetMember))) return undefined
  const memberShape = callingShape(self, sourceMember)
  if (memberShape === undefined) return undefined
  const sourceOffset = Type.isNominal(sourceType) ? 0 : 1
  const targetPayloadLanes = Object.freeze(targetShape.lanes.slice(1))
  const lanes: Array<FailurePayloadLane> = []
  for (const [ordinal, member] of memberShape.lanes.entries()) {
    const source = sourceShape.lanes.at(sourceOffset + ordinal)
    const target = targetPayloadLanes.at(ordinal)
    if (source === undefined || target === undefined) return undefined
    lanes.push(
      Object.freeze({
        sourceOrdinal: sourceOffset + ordinal,
        source,
        member,
        targetOrdinal: ordinal,
        target,
      }),
    )
  }
  return Object.freeze({
    member: sourceMember,
    targetPayloadLanes,
    lanes: Object.freeze(lanes),
  })
}

/** Resolves one canonical callable-environment identity in this target's runtime plan. */
export const callableEnvironmentByIdentity = (
  self: Plan,
  identity: Type.CallableEnvironmentIdentity,
): Extract<CallableEnvironment, { readonly _tag: 'CallableEnvironment' }> | undefined =>
  self.callableEnvironments.find(
    (
      candidate,
    ): candidate is Extract<CallableEnvironment, { readonly _tag: 'CallableEnvironment' }> =>
      candidate._tag === 'CallableEnvironment' &&
      Type.runtimeCallableEnvironmentIdentityKey(
        Instances.callableEnvironmentIdentity(candidate.callable),
      ) === Type.runtimeCallableEnvironmentIdentityKey(identity),
  )

/** Resolves the Effect environment a capture field's identity names, including success carriers. */
export const effectEnvironmentByFieldIdentity = (
  self: Plan,
  identity: string,
): Extract<EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> | undefined => {
  const matches = self.effectEnvironments.filter(
    (candidate): candidate is Extract<EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> =>
      candidate._tag === 'EffectEnvironment' &&
      (Instances.effectIdentity(candidate.instance, candidate.site) === identity ||
        candidate.successEffectIdentity === identity),
  )
  return matches.at(0)
}

/** Materializes the ABI lanes of one Effect environment capture field. */
export const effectFieldLanes = (
  self: Plan,
  field: EffectEnvironmentField,
): ReadonlyArray<CallingLane> => {
  if (field.representation === 'Borrow') {
    return Object.freeze([
      Object.freeze({
        _tag: 'CallingLane' as const,
        path: Object.freeze([]),
        type: Object.freeze({
          _tag: 'Address' as const,
          element: field.type,
          bits: self.target.pointerSize === 4 ? 32 : 64,
        }),
      }),
    ])
  }
  if (field.callableIdentity !== undefined) {
    const captured =
      field.callableIdentity.environment === undefined
        ? undefined
        : callableEnvironmentByIdentity(self, field.callableIdentity.environment)
    return captured?._tag === 'CallableEnvironment'
      ? callableEnvironmentLanes(self, captured)
      : Object.freeze([])
  }
  if (field.effectIdentity !== undefined) {
    const captured = effectEnvironmentByFieldIdentity(self, field.effectIdentity)
    return captured !== undefined ? effectEnvironmentLanes(self, captured) : Object.freeze([])
  }
  return callingShape(self, field.type)?.lanes ?? Object.freeze([])
}

/** One scalar lane's storage root and byte base within an executable environment. */
export interface EnvironmentLanePlacement {
  readonly lane: CallingLane
  readonly byteOffset: number
  /** The ordinary value whose selector path supplies the remainder of the byte offset. */
  readonly root?: DeclarationFacts.SemanticType
}

const ordinaryLanePlacements = (
  self: Plan,
  type: DeclarationFacts.SemanticType,
  byteOffset: number,
): ReadonlyArray<EnvironmentLanePlacement> =>
  Object.freeze(
    (callingShape(self, type)?.lanes ?? []).map((lane) =>
      Object.freeze({ lane, byteOffset, root: type }),
    ),
  )

/** Places every scalar lane stored by one hidden Effect environment. */
export const effectEnvironmentLanePlacements = (
  self: Plan,
  environment: Extract<EffectEnvironment, { readonly _tag: 'EffectEnvironment' }>,
  byteOffset = 0,
): ReadonlyArray<EnvironmentLanePlacement> =>
  Object.freeze(
    environment.fields.flatMap((field) => effectFieldLanePlacements(self, field, byteOffset)),
  )

/** Places every scalar lane stored by one Effect capture field. */
export const effectFieldLanePlacements = (
  self: Plan,
  field: EffectEnvironmentField,
  byteOffset = 0,
): ReadonlyArray<EnvironmentLanePlacement> => {
  const fieldOffset = byteOffset + field.offset
  if (field.representation === 'Borrow') {
    const [lane] = effectFieldLanes(self, field)
    return lane === undefined
      ? Object.freeze([])
      : Object.freeze([Object.freeze({ lane, byteOffset: fieldOffset })])
  }
  if (field.callableIdentity !== undefined) {
    const captured =
      field.callableIdentity.environment === undefined
        ? undefined
        : callableEnvironmentByIdentity(self, field.callableIdentity.environment)
    return captured === undefined
      ? Object.freeze([])
      : callableEnvironmentLanePlacements(self, captured, fieldOffset)
  }
  if (field.effectIdentity !== undefined) {
    const captured = effectEnvironmentByFieldIdentity(self, field.effectIdentity)
    return captured === undefined
      ? Object.freeze([])
      : effectEnvironmentLanePlacements(self, captured, fieldOffset)
  }
  return ordinaryLanePlacements(self, field.type, fieldOffset)
}

/** Materializes the ABI lanes of one hidden Effect environment separately from its outcome. */
export const effectEnvironmentLanes = (
  self: Plan,
  environment: Extract<EffectEnvironment, { readonly _tag: 'EffectEnvironment' }>,
): ReadonlyArray<CallingLane> =>
  Object.freeze(environment.fields.flatMap((field) => effectFieldLanes(self, field)))

/** Materializes the ABI lanes of one hidden callable capture environment. */
export const callableEnvironmentLanes = (
  self: Plan,
  environment: Extract<CallableEnvironment, { readonly _tag: 'CallableEnvironment' }>,
): ReadonlyArray<CallingLane> =>
  Object.freeze(environment.fields.flatMap((field) => callableFieldLanes(self, field)))

/** Reconstructs the complete specialized target key of one callable environment. */
export const callableTargetArguments = (
  environment: Extract<CallableEnvironment, { readonly _tag: 'CallableEnvironment' }>,
): ReadonlyArray<Type.GenericArgument> =>
  Object.freeze([
    ...environment.callable.typeArguments,
    ...environment.callable.captures
      .filter(
        (
          capture,
        ): capture is typeof capture & {
          readonly callableIdentity: Type.CallableIdentityArgument
        } => capture.callableIdentity !== undefined,
      )
      .sort((left, right) => left.parameterOrdinal - right.parameterOrdinal)
      .map((capture) => capture.callableIdentity),
  ])

/** Materializes the ABI lanes stored for one hidden callable capture field. */
export const callableFieldLanes = (
  self: Plan,
  field: CallableEnvironmentField,
): ReadonlyArray<CallingLane> => {
  if (field.representation === 'Borrow') {
    return Object.freeze([
      Object.freeze({
        _tag: 'CallingLane' as const,
        path: Object.freeze([]),
        type: Object.freeze({
          _tag: 'Address' as const,
          element: field.type,
          bits: self.target.pointerSize === 4 ? 32 : 64,
        }),
      }),
    ])
  }
  if (field.callableIdentity !== undefined) {
    const environment =
      field.callableIdentity.environment === undefined
        ? undefined
        : callableEnvironmentByIdentity(self, field.callableIdentity.environment)
    return environment === undefined
      ? Object.freeze([])
      : callableEnvironmentLanes(self, environment)
  }
  return callingShape(self, field.type)?.lanes ?? Object.freeze([])
}

/** Places every scalar lane stored by one hidden callable environment. */
export const callableEnvironmentLanePlacements = (
  self: Plan,
  environment: Extract<CallableEnvironment, { readonly _tag: 'CallableEnvironment' }>,
  byteOffset = 0,
): ReadonlyArray<EnvironmentLanePlacement> =>
  Object.freeze(
    environment.fields.flatMap((field) => callableFieldLanePlacements(self, field, byteOffset)),
  )

/** Places every scalar lane stored by one callable capture field. */
export const callableFieldLanePlacements = (
  self: Plan,
  field: CallableEnvironmentField,
  byteOffset = 0,
): ReadonlyArray<EnvironmentLanePlacement> => {
  const fieldOffset = byteOffset + field.offset
  if (field.representation === 'Borrow') {
    const [lane] = callableFieldLanes(self, field)
    return lane === undefined
      ? Object.freeze([])
      : Object.freeze([Object.freeze({ lane, byteOffset: fieldOffset })])
  }
  if (field.callableIdentity?.environment !== undefined) {
    const captured = callableEnvironmentByIdentity(self, field.callableIdentity.environment)
    return captured === undefined
      ? Object.freeze([])
      : callableEnvironmentLanePlacements(self, captured, fieldOffset)
  }
  return ordinaryLanePlacements(self, field.type, fieldOffset)
}

/** The logical lane and byte range occupied by one capture in a specialized environment. */
export interface CallableCaptureRange {
  readonly laneOffset: number
  readonly laneCount: number
  readonly byteOffset: number
}

/** Resolves one owned capture's runtime range from its canonical environment identity. */
export const callableCaptureRange = (
  self: Plan,
  identity: Type.CallableEnvironmentIdentity,
  capture: number,
): CallableCaptureRange | undefined => {
  const environment = callableEnvironmentByIdentity(self, identity)
  if (environment === undefined) return undefined
  let laneOffset = 0
  for (const field of environment.fields) {
    const laneCount = callableFieldLanes(self, field).length
    if (field.ordinal === capture)
      return Object.freeze({ laneOffset, laneCount, byteOffset: field.offset })
    laneOffset += laneCount
  }
  return undefined
}

const fieldSlice = (
  node: CallingShapeNode,
  path: ReadonlyArray<DeclarationFacts.FieldId>,
  offset = 0,
): { readonly offset: number; readonly length: number } | undefined => {
  const [field, ...rest] = path
  if (field === undefined) return Object.freeze({ offset, length: node.laneCount })
  if (node._tag !== 'ProductShape') return undefined
  let fieldOffset = offset
  for (const candidate of node.fields) {
    if (DeclarationFacts.sameFieldId(candidate.field, field)) {
      return fieldSlice(candidate.shape, rest, fieldOffset)
    }
    fieldOffset += candidate.shape.laneCount
  }
  return undefined
}

/** Physical calling-lane slots for one logical member payload field path. */
export const memberFieldSlots = (
  shape: CallingShape,
  member: Type.Type,
  path: ReadonlyArray<DeclarationFacts.FieldId>,
): ReadonlyArray<number> | undefined => {
  if (path.length === 0 && Type.runtimeKey(shape.type) === Type.runtimeKey(member))
    return Object.freeze(Array.from({ length: shape.laneCount }, (_, ordinal) => ordinal))
  let selected: { readonly shape: CallingShapeNode; readonly physicalOffset: number } | undefined
  if (
    shape.tree._tag === 'ProductShape' &&
    Type.runtimeKey(shape.tree.type) === Type.runtimeKey(member)
  ) {
    selected = Object.freeze({ shape: shape.tree, physicalOffset: 0 })
  } else if (shape.tree._tag === 'SumShape') {
    const candidate = shape.tree.members.find(
      (entry) => Type.runtimeKey(entry.member) === Type.runtimeKey(member),
    )
    if (candidate !== undefined) {
      selected = Object.freeze({ shape: candidate.shape, physicalOffset: 1 })
    }
  }
  if (selected === undefined) return undefined
  const slice = fieldSlice(selected.shape, path)
  return slice === undefined
    ? undefined
    : Object.freeze(
        Array.from(
          { length: slice.length },
          (_, ordinal) => selected.physicalOffset + slice.offset + ordinal,
        ),
      )
}

/** Canonical match leaves described by a realized calling shape. */
export const coverageMembers = (shape: CallingShape): ReadonlyArray<Match.CoverageIdentity> => {
  const variants = (
    root: Type.Type,
    node: Extract<CallingShapeNode, { readonly _tag: 'NominalUnionShape' }>,
  ): ReadonlyArray<Match.CoverageIdentity> =>
    node.variants.map((variant) =>
      Match.nominalUnionVariant(root, node.type, variant.variant, variant.ordinal),
    )
  if (shape.tree._tag === 'NominalUnionShape')
    return Object.freeze(variants(shape.type, shape.tree))
  if (shape.tree._tag !== 'SumShape') return Match.membersOf(shape.type)
  return Object.freeze(
    shape.tree.members.flatMap((member) =>
      member.shape._tag === 'NominalUnionShape'
        ? variants(member.member, member.shape)
        : [Match.structuralMember(member.member)],
    ),
  )
}

/** Physical calling-lane slots for a field selected by one exact match coverage identity. */
export const coverageFieldSlots = (
  shape: CallingShape,
  member: Match.CoverageIdentity,
  path: ReadonlyArray<DeclarationFacts.FieldId>,
): ReadonlyArray<number> | undefined => {
  if (member._tag !== 'NominalUnionVariant')
    return memberFieldSlots(shape, Match.sourceType(member), path)
  let selected: { readonly shape: CallingShapeNode; readonly physicalOffset: number } | undefined
  if (
    shape.tree._tag === 'NominalUnionShape' &&
    Type.runtimeKey(shape.tree.type) === Type.runtimeKey(member.type)
  ) {
    const variant = shape.tree.variants.find(
      (candidate) =>
        candidate.ordinal === member.variantOrdinal &&
        candidate.variant.union.module === member.variant.union.module &&
        candidate.variant.union.name === member.variant.union.name &&
        candidate.variant.name === member.variant.name,
    )
    if (variant !== undefined) selected = { shape: variant.shape, physicalOffset: 1 }
  } else if (shape.tree._tag === 'SumShape') {
    const outer = shape.tree.members.find(
      (candidate) => Type.runtimeKey(candidate.member) === Type.runtimeKey(member.root),
    )
    if (outer?.shape._tag === 'NominalUnionShape') {
      const variant = outer.shape.variants.find(
        (candidate) =>
          candidate.ordinal === member.variantOrdinal &&
          candidate.variant.union.module === member.variant.union.module &&
          candidate.variant.union.name === member.variant.union.name &&
          candidate.variant.name === member.variant.name,
      )
      if (variant !== undefined) selected = { shape: variant.shape, physicalOffset: 2 }
    }
  }
  if (selected === undefined) return undefined
  const slice = fieldSlice(selected.shape, path)
  return slice === undefined
    ? undefined
    : Object.freeze(
        Array.from(
          { length: slice.length },
          (_, ordinal) => selected.physicalOffset + slice.offset + ordinal,
        ),
      )
}

/** Looks up one available or unavailable nominal catalog entry. */
export const catalogEntry = (
  self: Catalog,
  type: DeclarationFacts.SemanticType,
): CatalogEntry | undefined =>
  self.entries.find((candidate) => Type.runtimeKey(candidate.type) === Type.runtimeKey(type))
