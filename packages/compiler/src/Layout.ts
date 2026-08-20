import * as CallableFieldRealization from './CallableFieldRealization.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import * as Ownership from './Ownership.js'
import * as RepresentationField from './RepresentationField.js'
import * as RowAlgebra from './RowAlgebra.js'
import * as Scalar from './Scalar.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as StaticText from './StaticText.js'
import * as Target from './Target.js'
import * as TargetConstant from './TargetConstant.js'
import * as Type from './Type.js'

/** One declaration-ordered physical field within an aggregate representation. */
export interface Field {
  readonly _tag: 'LayoutField'
  readonly id: DeclarationIndex.FieldId
  readonly name: string
  readonly type: DeclarationIndex.SemanticType
  readonly offset: number
  readonly size: number
  readonly alignment: number
  readonly padding: number
}

/** The initial closed representation vocabulary for concrete runtime types. */
export type Representation =
  | { readonly _tag: 'SignedInteger'; readonly bits: Scalar.FixedBits }
  | { readonly _tag: 'UnsignedInteger'; readonly bits: Scalar.FixedBits }
  | { readonly _tag: 'Floating'; readonly bits: 32 | 64; readonly ieee: true }
  | { readonly _tag: 'Boolean'; readonly bits: 32; readonly falseValue: 0; readonly trueValue: 1 }
  | {
      readonly _tag: 'Aggregate'
      readonly fields: ReadonlyArray<Field>
      readonly tailPadding: number
      /** Static cleanup hook required before structural field cleanup; contributes no ABI bytes. */
      readonly cleanupHook?: {
        readonly hook: DeclarationIndex.CanonicalId
        readonly typeArguments: ReadonlyArray<Type.GenericArgument>
      }
    }
  | {
      readonly _tag: 'CallableEnvironment'
      readonly realization: CallableFieldRealization.CallableRealization
      readonly fields: ReadonlyArray<CallableEnvironmentField>
      readonly tailPadding: number
    }
  | {
      readonly _tag: 'StoredEffectEnvironment'
      readonly realization: CallableFieldRealization.EffectRealization
      readonly fields: ReadonlyArray<StoredEffectEnvironmentField>
      readonly tailPadding: number
    }
  | {
      readonly _tag: 'Repeated'
      readonly element: DeclarationIndex.SemanticType
      readonly length: number
      readonly stride: number
    }
  | {
      readonly _tag: 'Slice'
      readonly element: DeclarationIndex.SemanticType
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
      readonly target: DeclarationIndex.SemanticType
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

/** One compiler-owned concrete layout entry. */
export interface Entry {
  readonly _tag: 'LayoutEntry'
  readonly type: DeclarationIndex.SemanticType
  readonly size: number
  readonly alignment: number
  readonly representation: Representation
}

/** Why one nominal declaration cannot have a concrete physical representation. */
export type UnavailableReason =
  | { readonly _tag: 'InvalidDeclaration'; readonly detail: string }
  | {
      readonly _tag: 'UnavailableField'
      readonly field?: DeclarationIndex.FieldId
      readonly detail: string
    }
  | { readonly _tag: 'UnavailableDependency'; readonly dependency: DeclarationIndex.SemanticType }

/** One retained nominal layout failure that does not prevent unrelated layouts. */
export interface UnavailableEntry {
  readonly _tag: 'UnavailableLayoutEntry'
  readonly type: DeclarationIndex.SemanticType
  readonly dependencies: ReadonlyArray<Type.Nominal>
  readonly reason: UnavailableReason
  readonly cause?: Diagnostic.Identity
}

export type CatalogEntry = Entry | UnavailableEntry

/** One valid target-word constant awaiting the selected target's exact range verdict. */
export interface UsizeConstantLiteral {
  readonly value: bigint
  readonly span: SourceSpan.SourceSpan
}

/** Every canonical nominal declaration laid out for one selected target. */
export interface Catalog {
  readonly _tag: 'LayoutCatalog'
  readonly target: Target.Target
  readonly entries: ReadonlyArray<CatalogEntry>
  readonly usizeConstants: ReadonlyArray<UsizeConstantLiteral>
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
  readonly literalVerdicts: ReadonlyArray<UsizeLiteralVerdict>
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

export interface EffectEnvironmentField {
  readonly source: 'Binding' | 'Parameter'
  readonly ordinal: number
  readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
  readonly type: DeclarationIndex.SemanticType
  readonly offset: number
  readonly size: number
  readonly alignment: number
  readonly padding: number
  readonly representation: 'Value' | 'Borrow' | 'Callable'
  readonly effectIdentity?: string
  readonly callableIdentity?: Type.CallableIdentityArgument
  readonly providedRequirement?: NonNullable<
    CallableFieldRealization.EffectEnvironmentSlot['providedRequirement']
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

export interface CallableEnvironmentField {
  readonly ordinal: number
  readonly parameterOrdinal: number
  readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
  readonly type: DeclarationIndex.SemanticType
  readonly offset: number
  readonly size: number
  readonly alignment: number
  readonly padding: number
  readonly representation: 'Value' | 'Borrow'
}

/** The ephemeral target-local pair passed at indirect callable application. */
export interface CallableView {
  readonly codeOffset: 0
  readonly environmentOffset: number
  readonly size: number
  readonly alignment: number
  readonly pointerBits: 32 | 64
}

/** A target-owned verdict for one reachable exact contextual `usize` literal. */
export type UsizeLiteralVerdict =
  | {
      readonly _tag: 'AvailableUsizeLiteral'
      readonly value: bigint
      readonly bits: 32 | 64
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'UnavailableUsizeLiteral'
      readonly value: bigint
      readonly bits: 32 | 64
      readonly span: SourceSpan.SourceSpan
      readonly cause: Diagnostic.Identity
    }

/** One compiler-owned scalar lane used to realize a logical value at a call boundary. */
export interface CallingLane {
  readonly _tag: 'CallingLane'
  readonly path: ReadonlyArray<Selector>
  readonly type: CallingScalar
}

export interface AddressScalar {
  readonly _tag: 'Address'
  readonly element: DeclarationIndex.SemanticType
  readonly bits: 32 | 64
}

export type CallingScalar = Type.Builtin | AddressScalar

export type Selector =
  | DeclarationIndex.FieldId
  | { readonly _tag: 'ElementSelector'; readonly index: number }
  | { readonly _tag: 'CallableCaptureSelector'; readonly ordinal: number }
  | { readonly _tag: 'EffectCaptureSelector'; readonly ordinal: number }
  | { readonly _tag: 'UnionTagSelector' }
  | { readonly _tag: 'UnionPayloadSelector'; readonly slot: number }
  | { readonly _tag: 'SliceAddressSelector' }
  | { readonly _tag: 'SliceLengthSelector' }
  | { readonly _tag: 'StringStorageSelector' }
  | { readonly _tag: 'StringByteLengthSelector' }
  | { readonly _tag: 'ReferenceAddressSelector' }

export type CallingShapeNode =
  | { readonly _tag: 'EmptyShape'; readonly type: Type.Bottom; readonly laneCount: 0 }
  | { readonly _tag: 'ScalarShape'; readonly type: Type.Builtin; readonly laneCount: 1 }
  | {
      readonly _tag: 'ProductShape'
      readonly type: Type.Nominal
      readonly fields: ReadonlyArray<{
        readonly field: DeclarationIndex.FieldId
        readonly shape: CallingShapeNode
      }>
      readonly laneCount: number
    }
  | {
      readonly _tag: 'RepeatedShape'
      readonly type: Type.FixedArray
      readonly length: number
      readonly element: CallingShapeNode
      readonly laneCount: number
    }
  | {
      readonly _tag: 'SliceShape'
      readonly type: Type.Slice
      readonly address: { readonly type: AddressScalar; readonly lane: 0 }
      readonly length: { readonly type: 'usize'; readonly lane: 1 }
      readonly laneCount: 2
    }
  | {
      readonly _tag: 'StringShape'
      readonly type: Type.String
      readonly storage: { readonly type: AddressScalar; readonly lane: 0 }
      readonly byteLength: { readonly type: 'usize'; readonly lane: 1 }
      readonly laneCount: 2
    }
  | {
      readonly _tag: 'ReferenceShape'
      readonly type: Type.Reference
      readonly address: { readonly type: AddressScalar; readonly lane: 0 }
      readonly laneCount: 1
    }
  | {
      readonly _tag: 'AddressShape'
      readonly type: DeclarationIndex.SemanticType
      readonly address: { readonly type: AddressScalar; readonly lane: 0 }
      readonly laneCount: 1
    }
  | {
      readonly _tag: 'CallableEnvironmentShape'
      readonly type: DeclarationIndex.SemanticType
      readonly fields: ReadonlyArray<{
        readonly capture: number
        readonly shape: CallingShapeNode
      }>
      readonly laneCount: number
    }
  | {
      readonly _tag: 'EffectEnvironmentShape'
      readonly type: DeclarationIndex.SemanticType
      readonly fields: ReadonlyArray<{
        readonly capture: number
        readonly shape: CallingShapeNode
      }>
      readonly laneCount: number
    }
  | {
      readonly _tag: 'SumShape'
      readonly type: Type.StructuralUnion
      readonly tag: { readonly type: 'i32'; readonly lane: 0 }
      readonly payloadLaneCount: number
      readonly payloadTypes: ReadonlyArray<Type.Builtin>
      readonly zeroFill: true
      readonly members: ReadonlyArray<{
        readonly member: Type.Type
        readonly ordinal: number
        readonly shape: CallingShapeNode
        readonly payloadSlots: ReadonlyArray<number>
      }>
      readonly laneCount: number
    }
  | {
      readonly _tag: 'OutcomeShape'
      readonly type: Type.Effect
      readonly success: CallingShapeNode
      readonly failures: ReadonlyArray<{
        readonly type: Type.Type
        readonly tag: number
        readonly shape: CallingShapeNode
      }>
      readonly payloadLaneCount: number
      readonly payloadTypes: ReadonlyArray<Type.Builtin>
      readonly laneCount: number
    }

/** The deterministic backend-neutral calling shape of one reachable logical type. */
export interface CallingShape {
  readonly _tag: 'CallingShape'
  readonly type: DeclarationIndex.SemanticType
  readonly tree: CallingShapeNode
  readonly laneCount: number
  /** Materialized only when a consumer explicitly requests physical lanes. */
  readonly lanes: ReadonlyArray<CallingLane>
}

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
    | 'InvalidCallingShape'
    | 'InvalidLiteralVerdict'
    | 'CatalogMismatch'
  readonly type?: DeclarationIndex.SemanticType
  readonly detail: string
}

const scalarEntry = (target: Target.Target, type: Type.Builtin): Entry => {
  const scalar = Scalar.find(type)
  if (scalar === undefined) throw new RangeError(`Layout lost scalar catalog entry for ${type}`)
  const layout = Scalar.resolveLayout(scalar, target.pointerSize, target.pointerAlignment)
  const bits = Scalar.bits(scalar, target.pointerSize === 4 ? 32 : 64)
  const representation: Representation =
    scalar.category === 'Boolean'
      ? Object.freeze({ _tag: 'Boolean', bits: 32, falseValue: 0, trueValue: 1 })
      : scalar.category === 'Floating'
        ? Object.freeze({ _tag: 'Floating', bits: bits as 32 | 64, ieee: true })
        : scalar.signedness === 'Signed'
          ? Object.freeze({ _tag: 'SignedInteger', bits })
          : Object.freeze({ _tag: 'UnsignedInteger', bits })
  return Object.freeze({
    _tag: 'LayoutEntry',
    type,
    size: layout.size,
    alignment: layout.alignment,
    representation,
  })
}

const alignUp = (offset: number, alignment: number): number =>
  Math.ceil(offset / alignment) * alignment

const repeatedEntry = (type: Type.FixedArray, element: Entry): Entry | undefined => {
  const stride = alignUp(element.size, element.alignment)
  const size = stride * type.length
  if (!Number.isSafeInteger(stride) || !Number.isSafeInteger(size)) return undefined
  return Object.freeze({
    _tag: 'LayoutEntry',
    type,
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

const sliceEntry = (target: Target.Target, type: Type.Slice, element: Entry): Entry => {
  const addressBits: 32 | 64 = target.pointerSize === 4 ? 32 : 64
  const lengthOffset = alignUp(target.pointerSize, target.pointerAlignment)
  const alignment = target.pointerAlignment
  const contentSize = lengthOffset + target.pointerSize
  const size = alignUp(contentSize, alignment)
  return Object.freeze({
    _tag: 'LayoutEntry',
    type,
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

const stringEntry = (target: Target.Target): Entry => {
  const addressBits: 32 | 64 = target.pointerSize === 4 ? 32 : 64
  const byteLengthOffset = alignUp(target.pointerSize, target.pointerAlignment)
  const alignment = target.pointerAlignment
  const contentSize = byteLengthOffset + target.pointerSize
  const size = alignUp(contentSize, alignment)
  return Object.freeze({
    _tag: 'LayoutEntry',
    type: Type.string,
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

const referenceEntry = (target: Target.Target, type: Type.Reference): Entry =>
  Object.freeze({
    _tag: 'LayoutEntry',
    type,
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

const unionEntry = (type: Type.StructuralUnion, members: ReadonlyArray<Entry>): Entry => {
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
const neverEntry = (): Entry =>
  Object.freeze({
    _tag: 'LayoutEntry',
    type: 'never',
    size: 0,
    alignment: 1,
    representation: Object.freeze({
      _tag: 'Aggregate',
      fields: Object.freeze([]),
      tailPadding: 0,
    }),
  })

const nominalOf = (struct: DeclarationIndex.StructFact): Type.Nominal | undefined =>
  struct.canonical._tag === 'Canonical'
    ? Type.nominal(struct.canonical.id.module, struct.canonical.id.name)
    : undefined

const dependenciesOf = (
  struct: DeclarationIndex.StructFact,
  substitution: Type.Substitution = new Map(),
): ReadonlyArray<Type.Nominal> => {
  const dependencies = new Map<string, Type.Nominal>()
  for (const field of struct.fields) {
    const types =
      field.declaredType._tag === 'Resolved'
        ? Type.nominals(Type.substitute(field.declaredType.type, substitution))
        : field.declaredType._tag === 'Unresolved' && field.declaredType.candidate !== undefined
          ? [field.declaredType.candidate]
          : []
    for (const type of types) dependencies.set(Type.key(type), type)
  }
  return Object.freeze([...dependencies.values()].sort(Type.compare))
}

const unavailable = (
  type: DeclarationIndex.SemanticType,
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
): Catalog => {
  const declarations = index.modules
    .flatMap((module) => module.structs)
    .flatMap((struct) => {
      const type = nominalOf(struct)
      return type === undefined ? [] : [Object.freeze({ struct, type })]
    })
    .sort((left, right) => Type.compare(left.type, right.type))
  const byType = new Map(
    declarations.map((declaration) => [
      `${declaration.type.module}\u0000${declaration.type.name}`,
      declaration,
    ]),
  )
  const completed = new Map<string, CatalogEntry>()
  const visiting = new Set<string>()
  const callableRealizations =
    discovery === undefined ? undefined : Instances.callableFieldRealizations(discovery, index)

  interface InlineEnvironmentLayout {
    readonly fields: ReadonlyArray<StoredEffectEnvironmentField>
    readonly size: number
    readonly alignment: number
    readonly tailPadding: number
  }

  const layoutEffectSlots = (
    slots: ReadonlyArray<CallableFieldRealization.EffectEnvironmentSlot>,
    active: ReadonlySet<string>,
  ): InlineEnvironmentLayout | undefined => {
    let cursor = 0
    let environmentAlignment = 1
    const fields: Array<StoredEffectEnvironmentField> = []
    for (const slot of slots) {
      const nestedEffect =
        slot.effectIdentity === undefined
          ? undefined
          : discovery?.effects.find(
              (candidate) =>
                candidate.identity === slot.effectIdentity ||
                candidate.representationIdentity === slot.effectIdentity,
            )
      const callableIdentity = slot.callableIdentity
      const nestedCallable =
        callableIdentity === undefined
          ? undefined
          : discovery?.callables.find((candidate) =>
              CallableFieldRealization.matchesIdentity(callableIdentity, candidate),
            )
      const stableDescriptor = Type.isSlice(slot.type) || Type.isReference(slot.type)
      const borrowed =
        (slot.access === 'Shared' || slot.access === 'Exclusive') &&
        nestedEffect === undefined &&
        nestedCallable === undefined &&
        !stableDescriptor
      let nestedLayout: { readonly size: number; readonly alignment: number } | undefined
      if (nestedEffect !== undefined) {
        if (active.has(nestedEffect.identity)) return undefined
        nestedLayout = layoutEffectSlots(
          CallableFieldRealization.effectEnvironmentOf(nestedEffect),
          new Set([...active, nestedEffect.identity]),
        )
      } else if (nestedCallable !== undefined) {
        let callableCursor = 0
        let callableAlignment = 1
        for (const capture of nestedCallable.captures) {
          const captureBorrowed = capture.access === 'Shared' || capture.access === 'Exclusive'
          const captureLayout = captureBorrowed ? undefined : layoutType(capture.type)
          if (captureLayout?._tag === 'UnavailableLayoutEntry') return undefined
          const size = captureBorrowed ? target.pointerSize : (captureLayout?.size ?? 0)
          const alignment = captureBorrowed
            ? target.pointerAlignment
            : (captureLayout?.alignment ?? 1)
          callableCursor = alignUp(callableCursor, alignment) + size
          callableAlignment = Math.max(callableAlignment, alignment)
        }
        nestedLayout = Object.freeze({
          size: alignUp(callableCursor, callableAlignment),
          alignment: callableAlignment,
        })
      } else if (!borrowed) {
        const candidate = layoutType(slot.type)
        if (candidate._tag === 'UnavailableLayoutEntry') return undefined
        nestedLayout = candidate
      }
      const size = borrowed ? target.pointerSize : (nestedLayout?.size ?? 0)
      const alignment = borrowed ? target.pointerAlignment : (nestedLayout?.alignment ?? 1)
      const offset = alignUp(cursor, alignment)
      fields.push(
        Object.freeze({
          capture: slot.ordinal,
          source: slot.source,
          ordinal: slot.sourceOrdinal,
          access: slot.access,
          type: nestedEffect?.type ?? slot.type,
          offset,
          size,
          alignment,
          padding: offset - cursor,
          representation: borrowed ? 'Borrow' : nestedCallable === undefined ? 'Value' : 'Callable',
          ...(slot.effectIdentity === undefined ? {} : { effectIdentity: slot.effectIdentity }),
          ...(slot.callableIdentity === undefined
            ? {}
            : { callableIdentity: slot.callableIdentity }),
          ...(slot.providedRequirement === undefined
            ? {}
            : { providedRequirement: slot.providedRequirement }),
        }),
      )
      cursor = offset + size
      environmentAlignment = Math.max(environmentAlignment, alignment)
    }
    const size = alignUp(cursor, environmentAlignment)
    return Object.freeze({
      fields: Object.freeze(fields),
      size,
      alignment: environmentAlignment,
      tailPadding: size - cursor,
    })
  }

  const layoutRepresentedCallable = (
    type: Type.Represented,
    realization: CallableFieldRealization.CallableRealization,
  ): CatalogEntry => {
    const key = Type.key(type)
    const existing = completed.get(key)
    if (existing !== undefined) return existing
    let cursor = 0
    let environmentAlignment = 1
    const fields: Array<CallableEnvironmentField> = []
    for (const capture of realization.captures) {
      const borrowed = capture.access === 'Shared' || capture.access === 'Exclusive'
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
      const offset = alignUp(cursor, alignment)
      fields.push(
        Object.freeze({
          ordinal: capture.ordinal,
          parameterOrdinal: capture.parameterOrdinal,
          access: capture.access,
          type: capture.type,
          offset,
          size,
          alignment,
          padding: offset - cursor,
          representation: borrowed ? 'Borrow' : 'Value',
        }),
      )
      cursor = offset + size
      environmentAlignment = Math.max(environmentAlignment, alignment)
    }
    const size = alignUp(cursor, environmentAlignment)
    const result: Entry = Object.freeze({
      _tag: 'LayoutEntry',
      type,
      size,
      alignment: environmentAlignment,
      representation: Object.freeze({
        _tag: 'CallableEnvironment',
        realization,
        fields: Object.freeze(fields),
        tailPadding: size - cursor,
      }),
    })
    completed.set(key, result)
    return result
  }

  const layoutRepresentedEffect = (
    type: Type.Represented,
    realization: CallableFieldRealization.EffectRealization,
  ): CatalogEntry => {
    const key = Type.key(type)
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
    const key = Type.key(type)
    const existing = completed.get(key)
    if (existing !== undefined) return existing
    if (Type.isIntrinsicNominal(type) || Type.equals(type, Type.unit)) {
      const ordinal = Type.equals(type, Type.unit)
        ? Type.intrinsicNominals.size
        : Type.intrinsicNominalOrdinal(type)
      const structId: DeclarationIndex.DeclarationId = Object.freeze({
        _tag: 'DeclarationId',
        sourceId: type.module,
        ordinal,
      })
      const fieldTypes: ReadonlyArray<readonly [string, Type.Type]> = Type.equals(type, Type.layout)
        ? Object.freeze([
            Object.freeze(['bytes', 'usize'] as const),
            Object.freeze(['alignment', 'usize'] as const),
          ])
        : Type.equals(type, Type.invalidAlignment)
          ? Object.freeze([Object.freeze(['alignment', 'usize'] as const)])
          : Type.equals(type, Type.allocation)
            ? Object.freeze([
                Object.freeze(['$base', 'usize'] as const),
                Object.freeze(['$bytes', 'usize'] as const),
                Object.freeze(['$alignment', 'usize'] as const),
                Object.freeze(['$reclaim', 'usize'] as const),
                Object.freeze(['$context', 'usize'] as const),
                Object.freeze(['$active', 'usize'] as const),
              ])
            : Type.equals(type, Type.osHandle)
              ? Object.freeze([
                  Object.freeze(['$identity', 'usize'] as const),
                  Object.freeze(['$kind', 'i32'] as const),
                  Object.freeze(['$active', 'i32'] as const),
                ])
              : Type.isRawBuffer(type)
                ? Object.freeze([
                    Object.freeze(['$allocation', Type.allocation] as const),
                    Object.freeze(['count', 'usize'] as const),
                  ])
                : Type.isSlot(type)
                  ? Object.freeze([Object.freeze(['$address', 'usize'] as const)])
                  : Object.freeze([])
      let cursor = 0
      const fields: Array<Field> = []
      for (const [fieldOrdinal, [name, fieldType]] of fieldTypes.entries()) {
        const fieldLayout = Type.isBuiltin(fieldType)
          ? scalarEntry(target, fieldType)
          : Type.isNominal(fieldType)
            ? layoutNominal(fieldType)
            : undefined
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
        const previous = cursor
        const offset = alignUp(cursor, fieldLayout.alignment)
        cursor = offset + fieldLayout.size
        fields.push(
          Object.freeze({
            _tag: 'LayoutField',
            id: Object.freeze({ _tag: 'FieldId', struct: structId, ordinal: fieldOrdinal }),
            name,
            type: fieldType,
            offset,
            size: fieldLayout.size,
            alignment: fieldLayout.alignment,
            padding: offset - previous,
          }),
        )
      }
      const alignment = fields.reduce((maximum, field) => Math.max(maximum, field.alignment), 1)
      const size = alignUp(cursor, alignment)
      const entry: Entry = Object.freeze({
        _tag: 'LayoutEntry',
        type,
        size,
        alignment,
        representation: Object.freeze({
          _tag: 'Aggregate',
          fields: Object.freeze(fields),
          tailPadding: size - cursor,
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
    const substitution = Type.substitution(parameters, type.arguments)
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
    const fields: Array<Field> = []
    let cursor = 0
    let aggregateAlignment = 1
    let failure: UnavailableEntry | undefined
    for (const field of declaration.struct.fields) {
      if (field.state._tag !== 'Unique' || field.name._tag !== 'Present') {
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
        failure = unavailable(
          type,
          dependencies,
          {
            _tag: 'UnavailableField',
            field: field.id,
            detail: 'field type is unavailable',
          },
          field.declaredType._tag === 'Unresolved'
            ? field.declaredType.cause
            : field.declaredType._tag === 'Resolved'
              ? field.declaredType.exposureCause
              : undefined,
        )
        break
      }
      const fieldType = Type.substitute(field.declaredType.type, substitution)
      const representationPlans = RepresentationField.plansOf(index, type).filter(
        (plan) => plan.id.ordinal === field.id.ordinal,
      )
      let representationOrdinal = 0
      const layoutFieldType = (candidate: DeclarationIndex.SemanticType): CatalogEntry => {
        if (Type.isRepresented(candidate)) {
          const plan = representationPlans.at(representationOrdinal)
          representationOrdinal += 1
          const realization =
            plan === undefined || callableRealizations === undefined
              ? undefined
              : CallableFieldRealization.realizationOf(callableRealizations, type, plan.id)
          if (realization === undefined) {
            return unavailable(candidate, Object.freeze(Type.nominals(candidate)), {
              _tag: 'InvalidDeclaration',
              detail: 'represented executable values remain unavailable to layout',
            })
          }
          return CallableFieldRealization.isCallableRealization(realization)
            ? layoutRepresentedCallable(candidate, realization)
            : layoutRepresentedEffect(candidate, realization)
        }
        if (Type.isFixedArray(candidate)) {
          const element = layoutFieldType(candidate.element)
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
          const element = layoutFieldType(candidate.element)
          return element._tag === 'UnavailableLayoutEntry'
            ? element
            : sliceEntry(target, candidate, element)
        }
        return layoutType(candidate)
      }
      const fieldLayout = layoutFieldType(fieldType)
      if (fieldLayout._tag === 'UnavailableLayoutEntry') {
        failure = unavailable(
          type,
          dependencies,
          { _tag: 'UnavailableDependency', dependency: fieldType },
          fieldLayout.cause,
        )
        break
      }
      const offset = alignUp(cursor, fieldLayout.alignment)
      fields.push(
        Object.freeze({
          _tag: 'LayoutField',
          id: field.id,
          name: field.name.spelling,
          type: fieldType,
          offset,
          size: fieldLayout.size,
          alignment: fieldLayout.alignment,
          padding: offset - cursor,
        }),
      )
      cursor = offset + fieldLayout.size
      aggregateAlignment = Math.max(aggregateAlignment, fieldLayout.alignment)
    }
    visiting.delete(key)
    if (failure !== undefined) {
      completed.set(key, failure)
      return failure
    }
    const size = alignUp(cursor, aggregateAlignment)
    const cleanup = Ownership.cleanupPlan(index, type)
    const entry: Entry = Object.freeze({
      _tag: 'LayoutEntry',
      type,
      size,
      alignment: aggregateAlignment,
      representation: Object.freeze({
        _tag: 'Aggregate',
        fields: Object.freeze(fields),
        tailPadding: size - cursor,
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

  const layoutType = (type: DeclarationIndex.SemanticType): CatalogEntry => {
    if (Type.isBuiltin(type)) return scalarEntry(target, type)
    if (Type.isString(type)) {
      const result = stringEntry(target)
      completed.set(Type.key(type), result)
      return result
    }
    if (Type.isNever(type)) {
      const result = neverEntry()
      completed.set(Type.key(type), result)
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
      const key = Type.key(type)
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
      completed.set(Type.key(type), result)
      return result
    }
    const key = Type.key(type)
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
      const result = unavailable(type, Object.freeze(Type.nominals(type)), {
        _tag: 'InvalidDeclaration',
        detail: 'represented executable values remain unavailable to layout',
      })
      completed.set(key, result)
      return result
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

  const referenced = new Map<string, DeclarationIndex.SemanticType>()
  const addReferenced = (type: DeclarationIndex.SemanticType): void => {
    if (!Type.isRuntimeConcrete(type)) return
    referenced.set(Type.key(type), type)
    if (Type.isFixedArray(type)) addReferenced(type.element)
    if (Type.isSlice(type)) addReferenced(type.element)
    else if (Type.isReference(type)) addReferenced(type.target)
    if (Type.isUnion(type)) for (const member of type.members) addReferenced(member)
    if (Type.isEffect(type)) {
      addReferenced(type.success)
      for (const failure of Type.failureMembers(type)) addReferenced(failure)
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
      } else if (member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration') {
        for (const operation of member.operations) {
          for (const parameter of operation.parameters)
            if (parameter.declaredType._tag === 'Resolved')
              addReferenced(parameter.declaredType.type)
          if (operation.returnType._tag === 'Resolved') addReferenced(operation.returnType.type)
        }
      } else if (member._tag === 'ConstantDeclaration' && member.declaredType._tag === 'Resolved') {
        addReferenced(member.declaredType.type)
      }
    }
  }
  for (const declaration of declarations) {
    if (declaration.struct.typeParameters.length === 0) layoutNominal(declaration.type)
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
      for (const child of Hir.expressionTree(expression).slice(1)) {
        if (child._tag !== 'Unavailable') addReferenced(Type.substitute(child.type, substitution))
      }
      for (const child of Hir.expressionTree(expression)) {
        if (child._tag === 'BuiltinCall') {
          for (const argument of child.typeArguments)
            addReferenced(Type.substitute(argument, substitution))
        }
        if (child._tag === 'EffectCatch' && child.protected._tag !== 'Unavailable') {
          const protected_ = Type.substitute(child.protected.type, substitution)
          if (Type.isEffect(protected_))
            addReferenced(
              Type.result(protected_.success, Type.failureValue(Type.failureMembers(protected_))),
            )
        }
      }
    }
    for (const statement of instance.function.statements) {
      for (const expression of Hir.statementExpressions(statement))
        addSpecializedExpression(expression)
    }
  }
  for (const type of referenced.values()) {
    if (!Type.isBuiltin(type)) layoutType(type)
  }

  return Object.freeze({
    _tag: 'LayoutCatalog',
    target,
    entries: Object.freeze(
      [...completed.values()].sort((left, right) => Type.compare(left.type, right.type)),
    ),
    usizeConstants: Object.freeze(
      index.modules.flatMap((module) =>
        module.constants.flatMap((constant) => {
          if (constant.declaredType._tag !== 'Resolved' || constant.declaredType.type !== 'usize')
            return []
          // A pointer-width fact is ranged at the target it selects for, so it is checked against
          // that value rather than against the widest one elaboration recorded.
          const literal = constant.literal
          if (literal._tag !== 'IntegerLiteral' && literal._tag !== 'TargetConstant') return []
          const value =
            literal._tag === 'IntegerLiteral'
              ? literal.value
              : TargetConstant.value(literal.selector, TargetConstant.pointerBits(target))
          return [Object.freeze({ value, span: literal.token.span })]
        }),
      ),
    ),
  })
}

const addExpressionTypes = (
  types: Map<string, DeclarationIndex.SemanticType>,
  expression: Hir.Expression,
  substitution: Type.Substitution = new Map(),
): void => {
  if (expression._tag === 'Unavailable') return
  const specialized = Type.substitute(expression.type, substitution)
  types.set(Type.key(specialized), specialized)
  if (expression._tag === 'BuiltinCall') {
    for (const argument of expression.typeArguments) {
      const type = Type.substitute(argument, substitution)
      types.set(Type.key(type), type)
    }
  }
  if (expression._tag === 'Move') addExpressionTypes(types, expression.subject, substitution)
  if (expression._tag === 'RuntimeStringView')
    addExpressionTypes(types, expression.source, substitution)
  if (expression._tag === 'ShortCircuit') {
    addExpressionTypes(types, expression.left, substitution)
    addExpressionTypes(types, expression.right, substitution)
  }
  if (expression._tag === 'StringEquality') {
    addExpressionTypes(types, expression.left, substitution)
    addExpressionTypes(types, expression.right, substitution)
  }
  if (expression._tag === 'UnionConvert') addExpressionTypes(types, expression.source, substitution)
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
  if (expression._tag === 'Construct') {
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
    expression._tag === 'BoundOperationCall'
  ) {
    for (const argument of expression.arguments) addExpressionTypes(types, argument, substitution)
    const contract =
      expression._tag === 'BoundOperationCall'
        ? expression.contract
        : expression._tag === 'BuiltinCall'
          ? expression.interfaceOperation?.contract
          : undefined
    for (const operand of contract?.operands ?? []) {
      if (operand.type._tag !== 'Resolved') continue
      const type = Type.substitute(operand.type.type, substitution)
      types.set(Type.key(type), type)
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
  if (expression._tag === 'EffectBindRequirement')
    addExpressionTypes(types, expression.protected, substitution)
  if (expression._tag === 'EffectCatch') {
    types.set(Type.key('never'), 'never')
    addExpressionTypes(types, expression.protected, substitution)
    addExpressionTypes(types, expression.handler, substitution)
    if (expression.protected._tag !== 'Unavailable') {
      const protected_ = Type.substitute(expression.protected.type, substitution)
      if (Type.isEffect(protected_)) {
        const reified = Type.result(
          protected_.success,
          Type.failureValue(Type.failureMembers(protected_)),
        )
        types.set(Type.key(reified), reified)
      }
    }
  }
  if (expression._tag === 'Match') {
    addExpressionTypes(types, expression.scrutinee, substitution)
    for (const member of expression.members) {
      const type = Type.substitute(member, substitution)
      types.set(Type.key(type), type)
    }
    for (const arm of expression.arms) {
      if (!arm.reachable) continue
      if (arm.member !== undefined) types.set(Type.key(arm.member), arm.member)
      for (const binding of arm.bindings) types.set(Type.key(binding.type), binding.type)
      if (arm.guard !== undefined) addExpressionTypes(types, arm.guard, substitution)
      addExpressionTypes(types, arm.result, substitution)
    }
  }
}

const addStatementTypes = (
  types: Map<string, DeclarationIndex.SemanticType>,
  statements: ReadonlyArray<Hir.Statement>,
  substitution: Type.Substitution = new Map(),
): void => {
  for (const statement of statements) {
    if (statement._tag === 'Unsafe') addStatementTypes(types, statement.statements, substitution)
    if (statement._tag === 'Bind') addExpressionTypes(types, statement.initializer, substitution)
    if (statement._tag === 'Evaluate') addExpressionTypes(types, statement.expression, substitution)
    if (statement._tag === 'Return') addExpressionTypes(types, statement.expression, substitution)
    if (statement._tag === 'Fail' || statement._tag === 'Drop')
      addExpressionTypes(types, statement.expression, substitution)
    if (statement._tag === 'If') {
      addExpressionTypes(types, statement.condition, substitution)
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
  types: Map<string, DeclarationIndex.SemanticType>,
  instance: Instances.Instance,
): void => {
  const fn = instance.function
  const substitution = instance.substitution
  for (const parameter of fn.declaration.parameters) {
    if (parameter.declaredType._tag === 'Resolved') {
      const type = Type.substitute(parameter.declaredType.type, substitution)
      types.set(Type.key(type), type)
    }
  }
  if (fn.declaration.returnType._tag === 'Resolved') {
    const type = Type.substitute(fn.declaration.returnType.type, substitution)
    types.set(Type.key(type), type)
    if (fn.declaration.functionKind === 'Effect') {
      const failures = fn.declaration.failureRow.failures.flatMap((failure) => {
        const specialized = Type.substitute(failure, substitution)
        return Type.isNominal(specialized) ? [specialized] : []
      })
      const requirements = fn.declaration.requirementRow.requirements.flatMap((requirement) => {
        const capability = Type.substitute(requirement.capability, substitution)
        return Type.isNominal(capability) ? [Object.freeze({ ...requirement, capability })] : []
      })
      const outcome = Type.effect(type, failures, 'Shared', requirements)
      types.set(Type.key(outcome), outcome)
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
    entries.map((candidate) => [Type.key(candidate.type), candidate] as const),
  )
  const environments: Array<EffectEnvironment> = []

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
      const bindingTypes = new Map<number, DeclarationIndex.SemanticType>()
      const collectBindings = (statements: ReadonlyArray<Hir.Statement>): void => {
        for (const statement of statements) {
          if (statement._tag === 'Bind' && statement.initializer._tag !== 'Unavailable') {
            bindingTypes.set(
              statement.binding.ordinal,
              Type.substitute(statement.initializer.type, instance.substitution),
            )
          } else if (statement._tag === 'If') {
            collectBindings(statement.taken)
            collectBindings(statement.otherwise)
          } else if (statement._tag === 'While') collectBindings(statement.body)
          else if (statement._tag === 'Unsafe') collectBindings(statement.statements)
          for (const expression of Hir.statementExpressions(statement)) {
            for (const child of Hir.expressionTree(expression)) {
              if (child._tag === 'EffectBlock') collectBindings(child.statements)
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
                      binding: undefined,
                      parameter: undefined,
                    }),
                    Object.freeze({
                      access: 'Take' as const,
                      binding: undefined,
                      parameter: undefined,
                    }),
                  ]),
                }),
              ],
        )
      const effectSites = Object.freeze([
        ...blocks.map((block) =>
          Object.freeze({ site: block.site, type: block.type, captures: block.captures }),
        ),
        ...catchSites,
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
            : CallableFieldRealization.effectEnvironmentOf(effectInstance)
        let effect = structuralEffect
        let cursor = 0
        let environmentAlignment = 1
        let unavailable: string | undefined
        const fields: Array<EffectEnvironmentField> = []
        for (const [captureOrdinal, capture] of block.captures.entries()) {
          const realized = realizedSlots.find((slot) => slot.ordinal === captureOrdinal)
          const source =
            realized?.source ?? (capture.binding === undefined ? 'Parameter' : 'Binding')
          const ordinal =
            realized?.sourceOrdinal ?? capture.binding?.ordinal ?? capture.parameter?.ordinal
          const type =
            realized?.type ??
            (capture.binding === undefined
              ? instance.function.contract._tag === 'Contract' && ordinal !== undefined
                ? instance.function.contract.parameters.at(ordinal)
                : undefined
              : ordinal === undefined
                ? undefined
                : bindingTypes.get(ordinal))
          if (ordinal === undefined || type === undefined) {
            unavailable = `capture ${source.toLowerCase()} has no concrete type`
            break
          }
          const specialized = realized?.type ?? Type.substitute(type, instance.substitution)
          const capturedEffectIdentity =
            realized?.effectIdentity ??
            (Type.isEffect(specialized) && source === 'Parameter'
              ? Instances.parameterEffectIdentity(instance.function, instance.key, ordinal)
              : undefined)
          const capturedEffectInstance =
            capturedEffectIdentity === undefined
              ? undefined
              : discovery.effects.find(
                  (candidate) =>
                    candidate.identity === capturedEffectIdentity ||
                    candidate.representationIdentity === capturedEffectIdentity,
                )
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
                    CallableFieldRealization.matchesIdentity(
                      capturedCallableIdentity,
                      candidate.callable,
                    ),
                )
          const fieldType =
            capturedEffectEnvironment?.effect ??
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
          // Slice and reference values are already stable borrow descriptors. Capturing their
          // descriptor inline preserves the underlying loan without retaining a pointer to the
          // effect factory's short-lived stack slot.
          const callable = capturedCallableIdentity !== undefined
          const borrowed =
            (access === 'Shared' || access === 'Exclusive') &&
            capturedEffectEnvironment === undefined &&
            !callable &&
            !Type.isSlice(fieldType) &&
            !Type.isReference(fieldType)
          const valueLayout =
            borrowed || callable
              ? undefined
              : (capturedEffectEnvironment ?? layouts.get(Type.key(fieldType)))
          if (!borrowed && !callable && valueLayout === undefined) {
            unavailable = `capture ${source.toLowerCase()} ${ordinal} has no value layout`
            break
          }
          const size = borrowed
            ? target.pointerSize
            : callable
              ? (capturedCallableEnvironment?.size ?? 0)
              : (valueLayout?.size ?? 0)
          const alignment = borrowed
            ? target.pointerAlignment
            : callable
              ? (capturedCallableEnvironment?.alignment ?? 1)
              : (valueLayout?.alignment ?? 1)
          const offset = alignUp(cursor, alignment)
          fields.push(
            Object.freeze({
              source,
              ordinal,
              access,
              type: fieldType,
              offset,
              size,
              alignment,
              padding: offset - cursor,
              representation: borrowed ? 'Borrow' : callable ? 'Callable' : 'Value',
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
          )
          cursor = offset + size
          environmentAlignment = Math.max(environmentAlignment, alignment)
        }
        if (unavailable === undefined) {
          const access = fields.some((field) => field.access === 'Take')
            ? 'Take'
            : fields.some((field) => field.access === 'Exclusive')
              ? 'Exclusive'
              : 'Shared'
          effect = Type.effectWithRows(
            structuralEffect.success,
            structuralEffect.failureRow,
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
        const size = alignUp(cursor, environmentAlignment)
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
            fields: Object.freeze(fields),
            size,
            alignment: environmentAlignment,
            tailPadding: size - cursor,
          }),
        )
      }

      const witnessEffects = instance.function.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .flatMap((expression) => {
          if (expression._tag !== 'BoundOperationCall' && expression._tag !== 'BuiltinCall')
            return []
          if (expression.witnessEffectSite === undefined) return []
          const contract =
            expression._tag === 'BoundOperationCall'
              ? expression.contract
              : expression._tag === 'BuiltinCall'
                ? expression.interfaceOperation?.contract
                : undefined
          return contract === undefined
            ? []
            : [Object.freeze({ expression, contract, site: expression.witnessEffectSite })]
        })
      for (const witness of witnessEffects) {
        const structuralEffect = Type.substitute(witness.expression.type, instance.substitution)
        if (!Type.isEffect(structuralEffect)) continue
        let cursor = 0
        let environmentAlignment = 1
        let unavailable: string | undefined
        const fields: Array<EffectEnvironmentField> = []
        for (const [ordinal, operand] of witness.contract.operands.entries()) {
          if (operand.type._tag !== 'Resolved') {
            unavailable = `interface operand ${ordinal} has no concrete type`
            break
          }
          const fieldType = Type.substitute(operand.type.type, instance.substitution)
          const valueLayout = layouts.get(Type.key(fieldType))
          if (valueLayout === undefined) {
            unavailable = `interface operand ${ordinal} has no value layout`
            break
          }
          const access =
            Type.isReference(fieldType) || Type.isSlice(fieldType) ? fieldType.access : 'Take'
          const offset = alignUp(cursor, valueLayout.alignment)
          fields.push(
            Object.freeze({
              source: 'Parameter',
              ordinal,
              access,
              type: fieldType,
              offset,
              size: valueLayout.size,
              alignment: valueLayout.alignment,
              padding: offset - cursor,
              representation: 'Value',
            }),
          )
          cursor = offset + valueLayout.size
          environmentAlignment = Math.max(environmentAlignment, valueLayout.alignment)
        }
        const access = fields.some((field) => field.access === 'Take')
          ? 'Take'
          : fields.some((field) => field.access === 'Exclusive')
            ? 'Exclusive'
            : 'Shared'
        const effect = Type.effectWithRows(
          structuralEffect.success,
          structuralEffect.failureRow,
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
        const size = alignUp(cursor, environmentAlignment)
        environments.push(
          Object.freeze({
            _tag: 'EffectEnvironment',
            instance: instance.key,
            site: witness.site,
            effect,
            fields: Object.freeze(fields),
            size,
            alignment: environmentAlignment,
            tailPadding: size - cursor,
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
  const layouts = new Map(entries.map((entry) => [Type.key(entry.type), entry] as const))
  const view = callableView(target)
  return Object.freeze(
    discovery.callables.map((callable): CallableEnvironment => {
      let cursor = 0
      let environmentAlignment = 1
      const fields: Array<CallableEnvironmentField> = []
      for (const capture of callable.captures) {
        const borrowed = capture.access === 'Shared' || capture.access === 'Exclusive'
        const valueLayout = borrowed ? undefined : layouts.get(Type.key(capture.type))
        if (!borrowed && valueLayout === undefined) {
          return Object.freeze({
            _tag: 'UnavailableCallableEnvironment',
            callable,
            reason: `capture ${capture.ordinal} has no concrete value layout`,
            view,
          })
        }
        const size = borrowed ? target.pointerSize : (valueLayout?.size ?? 0)
        const alignment = borrowed ? target.pointerAlignment : (valueLayout?.alignment ?? 1)
        const offset = alignUp(cursor, alignment)
        fields.push(
          Object.freeze({
            ordinal: capture.ordinal,
            parameterOrdinal: capture.parameterOrdinal,
            access: capture.access,
            type: capture.type,
            offset,
            size,
            alignment,
            padding: offset - cursor,
            representation: borrowed ? 'Borrow' : 'Value',
          }),
        )
        cursor = offset + size
        environmentAlignment = Math.max(environmentAlignment, alignment)
      }
      const size = alignUp(cursor, environmentAlignment)
      return Object.freeze({
        _tag: 'CallableEnvironment',
        callable,
        fields: Object.freeze(fields),
        size,
        alignment: environmentAlignment,
        tailPadding: size - cursor,
        view,
      })
    }),
  )
}

const usizeLiteralVerdicts = (
  target: Target.Target,
  discovery: Instances.Discovery,
  constants: ReadonlyArray<UsizeConstantLiteral>,
): {
  readonly verdicts: ReadonlyArray<UsizeLiteralVerdict>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const bits: 32 | 64 = target.pointerSize === 4 ? 32 : 64
  const maximum = bits === 32 ? 4294967295n : 18446744073709551615n
  const verdicts: Array<UsizeLiteralVerdict> = []
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const seen = new Set<string>()
  const add = (value: bigint, span: SourceSpan.SourceSpan): void => {
    const key = `${span.sourceId}:${span.start}:${span.end}:${value}`
    if (seen.has(key)) return
    seen.add(key)
    if (value <= maximum) {
      verdicts.push(
        Object.freeze({
          _tag: 'AvailableUsizeLiteral',
          value,
          bits,
          span,
        }),
      )
      return
    }
    const diagnostic = Diagnostic.usizeTargetOutOfRange(value.toString(), target.id, bits, span)
    diagnostics.push(diagnostic)
    verdicts.push(
      Object.freeze({
        _tag: 'UnavailableUsizeLiteral',
        value,
        bits,
        span,
        cause: Diagnostic.identity(diagnostic),
      }),
    )
  }
  for (const constant of constants) add(constant.value, constant.span)
  for (const instance of discovery.instances) {
    const expressions = instance.function.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)
    for (const expression of expressions) {
      if (
        expression._tag !== 'IntegerLiteral' ||
        expression.constant !== undefined ||
        Type.substitute(expression.type, instance.substitution) !== 'usize'
      ) {
        continue
      }
      const value = BigInt(expression.value)
      add(value, expression.span)
    }
  }
  return Object.freeze({
    verdicts: Object.freeze(verdicts),
    diagnostics: Object.freeze(diagnostics),
  })
}

/** Selects runtime-reachable entries while reusing nominal decisions from the catalog. */
export const plan = (self: Catalog, discovery: Instances.Discovery): Plan => {
  const reached = new Map<string, DeclarationIndex.SemanticType>()
  for (const instance of discovery.instances) addFunctionTypes(reached, instance)
  if (
    discovery.entry._tag === 'Resolved' &&
    (discovery.entry.kind === 'Effect' || discovery.entry.result === 'Unit')
  ) {
    reached.set(Type.key('i32'), 'i32')
  }
  for (const callable of discovery.callables) {
    for (const capture of callable.captures) reached.set(Type.key(capture.type), capture.type)
  }
  const entries = new Map<string, Entry>()
  const resolve = (type: DeclarationIndex.SemanticType): Entry | undefined => {
    if (Type.isBuiltin(type)) return scalarEntry(self.target, type)
    if (Type.isString(type)) return stringEntry(self.target)
    if (Type.isNever(type)) return neverEntry()
    const candidate = catalogEntry(self, type)
    if (candidate?._tag === 'LayoutEntry') return candidate
    if (Type.isSlice(type)) {
      if (candidate?._tag === 'UnavailableLayoutEntry') return undefined
      const element = resolve(type.element)
      return element === undefined ? undefined : sliceEntry(self.target, type, element)
    }
    if (Type.isReference(type)) return referenceEntry(self.target, type)
    if (!Type.isFixedArray(type) || candidate?._tag === 'UnavailableLayoutEntry') return undefined
    const element = resolve(type.element)
    return element === undefined ? undefined : repeatedEntry(type, element)
  }
  const add = (type: DeclarationIndex.SemanticType): void => {
    const key = Type.key(type)
    if (Type.isEffect(type)) {
      add(type.success)
      for (const failure of Type.failureMembers(type)) add(failure)
      return
    }
    if (entries.has(key)) return
    const candidate = resolve(type)
    if (candidate === undefined) return
    entries.set(key, candidate)
    if (candidate.representation._tag === 'Aggregate') {
      for (const field of candidate.representation.fields) add(field.type)
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
    [...entries.values()].sort((left, right) => Type.compare(left.type, right.type)),
  )
  const literals = usizeLiteralVerdicts(self.target, discovery, self.usizeConstants)
  const shaped = new Map(orderedEntries.map((entry) => [Type.key(entry.type), entry.type] as const))
  for (const type of reached.values()) {
    if (Type.isRuntimeConcrete(type) && (Type.isEffect(type) || Type.isNever(type)))
      shaped.set(Type.key(type), type)
  }
  const shapeTypes = Object.freeze([...shaped.values()].sort(Type.compare))
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
  const specializedShapeTypes = new Map(shapeTypes.map((type) => [Type.key(type), type] as const))
  for (const environment of effectPlans)
    specializedShapeTypes.set(Type.key(environment.effect), environment.effect)
  return Object.freeze({
    _tag: 'LayoutPlan',
    target: self.target,
    entries: orderedEntries,
    effectEnvironments: effectPlans,
    callableEnvironments: callablePlans,
    callingShapes: callingShapes(
      self.target,
      orderedEntries,
      [...specializedShapeTypes.values()].sort(Type.compare),
      effectPlans,
      callablePlans,
    ),
    staticData,
    literalVerdicts: literals.verdicts,
    diagnostics: literals.diagnostics,
  })
}

/** Constructs a scalar plan for hand-built MIR samples and focused tests. */
export const make = (target: Target.Target, types: ReadonlyArray<Type.Builtin>): Plan => {
  const entries = new Map(types.map((type) => [Type.key(type), scalarEntry(target, type)]))
  const orderedEntries = Object.freeze(
    [...entries.values()].sort((left, right) => Type.compare(left.type, right.type)),
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
  type: DeclarationIndex.SemanticType,
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
  field: EffectEnvironmentField,
): CallingShapeNode => {
  if (field.representation === 'Borrow') return borrowedShape(context, field.type)
  if (field.callableIdentity !== undefined) {
    const identity = field.callableIdentity
    const environment = context.callableEnvironments.find(
      (
        candidate,
      ): candidate is Extract<CallableEnvironment, { readonly _tag: 'CallableEnvironment' }> =>
        candidate._tag === 'CallableEnvironment' &&
        CallableFieldRealization.matchesIdentity(identity, candidate.callable),
    )
    if (environment === undefined)
      throw new RangeError(
        `callable environment ${Type.genericArgumentKey(identity)} is unavailable to calling-shape planning`,
      )
    const nested = withActiveShape(context, `callable:${Type.genericArgumentKey(identity)}`)
    const fields = environment.fields.map((capture) =>
      Object.freeze({
        capture: capture.ordinal,
        shape:
          capture.representation === 'Borrow'
            ? borrowedShape(nested, capture.type)
            : shapeNode(capture.type, nested),
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
  type: DeclarationIndex.SemanticType,
  context: ShapeContext,
): CallingShapeNode => {
  const { target, entries } = context
  if (Type.isBuiltin(type)) {
    return Object.freeze({ _tag: 'ScalarShape', type, laneCount: 1 })
  }
  if (Type.isString(type)) {
    return Object.freeze({
      _tag: 'StringShape',
      type,
      storage: Object.freeze({
        type: Object.freeze({
          _tag: 'Address',
          element: Type.string,
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
  if (Type.isCallable(type)) {
    throw new RangeError(
      `callable ${Type.encode(type)} needs a hidden concrete identity before calling-shape planning`,
    )
  }
  if (Type.isRepresented(type)) {
    const representation = entries.get(Type.key(type))?.representation
    if (
      representation?._tag !== 'CallableEnvironment' &&
      representation?._tag !== 'StoredEffectEnvironment'
    ) {
      throw new RangeError(
        `represented executable ${Type.encode(type)} is unavailable to calling-shape planning`,
      )
    }
    const fields =
      representation._tag === 'CallableEnvironment'
        ? representation.fields.map((field) =>
            Object.freeze({
              capture: field.ordinal,
              shape:
                field.representation === 'Borrow'
                  ? borrowedShape(context, field.type)
                  : shapeNode(field.type, context),
            }),
          )
        : representation.fields.map((field) =>
            Object.freeze({
              capture: field.capture,
              shape: executableEnvironmentFieldShape(context, field),
            }),
          )
    return Object.freeze({
      _tag:
        representation._tag === 'CallableEnvironment'
          ? ('CallableEnvironmentShape' as const)
          : ('EffectEnvironmentShape' as const),
      type,
      fields: Object.freeze(fields),
      laneCount: fields.reduce((total, field) => total + field.shape.laneCount, 0),
    })
  }
  const candidate = entries.get(Type.key(type))
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
    const payloadTypes = Object.freeze(
      Array.from({ length: payloadLaneCount }, (_, slot): Type.Builtin => {
        const candidates = members.flatMap((member) => {
          const lane = materializeLanes(member.shape).at(slot)
          if (lane === undefined) return []
          const candidate: Type.Builtin = typeof lane.type === 'string' ? lane.type : 'usize'
          return [candidate]
        })
        return (
          candidates
            .sort((left, right) => {
              const leftScalar = Scalar.find(left)
              const rightScalar = Scalar.find(right)
              const pointerBits = target.pointerSize === 4 ? 32 : 64
              const leftBits = leftScalar === undefined ? 32 : Scalar.bits(leftScalar, pointerBits)
              const rightBits =
                rightScalar === undefined ? 32 : Scalar.bits(rightScalar, pointerBits)
              return rightBits - leftBits || Type.compare(left, right)
            })
            .at(0) ?? 'i32'
        )
      }),
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
    const payloadTypes = Object.freeze(
      Array.from({ length: payloadLaneCount }, (_, slot): Type.Builtin => {
        const candidates = variants.flatMap((variant) => {
          const lane = materializeLanes(variant).at(slot)
          if (lane === undefined) return []
          const candidate: Type.Builtin = typeof lane.type === 'string' ? lane.type : 'usize'
          return [candidate]
        })
        return (
          candidates
            .sort((left, right) => {
              const leftScalar = Scalar.find(left)
              const rightScalar = Scalar.find(right)
              const pointerBits = target.pointerSize === 4 ? 32 : 64
              const leftBits = leftScalar === undefined ? 32 : Scalar.bits(leftScalar, pointerBits)
              const rightBits =
                rightScalar === undefined ? 32 : Scalar.bits(rightScalar, pointerBits)
              return rightBits - leftBits || Type.compare(left, right)
            })
            .at(0) ?? 'i32'
        )
      }),
    )
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

const materializeLanes = (
  node: CallingShapeNode,
  path: ReadonlyArray<Selector> = Object.freeze([]),
): ReadonlyArray<CallingLane> => {
  if (node._tag === 'EmptyShape') return Object.freeze([])
  if (node._tag === 'ScalarShape') {
    return Object.freeze([Object.freeze({ _tag: 'CallingLane', path, type: node.type })])
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
  const lanes: Array<CallingLane> = []
  for (let index = 0; index < node.length; index += 1) {
    const selector: Selector = Object.freeze({ _tag: 'ElementSelector', index })
    lanes.push(...materializeLanes(node.element, Object.freeze([...path, selector])))
  }
  return Object.freeze(lanes)
}

const shapeOf = (
  target: Target.Target,
  type: DeclarationIndex.SemanticType,
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

const callingShapes = (
  target: Target.Target,
  entries: ReadonlyArray<Entry>,
  types: ReadonlyArray<DeclarationIndex.SemanticType> = entries.map((entry) => entry.type),
  effectEnvironments: ReadonlyArray<EffectEnvironment> = Object.freeze([]),
  callableEnvironments: ReadonlyArray<CallableEnvironment> = Object.freeze([]),
): ReadonlyArray<CallingShape> => {
  const byType = new Map(entries.map((candidate) => [Type.key(candidate.type), candidate]))
  return Object.freeze(
    types.map((type) => shapeOf(target, type, byType, effectEnvironments, callableEnvironments)),
  )
}

/** Looks up one canonical runtime-plan entry. */
export const entry = (self: Plan, type: DeclarationIndex.SemanticType): Entry | undefined =>
  self.entries.find((candidate) => Type.equals(candidate.type, type))

/** Looks up one compiler-owned calling shape by logical type. */
export const callingShape = (
  self: Plan,
  type: DeclarationIndex.SemanticType,
): CallingShape | undefined =>
  self.callingShapes.find((candidate) => Type.equals(candidate.type, type))

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
  sourceType: DeclarationIndex.SemanticType,
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
  if (!Type.equals(sourceMember, targetMember)) return undefined
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
      Type.equalsCallableEnvironmentIdentity(
        Instances.callableEnvironmentIdentity(candidate.callable),
        identity,
      ),
  )

/** Resolves the Effect environment a capture field's identity names, including success carriers. */
export const effectEnvironmentByFieldIdentity = (
  self: Plan,
  identity: string,
): Extract<EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> | undefined =>
  self.effectEnvironments.find(
    (candidate): candidate is Extract<EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> =>
      candidate._tag === 'EffectEnvironment' &&
      (Instances.effectIdentity(candidate.instance, candidate.site) === identity ||
        candidate.successEffectIdentity === identity),
  )

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
  Object.freeze(
    environment.fields.flatMap((field): ReadonlyArray<CallingLane> => {
      if (field.representation === 'Borrow') {
        return [
          Object.freeze({
            _tag: 'CallingLane',
            path: Object.freeze([]),
            type: Object.freeze({
              _tag: 'Address',
              element: field.type,
              bits: self.target.pointerSize === 4 ? 32 : 64,
            }),
          }),
        ]
      }
      return callingShape(self, field.type)?.lanes ?? Object.freeze([])
    }),
  )

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
    const laneCount =
      field.representation === 'Borrow' ? 1 : (callingShape(self, field.type)?.laneCount ?? 0)
    if (field.ordinal === capture)
      return Object.freeze({ laneOffset, laneCount, byteOffset: field.offset })
    laneOffset += laneCount
  }
  return undefined
}

const fieldSlice = (
  node: CallingShapeNode,
  path: ReadonlyArray<DeclarationIndex.FieldId>,
  offset = 0,
): { readonly offset: number; readonly length: number } | undefined => {
  const [field, ...rest] = path
  if (field === undefined) return Object.freeze({ offset, length: node.laneCount })
  if (node._tag !== 'ProductShape') return undefined
  let fieldOffset = offset
  for (const candidate of node.fields) {
    if (
      candidate.field.ordinal === field.ordinal &&
      candidate.field.struct.sourceId === field.struct.sourceId &&
      candidate.field.struct.ordinal === field.struct.ordinal
    ) {
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
  path: ReadonlyArray<DeclarationIndex.FieldId>,
): ReadonlyArray<number> | undefined => {
  const selected =
    shape.tree._tag === 'ProductShape' && Type.equals(shape.tree.type, member)
      ? Object.freeze({ shape: shape.tree, physicalOffset: 0 })
      : shape.tree._tag === 'SumShape'
        ? (() => {
            const candidate = shape.tree.members.find((entry) => Type.equals(entry.member, member))
            return candidate === undefined
              ? undefined
              : Object.freeze({ shape: candidate.shape, physicalOffset: 1 })
          })()
        : undefined
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
  type: DeclarationIndex.SemanticType,
): CatalogEntry | undefined => self.entries.find((candidate) => Type.equals(candidate.type, type))

const representationEquals = (left: Representation, right: Representation): boolean => {
  if (left._tag !== right._tag) return false
  if (left._tag === 'SignedInteger')
    return right._tag === 'SignedInteger' && left.bits === right.bits
  if (left._tag === 'UnsignedInteger')
    return right._tag === 'UnsignedInteger' && left.bits === right.bits
  if (left._tag === 'Floating')
    return right._tag === 'Floating' && left.bits === right.bits && right.ieee
  if (left._tag === 'Boolean') {
    return (
      right._tag === 'Boolean' &&
      left.bits === right.bits &&
      left.falseValue === right.falseValue &&
      left.trueValue === right.trueValue
    )
  }
  if (left._tag === 'CallableEnvironment') {
    return (
      right._tag === 'CallableEnvironment' &&
      CallableFieldRealization.equals(left.realization, right.realization) &&
      left.tailPadding === right.tailPadding &&
      left.fields.length === right.fields.length &&
      left.fields.every((field, ordinal) => {
        const other = right.fields.at(ordinal)
        return (
          other !== undefined &&
          field.ordinal === other.ordinal &&
          field.parameterOrdinal === other.parameterOrdinal &&
          field.access === other.access &&
          Type.equals(field.type, other.type) &&
          field.offset === other.offset &&
          field.size === other.size &&
          field.alignment === other.alignment &&
          field.padding === other.padding &&
          field.representation === other.representation
        )
      })
    )
  }
  if (left._tag === 'StoredEffectEnvironment') {
    return (
      right._tag === 'StoredEffectEnvironment' &&
      CallableFieldRealization.equals(left.realization, right.realization) &&
      left.tailPadding === right.tailPadding &&
      left.fields.length === right.fields.length &&
      left.fields.every((field, ordinal) => {
        const other = right.fields.at(ordinal)
        return (
          other !== undefined &&
          field.capture === other.capture &&
          field.source === other.source &&
          field.ordinal === other.ordinal &&
          field.access === other.access &&
          Type.equals(field.type, other.type) &&
          field.offset === other.offset &&
          field.size === other.size &&
          field.alignment === other.alignment &&
          field.padding === other.padding &&
          field.representation === other.representation &&
          field.effectIdentity === other.effectIdentity &&
          ((field.callableIdentity === undefined && other.callableIdentity === undefined) ||
            (field.callableIdentity !== undefined &&
              other.callableIdentity !== undefined &&
              Type.equalsGenericArgument(field.callableIdentity, other.callableIdentity)))
        )
      })
    )
  }
  if (left._tag === 'Repeated') {
    return (
      right._tag === 'Repeated' &&
      Type.equals(left.element, right.element) &&
      left.length === right.length &&
      left.stride === right.stride
    )
  }
  if (left._tag === 'Slice') {
    return (
      right._tag === 'Slice' &&
      Type.equals(left.element, right.element) &&
      left.address.bits === right.address.bits &&
      left.address.offset === right.address.offset &&
      left.address.size === right.address.size &&
      left.address.alignment === right.address.alignment &&
      left.length.offset === right.length.offset &&
      left.addressPadding === right.addressPadding &&
      left.tailPadding === right.tailPadding &&
      left.stride === right.stride
    )
  }
  if (left._tag === 'String') {
    return (
      right._tag === 'String' &&
      left.storage.provenance === right.storage.provenance &&
      left.storage.bits === right.storage.bits &&
      left.storage.offset === right.storage.offset &&
      left.storage.size === right.storage.size &&
      left.storage.alignment === right.storage.alignment &&
      left.byteLength.type === right.byteLength.type &&
      left.byteLength.offset === right.byteLength.offset &&
      left.byteLength.size === right.byteLength.size &&
      left.storagePadding === right.storagePadding &&
      left.tailPadding === right.tailPadding
    )
  }
  if (left._tag === 'Reference') {
    return (
      right._tag === 'Reference' &&
      Type.equals(left.target, right.target) &&
      left.address.bits === right.address.bits &&
      left.address.offset === right.address.offset &&
      left.address.size === right.address.size &&
      left.address.alignment === right.address.alignment
    )
  }
  if (left._tag === 'Union') {
    return (
      right._tag === 'Union' &&
      left.payloadOffset === right.payloadOffset &&
      left.payloadSize === right.payloadSize &&
      left.payloadAlignment === right.payloadAlignment &&
      left.tagPadding === right.tagPadding &&
      left.tailPadding === right.tailPadding &&
      left.members.length === right.members.length &&
      left.members.every((member, ordinal) => {
        const other = right.members.at(ordinal)
        return (
          other !== undefined &&
          Type.equals(member.type, other.type) &&
          member.ordinal === other.ordinal &&
          member.size === other.size &&
          member.alignment === other.alignment
        )
      })
    )
  }
  const cleanupHooksEqual = (
    leftHook: Extract<Representation, { readonly _tag: 'Aggregate' }>['cleanupHook'],
    rightHook: Extract<Representation, { readonly _tag: 'Aggregate' }>['cleanupHook'],
  ): boolean =>
    leftHook === undefined
      ? rightHook === undefined
      : rightHook !== undefined &&
        leftHook.hook.module === rightHook.hook.module &&
        leftHook.hook.name === rightHook.hook.name &&
        leftHook.typeArguments.length === rightHook.typeArguments.length &&
        leftHook.typeArguments.every((argument, ordinal) => {
          const other = rightHook.typeArguments.at(ordinal)
          return other !== undefined && Type.equalsGenericArgument(argument, other)
        })
  return (
    right._tag === 'Aggregate' &&
    cleanupHooksEqual(left.cleanupHook, right.cleanupHook) &&
    left.tailPadding === right.tailPadding &&
    left.fields.length === right.fields.length &&
    left.fields.every((field, index) => {
      const other = right.fields[index]
      return (
        other !== undefined &&
        field.id.ordinal === other.id.ordinal &&
        field.name === other.name &&
        Type.equals(field.type, other.type) &&
        field.offset === other.offset &&
        field.size === other.size &&
        field.alignment === other.alignment &&
        field.padding === other.padding
      )
    })
  )
}

const invalid = (
  rule: Violation['rule'],
  type: DeclarationIndex.SemanticType,
  detail: string,
): Violation => Object.freeze({ _tag: 'LayoutViolation', rule, type, detail })

const verifyEntry = (
  target: Target.Target,
  candidate: Entry,
  available: ReadonlyMap<string, Entry>,
): ReadonlyArray<Violation> => {
  if (Type.isBuiltin(candidate.type)) {
    const expected = scalarEntry(target, candidate.type)
    return candidate.size === expected.size &&
      candidate.alignment === expected.alignment &&
      representationEquals(candidate.representation, expected.representation)
      ? Object.freeze([])
      : Object.freeze([
          invalid(
            'InvalidScalar',
            candidate.type,
            `${Type.encode(candidate.type)} does not match the canonical scalar layout`,
          ),
        ])
  }
  if (Type.isFixedArray(candidate.type)) {
    const element = Type.isBuiltin(candidate.type.element)
      ? scalarEntry(target, candidate.type.element)
      : available.get(Type.key(candidate.type.element))
    if (element === undefined || candidate.representation._tag !== 'Repeated') {
      return Object.freeze([
        invalid(
          'InvalidAggregate',
          candidate.type,
          `${Type.encode(candidate.type)} has no repeated-element representation`,
        ),
      ])
    }
    const stride = alignUp(element.size, element.alignment)
    const size = stride * candidate.type.length
    return candidate.representation.length === candidate.type.length &&
      Type.equals(candidate.representation.element, candidate.type.element) &&
      candidate.representation.stride === stride &&
      candidate.size === size &&
      candidate.alignment === element.alignment
      ? Object.freeze([])
      : Object.freeze([
          invalid(
            'InvalidAggregate',
            candidate.type,
            `${Type.encode(candidate.type)} has non-canonical repeated layout facts`,
          ),
        ])
  }
  if (Type.isString(candidate.type)) {
    const expected = stringEntry(target)
    return candidate.size === expected.size &&
      candidate.alignment === expected.alignment &&
      representationEquals(candidate.representation, expected.representation)
      ? Object.freeze([])
      : Object.freeze([
          invalid(
            'InvalidAggregate',
            candidate.type,
            'string does not match the canonical UTF-8 storage-provenance layout',
          ),
        ])
  }
  if (Type.isSlice(candidate.type)) {
    const element = Type.isBuiltin(candidate.type.element)
      ? scalarEntry(target, candidate.type.element)
      : available.get(Type.key(candidate.type.element))
    if (element === undefined) {
      return Object.freeze([
        invalid(
          'InvalidAggregate',
          candidate.type,
          `${Type.encode(candidate.type)} has no element layout`,
        ),
      ])
    }
    const expected = sliceEntry(target, candidate.type, element)
    return candidate.size === expected.size &&
      candidate.alignment === expected.alignment &&
      representationEquals(candidate.representation, expected.representation)
      ? Object.freeze([])
      : Object.freeze([
          invalid(
            'InvalidAggregate',
            candidate.type,
            `${Type.encode(candidate.type)} has non-canonical slice layout facts`,
          ),
        ])
  }
  if (Type.isReference(candidate.type)) {
    const expected = referenceEntry(target, candidate.type)
    return candidate.size === expected.size &&
      candidate.alignment === expected.alignment &&
      representationEquals(candidate.representation, expected.representation)
      ? Object.freeze([])
      : Object.freeze([
          invalid(
            'InvalidScalar',
            candidate.type,
            `${Type.encode(candidate.type)} does not match the canonical reference layout`,
          ),
        ])
  }
  if (Type.isUnion(candidate.type)) {
    const members = candidate.type.members.flatMap((member): ReadonlyArray<Entry> => {
      const memberLayout = available.get(Type.key(member))
      return memberLayout === undefined ? [] : [memberLayout]
    })
    if (members.length !== candidate.type.members.length) {
      return Object.freeze([
        invalid(
          'InvalidAggregate',
          candidate.type,
          `${Type.encode(candidate.type)} has unavailable union members`,
        ),
      ])
    }
    const expected = unionEntry(candidate.type, Object.freeze(members))
    return candidate.size === expected.size &&
      candidate.alignment === expected.alignment &&
      representationEquals(candidate.representation, expected.representation)
      ? Object.freeze([])
      : Object.freeze([
          invalid(
            'InvalidAggregate',
            candidate.type,
            `${Type.encode(candidate.type)} has non-canonical union layout facts`,
          ),
        ])
  }
  if (Type.isNever(candidate.type)) {
    const canonical = neverEntry()
    return candidate.size === canonical.size &&
      candidate.alignment === canonical.alignment &&
      representationEquals(candidate.representation, canonical.representation)
      ? Object.freeze([])
      : Object.freeze([
          invalid(
            'InvalidAggregate',
            candidate.type,
            'never must use its zero-sized uninhabited placeholder layout',
          ),
        ])
  }
  if (Type.isRepresented(candidate.type)) {
    if (candidate.representation._tag === 'StoredEffectEnvironment') {
      const violations: Array<Violation> = []
      let cursor = 0
      let alignment = 1
      for (const [ordinal, field] of candidate.representation.fields.entries()) {
        const slot = candidate.representation.realization.environment.at(ordinal)
        const borrowed = field.representation === 'Borrow'
        const executable =
          field.effectIdentity !== undefined || field.callableIdentity !== undefined
        const fieldLayout =
          borrowed || executable
            ? undefined
            : Type.isBuiltin(field.type)
              ? scalarEntry(target, field.type)
              : available.get(Type.key(field.type))
        const expectedSize = borrowed
          ? target.pointerSize
          : executable
            ? field.size
            : fieldLayout?.size
        const expectedAlignment = borrowed
          ? target.pointerAlignment
          : executable
            ? field.alignment
            : fieldLayout?.alignment
        const offset =
          expectedAlignment === undefined ? undefined : alignUp(cursor, expectedAlignment)
        if (
          slot === undefined ||
          slot.ordinal !== field.capture ||
          slot.source !== field.source ||
          slot.sourceOrdinal !== field.ordinal ||
          slot.access !== field.access ||
          !(
            Type.equals(slot.type, field.type) ||
            (field.effectIdentity !== undefined &&
              Type.isEffect(slot.type) &&
              Type.isEffect(field.type) &&
              Type.equals(
                Type.effectWithRows(
                  slot.type.success,
                  slot.type.failureRow,
                  field.type.access,
                  slot.type.requirementRow,
                ),
                field.type,
              ))
          ) ||
          slot.effectIdentity !== field.effectIdentity ||
          (slot.callableIdentity === undefined && field.callableIdentity !== undefined) ||
          (slot.callableIdentity !== undefined &&
            (field.callableIdentity === undefined ||
              !Type.equalsGenericArgument(slot.callableIdentity, field.callableIdentity))) ||
          expectedSize === undefined ||
          expectedAlignment === undefined ||
          offset === undefined ||
          expectedAlignment < 1 ||
          expectedSize < 0 ||
          field.offset !== offset ||
          field.size !== expectedSize ||
          field.alignment !== expectedAlignment ||
          field.padding !== offset - cursor
        ) {
          violations.push(
            invalid(
              'InvalidAggregate',
              candidate.type,
              `Effect capture ${field.capture} has non-canonical physical facts`,
            ),
          )
          continue
        }
        cursor = offset + expectedSize
        alignment = Math.max(alignment, expectedAlignment)
      }
      const size = alignUp(cursor, alignment)
      if (
        candidate.size !== size ||
        candidate.alignment !== alignment ||
        candidate.representation.tailPadding !== size - cursor
      ) {
        violations.push(
          invalid(
            'InvalidAggregate',
            candidate.type,
            `${Type.encode(candidate.type)} has non-canonical stored Effect environment size or alignment`,
          ),
        )
      }
      return Object.freeze(violations)
    }
    if (candidate.representation._tag !== 'CallableEnvironment') {
      return Object.freeze([
        invalid(
          'InvalidAggregate',
          candidate.type,
          `${Type.encode(candidate.type)} has no concrete callable environment`,
        ),
      ])
    }
    const violations: Array<Violation> = []
    let cursor = 0
    let alignment = 1
    for (const [ordinal, field] of candidate.representation.fields.entries()) {
      const capture = candidate.representation.realization.captures.at(ordinal)
      const borrowed = field.representation === 'Borrow'
      const fieldLayout = borrowed
        ? undefined
        : Type.isBuiltin(field.type)
          ? scalarEntry(target, field.type)
          : available.get(Type.key(field.type))
      const expectedSize = borrowed ? target.pointerSize : fieldLayout?.size
      const expectedAlignment = borrowed ? target.pointerAlignment : fieldLayout?.alignment
      const offset =
        expectedAlignment === undefined ? undefined : alignUp(cursor, expectedAlignment)
      if (
        capture === undefined ||
        capture.ordinal !== field.ordinal ||
        capture.parameterOrdinal !== field.parameterOrdinal ||
        capture.access !== field.access ||
        !Type.equals(capture.type, field.type) ||
        expectedSize === undefined ||
        expectedAlignment === undefined ||
        offset === undefined ||
        field.offset !== offset ||
        field.size !== expectedSize ||
        field.alignment !== expectedAlignment ||
        field.padding !== offset - cursor
      ) {
        violations.push(
          invalid(
            'InvalidAggregate',
            candidate.type,
            `callable capture ${field.ordinal} has non-canonical physical facts`,
          ),
        )
        continue
      }
      cursor = offset + expectedSize
      alignment = Math.max(alignment, expectedAlignment)
    }
    const size = alignUp(cursor, alignment)
    if (
      candidate.size !== size ||
      candidate.alignment !== alignment ||
      candidate.representation.tailPadding !== size - cursor
    ) {
      violations.push(
        invalid(
          'InvalidAggregate',
          candidate.type,
          `${Type.encode(candidate.type)} has non-canonical callable environment size or alignment`,
        ),
      )
    }
    return Object.freeze(violations)
  }
  if (candidate.representation._tag !== 'Aggregate') {
    return Object.freeze([
      invalid(
        'InvalidAggregate',
        candidate.type,
        `${Type.encode(candidate.type)} is nominal but not aggregate`,
      ),
    ])
  }
  const violations: Array<Violation> = []
  const cleanupHook = candidate.representation.cleanupHook
  if (
    cleanupHook !== undefined &&
    (cleanupHook.hook.module.length === 0 ||
      cleanupHook.hook.name.length === 0 ||
      cleanupHook.typeArguments.some(
        (argument) => !Type.isRuntimeConcreteGenericArgument(argument),
      ))
  ) {
    violations.push(
      invalid(
        'InvalidAggregate',
        candidate.type,
        `${Type.encode(candidate.type)} has a non-canonical cleanup hook`,
      ),
    )
  }
  let cursor = 0
  let alignment = 1
  let previousOrdinal = -1
  for (const field of candidate.representation.fields) {
    const fieldLayout = Type.isBuiltin(field.type)
      ? scalarEntry(target, field.type)
      : available.get(Type.key(field.type))
    if (field.id.ordinal <= previousOrdinal) {
      violations.push(
        invalid(
          'InvalidAggregate',
          candidate.type,
          `field ${field.name} is out of declaration order`,
        ),
      )
    }
    if (fieldLayout === undefined) {
      violations.push(
        invalid(
          'InvalidAggregate',
          candidate.type,
          `field ${field.name} has no available dependency layout`,
        ),
      )
      previousOrdinal = field.id.ordinal
      continue
    }
    const offset = alignUp(cursor, fieldLayout.alignment)
    if (
      field.offset !== offset ||
      field.padding !== offset - cursor ||
      field.size !== fieldLayout.size ||
      field.alignment !== fieldLayout.alignment
    ) {
      violations.push(
        invalid(
          'InvalidAggregate',
          candidate.type,
          `field ${field.name} has non-canonical physical facts`,
        ),
      )
    }
    cursor = offset + fieldLayout.size
    alignment = Math.max(alignment, fieldLayout.alignment)
    previousOrdinal = field.id.ordinal
  }
  const size = alignUp(cursor, alignment)
  if (
    candidate.alignment !== alignment ||
    candidate.size !== size ||
    candidate.representation.tailPadding !== size - cursor
  ) {
    violations.push(
      invalid(
        'InvalidAggregate',
        candidate.type,
        `${Type.encode(candidate.type)} has non-canonical size or alignment`,
      ),
    )
  }
  return Object.freeze(violations)
}

const commonViolations = (
  target: Target.Target,
  entries: ReadonlyArray<CatalogEntry>,
): ReadonlyArray<Violation> => {
  const violations: Array<Violation> = []
  if (!Target.isCanonical(target)) {
    violations.push(
      Object.freeze({
        _tag: 'LayoutViolation',
        rule: 'NonCanonicalTarget',
        detail: `target ${target.id} does not match its canonical profile`,
      }),
    )
  }
  const available = new Map(
    entries.flatMap((candidate) =>
      candidate._tag === 'LayoutEntry' ? [[Type.key(candidate.type), candidate] as const] : [],
    ),
  )
  const seen = new Set<string>()
  let previous: DeclarationIndex.SemanticType | undefined
  for (const candidate of entries) {
    const key = Type.key(candidate.type)
    if (seen.has(key)) {
      violations.push(
        invalid(
          'DuplicateType',
          candidate.type,
          `layout contains duplicate ${Type.encode(candidate.type)} entry`,
        ),
      )
    }
    if (previous !== undefined && Type.compare(previous, candidate.type) > 0) {
      violations.push(
        invalid(
          'NonCanonicalOrder',
          candidate.type,
          `${Type.encode(candidate.type)} follows ${Type.encode(previous)} out of canonical order`,
        ),
      )
    }
    if (candidate._tag === 'LayoutEntry') {
      violations.push(...verifyEntry(target, candidate, available))
    }
    seen.add(key)
    previous = candidate.type
  }
  return Object.freeze(violations)
}

const fieldIdEquals = (left: DeclarationIndex.FieldId, right: DeclarationIndex.FieldId): boolean =>
  left.ordinal === right.ordinal &&
  left.struct.sourceId === right.struct.sourceId &&
  left.struct.ordinal === right.struct.ordinal

/** Compares two compiler-planned physical selectors. */
export const selectorEquals = (left: Selector, right: Selector): boolean =>
  left._tag === 'ElementSelector'
    ? right._tag === 'ElementSelector' && left.index === right.index
    : left._tag === 'CallableCaptureSelector'
      ? right._tag === 'CallableCaptureSelector' && left.ordinal === right.ordinal
      : left._tag === 'EffectCaptureSelector'
        ? right._tag === 'EffectCaptureSelector' && left.ordinal === right.ordinal
        : left._tag === 'UnionTagSelector'
          ? right._tag === 'UnionTagSelector'
          : left._tag === 'UnionPayloadSelector'
            ? right._tag === 'UnionPayloadSelector' && left.slot === right.slot
            : left._tag === 'SliceAddressSelector'
              ? right._tag === 'SliceAddressSelector'
              : left._tag === 'SliceLengthSelector'
                ? right._tag === 'SliceLengthSelector'
                : left._tag === 'StringStorageSelector'
                  ? right._tag === 'StringStorageSelector'
                  : left._tag === 'StringByteLengthSelector'
                    ? right._tag === 'StringByteLengthSelector'
                    : left._tag === 'ReferenceAddressSelector'
                      ? right._tag === 'ReferenceAddressSelector'
                      : right._tag === 'FieldId' && fieldIdEquals(left, right)

/** Resolves one compiler-planned scalar lane to its byte offset within a logical value. */
export const laneOffset = (
  self: Plan,
  root: DeclarationIndex.SemanticType,
  path: ReadonlyArray<Selector>,
): number | undefined => {
  let current: DeclarationIndex.SemanticType = root
  let offset = 0
  for (const [ordinal, selector] of path.entries()) {
    const candidate = entry(self, current)
    if (candidate === undefined) return undefined
    if (selector._tag === 'FieldId') {
      if (candidate.representation._tag !== 'Aggregate') return undefined
      const field = candidate.representation.fields.find((item) => fieldIdEquals(item.id, selector))
      if (field === undefined) return undefined
      offset += field.offset
      current = field.type
      continue
    }
    if (selector._tag === 'ElementSelector') {
      if (candidate.representation._tag !== 'Repeated') return undefined
      if (selector.index < 0 || selector.index >= candidate.representation.length) return undefined
      offset += selector.index * candidate.representation.stride
      current = candidate.representation.element
      continue
    }
    if (selector._tag === 'CallableCaptureSelector') {
      if (candidate.representation._tag !== 'CallableEnvironment') return undefined
      const field = candidate.representation.fields.find(
        (capture) => capture.ordinal === selector.ordinal,
      )
      if (field === undefined) return undefined
      offset += field.offset
      current = field.type
      continue
    }
    if (selector._tag === 'EffectCaptureSelector') {
      if (candidate.representation._tag !== 'StoredEffectEnvironment') return undefined
      const field = candidate.representation.fields.find(
        (capture) => capture.capture === selector.ordinal,
      )
      if (field === undefined) return undefined
      offset += field.offset
      current = field.type
      continue
    }
    if (selector._tag === 'UnionTagSelector') {
      return ordinal === path.length - 1 && candidate.representation._tag === 'Union'
        ? offset
        : undefined
    }
    if (selector._tag === 'UnionPayloadSelector') {
      if (ordinal !== path.length - 1 || candidate.representation._tag !== 'Union') {
        return undefined
      }
      const shape = callingShape(self, current)
      if (shape?.tree._tag !== 'SumShape') return undefined
      let payloadOffset = 0
      for (let slot = 0; slot <= selector.slot; slot += 1) {
        const type = shape.tree.payloadTypes.at(slot)
        if (type === undefined) return undefined
        const scalar = entry(self, type)
        if (scalar === undefined) return undefined
        payloadOffset = alignUp(payloadOffset, scalar.alignment)
        if (slot === selector.slot) {
          return offset + candidate.representation.payloadOffset + payloadOffset
        }
        payloadOffset += scalar.size
      }
      return undefined
    }
    if (selector._tag === 'SliceAddressSelector') {
      return ordinal === path.length - 1 && candidate.representation._tag === 'Slice'
        ? offset + candidate.representation.address.offset
        : undefined
    }
    if (selector._tag === 'StringStorageSelector') {
      return ordinal === path.length - 1 && candidate.representation._tag === 'String'
        ? offset + candidate.representation.storage.offset
        : undefined
    }
    if (selector._tag === 'StringByteLengthSelector') {
      return ordinal === path.length - 1 && candidate.representation._tag === 'String'
        ? offset + candidate.representation.byteLength.offset
        : undefined
    }
    if (selector._tag === 'ReferenceAddressSelector') {
      return ordinal === path.length - 1 && candidate.representation._tag === 'Reference'
        ? offset + candidate.representation.address.offset
        : undefined
    }
    return ordinal === path.length - 1 && candidate.representation._tag === 'Slice'
      ? offset + candidate.representation.length.offset
      : undefined
  }
  return offset
}

const callingScalarEquals = (left: CallingScalar, right: CallingScalar): boolean =>
  typeof left === 'string'
    ? left === right
    : typeof right !== 'string' &&
      Type.equals(left.element, right.element) &&
      left.bits === right.bits

const verifyCallingShapes = (self: Plan): ReadonlyArray<Violation> => {
  const expected = callingShapes(
    self.target,
    self.entries,
    self.entries.map((entry) => entry.type),
    self.effectEnvironments,
    self.callableEnvironments,
  )
  const violations: Array<Violation> = []
  for (const entry of self.entries) {
    const actual = callingShape(self, entry.type)
    const canonical = expected.find((candidate) => Type.equals(candidate.type, entry.type))
    const matches =
      actual !== undefined &&
      canonical !== undefined &&
      actual.laneCount === canonical.laneCount &&
      actual.lanes.length === canonical.lanes.length &&
      actual.lanes.every((lane, laneIndex) => {
        const other = canonical.lanes.at(laneIndex)
        return (
          other !== undefined &&
          callingScalarEquals(lane.type, other.type) &&
          lane.path.length === other.path.length &&
          lane.path.every((selector, selectorIndex) => {
            const otherSelector = other.path.at(selectorIndex)
            return otherSelector !== undefined && selectorEquals(selector, otherSelector)
          })
        )
      })
    if (!matches) {
      violations.push(
        invalid(
          'InvalidCallingShape',
          entry.type,
          `${Type.encode(entry.type)} does not match its canonical scalar-lane shape`,
        ),
      )
    }
  }
  if (self.callingShapes.length < self.entries.length) {
    violations.push(
      Object.freeze({
        _tag: 'LayoutViolation',
        rule: 'InvalidCallingShape',
        detail: 'calling-shape collection does not match the reachable layout entries',
      }),
    )
  }
  return Object.freeze(violations)
}

const verifyLiteralVerdicts = (self: Plan): ReadonlyArray<Violation> => {
  const bits: 32 | 64 = self.target.pointerSize === 4 ? 32 : 64
  const maximum = bits === 32 ? 4294967295n : 18446744073709551615n
  const violations: Array<Violation> = []
  const unavailable = self.literalVerdicts.filter(
    (verdict) => verdict._tag === 'UnavailableUsizeLiteral',
  )
  for (const verdict of self.literalVerdicts) {
    const expectedTag =
      verdict.value >= 0n && verdict.value <= maximum
        ? 'AvailableUsizeLiteral'
        : 'UnavailableUsizeLiteral'
    if (verdict.bits !== bits || verdict._tag !== expectedTag) {
      violations.push(
        Object.freeze({
          _tag: 'LayoutViolation',
          rule: 'InvalidLiteralVerdict',
          type: 'usize',
          detail: `${verdict.value.toString()} has a non-canonical ${verdict.bits}-bit verdict`,
        }),
      )
    }
  }
  if (
    self.diagnostics.length !== unavailable.length ||
    unavailable.some((verdict) =>
      self.diagnostics.every(
        (diagnostic) =>
          diagnostic.code !== Diagnostic.usizeTargetOutOfRangeCode ||
          diagnostic.span.sourceId !== verdict.span.sourceId ||
          diagnostic.span.start !== verdict.span.start ||
          diagnostic.span.end !== verdict.span.end ||
          diagnostic.reason._tag !== 'UsizeTargetOutOfRange' ||
          diagnostic.reason.spelling !== verdict.value.toString() ||
          diagnostic.reason.target !== self.target.id ||
          diagnostic.reason.bits !== bits,
      ),
    )
  ) {
    violations.push(
      Object.freeze({
        _tag: 'LayoutViolation',
        rule: 'InvalidLiteralVerdict',
        type: 'usize',
        detail: 'target literal diagnostics do not match unavailable verdicts',
      }),
    )
  }
  return Object.freeze(violations)
}

const verifyStaticData = (self: Plan): ReadonlyArray<Violation> => {
  const expectedBits = self.target.pointerSize === 4 ? 32 : 64
  const valid = (self.staticData ?? []).every((placement, ordinal, all) => {
    const previous = ordinal === 0 ? undefined : all.at(ordinal - 1)
    return (
      (previous === undefined || previous.data.id < placement.data.id) &&
      placement.alignment === 1 &&
      placement.addressBits === expectedBits &&
      placement.lengthBits === expectedBits &&
      placement.data.bytes.every((byte) => Number.isInteger(byte) && byte >= 0 && byte <= 255)
    )
  })
  return valid
    ? Object.freeze([])
    : Object.freeze([
        Object.freeze({
          _tag: 'LayoutViolation' as const,
          rule: 'InvalidCallingShape' as const,
          detail: 'static data placements are not canonical immutable target data',
        }),
      ])
}

/** Verifies canonical target, ordering, uniqueness, representation, and ABI facts. */
export const verify = (self: Plan): ReadonlyArray<Violation> =>
  Object.freeze([
    ...commonViolations(self.target, self.entries),
    ...verifyCallingShapes(self),
    ...verifyLiteralVerdicts(self),
    ...verifyStaticData(self),
  ])

/** Verifies all available entries and deterministic ordering within a nominal catalog. */
export const verifyCatalog = (self: Catalog): ReadonlyArray<Violation> =>
  commonViolations(self.target, self.entries)

/** Verifies that every planned nominal layout is exactly the catalog decision. */
export const verifyAgainstCatalog = (self: Plan, catalog: Catalog): ReadonlyArray<Violation> =>
  Object.freeze(
    self.entries.flatMap((candidate) => {
      if (
        Type.isBuiltin(candidate.type) ||
        Type.isFixedArray(candidate.type) ||
        Type.isReference(candidate.type)
      )
        return []
      const expected = catalogEntry(catalog, candidate.type)
      return expected?._tag === 'LayoutEntry' &&
        candidate.size === expected.size &&
        candidate.alignment === expected.alignment &&
        representationEquals(candidate.representation, expected.representation)
        ? []
        : [
            invalid(
              'CatalogMismatch',
              candidate.type,
              `${Type.encode(candidate.type)} differs from its catalog entry`,
            ),
          ]
    }),
  )

const representationText = (representation: Representation): string =>
  representation._tag === 'SignedInteger'
    ? `signed-i${representation.bits}`
    : representation._tag === 'UnsignedInteger'
      ? `unsigned-i${representation.bits}`
      : representation._tag === 'Floating'
        ? `float${representation.bits}`
        : representation._tag === 'Boolean'
          ? `bool-i${representation.bits} false=${representation.falseValue} true=${representation.trueValue}`
          : representation._tag === 'CallableEnvironment'
            ? `callable-environment target=${
                representation.realization.target._tag === 'Declaration'
                  ? `${representation.realization.target.module}.${representation.realization.target.name}`
                  : `${representation.realization.target.actor}.${representation.realization.target.operation}`
              } environment=${representation.realization.environment === undefined ? 'none' : Type.callableEnvironmentKey(representation.realization.environment)} tail-padding=${representation.tailPadding}`
            : representation._tag === 'StoredEffectEnvironment'
              ? `stored-effect-environment runner=${representation.realization.runner.module}.${representation.realization.runner.name} identity=${representation.realization.runnerIdentity} access=${representation.realization.access.toLowerCase()} suspendable=${representation.realization.suspendable ? 'yes' : 'no'} tail-padding=${representation.tailPadding}`
              : representation._tag === 'Repeated'
                ? `repeated element=${Type.encode(representation.element)} length=${representation.length} stride=${representation.stride}`
                : representation._tag === 'Slice'
                  ? `slice element=${Type.encode(representation.element)} address=i${representation.address.bits}@${representation.address.offset}/${representation.address.size}/${representation.address.alignment} length=usize@${representation.length.offset}/${representation.length.size} address-padding=${representation.addressPadding} tail-padding=${representation.tailPadding} stride=${representation.stride}`
                  : representation._tag === 'String'
                    ? `string storage=${representation.storage.provenance}:i${representation.storage.bits}@${representation.storage.offset}/${representation.storage.size}/${representation.storage.alignment} byte-length=usize@${representation.byteLength.offset}/${representation.byteLength.size} storage-padding=${representation.storagePadding} tail-padding=${representation.tailPadding}`
                    : representation._tag === 'Reference'
                      ? `reference target=${Type.encode(representation.target)} address=i${representation.address.bits}@${representation.address.offset}/${representation.address.size}/${representation.address.alignment}`
                      : representation._tag === 'Union'
                        ? `union tag=i${representation.tag.bits} payload-offset=${representation.payloadOffset} payload-size=${representation.payloadSize} payload-align=${representation.payloadAlignment} tag-padding=${representation.tagPadding} tail-padding=${representation.tailPadding}`
                        : `aggregate cleanup-hook=${
                            representation.cleanupHook === undefined
                              ? 'none'
                              : `${representation.cleanupHook.hook.module}.${representation.cleanupHook.hook.name}<${representation.cleanupHook.typeArguments.map(Type.encodeGenericArgument).join(',')}>`
                          } tail-padding=${representation.tailPadding}`

const entryLines = (candidate: Entry): ReadonlyArray<string> => [
  `layout ${Type.encode(candidate.type)} size=${candidate.size} align=${candidate.alignment} repr=${representationText(candidate.representation)}`,
  ...(candidate.representation._tag === 'Aggregate'
    ? candidate.representation.fields.map(
        (field) =>
          `  field ${field.id.ordinal} ${field.name}: ${Type.encode(field.type)} offset=${field.offset} size=${field.size} align=${field.alignment} padding=${field.padding}`,
      )
    : candidate.representation._tag === 'CallableEnvironment'
      ? candidate.representation.fields.map(
          (field) =>
            `  capture ${field.ordinal}->p${field.parameterOrdinal}: ${Type.encode(field.type)} access=${field.access.toLowerCase()} representation=${field.representation.toLowerCase()} offset=${field.offset} size=${field.size} align=${field.alignment} padding=${field.padding}`,
        )
      : candidate.representation._tag === 'StoredEffectEnvironment'
        ? candidate.representation.fields.map(
            (field) =>
              `  effect-capture ${field.capture} ${field.source.toLowerCase()}${field.ordinal}: ${Type.encode(field.type)} access=${field.access.toLowerCase()} representation=${field.representation.toLowerCase()} offset=${field.offset} size=${field.size} align=${field.alignment} padding=${field.padding}`,
          )
        : candidate.representation._tag === 'Repeated'
          ? [
              `  elements ${Type.encode(candidate.representation.element)} count=${candidate.representation.length} stride=${candidate.representation.stride}`,
            ]
          : candidate.representation._tag === 'Slice'
            ? [
                `  address Address<${Type.encode(candidate.representation.element)}> bits=${candidate.representation.address.bits} offset=${candidate.representation.address.offset} size=${candidate.representation.address.size} align=${candidate.representation.address.alignment}`,
                `  length usize offset=${candidate.representation.length.offset} size=${candidate.representation.length.size} stride=${candidate.representation.stride}`,
              ]
            : candidate.representation._tag === 'String'
              ? [
                  `  storage StringUtf8 bits=${candidate.representation.storage.bits} offset=${candidate.representation.storage.offset} size=${candidate.representation.storage.size} align=${candidate.representation.storage.alignment}`,
                  `  byte-length usize offset=${candidate.representation.byteLength.offset} size=${candidate.representation.byteLength.size}`,
                ]
              : candidate.representation._tag === 'Reference'
                ? [
                    `  address Address<${Type.encode(candidate.representation.target)}> bits=${candidate.representation.address.bits} offset=0 size=${candidate.representation.address.size} align=${candidate.representation.address.alignment}`,
                  ]
                : candidate.representation._tag === 'Union'
                  ? candidate.representation.members.map(
                      (member) =>
                        `  member ${member.ordinal} ${Type.encode(member.type)} size=${member.size} align=${member.alignment}`,
                    )
                  : []),
]

/** Deterministic textual encoding of a complete runtime layout plan. */
const callingScalarText = (scalar: CallingScalar): string =>
  typeof scalar === 'string' ? scalar : `Address<${Type.encode(scalar.element)},i${scalar.bits}>`

export const encode = (self: Plan): string =>
  [
    `target ${Target.encode(self.target)}`,
    ...self.entries.flatMap(entryLines),
    ...self.effectEnvironments.map((environment) =>
      environment._tag === 'UnavailableEffectEnvironment'
        ? `effect-environment ${environment.instance.declaration.module}.${environment.instance.declaration.name}@${Hir.executableSiteLabel(environment.site)} unavailable=${environment.reason}`
        : `effect-environment ${environment.instance.declaration.module}.${environment.instance.declaration.name}@${Hir.executableSiteLabel(environment.site)} size=${environment.size} align=${environment.alignment} fields=${environment.fields.map((field) => `${field.source.toLowerCase()}${field.ordinal}:${field.access.toLowerCase()}:${field.representation.toLowerCase()}@${field.offset}`).join(',') || 'none'}`,
    ),
    ...self.callableEnvironments.map((environment) => {
      const callable = environment.callable
      const identity = `${callable.owner.declaration.module}.${callable.owner.declaration.name}@${Hir.executableSiteLabel(callable.site)}`
      return environment._tag === 'UnavailableCallableEnvironment'
        ? `callable-environment ${identity} unavailable=${environment.reason} view=code@${environment.view.codeOffset},env@${environment.view.environmentOffset},size=${environment.view.size}`
        : `callable-environment ${identity} mode=${callable.mode.toLowerCase()} size=${environment.size} align=${environment.alignment} fields=${environment.fields.map((field) => `capture${field.ordinal}->p${field.parameterOrdinal}:${field.access.toLowerCase()}:${field.representation.toLowerCase()}@${field.offset}`).join(',') || 'none'} view=code@${environment.view.codeOffset},env@${environment.view.environmentOffset},size=${environment.view.size}`
    }),
    ...self.callingShapes.map(
      (shape) =>
        `calling ${Type.encode(shape.type)} lanes=${shape.laneCount}${
          shape.laneCount === 0
            ? ''
            : ` ${shape.lanes
                .map(
                  (lane) =>
                    `${callingScalarText(lane.type)}[${lane.path
                      .map((selector) =>
                        selector._tag === 'ElementSelector'
                          ? `[${selector.index}]`
                          : selector._tag === 'CallableCaptureSelector'
                            ? `capture[${selector.ordinal}]`
                            : selector._tag === 'EffectCaptureSelector'
                              ? `effect-capture[${selector.ordinal}]`
                              : selector._tag === 'UnionTagSelector'
                                ? 'tag'
                                : selector._tag === 'UnionPayloadSelector'
                                  ? `payload[${selector.slot}]`
                                  : selector._tag === 'SliceAddressSelector'
                                    ? 'address'
                                    : selector._tag === 'SliceLengthSelector'
                                      ? 'length'
                                      : selector._tag === 'StringStorageSelector'
                                        ? 'storage'
                                        : selector._tag === 'StringByteLengthSelector'
                                          ? 'byte-length'
                                          : selector._tag === 'ReferenceAddressSelector'
                                            ? 'address'
                                            : `${selector.struct.sourceId}#${selector.struct.ordinal}.${selector.ordinal}`,
                      )
                      .join('.')}]`,
                )
                .join(',')}`
        }`,
    ),
    ...(self.staticData ?? []).map(
      (placement) =>
        `static-data ${placement.data.id} bytes=${placement.data.bytes.map((byte) => byte.toString(16).padStart(2, '0')).join('')} align=${placement.alignment} address=i${placement.addressBits} length=usize:i${placement.lengthBits}`,
    ),
    ...self.literalVerdicts.map(
      (verdict) =>
        `usize-literal ${verdict.value.toString()} bits=${verdict.bits} ${verdict._tag === 'AvailableUsizeLiteral' ? 'available' : `unavailable cause=${verdict.cause.code}`} [${verdict.span.start}, ${verdict.span.end})`,
    ),
    '',
  ].join('\n')

const unavailableText = (candidate: UnavailableEntry): string => {
  const reason =
    candidate.reason._tag === 'UnavailableDependency'
      ? `dependency=${Type.encode(candidate.reason.dependency)}`
      : `detail=${JSON.stringify(candidate.reason.detail)}`
  const cause =
    candidate.cause === undefined
      ? ''
      : ` cause=${candidate.cause.code}@${candidate.cause.span.sourceId}:${candidate.cause.span.start}-${candidate.cause.span.end}`
  return `layout ${Type.encode(candidate.type)} unavailable reason=${candidate.reason._tag} ${reason}${cause}`
}

/** Deterministic textual encoding of every nominal catalog fact. */
export const encodeCatalog = (self: Catalog): string =>
  [
    `target ${Target.encode(self.target)}`,
    ...self.entries.flatMap((candidate) =>
      candidate._tag === 'LayoutEntry' ? entryLines(candidate) : [unavailableText(candidate)],
    ),
    ...self.usizeConstants.map(
      (constant) =>
        `usize-constant ${constant.value.toString()} [${constant.span.sourceId}:${constant.span.start}, ${constant.span.end})`,
    ),
    '',
  ].join('\n')
