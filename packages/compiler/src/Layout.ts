import * as CleanupPlan from './CleanupPlan.js'
import * as ConformanceProof from './ConformanceProof.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
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
import * as OpaqueRealization from './OpaqueRealization.js'
import * as RepresentationField from './RepresentationField.js'
import * as RowAlgebra from './RowAlgebra.js'
import * as Scalar from './Scalar.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as StaticText from './StaticText.js'
import type * as Target from './Target.js'
import * as TargetConstant from './TargetConstant.js'
import * as Type from './Type.js'

/** Physical placement shared by every aggregate and hidden-environment field. */
export interface PlacedField extends Packing.PlacedField {}

/** One declaration-ordered physical field within an aggregate representation. */
export interface Field extends PlacedField {
  readonly _tag: 'LayoutField'
  readonly id: DeclarationFacts.FieldId
  readonly name: string
  readonly type: DeclarationFacts.SemanticType
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
        readonly hook: DeclarationFacts.CanonicalId
        readonly typeArguments: ReadonlyArray<Type.GenericArgument>
      }
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

export interface EffectEnvironmentField extends PlacedField {
  readonly source: 'Binding' | 'Parameter'
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

export const stringEntry = (target: Target.Target): Entry => {
  const addressBits: 32 | 64 = target.pointerSize === 4 ? 32 : 64
  const byteLengthOffset = alignUp(target.pointerSize, target.pointerAlignment)
  const alignment = target.pointerAlignment
  const contentSize = byteLengthOffset + target.pointerSize
  const size = alignUp(contentSize, alignment)
  return Object.freeze({
    _tag: 'LayoutEntry',
    type: Type.string,
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

const nominalOf = (struct: DeclarationFacts.StructFact): Type.Nominal | undefined =>
  struct.canonical._tag === 'Canonical'
    ? Type.nominal(struct.canonical.id.module, struct.canonical.id.name)
    : undefined

const dependenciesOf = (
  struct: DeclarationFacts.StructFact,
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
              FieldRealization.matchesIdentity(callableIdentity, candidate),
            )
      const stableDescriptor = Type.isSlice(slot.type) || Type.isReference(slot.type)
      const borrowed =
        (slot.access === 'Shared' || slot.access === 'Exclusive') &&
        nestedEffect === undefined &&
        nestedCallable === undefined &&
        !stableDescriptor
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
          const captureBorrowed = capture.access === 'Shared' || capture.access === 'Exclusive'
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
      fieldInputs.push({
        value: Object.freeze({
          capture: slot.ordinal,
          source: slot.source,
          ordinal: slot.sourceOrdinal,
          access: slot.access,
          type: nestedEffect?.type ?? slot.type,
          representation: borrowed ? 'Borrow' : nestedCallable === undefined ? 'Value' : 'Callable',
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
    const key = Type.key(type)
    const existing = completed.get(key)
    if (existing !== undefined) return existing
    let copy = true
    const inputs: Array<Packing.Input<Omit<CallableEnvironmentField, keyof Packing.PlacedField>>> =
      []
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
    const key = Type.key(type)
    const existing = completed.get(key)
    if (existing !== undefined) return existing
    if (Type.isSharedCore(type)) {
      const result = unavailable(type, Object.freeze([]), {
        _tag: 'InvalidDeclaration',
        detail: 'sealed local shared ownership has no target representation in this layer',
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
      const inputs: Array<Packing.Input<Omit<Field, keyof Packing.PlacedField>>> = []
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
        inputs.push(
          Object.freeze({
            value: Object.freeze({
              _tag: 'LayoutField' as const,
              id: Object.freeze({
                _tag: 'FieldId' as const,
                struct: structId,
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
      const layoutFieldType = (candidate: DeclarationFacts.SemanticType): CatalogEntry => {
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
    const typeKey = Type.key(type)
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
        const borrowed = capture.access === 'Shared' || capture.access === 'Exclusive'
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
      const effect = discovery?.effects.find(
        (candidate) =>
          (candidate.identity === identity.identity ||
            candidate.representationIdentity === identity.identity) &&
          (identity.owner === undefined ||
            (candidate.owner.declaration.module === identity.owner.declaration.module &&
              candidate.owner.declaration.name === identity.owner.declaration.name &&
              candidate.owner.typeArguments.length === identity.owner.typeArguments.length &&
              candidate.owner.typeArguments.every((value, ordinal) => {
                const expected = identity.owner?.typeArguments.at(ordinal)
                return expected !== undefined && Type.equalsGenericArgument(value, expected)
              }))),
      )
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

  const layoutType = (
    type: DeclarationFacts.SemanticType,
    executableUnionMember = false,
  ): CatalogEntry => {
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
        const memberLayout = layoutType(member, true)
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
      if (executableUnionMember) return layoutDirectRepresented(type)
      const result = unavailable(type, Object.freeze(Type.nominals(type)), {
        _tag: 'InvalidDeclaration',
        detail: 'represented executable values require a containing storage plan',
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

  const referenced = new Map<string, DeclarationFacts.SemanticType>()
  const addReferenced = (type: DeclarationFacts.SemanticType): void => {
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
      if (expression._tag === 'UnionConvert')
        addReferenced(Type.substitute(expression.sourceType, substitution))
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
    const addPatternStatementTypes = (statement: Hir.Statement): void => {
      if (statement._tag === 'PatternBind' || statement._tag === 'IfLet') {
        addReferenced('bool')
        for (const member of statement.selection.members)
          addReferenced(Type.substitute(member, substitution))
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
  types: Map<string, DeclarationFacts.SemanticType>,
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
  if (expression._tag === 'UnionConvert') {
    const sourceType = Type.substitute(expression.sourceType, substitution)
    types.set(Type.key(sourceType), sourceType)
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
  types: Map<string, DeclarationFacts.SemanticType>,
  statements: ReadonlyArray<Hir.Statement>,
  substitution: Type.Substitution = new Map(),
): void => {
  for (const statement of statements) {
    if (statement._tag === 'Unsafe') addStatementTypes(types, statement.statements, substitution)
    if (statement._tag === 'Bind') addExpressionTypes(types, statement.initializer, substitution)
    if (statement._tag === 'PatternBind') {
      types.set(Type.key('bool'), 'bool')
      addExpressionTypes(types, statement.selection.subject, substitution)
      for (const member of statement.selection.members) {
        const type = Type.substitute(member, substitution)
        types.set(Type.key(type), type)
      }
      for (const binding of statement.selection.bindings) {
        const type = Type.substitute(binding.type, substitution)
        types.set(Type.key(type), type)
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
      types.set(Type.key('bool'), 'bool')
      addExpressionTypes(types, statement.selection.subject, substitution)
      for (const member of statement.selection.members) {
        const type = Type.substitute(member, substitution)
        types.set(Type.key(type), type)
      }
      for (const binding of statement.selection.bindings) {
        const type = Type.substitute(binding.type, substitution)
        types.set(Type.key(type), type)
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
      const collectBindings = (statements: ReadonlyArray<Hir.Statement>): void => {
        for (const statement of statements) {
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
            : FieldRealization.effectEnvironmentOf(effectInstance)
        let effect = structuralEffect
        let unavailable: string | undefined
        const fieldInputs: Array<Packing.Input<EffectFieldDraft>> = []
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
                    FieldRealization.matchesIdentity(capturedCallableIdentity, candidate.callable),
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
          fieldInputs.push({
            value: Object.freeze({
              source,
              ordinal,
              access,
              type: fieldType,
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
            size,
            alignment,
          })
        }
        if (unavailable === undefined) {
          const access = fieldInputs.some((field) => field.value.access === 'Take')
            ? 'Take'
            : fieldInputs.some((field) => field.value.access === 'Exclusive')
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
        let unavailable: string | undefined
        const fieldInputs: Array<Packing.Input<EffectFieldDraft>> = []
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
        const access = fieldInputs.some((field) => field.value.access === 'Take')
          ? 'Take'
          : fieldInputs.some((field) => field.value.access === 'Exclusive')
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
  const layouts = new Map(entries.map((entry) => [Type.key(entry.type), entry] as const))
  const view = callableView(target)
  return Object.freeze(
    discovery.callables.map((callable): CallableEnvironment => {
      const inputs: Array<
        Packing.Input<Omit<CallableEnvironmentField, 'offset' | 'size' | 'alignment' | 'padding'>>
      > = []
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
        inputs.push(
          Object.freeze({
            value: Object.freeze({
              ordinal: capture.ordinal,
              parameterOrdinal: capture.parameterOrdinal,
              access: capture.access,
              type: capture.type,
              representation: borrowed ? 'Borrow' : 'Value',
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
      return Object.freeze({
        _tag: 'CallableEnvironment',
        callable,
        fields,
        size: packed.size,
        alignment: packed.alignment,
        tailPadding: packed.tailPadding,
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
  const reached = new Map<string, DeclarationFacts.SemanticType>()
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
  const resolve = (type: DeclarationFacts.SemanticType): Entry | undefined => {
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
  const add = (type: DeclarationFacts.SemanticType): void => {
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
    for (const field of candidate.executable?.fields ?? []) add(field.type)
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
    if (
      Type.isRuntimeConcrete(type) &&
      (Type.isEffect(type) ||
        Type.isNever(type) ||
        (Type.isRepresented(type) &&
          Type.isCompositeEffectRepresentationArgument(type.representation.argument)))
    )
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
    const environment = context.callableEnvironments.find(
      (
        candidate,
      ): candidate is Extract<CallableEnvironment, { readonly _tag: 'CallableEnvironment' }> =>
        candidate._tag === 'CallableEnvironment' &&
        FieldRealization.matchesIdentity(identity, candidate.callable),
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
  type: DeclarationFacts.SemanticType,
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
    const argument = type.representation.argument
    if (Type.isEffect(type.contract) && Type.isCompositeEffectRepresentationArgument(argument)) {
      const alternatives = argument.alternatives.map((alternative) => {
        if (!Type.isEffectIdentityArgument(alternative.identity))
          throw new RangeError('Effect composite retained a non-Effect alternative')
        const identity = alternative.identity
        const environment = context.effectEnvironments.find(
          (
            candidate,
          ): candidate is Extract<EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> =>
            candidate._tag === 'EffectEnvironment' &&
            Hir.effectRepresentationIdentity(candidate.site) === identity.identity &&
            identity.owner !== undefined &&
            candidate.instance.declaration.module === identity.owner.declaration.module &&
            candidate.instance.declaration.name === identity.owner.declaration.name &&
            candidate.instance.typeArguments.length === identity.owner.typeArguments.length &&
            candidate.instance.typeArguments.every((value, ordinal) => {
              const expected = identity.owner?.typeArguments.at(ordinal)
              return expected !== undefined && Type.equalsGenericArgument(value, expected)
            }),
        )
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
    const entry = entries.get(Type.key(type))
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
    const fields =
      executable !== undefined
        ? executable.fields.map((field) =>
            Object.freeze({
              capture: field.capture,
              shape:
                executable._tag === 'Callable' && field.representation !== 'Borrow'
                  ? shapeNode(field.type, context)
                  : executableEnvironmentFieldShape(context, field),
            }),
          )
        : storedCallable !== undefined
          ? storedCallable.fields.map((field) =>
              Object.freeze({
                capture: field.ordinal,
                shape:
                  field.representation === 'Borrow'
                    ? borrowedShape(context, field.type)
                    : shapeNode(field.type, context),
              }),
            )
          : (storedEffect?.fields ?? []).map((field) =>
              Object.freeze({
                capture: field.capture,
                shape: executableEnvironmentFieldShape(context, field),
              }),
            )
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
            return rightBits - leftBits || Type.compare(left, right)
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
  const byType = new Map(entries.map((candidate) => [Type.key(candidate.type), candidate]))
  return Object.freeze(
    types.map((type) => shapeOf(target, type, byType, effectEnvironments, callableEnvironments)),
  )
}

/** Looks up one canonical runtime-plan entry. */
export const entry = (self: Plan, type: DeclarationFacts.SemanticType): Entry | undefined =>
  self.entries.find((candidate) => Type.equals(candidate.type, type))

/** Looks up one compiler-owned calling shape by logical type. */
export const callingShape = (
  self: Plan,
  type: DeclarationFacts.SemanticType,
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
  path: ReadonlyArray<DeclarationFacts.FieldId>,
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
  path: ReadonlyArray<DeclarationFacts.FieldId>,
): ReadonlyArray<number> | undefined => {
  if (path.length === 0 && Type.equals(shape.type, member))
    return Object.freeze(Array.from({ length: shape.laneCount }, (_, ordinal) => ordinal))
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
  type: DeclarationFacts.SemanticType,
): CatalogEntry | undefined => self.entries.find((candidate) => Type.equals(candidate.type, type))
