import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as Hir from './Hir.js'
import type * as Layout from './Layout.js'
import type * as Mir from './Mir.js'
import type * as Scalar from './Scalar.js'
import * as Type from './Type.js'

export interface IntegerValue {
  readonly _tag: 'IntegerValue'
  readonly type: Scalar.IntegerSpelling
  readonly value: bigint
}

/** One immutable logical scalar-enum member, before physical lane realization. */
export interface EnumValue {
  readonly _tag: 'EnumValue'
  readonly enum: DeclarationFacts.CanonicalId
  readonly member: DeclarationFacts.CanonicalEnumMemberId
  readonly discriminant: bigint
  readonly representation: Extract<Layout.Representation, { readonly _tag: 'ScalarEnum' }>
}

/**
 * One Unicode scalar value.
 *
 * `char` is its own scalar category rather than an integer, so it is its own value rather than an
 * integer view: nothing that reads an integer accepts it, and the only operations that read it are
 * the equality and ordering lanes the catalog declares.
 */
export interface CharacterValue {
  readonly _tag: 'CharacterValue'
  readonly value: number
}

export interface FloatValue {
  readonly _tag: 'FloatValue'
  readonly type: Scalar.FloatSpelling
  readonly bits: bigint
}

export interface AggregateValue {
  readonly _tag: 'AggregateValue'
  readonly type: Type.Nominal
  readonly fields: ReadonlyArray<{
    readonly field: DeclarationFacts.FieldId
    readonly value: Value
  }>
}

export interface NominalUnionValue {
  readonly _tag: 'NominalUnionValue'
  readonly type: Type.Nominal
  readonly variant: DeclarationFacts.CanonicalUnionVariantId
  readonly variantOrdinal: number
  readonly fields: ReadonlyArray<{
    readonly field: DeclarationFacts.FieldId
    readonly value: Value
  }>
}

export interface ArrayValue {
  readonly _tag: 'ArrayValue'
  readonly type: Type.FixedArray
  readonly elements: ReadonlyArray<Value>
}

/** A logical borrowed view. Permission and loan identity remain compiler facts, not values. */
export interface SliceValue {
  readonly _tag: 'SliceValue'
  readonly frame: number
  readonly cell: number
  readonly base: number
  readonly length: number
  /** Place path from the backing cell to the fixed array viewed by this slice. */
  readonly selectors?: ReadonlyArray<
    Extract<Mir.PlaceSelector, { readonly _tag: 'FieldSelector' | 'ElementSelector' }>
  >
  readonly indexes?: ReadonlyArray<number>
  /** Present only for a zero-copy RawBuffer-backed slice. */
  readonly ticket?: number
}

/** Allocation-free immutable view of one compiler-owned static-data entry. */
export interface StaticViewValue {
  readonly _tag: 'StaticViewValue'
  readonly data: string
  readonly bytes: ReadonlyArray<number>
  readonly length: number
}

export type StringStorage =
  | {
      readonly _tag: 'StaticTextStorage'
      readonly data: string
      readonly bytes: ReadonlyArray<number>
    }
  | {
      readonly _tag: 'StaticByteStorage'
      readonly data: string
      readonly bytes: ReadonlyArray<number>
    }
  | {
      readonly _tag: 'RuntimeSliceStorage'
      readonly view: SliceValue
    }

/** A logical valid UTF-8 view retaining storage identity and lexical backing facts. */
export interface StringValue {
  readonly _tag: 'StringValue'
  readonly storage: StringStorage
  readonly bytes: ReadonlyArray<number>
  readonly byteLength: number
  readonly heldLoans: ReadonlyArray<string>
}

export interface ReferenceValue {
  readonly _tag: 'ReferenceValue'
  readonly frame: number
  readonly cell: number
  readonly selectors: ReadonlyArray<Mir.PlaceSelector>
  /** Selector ordinals captured in the frame where the loan formed. */
  readonly indexes: ReadonlyArray<number>
}

/**
 * One logical raw address. A frame address names the borrowed place; `elements` marks a place
 * whose fixed array the pointer walks, so `offset` indexes it, while a pointer to one scalar place
 * carries `elements: false` and must stay at offset zero. A ticket address walks raw-buffer storage.
 */
export type PointerAddress =
  | {
      readonly _tag: 'Frame'
      readonly frame: number
      readonly cell: number
      readonly selectors: ReadonlyArray<Mir.PlaceSelector>
      readonly indexes: ReadonlyArray<number>
      readonly elements: boolean
      readonly offset: number
    }
  | {
      readonly _tag: 'Ticket'
      readonly ticket: number
      readonly offset: number
    }

/** A raw pointer: null or a logical address that holds no loan and keeps nothing alive. */
export interface PointerValue {
  readonly _tag: 'PointerValue'
  readonly address: PointerAddress | null
}

export interface UnionValue {
  readonly _tag: 'UnionValue'
  readonly type: Type.StructuralUnion
  readonly member: Type.Type
  readonly payload: Value
}

export interface EffectOutcomeValue {
  readonly _tag: 'EffectOutcomeValue'
  readonly type: Type.Effect
  readonly tag: number
  readonly payload: Value
}

export const repackFailurePayload = (
  payload: Value,
  sourceType: DeclarationFacts.SemanticType,
  sourceTag: number,
  targetType: Type.Effect,
  targetTag: number,
): AggregateValue => {
  const sourceMember = Type.failureCarrierMember(
    sourceType,
    sourceTag,
    Type.isEffect(sourceType) ? 'OneBased' : 'ZeroBased',
  )
  const targetMember = Type.failureCarrierMember(targetType, targetTag, 'OneBased')
  if (
    payload._tag !== 'AggregateValue' ||
    sourceMember === undefined ||
    targetMember === undefined ||
    !Type.equals(sourceMember, targetMember) ||
    !Type.equals(payload.type, sourceMember)
  )
    throw new RangeError('MIR failure payload does not match its canonical member mapping')
  return payload
}

export interface EffectBorrowValue {
  readonly _tag: 'EffectBorrowValue'
  readonly frame: number
  readonly cell: number
  readonly access: 'Shared' | 'Exclusive'
}

export interface CallableBorrowValue {
  readonly _tag: 'CallableBorrowValue'
  readonly frame: number
  readonly cell: number
  readonly access: 'Shared' | 'Exclusive'
}

export interface CallableValue {
  readonly _tag: 'CallableValue'
  readonly ticket: number
  readonly type: Type.Callable
  readonly target: Hir.CallableTarget
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  readonly captures: ReadonlyArray<{
    readonly ordinal: number
    readonly parameterOrdinal: number
    readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
    readonly value: Value
  }>
}

export interface EffectValue {
  readonly _tag: 'EffectValue'
  readonly type: Type.Effect
  readonly site: Hir.EffectSiteId
  readonly runner: DeclarationFacts.CanonicalId
  readonly runnerTypeArguments: ReadonlyArray<Type.GenericArgument>
  readonly captures: ReadonlyArray<Value>
}

export interface EffectCompositeValue {
  readonly _tag: 'EffectCompositeValue'
  readonly alternative: number
  readonly effect: EffectValue
}

/** One logical heap block; identity and liveness live in evaluator state, not JS identity. */
export interface AllocationValue {
  readonly _tag: 'AllocationValue'
  readonly type: Type.Nominal
  readonly ticket: number
  readonly bytes: bigint
  readonly alignment: bigint
}

export interface RawBufferValue {
  readonly _tag: 'RawBufferValue'
  readonly type: Type.Nominal
  readonly ticket: number
  readonly count: bigint
  readonly element: Type.Type
  readonly stride: number
}

/** One affine handle to evaluator-owned local-shared control-block state. */
export interface SharedCoreValue {
  readonly _tag: 'SharedCoreValue'
  readonly type: Type.Nominal
  readonly ticket: number
  readonly element: Type.Type
}

/** One affine handle to evaluator-owned independent execution package state. */
export interface ExecutionValue {
  readonly _tag: 'ExecutionValue'
  readonly type: Type.Nominal
  readonly ticket: number
}

/** One generation-bound affine readiness authority for an evaluator execution package. */
export interface WakeValue {
  readonly _tag: 'WakeValue'
  readonly type: Type.Nominal
  readonly ticket: number
  readonly generation: number
}

export interface SlotValue {
  readonly _tag: 'SlotValue'
  readonly type: Type.Nominal
  readonly ticket: number
  readonly index: bigint
  readonly element: Type.Type
}

/** One immutable logical evaluator value, independent of backend lane realization. */
export type Value =
  | IntegerValue
  | EnumValue
  | CharacterValue
  | FloatValue
  | AggregateValue
  | NominalUnionValue
  | ArrayValue
  | SliceValue
  | StaticViewValue
  | StringValue
  | ReferenceValue
  | PointerValue
  | UnionValue
  | EffectBorrowValue
  | CallableBorrowValue
  | EffectValue
  | EffectCompositeValue
  | CallableValue
  | EffectOutcomeValue
  | AllocationValue
  | RawBufferValue
  | SharedCoreValue
  | ExecutionValue
  | WakeValue
  | SlotValue

/** Entered the resolved entry instance. */
