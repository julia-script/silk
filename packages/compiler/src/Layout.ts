import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Hir from './Hir.js'
import type * as Instances from './Instances.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Target from './Target.js'
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
  | { readonly _tag: 'SignedInteger'; readonly bits: 32 }
  | { readonly _tag: 'UnsignedInteger'; readonly bits: 32 | 64 }
  | { readonly _tag: 'Boolean'; readonly bits: 32; readonly falseValue: 0; readonly trueValue: 1 }
  | {
      readonly _tag: 'Aggregate'
      readonly fields: ReadonlyArray<Field>
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
      readonly length: { readonly type: 'I32'; readonly offset: number; readonly size: 4 }
      readonly addressPadding: number
      readonly tailPadding: number
      readonly stride: number
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
        readonly type: Type.Nominal
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

/** Every canonical nominal declaration laid out for one selected target. */
export interface Catalog {
  readonly _tag: 'LayoutCatalog'
  readonly target: Target.Target
  readonly entries: ReadonlyArray<CatalogEntry>
}

/** The concrete layouts reached by one target-aware MIR program. */
export interface Plan {
  readonly _tag: 'LayoutPlan'
  readonly target: Target.Target
  readonly entries: ReadonlyArray<Entry>
  readonly effectEnvironments: ReadonlyArray<EffectEnvironment>
  readonly callableEnvironments: ReadonlyArray<CallableEnvironment>
  readonly callingShapes: ReadonlyArray<CallingShape>
  readonly literalVerdicts: ReadonlyArray<UsizeLiteralVerdict>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

/** Target-owned storage for one monomorphized hidden Effect closure environment. */
export type EffectEnvironment =
  | {
      readonly _tag: 'EffectEnvironment'
      readonly instance: Instances.InstanceKey
      readonly site: Hir.EffectSiteId
      readonly effect: Type.Effect
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
  readonly representation: 'Value' | 'Borrow'
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

/** A target-owned verdict for one reachable exact contextual `Usize` literal. */
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
  | { readonly _tag: 'UnionTagSelector' }
  | { readonly _tag: 'UnionPayloadSelector'; readonly slot: number }
  | { readonly _tag: 'SliceAddressSelector' }
  | { readonly _tag: 'SliceLengthSelector' }
  | { readonly _tag: 'ReferenceAddressSelector' }

export type CallingShapeNode =
  | { readonly _tag: 'EmptyShape'; readonly type: Type.Never; readonly laneCount: 0 }
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
      readonly length: { readonly type: 'I32'; readonly lane: 1 }
      readonly laneCount: 2
    }
  | {
      readonly _tag: 'ReferenceShape'
      readonly type: Type.Reference
      readonly address: { readonly type: AddressScalar; readonly lane: 0 }
      readonly laneCount: 1
    }
  | {
      readonly _tag: 'SumShape'
      readonly type: Type.StructuralUnion
      readonly tag: { readonly type: 'I32'; readonly lane: 0 }
      readonly payloadLaneCount: number
      readonly payloadTypes: ReadonlyArray<Type.Builtin>
      readonly zeroFill: true
      readonly members: ReadonlyArray<{
        readonly member: Type.Nominal
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
        readonly type: Type.Nominal
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

const i32 = (): Entry =>
  Object.freeze({
    _tag: 'LayoutEntry',
    type: 'I32',
    size: 4,
    alignment: 4,
    representation: Object.freeze({ _tag: 'SignedInteger', bits: 32 }),
  })

const bool = (): Entry =>
  Object.freeze({
    _tag: 'LayoutEntry',
    type: 'Bool',
    size: 4,
    alignment: 4,
    representation: Object.freeze({
      _tag: 'Boolean',
      bits: 32,
      falseValue: 0,
      trueValue: 1,
    }),
  })

const usize = (target: Target.Target): Entry =>
  Object.freeze({
    _tag: 'LayoutEntry',
    type: 'Usize',
    size: target.pointerSize,
    alignment: target.pointerAlignment,
    representation: Object.freeze({
      _tag: 'UnsignedInteger',
      bits: target.pointerSize === 4 ? 32 : 64,
    }),
  })

const scalarEntry = (target: Target.Target, type: Type.Builtin): Entry =>
  type === 'Bool' ? bool() : type === 'Usize' ? usize(target) : i32()

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
  const lengthOffset = alignUp(target.pointerSize, 4)
  const alignment = Math.max(target.pointerAlignment, 4)
  const contentSize = lengthOffset + 4
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
      length: Object.freeze({ type: 'I32', offset: lengthOffset, size: 4 }),
      addressPadding: lengthOffset - target.pointerSize,
      tailPadding: size - contentSize,
      stride: alignUp(element.size, element.alignment),
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

const nominalOf = (struct: DeclarationIndex.StructFact): Type.Nominal | undefined =>
  struct.canonical._tag === 'Canonical'
    ? Type.nominal(struct.canonical.id.module, struct.canonical.id.name)
    : undefined

const dependenciesOf = (
  struct: DeclarationIndex.StructFact,
  substitution: ReadonlyMap<string, Type.Type> = new Map(),
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

  const layoutNominal = (type: Type.Nominal): CatalogEntry => {
    const key = Type.key(type)
    const existing = completed.get(key)
    if (existing !== undefined) return existing
    if (Type.isIntrinsicNominal(type)) {
      const ordinal = Type.intrinsicNominalOrdinal(type)
      const structId: DeclarationIndex.DeclarationId = Object.freeze({
        _tag: 'DeclarationId',
        sourceId: type.module,
        ordinal,
      })
      const fieldTypes: ReadonlyArray<readonly [string, Type.Type]> = Type.equals(
        type,
        Type.layout,
      )
        ? Object.freeze([
            Object.freeze(['bytes', 'Usize'] as const),
            Object.freeze(['alignment', 'Usize'] as const),
          ])
        : Type.equals(type, Type.invalidAlignment)
          ? Object.freeze([Object.freeze(['alignment', 'Usize'] as const)])
          : Type.equals(type, Type.allocation)
            ? Object.freeze([
                Object.freeze(['$base', 'Usize'] as const),
                Object.freeze(['$bytes', 'Usize'] as const),
                Object.freeze(['$alignment', 'Usize'] as const),
                Object.freeze(['$reclaim', 'Usize'] as const),
                Object.freeze(['$context', 'Usize'] as const),
                Object.freeze(['$active', 'Usize'] as const),
              ])
            : Type.isRawBuffer(type)
              ? Object.freeze([
                  Object.freeze(['$allocation', Type.allocation] as const),
                  Object.freeze(['count', 'Usize'] as const),
                ])
              : Type.isSlot(type)
                ? Object.freeze([Object.freeze(['$address', 'Usize'] as const)])
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
        fields.push(Object.freeze({
          _tag: 'LayoutField',
          id: Object.freeze({ _tag: 'FieldId', struct: structId, ordinal: fieldOrdinal }),
          name,
          type: fieldType,
          offset,
          size: fieldLayout.size,
          alignment: fieldLayout.alignment,
          padding: offset - previous,
        }))
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
      const fieldLayout = layoutType(fieldType)
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
    const entry: Entry = Object.freeze({
      _tag: 'LayoutEntry',
      type,
      size,
      alignment: aggregateAlignment,
      representation: Object.freeze({
        _tag: 'Aggregate',
        fields: Object.freeze(fields),
        tailPadding: size - cursor,
      }),
    })
    completed.set(key, entry)
    return entry
  }

  const layoutType = (type: DeclarationIndex.SemanticType): CatalogEntry => {
    if (Type.isBuiltin(type)) return scalarEntry(target, type)
    if (Type.isNever(type)) {
      return unavailable(type, Object.freeze([]), {
        _tag: 'InvalidDeclaration',
        detail: 'Never is uninhabited and has no runtime layout',
      })
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
        const memberLayout = layoutNominal(member)
        if (memberLayout._tag === 'UnavailableLayoutEntry') {
          const result = unavailable(
            type,
            type.members,
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
    if (!Type.isConcrete(type)) return
    referenced.set(Type.key(type), type)
    if (Type.isFixedArray(type)) addReferenced(type.element)
    if (Type.isSlice(type)) addReferenced(type.element)
    else if (Type.isReference(type)) addReferenced(type.target)
    if (Type.isUnion(type)) for (const member of type.members) addReferenced(member)
    if (Type.isEffect(type)) {
      addReferenced(type.success)
      for (const failure of type.failures) addReferenced(failure)
    }
  }
  for (const module of index.modules) {
    for (const member of module.members) {
      if (member._tag === 'FunctionDeclaration') {
        for (const parameter of member.parameters) {
          if (parameter.declaredType._tag === 'Resolved') addReferenced(parameter.declaredType.type)
        }
        if (member.returnType._tag === 'Resolved') addReferenced(member.returnType.type)
      } else {
        for (const field of member.fields) {
          if (field.declaredType._tag === 'Resolved') addReferenced(field.declaredType.type)
        }
      }
    }
  }
  for (const declaration of declarations) {
    if (declaration.struct.typeParameters.length === 0) layoutNominal(declaration.type)
  }
  for (const instance of discovery?.instances ?? []) {
    const substitution = instance.substitution
    if (instance.function.contract._tag === 'Contract') {
      for (const parameter of instance.function.contract.parameters) {
        addReferenced(Type.substitute(parameter, substitution))
      }
      addReferenced(Type.substitute(instance.function.contract.result, substitution))
    }
    const addSpecializedExpression = (expression: Hir.Expression): void => {
      if (expression._tag === 'Unavailable') return
      addReferenced(Type.substitute(expression.type, substitution))
      for (const child of Hir.expressionTree(expression).slice(1)) {
        if (child._tag !== 'Unavailable') addReferenced(Type.substitute(child.type, substitution))
      }
      for (const child of Hir.expressionTree(expression)) {
        if (child._tag !== 'BuiltinCall') continue
        for (const argument of child.typeArguments)
          addReferenced(Type.substitute(argument, substitution))
      }
    }
    for (const statement of instance.function.statements) {
      for (const expression of Hir.statementExpressions(statement))
        addSpecializedExpression(expression)
    }
  }
  for (const type of referenced.values()) {
    if (!Type.isBuiltin(type) && !Type.isNever(type)) layoutType(type)
  }

  return Object.freeze({
    _tag: 'LayoutCatalog',
    target,
    entries: Object.freeze(
      [...completed.values()].sort((left, right) => Type.compare(left.type, right.type)),
    ),
  })
}

const addExpressionTypes = (
  types: Map<string, DeclarationIndex.SemanticType>,
  expression: Hir.Expression,
  substitution: ReadonlyMap<string, Type.Type> = new Map(),
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
    expression._tag === 'BuiltinCall'
  ) {
    for (const argument of expression.arguments) addExpressionTypes(types, argument, substitution)
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
  if (expression._tag === 'EffectCatch') {
    addExpressionTypes(types, expression.protected, substitution)
    addExpressionTypes(types, expression.handler, substitution)
  }
  if (expression._tag === 'EffectRetry') {
    addExpressionTypes(types, expression.protected, substitution)
    addExpressionTypes(types, expression.retries, substitution)
  }
  if (expression._tag === 'EffectTransform') {
    addExpressionTypes(types, expression.protected, substitution)
    addExpressionTypes(types, expression.callback, substitution)
  }
  if (expression._tag === 'EffectProvide')
    addExpressionTypes(types, expression.protected, substitution)
  if (expression._tag === 'EffectProvideWith') {
    addExpressionTypes(types, expression.protected, substitution)
    addExpressionTypes(types, expression.acquisition, substitution)
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
  substitution: ReadonlyMap<string, Type.Type> = new Map(),
): void => {
  for (const statement of statements) {
    if (statement._tag === 'Unsafe') addStatementTypes(types, statement.statements, substitution)
    if (statement._tag === 'Bind') addExpressionTypes(types, statement.initializer, substitution)
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
): ReadonlyArray<EffectEnvironment> => {
  const layouts = new Map(
    entries.map((candidate) => [Type.key(candidate.type), candidate] as const),
  )
  const environments: Array<EffectEnvironment> = []

  for (const instance of discovery.instances) {
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
    for (const block of blocks) {
      const effect = Type.substitute(block.type, instance.substitution)
      if (!Type.isEffect(effect)) continue
      let cursor = 0
      let environmentAlignment = 1
      let unavailable: string | undefined
      const fields: Array<EffectEnvironmentField> = []
      for (const capture of block.captures) {
        const source = capture.binding === undefined ? 'Parameter' : 'Binding'
        const ordinal = capture.binding?.ordinal ?? capture.parameter?.ordinal
        const type =
          capture.binding === undefined
            ? instance.function.contract._tag === 'Contract' && ordinal !== undefined
              ? instance.function.contract.parameters.at(ordinal)
              : undefined
            : ordinal === undefined
              ? undefined
              : bindingTypes.get(ordinal)
        if (ordinal === undefined || type === undefined) {
          unavailable = `capture ${source.toLowerCase()} has no concrete type`
          break
        }
        const specialized = Type.substitute(type, instance.substitution)
        const borrowed = capture.access === 'Shared' || capture.access === 'Exclusive'
        const valueLayout = borrowed ? undefined : layouts.get(Type.key(specialized))
        if (!borrowed && valueLayout === undefined) {
          unavailable = `capture ${source.toLowerCase()} ${ordinal} has no value layout`
          break
        }
        const size = borrowed ? target.pointerSize : (valueLayout?.size ?? 0)
        const alignment = borrowed ? target.pointerAlignment : (valueLayout?.alignment ?? 1)
        const offset = alignUp(cursor, alignment)
        fields.push(
          Object.freeze({
            source,
            ordinal,
            access: capture.access,
            type: specialized,
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
      environments.push(
        Object.freeze({
          _tag: 'EffectEnvironment',
          instance: instance.key,
          site: block.site,
          effect,
          fields: Object.freeze(fields),
          size,
          alignment: environmentAlignment,
          tailPadding: size - cursor,
        }),
      )
    }
  }

  return Object.freeze(
    environments.sort(
      (left, right) =>
        left.instance.declaration.module.localeCompare(right.instance.declaration.module) ||
        left.instance.declaration.name.localeCompare(right.instance.declaration.name) ||
        left.site.span.start - right.site.span.start,
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
): {
  readonly verdicts: ReadonlyArray<UsizeLiteralVerdict>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const bits: 32 | 64 = target.pointerSize === 4 ? 32 : 64
  const maximum = bits === 32 ? 4294967295n : 18446744073709551615n
  const verdicts: Array<UsizeLiteralVerdict> = []
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const seen = new Set<string>()
  for (const instance of discovery.instances) {
    const expressions = instance.function.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)
    for (const expression of expressions) {
      if (
        expression._tag !== 'IntegerLiteral' ||
        Type.substitute(expression.type, instance.substitution) !== 'Usize'
      ) {
        continue
      }
      const value = BigInt(expression.value)
      const key = `${expression.span.sourceId}:${expression.span.start}:${expression.span.end}:${value}`
      if (seen.has(key)) continue
      seen.add(key)
      if (value <= maximum) {
        verdicts.push(
          Object.freeze({
            _tag: 'AvailableUsizeLiteral',
            value,
            bits,
            span: expression.span,
          }),
        )
        continue
      }
      const diagnostic = Diagnostic.usizeTargetOutOfRange(
        value.toString(),
        target.id,
        bits,
        expression.span,
      )
      diagnostics.push(diagnostic)
      verdicts.push(
        Object.freeze({
          _tag: 'UnavailableUsizeLiteral',
          value,
          bits,
          span: expression.span,
          cause: Diagnostic.identity(diagnostic),
        }),
      )
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
  for (const callable of discovery.callables) {
    for (const capture of callable.captures) reached.set(Type.key(capture.type), capture.type)
  }
  const entries = new Map<string, Entry>()
  const resolve = (type: DeclarationIndex.SemanticType): Entry | undefined => {
    if (Type.isBuiltin(type)) return scalarEntry(self.target, type)
    if (Type.isNever(type)) return undefined
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
      for (const failure of type.failures) add(failure)
      return
    }
    if (entries.has(key)) return
    const candidate = resolve(type)
    if (candidate === undefined) return
    entries.set(key, candidate)
    if (candidate.representation._tag === 'Aggregate') {
      for (const field of candidate.representation.fields) add(field.type)
    } else if (candidate.representation._tag === 'Repeated') {
      add(candidate.representation.element)
    } else if (candidate.representation._tag === 'Slice') {
      add(candidate.representation.element)
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
  const literals = usizeLiteralVerdicts(self.target, discovery)
  const shaped = new Map(orderedEntries.map((entry) => [Type.key(entry.type), entry.type] as const))
  for (const type of reached.values()) {
    if (Type.isConcrete(type) && (Type.isEffect(type) || Type.isNever(type)))
      shaped.set(Type.key(type), type)
  }
  const shapeTypes = Object.freeze([...shaped.values()].sort(Type.compare))
  return Object.freeze({
    _tag: 'LayoutPlan',
    target: self.target,
    entries: orderedEntries,
    effectEnvironments: effectEnvironments(self.target, orderedEntries, discovery),
    callableEnvironments: callableEnvironments(self.target, orderedEntries, discovery),
    callingShapes: callingShapes(self.target, orderedEntries, shapeTypes),
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
    literalVerdicts: Object.freeze([]),
    diagnostics: Object.freeze([]),
  })
}

const shapeNode = (
  target: Target.Target,
  type: DeclarationIndex.SemanticType,
  entries: ReadonlyMap<string, Entry>,
): CallingShapeNode => {
  if (Type.isBuiltin(type)) {
    return Object.freeze({ _tag: 'ScalarShape', type, laneCount: 1 })
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
      length: Object.freeze({ type: 'I32', lane: 1 }),
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
  const candidate = entries.get(Type.key(type))
  if (Type.isFixedArray(type)) {
    const element = shapeNode(target, type.element, entries)
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
        const shape = shapeNode(target, member, entries)
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
      Array.from(
        { length: payloadLaneCount },
        (_, slot): Type.Builtin =>
          members.some((member) => materializeLanes(member.shape).at(slot)?.type === 'Usize')
            ? 'Usize'
            : 'I32',
      ),
    )
    return Object.freeze({
      _tag: 'SumShape',
      type,
      tag: Object.freeze({ type: 'I32', lane: 0 }),
      payloadLaneCount,
      payloadTypes,
      zeroFill: true,
      members,
      laneCount: 1 + payloadLaneCount,
    })
  }
  if (Type.isEffect(type)) {
    const success = shapeNode(target, type.success, entries)
    const failures = type.failures.map((failure, index) =>
      Object.freeze({
        type: failure,
        tag: index + 1,
        shape: shapeNode(target, failure, entries),
      }),
    )
    const variants = [success, ...failures.map((failure) => failure.shape)]
    const payloadLaneCount = variants.reduce(
      (maximum, variant) => Math.max(maximum, variant.laneCount),
      0,
    )
    const payloadTypes = Object.freeze(
      Array.from(
        { length: payloadLaneCount },
        (_, slot): Type.Builtin =>
          variants.some((variant) => materializeLanes(variant).at(slot)?.type === 'Usize')
            ? 'Usize'
            : 'I32',
      ),
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
          Object.freeze({ field: field.id, shape: shapeNode(target, field.type, entries) }),
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
        type: 'I32',
      }),
    ])
  }
  if (node._tag === 'ReferenceShape') {
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
  if (node._tag === 'SumShape') {
    return Object.freeze([
      Object.freeze({
        _tag: 'CallingLane' as const,
        path: Object.freeze([...path, Object.freeze({ _tag: 'UnionTagSelector' as const })]),
        type: 'I32' as const,
      }),
      ...Array.from({ length: node.payloadLaneCount }, (_, slot) =>
        Object.freeze({
          _tag: 'CallingLane' as const,
          path: Object.freeze([
            ...path,
            Object.freeze({ _tag: 'UnionPayloadSelector' as const, slot }),
          ]),
          type: node.payloadTypes.at(slot) ?? ('I32' as const),
        }),
      ),
    ])
  }
  if (node._tag === 'OutcomeShape') {
    return Object.freeze([
      Object.freeze({
        _tag: 'CallingLane' as const,
        path: Object.freeze([...path, Object.freeze({ _tag: 'UnionTagSelector' as const })]),
        type: 'I32' as const,
      }),
      ...Array.from({ length: node.payloadLaneCount }, (_, slot) =>
        Object.freeze({
          _tag: 'CallingLane' as const,
          path: Object.freeze([
            ...path,
            Object.freeze({ _tag: 'UnionPayloadSelector' as const, slot }),
          ]),
          type: node.payloadTypes.at(slot) ?? ('I32' as const),
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
): CallingShape => {
  const tree = shapeNode(target, type, entries)
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
): ReadonlyArray<CallingShape> => {
  const byType = new Map(entries.map((candidate) => [Type.key(candidate.type), candidate]))
  return Object.freeze(types.map((type) => shapeOf(target, type, byType)))
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

/** Materializes the ABI lanes of one hidden Effect environment separately from its outcome. */
export const effectEnvironmentLanes = (
  self: Plan,
  environment: Extract<EffectEnvironment, { readonly _tag: 'EffectEnvironment' }>,
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
  member: Type.Nominal,
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
  if (left._tag === 'Boolean') {
    return (
      right._tag === 'Boolean' &&
      left.bits === right.bits &&
      left.falseValue === right.falseValue &&
      left.trueValue === right.trueValue
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
  return (
    right._tag === 'Aggregate' &&
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
    return Object.freeze([
      invalid('InvalidAggregate', candidate.type, 'Never cannot have a runtime layout entry'),
    ])
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
    : left._tag === 'UnionTagSelector'
      ? right._tag === 'UnionTagSelector'
      : left._tag === 'UnionPayloadSelector'
        ? right._tag === 'UnionPayloadSelector' && left.slot === right.slot
        : left._tag === 'SliceAddressSelector'
          ? right._tag === 'SliceAddressSelector'
          : left._tag === 'SliceLengthSelector'
            ? right._tag === 'SliceLengthSelector'
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
  const expected = callingShapes(self.target, self.entries)
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
          type: 'Usize',
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
        type: 'Usize',
        detail: 'target literal diagnostics do not match unavailable verdicts',
      }),
    )
  }
  return Object.freeze(violations)
}

/** Verifies canonical target, ordering, uniqueness, representation, and ABI facts. */
export const verify = (self: Plan): ReadonlyArray<Violation> =>
  Object.freeze([
    ...commonViolations(self.target, self.entries),
    ...verifyCallingShapes(self),
    ...verifyLiteralVerdicts(self),
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
      : representation._tag === 'Boolean'
        ? `bool-i${representation.bits} false=${representation.falseValue} true=${representation.trueValue}`
        : representation._tag === 'Repeated'
          ? `repeated element=${Type.encode(representation.element)} length=${representation.length} stride=${representation.stride}`
          : representation._tag === 'Slice'
            ? `slice element=${Type.encode(representation.element)} address=i${representation.address.bits}@${representation.address.offset}/${representation.address.size}/${representation.address.alignment} length=I32@${representation.length.offset}/4 address-padding=${representation.addressPadding} tail-padding=${representation.tailPadding} stride=${representation.stride}`
            : representation._tag === 'Reference'
              ? `reference target=${Type.encode(representation.target)} address=i${representation.address.bits}@${representation.address.offset}/${representation.address.size}/${representation.address.alignment}`
            : representation._tag === 'Union'
              ? `union tag=i${representation.tag.bits} payload-offset=${representation.payloadOffset} payload-size=${representation.payloadSize} payload-align=${representation.payloadAlignment} tag-padding=${representation.tagPadding} tail-padding=${representation.tailPadding}`
              : `aggregate tail-padding=${representation.tailPadding}`

const entryLines = (candidate: Entry): ReadonlyArray<string> => [
  `layout ${Type.encode(candidate.type)} size=${candidate.size} align=${candidate.alignment} repr=${representationText(candidate.representation)}`,
  ...(candidate.representation._tag === 'Aggregate'
    ? candidate.representation.fields.map(
        (field) =>
          `  field ${field.id.ordinal} ${field.name}: ${Type.encode(field.type)} offset=${field.offset} size=${field.size} align=${field.alignment} padding=${field.padding}`,
      )
    : candidate.representation._tag === 'Repeated'
      ? [
          `  elements ${Type.encode(candidate.representation.element)} count=${candidate.representation.length} stride=${candidate.representation.stride}`,
        ]
      : candidate.representation._tag === 'Slice'
        ? [
            `  address Address<${Type.encode(candidate.representation.element)}> bits=${candidate.representation.address.bits} offset=${candidate.representation.address.offset} size=${candidate.representation.address.size} align=${candidate.representation.address.alignment}`,
            `  length I32 offset=${candidate.representation.length.offset} size=4 stride=${candidate.representation.stride}`,
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
        ? `effect-environment ${environment.instance.declaration.module}.${environment.instance.declaration.name}@${environment.site.span.start} unavailable=${environment.reason}`
        : `effect-environment ${environment.instance.declaration.module}.${environment.instance.declaration.name}@${environment.site.span.start} size=${environment.size} align=${environment.alignment} fields=${environment.fields.map((field) => `${field.source.toLowerCase()}${field.ordinal}:${field.access.toLowerCase()}:${field.representation.toLowerCase()}@${field.offset}`).join(',') || 'none'}`,
    ),
    ...self.callableEnvironments.map((environment) => {
      const callable = environment.callable
      const identity = `${callable.owner.declaration.module}.${callable.owner.declaration.name}@${callable.site.span.start}`
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
                          : selector._tag === 'UnionTagSelector'
                            ? 'tag'
                            : selector._tag === 'UnionPayloadSelector'
                              ? `payload[${selector.slot}]`
                              : selector._tag === 'SliceAddressSelector'
                                ? 'address'
                                : selector._tag === 'SliceLengthSelector'
                                  ? 'length'
                                  : selector._tag === 'ReferenceAddressSelector'
                                    ? 'address'
                                    : `${selector.struct.sourceId}#${selector.struct.ordinal}.${selector.ordinal}`,
                      )
                      .join('.')}]`,
                )
                .join(',')}`
        }`,
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
    '',
  ].join('\n')
