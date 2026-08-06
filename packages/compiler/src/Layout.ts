import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Diagnostic from './Diagnostic.js'
import type * as Hir from './Hir.js'
import type * as Instances from './Instances.js'
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
  readonly callingShapes: ReadonlyArray<CallingShape>
}

/** One compiler-owned scalar lane used to realize a logical value at a call boundary. */
export interface CallingLane {
  readonly _tag: 'CallingLane'
  readonly path: ReadonlyArray<Selector>
  readonly type: Type.Builtin
}

export type Selector =
  | DeclarationIndex.FieldId
  | { readonly _tag: 'ElementSelector'; readonly index: number }
  | { readonly _tag: 'UnionTagSelector' }
  | { readonly _tag: 'UnionPayloadSelector'; readonly slot: number }

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
      readonly _tag: 'SumShape'
      readonly type: Type.StructuralUnion
      readonly tag: { readonly type: 'I32'; readonly lane: 0 }
      readonly payloadLaneCount: number
      readonly zeroFill: true
      readonly members: ReadonlyArray<{
        readonly member: Type.Nominal
        readonly ordinal: number
        readonly shape: CallingShapeNode
        readonly payloadSlots: ReadonlyArray<number>
      }>
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

const scalarEntry = (type: Type.Builtin): Entry => (type === 'Bool' ? bool() : i32())

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

const dependenciesOf = (struct: DeclarationIndex.StructFact): ReadonlyArray<Type.Nominal> => {
  const dependencies = new Map<string, Type.Nominal>()
  for (const field of struct.fields) {
    const types =
      field.declaredType._tag === 'Resolved'
        ? Type.nominals(field.declaredType.type)
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
export const catalog = (target: Target.Target, index: DeclarationIndex.Index): Catalog => {
  const declarations = index.modules
    .flatMap((module) => module.structs)
    .flatMap((struct) => {
      const type = nominalOf(struct)
      return type === undefined ? [] : [Object.freeze({ struct, type })]
    })
    .sort((left, right) => Type.compare(left.type, right.type))
  const byType = new Map(
    declarations.map((declaration) => [Type.key(declaration.type), declaration]),
  )
  const completed = new Map<string, CatalogEntry>()
  const visiting = new Set<string>()

  const layoutNominal = (type: Type.Nominal): CatalogEntry => {
    const key = Type.key(type)
    const existing = completed.get(key)
    if (existing !== undefined) return existing
    const declaration = byType.get(key)
    if (declaration === undefined) {
      return unavailable(type, Object.freeze([]), {
        _tag: 'InvalidDeclaration',
        detail: `missing canonical declaration for ${Type.encode(type)}`,
      })
    }
    const dependencies = dependenciesOf(declaration.struct)
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
      const fieldType = field.declaredType.type
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
          type: field.declaredType.type,
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
    if (Type.isBuiltin(type)) return scalarEntry(type)
    if (Type.isNever(type)) {
      return unavailable(type, Object.freeze([]), {
        _tag: 'InvalidDeclaration',
        detail: 'Never is uninhabited and has no runtime layout',
      })
    }
    if (Type.isNominal(type)) return layoutNominal(type)
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
    referenced.set(Type.key(type), type)
    if (Type.isFixedArray(type)) addReferenced(type.element)
    if (Type.isUnion(type)) for (const member of type.members) addReferenced(member)
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
  for (const declaration of declarations) layoutNominal(declaration.type)
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
): void => {
  if (expression._tag === 'Unavailable') return
  types.set(Type.key(expression.type), expression.type)
  if (expression._tag === 'Move') addExpressionTypes(types, expression.subject)
  if (expression._tag === 'UnionConvert') addExpressionTypes(types, expression.source)
  if (expression._tag === 'Project') addExpressionTypes(types, expression.subject)
  if (expression._tag === 'IndexPlace') {
    addExpressionTypes(types, expression.subject)
    addExpressionTypes(types, expression.index)
  }
  if (expression._tag === 'Construct') {
    for (const field of expression.fields) addExpressionTypes(types, field.value)
  }
  if (expression._tag === 'ArrayConstruct') {
    for (const element of expression.elements) addExpressionTypes(types, element)
  }
  if (expression._tag === 'Call' || expression._tag === 'BuiltinCall') {
    for (const argument of expression.arguments) addExpressionTypes(types, argument)
  }
}

const addStatementTypes = (
  types: Map<string, DeclarationIndex.SemanticType>,
  statements: ReadonlyArray<Hir.Statement>,
): void => {
  for (const statement of statements) {
    if (statement._tag === 'Bind') addExpressionTypes(types, statement.initializer)
    if (statement._tag === 'Return') addExpressionTypes(types, statement.expression)
    if (statement._tag === 'If') {
      addExpressionTypes(types, statement.condition)
      addStatementTypes(types, statement.taken)
      addStatementTypes(types, statement.otherwise)
    }
    if (statement._tag === 'Write') {
      addExpressionTypes(types, statement.value)
      for (const selector of statement.place.selectors) {
        if (selector._tag === 'Index') addExpressionTypes(types, selector.index)
      }
    }
    if (statement._tag === 'While') {
      addExpressionTypes(types, statement.condition)
      addStatementTypes(types, statement.body)
    }
  }
}

const addFunctionTypes = (
  types: Map<string, DeclarationIndex.SemanticType>,
  fn: Hir.HirFunction,
): void => {
  for (const parameter of fn.declaration.parameters) {
    if (parameter.declaredType._tag === 'Resolved') {
      types.set(Type.key(parameter.declaredType.type), parameter.declaredType.type)
    }
  }
  if (fn.declaration.returnType._tag === 'Resolved') {
    types.set(Type.key(fn.declaration.returnType.type), fn.declaration.returnType.type)
  }
  addStatementTypes(types, fn.statements)
}

/** Selects runtime-reachable entries while reusing nominal decisions from the catalog. */
export const plan = (self: Catalog, discovery: Instances.Discovery): Plan => {
  const reached = new Map<string, DeclarationIndex.SemanticType>()
  for (const instance of discovery.instances) addFunctionTypes(reached, instance.function)
  const entries = new Map<string, Entry>()
  const resolve = (type: DeclarationIndex.SemanticType): Entry | undefined => {
    if (Type.isBuiltin(type)) return scalarEntry(type)
    if (Type.isNever(type)) return undefined
    const candidate = catalogEntry(self, type)
    if (candidate?._tag === 'LayoutEntry') return candidate
    if (!Type.isFixedArray(type) || candidate?._tag === 'UnavailableLayoutEntry') return undefined
    const element = resolve(type.element)
    return element === undefined ? undefined : repeatedEntry(type, element)
  }
  const add = (type: DeclarationIndex.SemanticType): void => {
    const key = Type.key(type)
    if (entries.has(key)) return
    const candidate = resolve(type)
    if (candidate === undefined) return
    entries.set(key, candidate)
    if (candidate.representation._tag === 'Aggregate') {
      for (const field of candidate.representation.fields) add(field.type)
    } else if (candidate.representation._tag === 'Repeated') {
      add(candidate.representation.element)
    } else if (candidate.representation._tag === 'Union') {
      for (const member of candidate.representation.members) add(member.type)
    }
  }
  for (const type of reached.values()) add(type)
  const orderedEntries = Object.freeze(
    [...entries.values()].sort((left, right) => Type.compare(left.type, right.type)),
  )
  return Object.freeze({
    _tag: 'LayoutPlan',
    target: self.target,
    entries: orderedEntries,
    callingShapes: callingShapes(orderedEntries),
  })
}

/** Constructs a scalar plan for hand-built MIR samples and focused tests. */
export const make = (target: Target.Target, types: ReadonlyArray<Type.Builtin>): Plan => {
  const entries = new Map(types.map((type) => [Type.key(type), scalarEntry(type)]))
  const orderedEntries = Object.freeze(
    [...entries.values()].sort((left, right) => Type.compare(left.type, right.type)),
  )
  return Object.freeze({
    _tag: 'LayoutPlan',
    target,
    entries: orderedEntries,
    callingShapes: callingShapes(orderedEntries),
  })
}

const shapeNode = (
  type: DeclarationIndex.SemanticType,
  entries: ReadonlyMap<string, Entry>,
): CallingShapeNode => {
  if (Type.isBuiltin(type)) {
    return Object.freeze({ _tag: 'ScalarShape', type, laneCount: 1 })
  }
  if (Type.isNever(type)) {
    return Object.freeze({ _tag: 'EmptyShape', type, laneCount: 0 })
  }
  const candidate = entries.get(Type.key(type))
  if (Type.isFixedArray(type)) {
    const element = shapeNode(type.element, entries)
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
        const shape = shapeNode(member, entries)
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
    return Object.freeze({
      _tag: 'SumShape',
      type,
      tag: Object.freeze({ type: 'I32', lane: 0 }),
      payloadLaneCount,
      zeroFill: true,
      members,
      laneCount: 1 + payloadLaneCount,
    })
  }
  const fields =
    candidate?.representation._tag === 'Aggregate'
      ? candidate.representation.fields.map((field) =>
          Object.freeze({ field: field.id, shape: shapeNode(field.type, entries) }),
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
          type: 'I32' as const,
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
  type: DeclarationIndex.SemanticType,
  entries: ReadonlyMap<string, Entry>,
): CallingShape => {
  const tree = shapeNode(type, entries)
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

const callingShapes = (entries: ReadonlyArray<Entry>): ReadonlyArray<CallingShape> => {
  const byType = new Map(entries.map((candidate) => [Type.key(candidate.type), candidate]))
  return Object.freeze(entries.map((candidate) => shapeOf(candidate.type, byType)))
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

/** Looks up one available or unavailable nominal catalog entry. */
export const catalogEntry = (
  self: Catalog,
  type: DeclarationIndex.SemanticType,
): CatalogEntry | undefined => self.entries.find((candidate) => Type.equals(candidate.type, type))

const representationEquals = (left: Representation, right: Representation): boolean => {
  if (left._tag !== right._tag) return false
  if (left._tag === 'SignedInteger')
    return right._tag === 'SignedInteger' && left.bits === right.bits
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
  candidate: Entry,
  available: ReadonlyMap<string, Entry>,
): ReadonlyArray<Violation> => {
  if (Type.isBuiltin(candidate.type)) {
    const expected = scalarEntry(candidate.type)
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
    const element = available.get(Type.key(candidate.type.element))
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
      ? scalarEntry(field.type)
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
    if (candidate._tag === 'LayoutEntry') violations.push(...verifyEntry(candidate, available))
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
        : right._tag === 'FieldId' && fieldIdEquals(left, right)

const verifyCallingShapes = (self: Plan): ReadonlyArray<Violation> => {
  const expected = callingShapes(self.entries)
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
          lane.type === other.type &&
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
  if (self.callingShapes.length !== self.entries.length) {
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

/** Verifies canonical target, ordering, uniqueness, representation, and ABI facts. */
export const verify = (self: Plan): ReadonlyArray<Violation> =>
  Object.freeze([...commonViolations(self.target, self.entries), ...verifyCallingShapes(self)])

/** Verifies all available entries and deterministic ordering within a nominal catalog. */
export const verifyCatalog = (self: Catalog): ReadonlyArray<Violation> =>
  commonViolations(self.target, self.entries)

/** Verifies that every planned nominal layout is exactly the catalog decision. */
export const verifyAgainstCatalog = (self: Plan, catalog: Catalog): ReadonlyArray<Violation> =>
  Object.freeze(
    self.entries.flatMap((candidate) => {
      if (Type.isBuiltin(candidate.type) || Type.isFixedArray(candidate.type)) return []
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
    : representation._tag === 'Boolean'
      ? `bool-i${representation.bits} false=${representation.falseValue} true=${representation.trueValue}`
      : representation._tag === 'Repeated'
        ? `repeated element=${Type.encode(representation.element)} length=${representation.length} stride=${representation.stride}`
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
      : candidate.representation._tag === 'Union'
        ? candidate.representation.members.map(
            (member) =>
              `  member ${member.ordinal} ${Type.encode(member.type)} size=${member.size} align=${member.alignment}`,
          )
        : []),
]

/** Deterministic textual encoding of a complete runtime layout plan. */
export const encode = (self: Plan): string =>
  [
    `target ${Target.encode(self.target)}`,
    ...self.entries.flatMap(entryLines),
    ...self.callingShapes.map(
      (shape) =>
        `calling ${Type.encode(shape.type)} lanes=${shape.laneCount}${
          shape.laneCount === 0
            ? ''
            : ` ${shape.lanes
                .map(
                  (lane) =>
                    `${lane.type}[${lane.path
                      .map((selector) =>
                        selector._tag === 'ElementSelector'
                          ? `[${selector.index}]`
                          : selector._tag === 'UnionTagSelector'
                            ? 'tag'
                            : selector._tag === 'UnionPayloadSelector'
                              ? `payload[${selector.slot}]`
                              : `${selector.struct.sourceId}#${selector.struct.ordinal}.${selector.ordinal}`,
                      )
                      .join('.')}]`,
                )
                .join(',')}`
        }`,
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
