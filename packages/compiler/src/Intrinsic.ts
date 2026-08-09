import type * as Hir from './Hir.js'
import * as Type from './Type.js'

/** Stable identity of one compiler-provided actor with no source declaration. */
export interface ActorId {
  readonly _tag: 'IntrinsicActorId'
  readonly name: string
}

/** Stable identity of one compiler-provided operation with no source declaration. */
export interface OperationId {
  readonly _tag: 'IntrinsicOperationId'
  readonly actor: string
  readonly name: string
}

/** One named generic parameter used by source-like presentation and completion. */
export interface TypeParameter {
  readonly name: string
}

/** One named value parameter used by source-like presentation and completion. */
export interface ValueParameter {
  readonly name: string
  readonly type: string
}

/** The elaboration rule selected by an intrinsic operation identity. */
export type Rule =
  | {
      readonly _tag: 'BuiltinRule'
      readonly operation: Hir.BuiltinOperation
      readonly typeParameters: ReadonlyArray<Type.Parameter>
      readonly parameters: ReadonlyArray<Type.Type>
      readonly result: Type.Type
    }
  | {
      readonly _tag: 'EffectRule'
      readonly operation: 'Catch' | 'Retry' | 'Map' | 'FlatMap' | 'Tap' | 'Provide' | 'ProvideWith'
    }
  | { readonly _tag: 'PlaceRule'; readonly operation: 'Replace' }

/** One compiler-provided operation shared by analysis, presentation, and completion. */
export interface Operation {
  readonly _tag: 'IntrinsicOperation'
  readonly id: OperationId
  readonly spelling: string
  readonly typeParameters: ReadonlyArray<TypeParameter>
  readonly parameters: ReadonlyArray<ValueParameter>
  readonly result: string
  readonly unsafe: boolean
  readonly rule: Rule
}

/** One compiler-provided source actor or namespace. */
export interface Actor {
  readonly _tag: 'IntrinsicActor'
  readonly id: ActorId
  readonly spelling: string
  readonly kind: 'Type' | 'Namespace'
  readonly operations: ReadonlyArray<Operation>
}

const actorId = (name: string): ActorId => Object.freeze({ _tag: 'IntrinsicActorId', name })

const operationId = (actor: string, name: string): OperationId =>
  Object.freeze({ _tag: 'IntrinsicOperationId', actor, name })

const typeParameter = (name: string): TypeParameter => Object.freeze({ name })

const valueParameter = (name: string, type: string): ValueParameter => Object.freeze({ name, type })

const builtin = (options: {
  readonly actor: string
  readonly name: string
  readonly operation: Hir.BuiltinOperation
  readonly typeParameters?: ReadonlyArray<string>
  readonly semanticTypeParameters?: ReadonlyArray<Type.Parameter>
  readonly parameters: ReadonlyArray<ValueParameter>
  readonly semanticParameters: ReadonlyArray<Type.Type>
  readonly result: string
  readonly semanticResult: Type.Type
  readonly unsafe?: boolean
}): Operation =>
  Object.freeze({
    _tag: 'IntrinsicOperation',
    id: operationId(options.actor, options.name),
    spelling: options.name,
    typeParameters: Object.freeze((options.typeParameters ?? []).map(typeParameter)),
    parameters: Object.freeze(Array.from(options.parameters)),
    result: options.result,
    unsafe: options.unsafe ?? false,
    rule: Object.freeze({
      _tag: 'BuiltinRule',
      operation: options.operation,
      typeParameters: Object.freeze(Array.from(options.semanticTypeParameters ?? [])),
      parameters: Object.freeze(Array.from(options.semanticParameters)),
      result: options.semanticResult,
    }),
  })

const effect = (options: {
  readonly name: string
  readonly operation: Extract<Rule, { readonly _tag: 'EffectRule' }>['operation']
  readonly typeParameters: ReadonlyArray<string>
  readonly parameters: ReadonlyArray<ValueParameter>
  readonly result: string
}): Operation =>
  Object.freeze({
    _tag: 'IntrinsicOperation',
    id: operationId('Effect', options.name),
    spelling: options.name,
    typeParameters: Object.freeze(options.typeParameters.map(typeParameter)),
    parameters: Object.freeze(Array.from(options.parameters)),
    result: options.result,
    unsafe: false,
    rule: Object.freeze({ _tag: 'EffectRule', operation: options.operation }),
  })

const actor = (
  spelling: string,
  kind: Actor['kind'],
  operations: ReadonlyArray<Operation>,
): Actor =>
  Object.freeze({
    _tag: 'IntrinsicActor',
    id: actorId(spelling),
    spelling,
    kind,
    operations: Object.freeze(Array.from(operations)),
  })

const rawElement = Type.parameter({ module: 'silk/core', name: '$RawStorage' }, 0, 'T')
const rawTypeParameters = Object.freeze([rawElement])

const normalizedUnion = (members: ReadonlyArray<Type.Nominal>): Type.Type => {
  const normalized = Type.union(members)
  return normalized._tag === 'Normalized' ? normalized.type : 'Never'
}

const binaryI32 = (name: string, operation: Hir.BuiltinOperation): Operation =>
  builtin({
    actor: 'I32',
    name,
    operation,
    parameters: Object.freeze([valueParameter('left', 'I32'), valueParameter('right', 'I32')]),
    semanticParameters: Object.freeze(['I32', 'I32']),
    result: 'I32',
    semanticResult: 'I32',
  })

const compareI32 = (name: string, operation: Hir.BuiltinOperation): Operation =>
  builtin({
    actor: 'I32',
    name,
    operation,
    parameters: Object.freeze([valueParameter('left', 'I32'), valueParameter('right', 'I32')]),
    semanticParameters: Object.freeze(['I32', 'I32']),
    result: 'Bool',
    semanticResult: 'Bool',
  })

const binaryUsize = (
  name: string,
  operation: Hir.BuiltinOperation,
  result: 'Usize' | 'Bool',
): Operation =>
  builtin({
    actor: 'Usize',
    name,
    operation,
    parameters: Object.freeze([valueParameter('left', 'Usize'), valueParameter('right', 'Usize')]),
    semanticParameters: Object.freeze(['Usize', 'Usize']),
    result,
    semanticResult: result,
  })

const operations = Object.freeze([
  actor(
    'I32',
    'Type',
    Object.freeze([
      builtin({
        actor: 'I32',
        name: 'negate',
        operation: 'Negate',
        parameters: Object.freeze([valueParameter('value', 'I32')]),
        semanticParameters: Object.freeze(['I32']),
        result: 'I32',
        semanticResult: 'I32',
      }),
      binaryI32('add', 'Add'),
      binaryI32('subtract', 'Subtract'),
      binaryI32('multiply', 'Multiply'),
      binaryI32('divide', 'Divide'),
      binaryI32('remainder', 'Remainder'),
      compareI32('equals', 'Equals'),
      compareI32('notEquals', 'NotEquals'),
      compareI32('lessThan', 'LessThan'),
      compareI32('lessOrEqual', 'LessOrEqual'),
      compareI32('greaterThan', 'GreaterThan'),
      compareI32('greaterOrEqual', 'GreaterOrEqual'),
    ]),
  ),
  actor(
    'Usize',
    'Type',
    Object.freeze([
      binaryUsize('add', 'Add', 'Usize'),
      binaryUsize('subtract', 'Subtract', 'Usize'),
      binaryUsize('multiply', 'Multiply', 'Usize'),
      binaryUsize('divide', 'Divide', 'Usize'),
      binaryUsize('remainder', 'Remainder', 'Usize'),
      binaryUsize('equals', 'Equals', 'Bool'),
      binaryUsize('notEquals', 'NotEquals', 'Bool'),
      binaryUsize('lessThan', 'LessThan', 'Bool'),
      binaryUsize('lessOrEqual', 'LessOrEqual', 'Bool'),
      binaryUsize('greaterThan', 'GreaterThan', 'Bool'),
      binaryUsize('greaterOrEqual', 'GreaterOrEqual', 'Bool'),
    ]),
  ),
  actor(
    'Bool',
    'Type',
    Object.freeze([
      builtin({
        actor: 'Bool',
        name: 'equals',
        operation: 'Equals',
        parameters: Object.freeze([
          valueParameter('left', 'Bool'),
          valueParameter('right', 'Bool'),
        ]),
        semanticParameters: Object.freeze(['Bool', 'Bool']),
        result: 'Bool',
        semanticResult: 'Bool',
      }),
      builtin({
        actor: 'Bool',
        name: 'notEquals',
        operation: 'NotEquals',
        parameters: Object.freeze([
          valueParameter('left', 'Bool'),
          valueParameter('right', 'Bool'),
        ]),
        semanticParameters: Object.freeze(['Bool', 'Bool']),
        result: 'Bool',
        semanticResult: 'Bool',
      }),
      builtin({
        actor: 'Bool',
        name: 'not',
        operation: 'Not',
        parameters: Object.freeze([valueParameter('value', 'Bool')]),
        semanticParameters: Object.freeze(['Bool']),
        result: 'Bool',
        semanticResult: 'Bool',
      }),
    ]),
  ),
  actor(
    'Layout',
    'Type',
    Object.freeze([
      builtin({
        actor: 'Layout',
        name: 'of',
        operation: 'LayoutOf',
        typeParameters: Object.freeze(['T']),
        semanticTypeParameters: rawTypeParameters,
        parameters: Object.freeze([]),
        semanticParameters: Object.freeze([]),
        result: 'Layout',
        semanticResult: Type.layout,
      }),
      builtin({
        actor: 'Layout',
        name: 'make',
        operation: 'LayoutMake',
        parameters: Object.freeze([
          valueParameter('size', 'Usize'),
          valueParameter('alignment', 'Usize'),
        ]),
        semanticParameters: Object.freeze(['Usize', 'Usize']),
        result: 'Layout | InvalidAlignment',
        semanticResult: normalizedUnion([Type.layout, Type.invalidAlignment]),
      }),
      builtin({
        actor: 'Layout',
        name: 'repeat',
        operation: 'LayoutRepeat',
        parameters: Object.freeze([
          valueParameter('layout', 'Layout'),
          valueParameter('count', 'Usize'),
        ]),
        semanticParameters: Object.freeze([Type.layout, 'Usize']),
        result: 'Layout | LayoutOverflow',
        semanticResult: normalizedUnion([Type.layout, Type.layoutOverflow]),
      }),
    ]),
  ),
  actor(
    'SystemAllocator',
    'Type',
    Object.freeze([
      builtin({
        actor: 'SystemAllocator',
        name: 'make',
        operation: 'SystemAllocatorMake',
        parameters: Object.freeze([]),
        semanticParameters: Object.freeze([]),
        result: 'SystemAllocator',
        semanticResult: Type.systemAllocator,
      }),
    ]),
  ),
  actor(
    'Allocator',
    'Type',
    Object.freeze([
      builtin({
        actor: 'Allocator',
        name: 'allocate',
        operation: 'AllocatorAllocate',
        parameters: Object.freeze([valueParameter('layout', 'Layout')]),
        semanticParameters: Object.freeze([Type.layout]),
        result: 'Effect<Allocation ! OutOfMemory ? &mut Allocator>',
        semanticResult: Type.effect(
          Type.allocation,
          Object.freeze([Type.outOfMemory]),
          'Exclusive',
          Object.freeze([
            Object.freeze({
              capability: Type.allocator,
              role: 'DefaultRole',
              access: 'Exclusive',
            }),
          ]),
        ),
      }),
    ]),
  ),
  actor('Report', 'Type', Object.freeze([])),
  actor(
    'Unit',
    'Type',
    Object.freeze([
      builtin({
        actor: 'Unit',
        name: 'make',
        operation: 'UnitMake',
        parameters: Object.freeze([]),
        semanticParameters: Object.freeze([]),
        result: 'Unit',
        semanticResult: Type.unit,
      }),
    ]),
  ),
  actor(
    'RawBuffer',
    'Type',
    Object.freeze([
      builtin({
        actor: 'RawBuffer',
        name: 'from',
        operation: 'RawBufferFrom',
        typeParameters: Object.freeze(['T']),
        semanticTypeParameters: rawTypeParameters,
        parameters: Object.freeze([
          valueParameter('allocation', 'Allocation'),
          valueParameter('count', 'Usize'),
        ]),
        semanticParameters: Object.freeze([Type.allocation, 'Usize']),
        result: 'RawBuffer<T>',
        semanticResult: Type.rawBuffer(rawElement),
        unsafe: true,
      }),
      builtin({
        actor: 'RawBuffer',
        name: 'slot',
        operation: 'RawBufferSlot',
        typeParameters: Object.freeze(['T']),
        semanticTypeParameters: rawTypeParameters,
        parameters: Object.freeze([
          valueParameter('buffer', '&mut RawBuffer<T>'),
          valueParameter('index', 'Usize'),
        ]),
        semanticParameters: Object.freeze([
          Type.reference('Exclusive', Type.rawBuffer(rawElement)),
          'Usize',
        ]),
        result: 'Slot<T>',
        semanticResult: Type.slot(rawElement),
        unsafe: true,
      }),
      builtin({
        actor: 'RawBuffer',
        name: 'count',
        operation: 'RawBufferCount',
        typeParameters: Object.freeze(['T']),
        semanticTypeParameters: rawTypeParameters,
        parameters: Object.freeze([valueParameter('buffer', '&RawBuffer<T>')]),
        semanticParameters: Object.freeze([Type.reference('Shared', Type.rawBuffer(rawElement))]),
        result: 'Usize',
        semanticResult: 'Usize',
      }),
    ]),
  ),
  actor(
    'Slot',
    'Type',
    Object.freeze([
      builtin({
        actor: 'Slot',
        name: 'write',
        operation: 'SlotWrite',
        typeParameters: Object.freeze(['T']),
        semanticTypeParameters: rawTypeParameters,
        parameters: Object.freeze([
          valueParameter('slot', 'Slot<T>'),
          valueParameter('value', 'T'),
        ]),
        semanticParameters: Object.freeze([Type.slot(rawElement), rawElement]),
        result: 'Unit',
        semanticResult: Type.unit,
        unsafe: true,
      }),
      builtin({
        actor: 'Slot',
        name: 'take',
        operation: 'SlotTake',
        typeParameters: Object.freeze(['T']),
        semanticTypeParameters: rawTypeParameters,
        parameters: Object.freeze([valueParameter('slot', 'Slot<T>')]),
        semanticParameters: Object.freeze([Type.slot(rawElement)]),
        result: 'T',
        semanticResult: rawElement,
        unsafe: true,
      }),
      builtin({
        actor: 'Slot',
        name: 'copy',
        operation: 'SlotCopy',
        typeParameters: Object.freeze(['T']),
        semanticTypeParameters: rawTypeParameters,
        parameters: Object.freeze([valueParameter('slot', 'Slot<T>')]),
        semanticParameters: Object.freeze([Type.slot(rawElement)]),
        result: 'T',
        semanticResult: rawElement,
        unsafe: true,
      }),
      builtin({
        actor: 'Slot',
        name: 'drop',
        operation: 'SlotDrop',
        typeParameters: Object.freeze(['T']),
        semanticTypeParameters: rawTypeParameters,
        parameters: Object.freeze([valueParameter('slot', 'Slot<T>')]),
        semanticParameters: Object.freeze([Type.slot(rawElement)]),
        result: 'Unit',
        semanticResult: Type.unit,
        unsafe: true,
      }),
    ]),
  ),
  actor(
    'Effect',
    'Namespace',
    Object.freeze([
      effect({
        name: 'catch',
        operation: 'Catch',
        typeParameters: Object.freeze(['E']),
        parameters: Object.freeze([
          valueParameter('protected', 'Effect<A ! E | Rest>'),
          valueParameter('handler', 'fn(E) -> Effect<A ! HandlerFailures>'),
        ]),
        result: 'Effect<A ! Rest | HandlerFailures>',
      }),
      effect({
        name: 'retry',
        operation: 'Retry',
        typeParameters: Object.freeze([]),
        parameters: Object.freeze([
          valueParameter('protected', 'Effect<A ! E>'),
          valueParameter('retries', 'I32'),
        ]),
        result: 'Effect<A ! E>',
      }),
      effect({
        name: 'map',
        operation: 'Map',
        typeParameters: Object.freeze([]),
        parameters: Object.freeze([
          valueParameter('protected', 'Effect<A ! E>'),
          valueParameter('callback', 'fn(A) -> B'),
        ]),
        result: 'Effect<B ! E>',
      }),
      effect({
        name: 'flatMap',
        operation: 'FlatMap',
        typeParameters: Object.freeze([]),
        parameters: Object.freeze([
          valueParameter('protected', 'Effect<A ! E>'),
          valueParameter('callback', 'fn(A) -> Effect<B ! F>'),
        ]),
        result: 'Effect<B ! E | F>',
      }),
      effect({
        name: 'tap',
        operation: 'Tap',
        typeParameters: Object.freeze([]),
        parameters: Object.freeze([
          valueParameter('protected', 'Effect<A ! E>'),
          valueParameter('callback', 'fn(A) -> Effect<B ! F>'),
        ]),
        result: 'Effect<A ! E | F>',
      }),
      effect({
        name: 'provide',
        operation: 'Provide',
        typeParameters: Object.freeze([]),
        parameters: Object.freeze([
          valueParameter('protected', 'Effect<A ? Capability>'),
          valueParameter('provider', 'Provider'),
        ]),
        result: 'Effect<A>',
      }),
      effect({
        name: 'provideWith',
        operation: 'ProvideWith',
        typeParameters: Object.freeze([]),
        parameters: Object.freeze([
          valueParameter('protected', 'Effect<A ? Capability>'),
          valueParameter('acquire', 'Effect<Provider>'),
        ]),
        result: 'Effect<A>',
      }),
    ]),
  ),
  actor(
    'Place',
    'Namespace',
    Object.freeze([
      Object.freeze({
        _tag: 'IntrinsicOperation',
        id: operationId('Place', 'replace'),
        spelling: 'replace',
        typeParameters: Object.freeze([typeParameter('T')]),
        parameters: Object.freeze([
          valueParameter('place', '&mut T'),
          valueParameter('value', 'T'),
        ]),
        result: 'T',
        unsafe: false,
        rule: Object.freeze({ _tag: 'PlaceRule', operation: 'Replace' }),
      }),
    ]),
  ),
])

/** Every intrinsic actor in stable presentation and completion order. */
export const all = (): ReadonlyArray<Actor> => operations

/** Finds an intrinsic actor by its accepted source spelling. */
export const findActor = (spelling: string): Actor | undefined =>
  operations.find((candidate) => candidate.spelling === spelling)

/** Finds an intrinsic operation by actor and member source spelling. */
export const findOperation = (actor_: string, spelling: string): Operation | undefined =>
  findActor(actor_)?.operations.find((candidate) => candidate.spelling === spelling)

/** Renders the source-like signature shared by hover and completion detail. */
export const signature = (self: Operation): string => {
  const typeParameters =
    self.typeParameters.length === 0
      ? ''
      : `<${self.typeParameters.map((parameter) => parameter.name).join(', ')}>`
  const parameters = self.parameters
    .map((parameter) => `${parameter.name}: ${parameter.type}`)
    .join(', ')
  return `${self.unsafe ? 'unsafe ' : ''}fn ${self.id.actor}.${self.spelling}${typeParameters}(${parameters}) -> ${self.result}`
}
