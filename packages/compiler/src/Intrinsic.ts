import type * as Hir from './Hir.js'
import * as Scalar from './Scalar.js'
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

/** Structural reason one source-callable primitive remains compiler-owned. */
export type AdmissionCategory =
  | 'Representation'
  | 'Scalar'
  | 'Ownership'
  | 'Effect'
  | 'Platform'
  | 'Language'

/** Closed execution surfaces against which intrinsic availability is validated. */
export type ExecutionTarget = 'Evaluator' | 'LLVM' | 'Wasm'

/** Canonical deterministic order for intrinsic execution targets. */
export const executionTargets: ReadonlyArray<ExecutionTarget> = Object.freeze([
  'Evaluator',
  'LLVM',
  'Wasm',
])

/** Normalizes an availability set to the compiler-owned target order. */
export const normalizeExecutionTargets = (
  targets: ReadonlyArray<ExecutionTarget>,
): ReadonlyArray<ExecutionTarget> =>
  Object.freeze(executionTargets.filter((target) => targets.includes(target)))

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
      readonly operation: 'Result' | 'BindRequirement'
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
  readonly admission?: AdmissionCategory
  readonly consumer?: string
  readonly targets: ReadonlyArray<ExecutionTarget>
  readonly invariant?: string
  readonly hostImport?: string
  readonly returnedBorrowParameter?: number
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
  readonly targets?: ReadonlyArray<ExecutionTarget>
  readonly returnedBorrowParameter?: number
}): Operation =>
  Object.freeze({
    _tag: 'IntrinsicOperation',
    id: operationId(options.actor, options.name),
    spelling: options.name,
    typeParameters: Object.freeze((options.typeParameters ?? []).map(typeParameter)),
    parameters: Object.freeze(Array.from(options.parameters)),
    result: options.result,
    unsafe: options.unsafe ?? false,
    targets: normalizeExecutionTargets(options.targets ?? executionTargets),
    ...(options.returnedBorrowParameter === undefined
      ? {}
      : { returnedBorrowParameter: options.returnedBorrowParameter }),
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
    targets: executionTargets,
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
const nativeTargets = Object.freeze<ReadonlyArray<ExecutionTarget>>(['Evaluator', 'LLVM'])
const byteSlice = Type.slice('Shared', 'u8')
const mutableI32 = Type.reference('Exclusive', 'i32')
const mutableU32 = Type.reference('Exclusive', 'u32')
const mutableUsize = Type.reference('Exclusive', 'usize')
const mutableHandle = Type.reference('Exclusive', Type.osHandle)

const osEffect = (value: Type.Type): Type.Effect =>
  Type.effect(value, Object.freeze([]), undefined, Object.freeze([]))

const osBuiltin = (options: {
  readonly name: string
  readonly operation: Extract<Hir.BuiltinOperation, `Os${string}`>
  readonly parameters: ReadonlyArray<ValueParameter>
  readonly semanticParameters: ReadonlyArray<Type.Type>
  readonly result: string
  readonly semanticResult: Type.Type
  readonly invariant: string
}): Operation =>
  Object.freeze({
    ...builtin({
      actor: 'Os',
      name: options.name,
      operation: options.operation,
      parameters: options.parameters,
      semanticParameters: options.semanticParameters,
      result: options.result,
      semanticResult: osEffect(options.semanticResult),
      unsafe: true,
      targets: nativeTargets,
    }),
    invariant: options.invariant,
  })

const scalarOperation = (scalar: Scalar.Scalar, operation: Scalar.Operation): Operation => {
  const concreteResult =
    operation.result === 'Self'
      ? scalar.spelling
      : operation.result === 'Boolean'
        ? Scalar.boolean.spelling
        : operation.result === 'OptionSelf'
          ? scalar.spelling
          : operation.result === 'OptionTarget'
            ? (Scalar.conversionTarget(operation.code)?.spelling ?? scalar.spelling)
            : operation.result
  const checked = operation.result === 'OptionSelf' || operation.result === 'OptionTarget'
  const result = checked ? `Option<${concreteResult}>` : concreteResult
  const semanticResult = checked ? Type.option(concreteResult) : concreteResult
  const parameterNames =
    operation.arity === 1 ? Object.freeze(['value']) : Object.freeze(['left', 'right'])
  const semanticParameters =
    operation.parameters ?? Object.freeze(parameterNames.map(() => scalar.spelling))
  return builtin({
    actor: scalar.spelling,
    name: operation.spelling,
    operation: operation.code,
    parameters: Object.freeze(
      parameterNames.map((name, ordinal) =>
        valueParameter(name, semanticParameters.at(ordinal) ?? scalar.spelling),
      ),
    ),
    semanticParameters,
    result,
    semanticResult,
  })
}

const scalarActor = (scalar: Scalar.Scalar): Actor =>
  actor(
    scalar.spelling,
    'Type',
    scalar.operations.map((operation) => scalarOperation(scalar, operation)),
  )

const stringOperations = actor(
  'string',
  'Type',
  Object.freeze([
    Object.freeze({
      ...builtin({
        actor: 'string',
        name: 'fromUtf8Unchecked',
        operation: 'StringFromUtf8Unchecked',
        parameters: Object.freeze([valueParameter('bytes', '&[u8]')]),
        semanticParameters: Object.freeze([byteSlice]),
        result: 'string',
        semanticResult: Type.string,
        unsafe: true,
        returnedBorrowParameter: 0,
      }),
      invariant:
        'bytes remain live and immutable for the returned view lifetime and contain complete valid UTF-8',
    }),
    builtin({
      actor: 'string',
      name: 'utf8Bytes',
      operation: 'StringUtf8Bytes',
      parameters: Object.freeze([valueParameter('value', 'string')]),
      semanticParameters: Object.freeze([Type.string]),
      result: '&[u8]',
      semanticResult: byteSlice,
      returnedBorrowParameter: 0,
    }),
    builtin({
      actor: 'string',
      name: 'byteLength',
      operation: 'StringByteLength',
      parameters: Object.freeze([valueParameter('value', 'string')]),
      semanticParameters: Object.freeze([Type.string]),
      result: 'usize',
      semanticResult: 'usize',
    }),
    builtin({
      actor: 'string',
      name: 'equalsExact',
      operation: 'StringEqualsExact',
      parameters: Object.freeze([
        valueParameter('left', 'string'),
        valueParameter('right', 'string'),
      ]),
      semanticParameters: Object.freeze([Type.string, Type.string]),
      result: 'bool',
      semanticResult: 'bool',
    }),
  ]),
)

const stringActor = actor('string', 'Type', Object.freeze([]))

const legacyActors = Object.freeze([
  ...Scalar.all().map(scalarActor),
  stringOperations,
  actor('OsHandle', 'Type', Object.freeze([])),
  actor(
    'Os',
    'Namespace',
    Object.freeze([
      osBuiltin({
        name: 'fileOpen',
        operation: 'OsFileOpen',
        parameters: Object.freeze([
          valueParameter('root', '&[u8]'),
          valueParameter('path', '&[u8]'),
          valueParameter('mode', 'i32'),
          valueParameter('reason', '&mut i32'),
          valueParameter('nativeCode', '&mut u32'),
        ]),
        semanticParameters: Object.freeze([byteSlice, byteSlice, 'i32', mutableI32, mutableU32]),
        result: 'Effect<Option<OsHandle>>',
        semanticResult: Type.option(Type.osHandle),
        invariant:
          'root is an absolute native path; path is normalized provider-absolute; outputs are initialized; traversal rejects symlinks and namespace escape',
      }),
      osBuiltin({
        name: 'fileRead',
        operation: 'OsFileRead',
        parameters: Object.freeze([
          valueParameter('handle', '&mut OsHandle'),
          valueParameter('output', '&mut [u8]'),
          valueParameter('reason', '&mut i32'),
          valueParameter('nativeCode', '&mut u32'),
        ]),
        semanticParameters: Object.freeze([
          mutableHandle,
          Type.slice('Exclusive', 'u8'),
          mutableI32,
          mutableU32,
        ]),
        result: 'Effect<Option<usize>>',
        semanticResult: Type.option('usize'),
        invariant:
          'handle is a live file; output is initialized writable storage; success reports the exact transferred byte count',
      }),
      osBuiltin({
        name: 'fileWrite',
        operation: 'OsFileWrite',
        parameters: Object.freeze([
          valueParameter('handle', '&mut OsHandle'),
          valueParameter('input', '&[u8]'),
          valueParameter('offset', 'usize'),
          valueParameter('reason', '&mut i32'),
          valueParameter('nativeCode', '&mut u32'),
        ]),
        semanticParameters: Object.freeze([
          mutableHandle,
          byteSlice,
          'usize',
          mutableI32,
          mutableU32,
        ]),
        result: 'Effect<Option<usize>>',
        semanticResult: Type.option('usize'),
        invariant:
          'handle is a live file; input is initialized; success reports the exact transferred byte count and may be partial',
      }),
      osBuiltin({
        name: 'directoryOpen',
        operation: 'OsDirectoryOpen',
        parameters: Object.freeze([
          valueParameter('root', '&[u8]'),
          valueParameter('path', '&[u8]'),
          valueParameter('reason', '&mut i32'),
          valueParameter('nativeCode', '&mut u32'),
        ]),
        semanticParameters: Object.freeze([byteSlice, byteSlice, mutableI32, mutableU32]),
        result: 'Effect<Option<OsHandle>>',
        semanticResult: Type.option(Type.osHandle),
        invariant: 'root and path satisfy confined traversal and outputs are initialized',
      }),
      osBuiltin({
        name: 'directoryNext',
        operation: 'OsDirectoryNext',
        parameters: Object.freeze([
          valueParameter('handle', '&mut OsHandle'),
          valueParameter('output', '&mut [u8]'),
          valueParameter('kind', '&mut i32'),
          valueParameter('requiredCapacity', '&mut usize'),
          valueParameter('reason', '&mut i32'),
          valueParameter('nativeCode', '&mut u32'),
        ]),
        semanticParameters: Object.freeze([
          mutableHandle,
          Type.slice('Exclusive', 'u8'),
          mutableI32,
          mutableUsize,
          mutableI32,
          mutableU32,
        ]),
        result: 'Effect<Option<usize>>',
        semanticResult: Type.option('usize'),
        invariant:
          'handle is a live directory; buffer-too-small does not advance and reports required capacity; zero means end',
      }),
      osBuiltin({
        name: 'pathInspect',
        operation: 'OsPathInspect',
        parameters: Object.freeze([
          valueParameter('root', '&[u8]'),
          valueParameter('path', '&[u8]'),
          valueParameter('kind', '&mut i32'),
          valueParameter('byteLength', '&mut usize'),
          valueParameter('reason', '&mut i32'),
          valueParameter('nativeCode', '&mut u32'),
        ]),
        semanticParameters: Object.freeze([
          byteSlice,
          byteSlice,
          mutableI32,
          mutableUsize,
          mutableI32,
          mutableU32,
        ]),
        result: 'Effect<bool>',
        semanticResult: 'bool',
        invariant:
          'root and path satisfy confined traversal; kind and byteLength outputs are initialized',
      }),
      ...(
        [
          ['directoryCreate', 'OsDirectoryCreate'],
          ['fileRemove', 'OsFileRemove'],
          ['directoryRemove', 'OsDirectoryRemove'],
        ] as const
      ).map(([name, operation]) =>
        osBuiltin({
          name,
          operation,
          parameters: Object.freeze([
            valueParameter('root', '&[u8]'),
            valueParameter('path', '&[u8]'),
            valueParameter('reason', '&mut i32'),
            valueParameter('nativeCode', '&mut u32'),
          ]),
          semanticParameters: Object.freeze([byteSlice, byteSlice, mutableI32, mutableU32]),
          result: 'Effect<bool>',
          semanticResult: 'bool',
          invariant: 'root and path satisfy confined traversal and failure outputs are initialized',
        }),
      ),
      osBuiltin({
        name: 'handleClose',
        operation: 'OsHandleClose',
        parameters: Object.freeze([
          valueParameter('handle', 'OsHandle'),
          valueParameter('reason', '&mut i32'),
          valueParameter('nativeCode', '&mut u32'),
        ]),
        semanticParameters: Object.freeze([Type.osHandle, mutableI32, mutableU32]),
        result: 'Effect<bool>',
        semanticResult: 'bool',
        invariant:
          'consumes exactly one live file or directory handle whether close succeeds or fails',
      }),
      osBuiltin({
        name: 'standardInputRead',
        operation: 'OsStandardInputRead',
        parameters: Object.freeze([
          valueParameter('output', '&mut [u8]'),
          valueParameter('reason', '&mut i32'),
          valueParameter('nativeCode', '&mut u32'),
        ]),
        semanticParameters: Object.freeze([Type.slice('Exclusive', 'u8'), mutableI32, mutableU32]),
        result: 'Effect<Option<usize>>',
        semanticResult: Type.option('usize'),
        invariant:
          'output is initialized writable storage; success reports the exact transferred byte count and zero means end of input',
      }),
      osBuiltin({
        name: 'processExecute',
        operation: 'OsProcessExecute',
        parameters: Object.freeze([
          valueParameter('program', '&[u8]'),
          valueParameter('arguments', '&[u8]'),
          valueParameter('environment', '&[u8]'),
          valueParameter('workingDirectory', '&[u8]'),
          valueParameter('status', '&mut i32'),
          valueParameter('code', '&mut i32'),
          valueParameter('outputLength', '&mut usize'),
          valueParameter('errorLength', '&mut usize'),
          valueParameter('reason', '&mut i32'),
          valueParameter('nativeCode', '&mut u32'),
        ]),
        semanticParameters: Object.freeze([
          byteSlice,
          byteSlice,
          byteSlice,
          byteSlice,
          mutableI32,
          mutableI32,
          mutableUsize,
          mutableUsize,
          mutableI32,
          mutableU32,
        ]),
        result: 'Effect<bool>',
        semanticResult: 'bool',
        invariant:
          'arguments and environment are NUL-terminated entry blocks and an empty workingDirectory inherits the caller directory; the child never interprets a shell and reads closed standard input; success retains exactly one capture until the next execute and reports its exact lengths with status zero for exit and one for signal',
      }),
      osBuiltin({
        name: 'processCapture',
        operation: 'OsProcessCapture',
        parameters: Object.freeze([
          valueParameter('stream', 'i32'),
          valueParameter('offset', 'usize'),
          valueParameter('output', '&mut [u8]'),
          valueParameter('reason', '&mut i32'),
          valueParameter('nativeCode', '&mut u32'),
        ]),
        semanticParameters: Object.freeze([
          'i32',
          'usize',
          Type.slice('Exclusive', 'u8'),
          mutableI32,
          mutableU32,
        ]),
        result: 'Effect<Option<usize>>',
        semanticResult: Type.option('usize'),
        invariant:
          'stream selects zero for standard output or one for standard error, offset is within the retained capture of the immediately preceding execute, and output is initialized writable storage',
      }),
      osBuiltin({
        name: 'hostArgumentCount',
        operation: 'OsHostArgumentCount',
        parameters: Object.freeze([
          valueParameter('count', '&mut usize'),
          valueParameter('reason', '&mut i32'),
          valueParameter('nativeCode', '&mut u32'),
        ]),
        semanticParameters: Object.freeze([mutableUsize, mutableI32, mutableU32]),
        result: 'Effect<bool>',
        semanticResult: 'bool',
        invariant: 'count output is initialized and reports the received argument count on success',
      }),
      osBuiltin({
        name: 'hostArgument',
        operation: 'OsHostArgument',
        parameters: Object.freeze([
          valueParameter('index', 'usize'),
          valueParameter('output', '&mut [u8]'),
          valueParameter('reason', '&mut i32'),
          valueParameter('nativeCode', '&mut u32'),
        ]),
        semanticParameters: Object.freeze([
          'usize',
          Type.slice('Exclusive', 'u8'),
          mutableI32,
          mutableU32,
        ]),
        result: 'Effect<Option<usize>>',
        semanticResult: Type.option('usize'),
        invariant:
          'output is initialized writable storage; success reports the complete argument byte length and copies the prefix that fits, and absence reports the not-found reason',
      }),
      osBuiltin({
        name: 'hostVariable',
        operation: 'OsHostVariable',
        parameters: Object.freeze([
          valueParameter('name', '&[u8]'),
          valueParameter('output', '&mut [u8]'),
          valueParameter('reason', '&mut i32'),
          valueParameter('nativeCode', '&mut u32'),
        ]),
        semanticParameters: Object.freeze([
          byteSlice,
          Type.slice('Exclusive', 'u8'),
          mutableI32,
          mutableU32,
        ]),
        result: 'Effect<Option<usize>>',
        semanticResult: Type.option('usize'),
        invariant:
          'output is initialized writable storage; success reports the complete value byte length and copies the prefix that fits, and an unset name reports the not-found reason',
      }),
      osBuiltin({
        name: 'hostWorkingDirectory',
        operation: 'OsHostWorkingDirectory',
        parameters: Object.freeze([
          valueParameter('output', '&mut [u8]'),
          valueParameter('reason', '&mut i32'),
          valueParameter('nativeCode', '&mut u32'),
        ]),
        semanticParameters: Object.freeze([Type.slice('Exclusive', 'u8'), mutableI32, mutableU32]),
        result: 'Effect<Option<usize>>',
        semanticResult: Type.option('usize'),
        invariant:
          'output is initialized writable storage; success reports the complete working-directory byte length and copies the prefix that fits',
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
    ]),
  ),
  actor(
    'Storage',
    'Namespace',
    Object.freeze([
      builtin({
        actor: 'Storage',
        name: 'acquire',
        operation: 'StorageAcquire',
        parameters: Object.freeze([valueParameter('layout', 'Layout')]),
        semanticParameters: Object.freeze([Type.layout]),
        result: 'Effect<Allocation ! OutOfMemory>',
        semanticResult: Type.effect(
          Type.allocation,
          Object.freeze([Type.outOfMemory]),
          undefined,
          Object.freeze([]),
        ),
      }),
    ]),
  ),
  actor(
    'Host',
    'Namespace',
    Object.freeze([
      builtin({
        actor: 'Host',
        name: 'write',
        operation: 'HostWrite',
        parameters: Object.freeze([
          valueParameter('destination', 'bool'),
          valueParameter('bytes', '&[u8]'),
        ]),
        semanticParameters: Object.freeze(['bool', Type.slice('Shared', 'u8')]),
        result: 'Effect<() ! StreamWriteFailure>',
        semanticResult: Type.effect(
          Type.unit,
          Object.freeze([Type.streamWriteFailure]),
          undefined,
          Object.freeze([]),
        ),
      }),
    ]),
  ),
  actor('Report', 'Type', Object.freeze([])),
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
          valueParameter('count', 'usize'),
        ]),
        semanticParameters: Object.freeze([Type.allocation, 'usize']),
        result: 'RawBuffer<T>',
        semanticResult: Type.rawBuffer(rawElement),
        unsafe: true,
      }),
      builtin({
        actor: 'RawBuffer',
        name: 'view',
        operation: 'RawBufferView',
        typeParameters: Object.freeze(['T']),
        semanticTypeParameters: rawTypeParameters,
        parameters: Object.freeze([
          valueParameter('buffer', '&RawBuffer<T>'),
          valueParameter('offset', 'usize'),
          valueParameter('length', 'usize'),
        ]),
        semanticParameters: Object.freeze([
          Type.reference('Shared', Type.rawBuffer(rawElement)),
          'usize',
          'usize',
        ]),
        result: '&[T]',
        semanticResult: Type.slice('Shared', rawElement),
        unsafe: true,
        returnedBorrowParameter: 0,
      }),
      builtin({
        actor: 'RawBuffer',
        name: 'viewMut',
        operation: 'RawBufferViewMut',
        typeParameters: Object.freeze(['T']),
        semanticTypeParameters: rawTypeParameters,
        parameters: Object.freeze([
          valueParameter('buffer', '&mut RawBuffer<T>'),
          valueParameter('offset', 'usize'),
          valueParameter('length', 'usize'),
        ]),
        semanticParameters: Object.freeze([
          Type.reference('Exclusive', Type.rawBuffer(rawElement)),
          'usize',
          'usize',
        ]),
        result: '&mut [T]',
        semanticResult: Type.slice('Exclusive', rawElement),
        unsafe: true,
        returnedBorrowParameter: 0,
      }),
      builtin({
        actor: 'RawBuffer',
        name: 'slot',
        operation: 'RawBufferSlot',
        typeParameters: Object.freeze(['T']),
        semanticTypeParameters: rawTypeParameters,
        parameters: Object.freeze([
          valueParameter('buffer', '&mut RawBuffer<T>'),
          valueParameter('index', 'usize'),
        ]),
        semanticParameters: Object.freeze([
          Type.reference('Exclusive', Type.rawBuffer(rawElement)),
          'usize',
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
        result: 'usize',
        semanticResult: 'usize',
      }),
      builtin({
        actor: 'RawBuffer',
        name: 'read',
        operation: 'RawBufferRead',
        typeParameters: Object.freeze(['T']),
        semanticTypeParameters: rawTypeParameters,
        parameters: Object.freeze([
          valueParameter('buffer', '&RawBuffer<T>'),
          valueParameter('index', 'usize'),
        ]),
        semanticParameters: Object.freeze([
          Type.reference('Shared', Type.rawBuffer(rawElement)),
          'usize',
        ]),
        result: 'T',
        semanticResult: rawElement,
        unsafe: true,
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
        result: '()',
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
        result: '()',
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
        name: 'result',
        operation: 'Result',
        typeParameters: Object.freeze([]),
        parameters: Object.freeze([valueParameter('protected', 'Effect<A ! E ? R>')]),
        result: 'Effect<Result<A, Row<!E>> ? R>',
      }),
      effect({
        name: 'bindRequirement',
        operation: 'BindRequirement',
        typeParameters: Object.freeze([]),
        parameters: Object.freeze([
          valueParameter('protected', 'Effect<A ! E ? Selected | Rest>'),
          valueParameter('provider', '&Provider'),
        ]),
        result: 'Effect<A ! E ? Rest>',
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
        targets: executionTargets,
        rule: Object.freeze({ _tag: 'PlaceRule', operation: 'Replace' }),
      }),
    ]),
  ),
])

const upperInitial = (value: string): string =>
  `${value.slice(0, 1).toUpperCase()}${value.slice(1)}`

const flatSpelling = (actor_: string, operation: string): string => {
  if (Scalar.isSpelling(actor_)) return `${actor_}${upperInitial(operation)}`
  if (actor_ === 'Effect' && operation === 'bindRequirement') return 'bindRequirement'
  if (actor_ === 'Place' && operation === 'replace') return 'replace'
  if (actor_ === 'Storage' && operation === 'acquire') return 'systemAllocationAcquire'
  if (actor_ === 'Host' && operation === 'write') return 'standardStreamWrite'
  if (actor_ === 'Os') return `os${upperInitial(operation)}`
  return `${actor_.slice(0, 1).toLowerCase()}${actor_.slice(1)}${upperInitial(operation)}`
}

/** The canonical standard-library consumer of one OS boundary operation. */
const osConsumer = (spelling: string): string => {
  if (spelling === 'standardInputRead') return 'silk/os-standard-input.read'
  if (spelling === 'processExecute') return 'silk/os-child-process.execute'
  if (spelling === 'processCapture') return 'silk/os-child-process.capture'
  if (spelling.startsWith('host'))
    return `silk/os-host-input.${spelling.slice(4, 5).toLowerCase()}${spelling.slice(5)}`
  return `silk/os-filesystem.${spelling}`
}

const flatOperations = Object.freeze(
  legacyActors.flatMap((actor_) =>
    actor_.operations.map((operation) => {
      const spelling = flatSpelling(actor_.spelling, operation.spelling)
      const admission: AdmissionCategory = Scalar.isSpelling(actor_.spelling)
        ? 'Scalar'
        : actor_.spelling === 'Effect'
          ? 'Effect'
          : actor_.spelling === 'Host' || actor_.spelling === 'Storage' || actor_.spelling === 'Os'
            ? 'Platform'
            : actor_.spelling === 'Layout' || actor_.spelling === 'string'
              ? 'Representation'
              : actor_.spelling === 'RawBuffer' || actor_.spelling === 'Slot'
                ? 'Ownership'
                : 'Language'
      const consumer = Scalar.isSpelling(actor_.spelling)
        ? `silk/${actor_.spelling}.${operation.spelling}`
        : actor_.spelling === 'Effect'
          ? `silk/effects.${operation.spelling}`
          : actor_.spelling === 'Storage'
            ? 'silk/core.allocate'
            : actor_.spelling === 'Host'
              ? 'silk/core.writeAll'
              : actor_.spelling === 'Os'
                ? osConsumer(operation.spelling)
                : actor_.spelling === 'Place'
                  ? 'language:place-replacement'
                  : `silk/${actor_.spelling.replaceAll(/([a-z])([A-Z])/g, '$1-$2').toLowerCase()}.${operation.spelling}`
      return Object.freeze({
        ...operation,
        id: operationId('Intrinsic', spelling),
        spelling,
        admission,
        consumer,
        targets: operation.targets,
        ...(operation.unsafe && operation.invariant === undefined
          ? {
              invariant:
                actor_.spelling === 'RawBuffer'
                  ? 'caller proves raw-buffer bounds, ownership, and initializedness required by the operation'
                  : 'caller proves the selected slot is in bounds and has the initializedness state required by the operation',
            }
          : {}),
        ...(operation.invariant === undefined ? {} : { invariant: operation.invariant }),
        ...(actor_.spelling === 'Host' && operation.spelling === 'write'
          ? { hostImport: 'silk_standard_stream_write_v1' }
          : {}),
      })
    }),
  ),
)

const operations = Object.freeze([stringActor, actor('Intrinsic', 'Namespace', flatOperations)])

/** Every intrinsic actor in stable presentation and completion order. */
export const all = (): ReadonlyArray<Actor> => operations

/** Finds an intrinsic actor by its accepted source spelling. */
export const findActor = (spelling: string): Actor | undefined =>
  operations.find((candidate) => candidate.spelling === spelling)

/** Finds an intrinsic operation by actor and member source spelling. */
export const findOperation = (actor_: string, spelling: string): Operation | undefined =>
  findActor(actor_)?.operations.find((candidate) => candidate.spelling === spelling) ??
  (Scalar.isSpelling(actor_)
    ? findActor('Intrinsic')?.operations.find(
        (candidate) => candidate.spelling === `${actor_}${upperInitial(spelling)}`,
      )
    : undefined)

/** Finds one sealed operation by its canonical compiler identity. */
export const findOperationById = (id: OperationId): Operation | undefined =>
  flatOperations.find(
    (candidate) => candidate.id.actor === id.actor && candidate.id.name === id.name,
  )

/** Stable textual form of one sealed operation identity. */
export const operationText = (id: OperationId): string => `${id.actor}.${id.name}`

/** One deterministic audit record spanning source identity and execution surfaces. */
export interface InventoryEntry {
  readonly operation: string
  readonly signature: string
  readonly unsafe: boolean
  readonly invariant?: string
  readonly admission: AdmissionCategory
  readonly consumer: string
  readonly hir: string
  readonly mir: string
  readonly evaluator: string
  readonly targets: ReadonlyArray<ExecutionTarget>
  readonly hostImport?: string
}

/** Publishes the closed intrinsic inventory used by verification and release review. */
export const inventory = (): ReadonlyArray<InventoryEntry> =>
  Object.freeze(
    flatOperations.map((operation) => {
      if (
        operation.admission === undefined ||
        operation.consumer === undefined ||
        operation.targets.length === 0
      )
        throw new RangeError(`Intrinsic ${operation.spelling} is missing admission metadata`)
      const normalizedTargets = normalizeExecutionTargets(operation.targets)
      if (
        normalizedTargets.length !== operation.targets.length ||
        normalizedTargets.some((target, index) => operation.targets.at(index) !== target)
      )
        throw new RangeError(`Intrinsic ${operation.spelling} has non-normalized target metadata`)
      const identity =
        operation.rule._tag === 'BuiltinRule'
          ? operation.rule.operation
          : `${operation.rule._tag}.${operation.rule.operation}`
      return Object.freeze({
        operation: `Intrinsic.${operation.spelling}`,
        signature: signature(operation),
        unsafe: operation.unsafe,
        ...(operation.invariant === undefined ? {} : { invariant: operation.invariant }),
        admission: operation.admission,
        consumer: operation.consumer,
        hir: identity,
        mir: identity,
        evaluator: identity,
        targets: normalizedTargets,
        ...(operation.hostImport === undefined ? {} : { hostImport: operation.hostImport }),
      })
    }),
  )

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
