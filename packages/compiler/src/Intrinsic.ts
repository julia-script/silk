import * as CallableContract from './CallableContract.js'
import * as Constraint from './Constraint.js'
import type * as Hir from './Hir.js'
import * as RowAlgebra from './RowAlgebra.js'
import * as Scalar from './Scalar.js'
import * as SourceSpan from './SourceSpan.js'
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
      readonly operation: 'Result'
    }
  | {
      readonly _tag: 'ContractRule'
      readonly contract: CallableContract.CallableContract
      readonly post: 'BindRequirement' | 'CatchFailure'
      readonly providerMode?: Constraint.ProviderMode
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

/** A target-neutral primitive with one resolved source-call ownership contract. */
export interface BuiltinOperation extends Operation {
  readonly callParameters: ReadonlyArray<Type.Type>
  readonly rule: Extract<Rule, { readonly _tag: 'BuiltinRule' }>
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

const upperInitial = (value: string): string =>
  `${value.slice(0, 1).toUpperCase()}${value.slice(1)}`

const intrinsicSpelling = (family: string, operation: string): string => {
  if (Scalar.isSpelling(family)) return `${family}${upperInitial(operation)}`
  if (family === 'Effect' && operation === 'suspendEffect') return 'suspendEffect'
  if (family === 'Effect' && operation.startsWith('bindRequirement')) return operation
  if (family === 'Effect' && operation === 'catchFailure') return operation
  if (family === 'Place' && operation === 'replace') return 'replace'
  if (family === 'Storage' && operation === 'acquire') return 'systemAllocationAcquire'
  if (family === 'Host' && operation === 'write') return 'standardStreamWrite'
  if (family === 'Os') return `os${upperInitial(operation)}`
  return `${family.slice(0, 1).toLowerCase()}${family.slice(1)}${upperInitial(operation)}`
}

const admission = (family: string): AdmissionCategory => {
  if (Scalar.isSpelling(family)) return 'Scalar'
  if (family === 'Effect') return 'Effect'
  if (family === 'Host' || family === 'Storage' || family === 'Os') return 'Platform'
  if (family === 'Layout' || family === 'string') return 'Representation'
  if (family === 'RawBuffer' || family === 'Slot' || family === 'Shared') return 'Ownership'
  return 'Language'
}

/** The canonical standard-library consumer of one OS boundary operation. */
const osConsumer = (spelling: string): string => {
  if (spelling === 'standardInputRead') return 'silk/os_standard_input.read'
  if (spelling === 'processExecute') return 'silk/os_child_process.execute'
  if (spelling === 'processCapture') return 'silk/os_child_process.capture'
  if (spelling.startsWith('host'))
    return `silk/os_host_input.${spelling.slice(4, 5).toLowerCase()}${spelling.slice(5)}`
  return `silk/os_filesystem.${spelling}`
}

const consumer = (family: string, operation: string): string => {
  if (Scalar.isSpelling(family)) return `silk/${family}.${operation}`
  if (family === 'Effect') return `silk/effect.${operation}`
  if (family === 'Shared')
    return operation === 'layout' || operation === 'fromAllocation'
      ? 'silk/shared.make'
      : `silk/shared.${operation}`
  if (family === 'Storage') return 'silk/core.allocate'
  if (family === 'Host') return 'silk/core.writeAll'
  if (family === 'Os') return osConsumer(operation)
  if (family === 'Place') return 'language:place-replacement'
  return `silk/${family.replaceAll(/([a-z])([A-Z])/g, '$1_$2').toLowerCase()}.${operation}`
}

const builtin = (options: {
  readonly actor: string
  readonly name: string
  readonly operation: Hir.BuiltinOperation
  readonly typeParameters?: ReadonlyArray<string>
  readonly semanticTypeParameters?: ReadonlyArray<Type.Parameter>
  readonly parameters: ReadonlyArray<ValueParameter>
  readonly semanticParameters: ReadonlyArray<Type.Type>
  readonly callParameters?: ReadonlyArray<Type.Type>
  readonly result: string
  readonly semanticResult: Type.Type
  readonly unsafe?: boolean
  readonly targets?: ReadonlyArray<ExecutionTarget>
  readonly returnedBorrowParameter?: number
}): BuiltinOperation => {
  const spelling = intrinsicSpelling(options.actor, options.name)
  return Object.freeze({
    _tag: 'IntrinsicOperation',
    id: operationId('Intrinsic', spelling),
    spelling,
    typeParameters: Object.freeze((options.typeParameters ?? []).map(typeParameter)),
    parameters: Object.freeze(Array.from(options.parameters)),
    callParameters: Object.freeze(Array.from(options.callParameters ?? options.semanticParameters)),
    result: options.result,
    unsafe: options.unsafe ?? false,
    admission: admission(options.actor),
    consumer: consumer(options.actor, options.name),
    targets: normalizeExecutionTargets(options.targets ?? executionTargets),
    ...(options.unsafe &&
    (options.actor === 'RawBuffer' || options.actor === 'Slot' || options.actor === 'Shared')
      ? {
          invariant:
            options.actor === 'RawBuffer'
              ? 'caller proves raw-buffer bounds, ownership, and initializedness required by the operation'
              : options.actor === 'Slot'
                ? 'caller proves the selected slot is in bounds and has the initializedness state required by the operation'
                : 'caller proves the allocation came from the exact shared layout specialization and transfers it and the value exactly once',
        }
      : {}),
    ...(options.actor === 'Host' && options.name === 'write'
      ? { hostImport: 'silk_standard_stream_write_v1' }
      : {}),
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
}

export const isBuiltinOperation = (operation: Operation): operation is BuiltinOperation =>
  operation.rule._tag === 'BuiltinRule' && 'callParameters' in operation

const effect = (options: {
  readonly name: string
  readonly operation: Extract<Rule, { readonly _tag: 'EffectRule' }>['operation']
  readonly typeParameters: ReadonlyArray<string>
  readonly parameters: ReadonlyArray<ValueParameter>
  readonly result: string
}): Operation => {
  const spelling = intrinsicSpelling('Effect', options.name)
  return Object.freeze({
    _tag: 'IntrinsicOperation',
    id: operationId('Intrinsic', spelling),
    spelling,
    typeParameters: Object.freeze(options.typeParameters.map(typeParameter)),
    parameters: Object.freeze(Array.from(options.parameters)),
    result: options.result,
    unsafe: false,
    admission: admission('Effect'),
    consumer: consumer('Effect', options.name),
    targets: executionTargets,
    rule: Object.freeze({
      _tag: 'EffectRule',
      operation: options.operation,
    }),
  })
}

const contractEffect = (options: {
  readonly name: string
  readonly typeParameters: ReadonlyArray<string>
  readonly parameters: ReadonlyArray<ValueParameter>
  readonly result: string
  readonly contract: CallableContract.CallableContract
  readonly post: Extract<Rule, { readonly _tag: 'ContractRule' }>['post']
  readonly providerMode?: Constraint.ProviderMode
  readonly targets?: ReadonlyArray<ExecutionTarget>
}): Operation => {
  const spelling = intrinsicSpelling('Effect', options.name)
  return Object.freeze({
    _tag: 'IntrinsicOperation',
    id: operationId('Intrinsic', spelling),
    spelling,
    typeParameters: Object.freeze(options.typeParameters.map(typeParameter)),
    parameters: Object.freeze(Array.from(options.parameters)),
    result: options.result,
    unsafe: false,
    admission: admission('Effect'),
    consumer: consumer('Effect', options.name),
    targets: normalizeExecutionTargets(options.targets ?? executionTargets),
    rule: Object.freeze({
      _tag: 'ContractRule',
      contract: options.contract,
      post: options.post,
      ...(options.providerMode === undefined ? {} : { providerMode: options.providerMode }),
    }),
  })
}

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
const sharedElement = Type.parameter({ module: 'Intrinsic', name: '$LocalShared' }, 0, 'T')
const sharedResult = Type.parameter({ module: 'Intrinsic', name: '$LocalShared' }, 1, 'A')
const sharedTypeParameters = Object.freeze([sharedElement])
const sharedLifecycleTypeParameters = Object.freeze([sharedElement, sharedResult])
const suspensionOwner = Object.freeze({ module: 'silk/core', name: '$EffectSuspend' })
const suspensionSuccess = Type.parameter(suspensionOwner, 0, 'A')
const suspensionFailure = Type.parameter(suspensionOwner, 1, 'E')
const suspensionRequirement = Type.parameter(suspensionOwner, 2, 'R', 'RequirementRow')
const suspensionTypeParameters = Object.freeze([
  suspensionSuccess,
  suspensionFailure,
  suspensionRequirement,
])
const bindingOwner = Object.freeze({ module: 'silk/core', name: '$BindRequirement' })
const bindingSelected = Type.parameter(bindingOwner, 0, 'S', 'RequirementRow')
const bindingSuccess = Type.parameter(bindingOwner, 1, 'A')
const bindingProvider = Type.parameter(bindingOwner, 2, 'P')
const bindingFailure = Type.parameter(bindingOwner, 3, 'E')
const bindingRequirements = Type.parameter(bindingOwner, 4, 'R', 'RequirementRow')
const bindingTypeParameters = Object.freeze([
  bindingSelected,
  bindingSuccess,
  bindingProvider,
  bindingFailure,
  bindingRequirements,
])
const intrinsicContractOrigin = (() => {
  const span = SourceSpan.fromOffsets('$intrinsic-contract', 0, 0)
  if (span === undefined) throw new RangeError('intrinsic contract span is invalid')
  return span
})()
const bindingFailureRow = RowAlgebra.singleton(
  Type.failureRowPolicy(),
  Type.failureMemberShape(bindingFailure),
  intrinsicContractOrigin,
)
const bindingRequirementRow = RowAlgebra.parameter<
  Type.Requirement,
  Type.Parameter,
  Type.RequirementMemberShape
>(bindingRequirements)
const bindingSelectedRow = RowAlgebra.parameter<
  Type.Requirement,
  Type.Parameter,
  Type.RequirementMemberShape
>(bindingSelected)
const bindingContract = (mode: Constraint.ProviderMode): CallableContract.CallableContract => {
  const provider = mode === 'Take' ? bindingProvider : Type.reference(mode, bindingProvider)
  return CallableContract.make({
    functionKind: 'Effect',
    binders: bindingTypeParameters,
    parameters: Object.freeze([
      Object.freeze({
        type: Type.effectWithRows(bindingSuccess, bindingFailureRow, 'Take', bindingRequirementRow),
        mode: 'Take' as const,
      }),
      Object.freeze({ type: provider, mode }),
    ]),
    result: Type.effectWithRows(
      bindingSuccess,
      bindingFailureRow,
      'Shared',
      RowAlgebra.without(Type.requirementRowPolicy(), bindingRequirementRow, bindingSelectedRow),
    ),
    constraints: Object.freeze([
      Constraint.providerSelection(
        mode,
        bindingProvider,
        bindingSelectedRow,
        bindingRequirementRow,
      ),
    ]),
  })
}
const catchOwner = Object.freeze({ module: 'silk/core', name: '$CatchFailure' })
const catchSelected = Type.parameter(catchOwner, 0, 'S')
const catchSuccess = Type.parameter(catchOwner, 1, 'A')
const catchHandlerSuccess = Type.parameter(catchOwner, 2, 'B')
const catchProtectedFailure = Type.parameter(catchOwner, 3, 'E')
const catchHandlerFailure = Type.parameter(catchOwner, 4, 'F')
const catchProtectedRequirements = Type.parameter(catchOwner, 5, 'R', 'RequirementRow')
const catchHandlerRequirements = Type.parameter(catchOwner, 6, 'Q', 'RequirementRow')
const catchTypeParameters = Object.freeze([
  catchSelected,
  catchSuccess,
  catchHandlerSuccess,
  catchProtectedFailure,
  catchHandlerFailure,
  catchProtectedRequirements,
  catchHandlerRequirements,
])
const catchProtectedFailureRow = RowAlgebra.singleton(
  Type.failureRowPolicy(),
  Type.failureMemberShape(catchProtectedFailure),
  intrinsicContractOrigin,
)
const catchHandlerFailureRow = RowAlgebra.singleton(
  Type.failureRowPolicy(),
  Type.failureMemberShape(catchHandlerFailure),
  intrinsicContractOrigin,
)
const catchProtectedRequirementRow = RowAlgebra.parameter<
  Type.Requirement,
  Type.Parameter,
  Type.RequirementMemberShape
>(catchProtectedRequirements)
const catchHandlerRequirementRow = RowAlgebra.parameter<
  Type.Requirement,
  Type.Parameter,
  Type.RequirementMemberShape
>(catchHandlerRequirements)
const suspensionFailureRow = RowAlgebra.singleton(
  Type.failureRowPolicy(),
  Type.failureMemberShape(suspensionFailure),
  intrinsicContractOrigin,
)
const suspensionRequirementRow = RowAlgebra.parameter<
  Type.Requirement,
  Type.Parameter,
  Type.RequirementMemberShape
>(suspensionRequirement)
const catchSelectedRow = RowAlgebra.singleton(
  Type.failureRowPolicy(),
  Type.failureMemberShape(catchSelected),
  intrinsicContractOrigin,
)
const catchJoinedSuccess = Type.union([catchSuccess, catchHandlerSuccess])
if (catchJoinedSuccess._tag !== 'Normalized')
  throw new RangeError('catch success parameters must form an ordinary union')
const catchContract = CallableContract.make({
  functionKind: 'Effect',
  binders: catchTypeParameters,
  parameters: Object.freeze([
    Object.freeze({
      type: Type.effectWithRows(
        catchSuccess,
        catchProtectedFailureRow,
        'Take',
        catchProtectedRequirementRow,
      ),
      mode: 'Take' as const,
    }),
    Object.freeze({
      type: Type.callable(
        Object.freeze([catchSelected]),
        Type.effectWithRows(
          catchHandlerSuccess,
          catchHandlerFailureRow,
          'Shared',
          catchHandlerRequirementRow,
        ),
        'Take',
      ),
      mode: 'Take' as const,
    }),
  ]),
  result: Type.effectWithRows(
    catchJoinedSuccess.type,
    RowAlgebra.union(
      Type.failureRowPolicy(),
      RowAlgebra.without(Type.failureRowPolicy(), catchProtectedFailureRow, catchSelectedRow),
      catchHandlerFailureRow,
    ),
    'Shared',
    RowAlgebra.union(
      Type.requirementRowPolicy(),
      catchProtectedRequirementRow,
      catchHandlerRequirementRow,
    ),
  ),
  constraints: Object.freeze([
    Constraint.failureSubset(catchSelectedRow, catchProtectedFailureRow),
  ]),
})
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
  const borrowed = operation.code === 'LessThan'
  const contractParameters = borrowed
    ? Object.freeze(semanticParameters.map((type) => Type.reference('Shared', type)))
    : semanticParameters
  return builtin({
    actor: scalar.spelling,
    name: operation.spelling,
    operation: operation.code,
    parameters: Object.freeze(
      parameterNames.map((name, ordinal) => {
        const type = semanticParameters.at(ordinal) ?? scalar.spelling
        return valueParameter(name, borrowed ? `&${type}` : type)
      }),
    ),
    semanticParameters,
    callParameters: contractParameters,
    result,
    semanticResult,
  })
}

const scalarOperations = (scalar: Scalar.Scalar): ReadonlyArray<Operation> =>
  scalar.operations.map((operation) => scalarOperation(scalar, operation))

const stringOperations = Object.freeze([
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
])

const stringActor = actor('string', 'Type', Object.freeze([]))

const replaceOperation: Operation = Object.freeze({
  _tag: 'IntrinsicOperation',
  id: operationId('Intrinsic', 'replace'),
  spelling: 'replace',
  typeParameters: Object.freeze([typeParameter('T')]),
  parameters: Object.freeze([valueParameter('place', '&mut T'), valueParameter('value', 'T')]),
  result: 'T',
  unsafe: false,
  admission: admission('Place'),
  consumer: consumer('Place', 'replace'),
  targets: executionTargets,
  rule: Object.freeze({ _tag: 'PlaceRule', operation: 'Replace' }),
})

const intrinsicOperations = Object.freeze([
  ...Scalar.all().flatMap(scalarOperations),
  ...stringOperations,
  ...Object.freeze([
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
    osBuiltin({
      name: 'directoryCreateUnique',
      operation: 'OsDirectoryCreateUnique',
      parameters: Object.freeze([
        valueParameter('root', '&[u8]'),
        valueParameter('parent', '&[u8]'),
        valueParameter('prefix', '&[u8]'),
        valueParameter('output', '&mut [u8]'),
        valueParameter('requiredCapacity', '&mut usize'),
        valueParameter('reason', '&mut i32'),
        valueParameter('nativeCode', '&mut u32'),
      ]),
      semanticParameters: Object.freeze([
        byteSlice,
        byteSlice,
        byteSlice,
        Type.slice('Exclusive', 'u8'),
        mutableUsize,
        mutableI32,
        mutableU32,
      ]),
      result: 'Effect<Option<usize>>',
      semanticResult: Type.option('usize'),
      invariant:
        'root and parent satisfy confined traversal; prefix is one valid final component fragment; the provider chooses the unique suffix, creates exactly one directory no other caller holds, and writes its complete final component name; buffer-too-small creates nothing and reports required capacity',
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
  ...Object.freeze([
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
  ...Object.freeze([
    builtin({
      actor: 'Shared',
      name: 'layout',
      operation: 'SharedLayout',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: sharedTypeParameters,
      parameters: Object.freeze([]),
      semanticParameters: Object.freeze([]),
      result: 'Layout',
      semanticResult: Type.layout,
    }),
    builtin({
      actor: 'Shared',
      name: 'fromAllocation',
      operation: 'SharedFromAllocation',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: sharedTypeParameters,
      parameters: Object.freeze([
        valueParameter('allocation', 'Allocation'),
        valueParameter('value', 'T'),
      ]),
      semanticParameters: Object.freeze([Type.allocation, sharedElement]),
      result: 'SharedCore<T>',
      semanticResult: Type.sharedCore(sharedElement),
      unsafe: true,
    }),
    builtin({
      actor: 'Shared',
      name: 'clone',
      operation: 'SharedClone',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: sharedTypeParameters,
      parameters: Object.freeze([valueParameter('self', '&SharedCore<T>')]),
      semanticParameters: Object.freeze([Type.reference('Shared', Type.sharedCore(sharedElement))]),
      result: 'SharedCore<T>',
      semanticResult: Type.sharedCore(sharedElement),
    }),
    builtin({
      actor: 'Shared',
      name: 'withMut',
      operation: 'SharedWithMut',
      typeParameters: Object.freeze(['T', 'A']),
      semanticTypeParameters: sharedLifecycleTypeParameters,
      parameters: Object.freeze([
        valueParameter('self', '&SharedCore<T>'),
        valueParameter('use', 'once fn(&mut T) -> A'),
        valueParameter('onConflict', 'once fn() -> A'),
      ]),
      semanticParameters: Object.freeze([
        Type.reference('Shared', Type.sharedCore(sharedElement)),
        Type.callable(
          Object.freeze([Type.reference('Exclusive', sharedElement)]),
          sharedResult,
          'Take',
        ),
        Type.callable(Object.freeze([]), sharedResult, 'Take'),
      ]),
      result: 'A',
      semanticResult: sharedResult,
    }),
  ]),
  ...Object.freeze([
    builtin({
      actor: 'Storage',
      name: 'acquire',
      operation: 'StorageAcquire',
      parameters: Object.freeze([valueParameter('layout', 'Layout')]),
      semanticParameters: Object.freeze([Type.layout]),
      result: 'Effect<Allocation ! OutOfMemoryError>',
      semanticResult: Type.effect(
        Type.allocation,
        Object.freeze([Type.outOfMemoryError]),
        undefined,
        Object.freeze([]),
      ),
    }),
  ]),
  ...Object.freeze([
    builtin({
      actor: 'Host',
      name: 'write',
      operation: 'HostWrite',
      parameters: Object.freeze([
        valueParameter('destination', 'bool'),
        valueParameter('bytes', '&[u8]'),
      ]),
      semanticParameters: Object.freeze(['bool', Type.slice('Shared', 'u8')]),
      result: 'Effect<() ! StreamWriteError>',
      semanticResult: Type.effect(
        Type.unit,
        Object.freeze([Type.streamWriteFailure]),
        undefined,
        Object.freeze([]),
      ),
    }),
  ]),
  ...Object.freeze([
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
    builtin({
      actor: 'RawBuffer',
      name: 'copy',
      operation: 'RawBufferCopy',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: rawTypeParameters,
      parameters: Object.freeze([
        valueParameter('destination', '&mut RawBuffer<T>'),
        valueParameter('destinationOffset', 'usize'),
        valueParameter('source', '&[T]'),
        valueParameter('length', 'usize'),
      ]),
      semanticParameters: Object.freeze([
        Type.reference('Exclusive', Type.rawBuffer(rawElement)),
        'usize',
        Type.slice('Shared', rawElement),
        'usize',
      ]),
      result: '()',
      semanticResult: Type.unit,
      unsafe: true,
    }),
    builtin({
      actor: 'RawBuffer',
      name: 'fill',
      operation: 'RawBufferFill',
      parameters: Object.freeze([
        valueParameter('buffer', '&mut RawBuffer<u8>'),
        valueParameter('offset', 'usize'),
        valueParameter('length', 'usize'),
        valueParameter('value', 'u8'),
      ]),
      semanticParameters: Object.freeze([
        Type.reference('Exclusive', Type.rawBuffer('u8')),
        'usize',
        'usize',
        'u8',
      ]),
      result: '()',
      semanticResult: Type.unit,
      unsafe: true,
    }),
  ]),
  ...Object.freeze([
    builtin({
      actor: 'Slot',
      name: 'write',
      operation: 'SlotWrite',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: rawTypeParameters,
      parameters: Object.freeze([valueParameter('slot', 'Slot<T>'), valueParameter('value', 'T')]),
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
  ...Object.freeze([
    builtin({
      actor: 'Effect',
      name: 'suspendEffect',
      operation: 'EffectSuspend',
      typeParameters: Object.freeze(['A', 'E', '?R']),
      semanticTypeParameters: suspensionTypeParameters,
      parameters: Object.freeze([valueParameter('deferred', 'once Effect<A ! E ? R>')]),
      semanticParameters: Object.freeze([
        Type.effectWithRows(
          suspensionSuccess,
          suspensionFailureRow,
          'Take',
          suspensionRequirementRow,
        ),
      ]),
      result: 'Effect<A ! E ? R>',
      semanticResult: Type.effectWithRows(
        suspensionSuccess,
        suspensionFailureRow,
        'Take',
        suspensionRequirementRow,
      ),
    }),
    effect({
      name: 'result',
      operation: 'Result',
      typeParameters: Object.freeze([]),
      parameters: Object.freeze([valueParameter('protected', 'Effect<A ! E ? R>')]),
      result: 'Effect<Result<A, E> ? R>',
    }),
    contractEffect({
      name: 'bindRequirement',
      post: 'BindRequirement',
      providerMode: 'Shared',
      typeParameters: Object.freeze(['?S', 'A', 'P', 'E', '?R']),
      parameters: Object.freeze([
        valueParameter('protected', 'once Effect<A ! E ? R>'),
        valueParameter('provider', '&P'),
      ]),
      result: 'Effect<A ! E ? Without<R, S>>',
      contract: bindingContract('Shared'),
    }),
    contractEffect({
      name: 'bindRequirementMut',
      post: 'BindRequirement',
      providerMode: 'Exclusive',
      typeParameters: Object.freeze(['?S', 'A', 'P', 'E', '?R']),
      parameters: Object.freeze([
        valueParameter('protected', 'once Effect<A ! E ? R>'),
        valueParameter('provider', '&mut P'),
      ]),
      result: 'Effect<A ! E ? Without<R, S>>',
      contract: bindingContract('Exclusive'),
    }),
    contractEffect({
      name: 'bindRequirementOwned',
      post: 'BindRequirement',
      providerMode: 'Take',
      typeParameters: Object.freeze(['?S', 'A', 'P', 'E', '?R']),
      parameters: Object.freeze([
        valueParameter('protected', 'once Effect<A ! E ? R>'),
        valueParameter('provider', 'P'),
      ]),
      result: 'Effect<A ! E ? Without<R, S>>',
      contract: bindingContract('Take'),
    }),
    contractEffect({
      name: 'catchFailure',
      post: 'CatchFailure',
      typeParameters: Object.freeze(['S', 'A', 'B', 'E', 'F', '?R', '?Q']),
      parameters: Object.freeze([
        valueParameter('protected', 'once Effect<A ! E ? R>'),
        valueParameter('handler', 'once fn(S) -> Effect<B ! F ? Q>'),
      ]),
      result: 'Effect<A | B ! Without<E, S> | F ? R | Q>',
      contract: catchContract,
    }),
  ]),
  replaceOperation,
])

const operations = Object.freeze([
  stringActor,
  actor('Intrinsic', 'Namespace', intrinsicOperations),
])

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
  intrinsicOperations.find(
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
    intrinsicOperations.map((operation) => {
      if (operation.admission === undefined || operation.consumer === undefined)
        throw new RangeError(`Intrinsic ${operation.spelling} is missing admission metadata`)
      if (operation.targets.length === 0)
        throw new RangeError(`Intrinsic ${operation.spelling} has no execution target`)
      const normalizedTargets = normalizeExecutionTargets(operation.targets)
      if (
        normalizedTargets.length !== operation.targets.length ||
        normalizedTargets.some((target, index) => operation.targets.at(index) !== target)
      )
        throw new RangeError(`Intrinsic ${operation.spelling} has non-normalized target metadata`)
      const identity =
        operation.rule._tag === 'BuiltinRule'
          ? operation.rule.operation
          : operation.rule._tag === 'EffectRule'
            ? `${operation.rule._tag}.${operation.rule.operation}`
            : operation.rule._tag === 'ContractRule'
              ? `${operation.rule._tag}.${operation.rule.post}`
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
        targets: operation.targets,
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
