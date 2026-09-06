import * as Lifetime from './Lifetime.js'
import * as CallableContract from './CallableContract.js'
import * as Constraint from './Constraint.js'
import type * as Hir from './Hir.js'
import * as RowAlgebra from './RowAlgebra.js'
import * as Scalar from './Scalar.js'
import * as SourceSpan from './SourceSpan.js'
import * as Target from './Target.js'
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
  /** A compiler-consumed lane is explicit and never enters an ordinary runtime calling shape. */
  readonly phase?: 'Static'
}

/** Structural reason one source-callable primitive remains compiler-owned. */
export type AdmissionCategory =
  | 'Representation'
  | 'Scalar'
  | 'Ownership'
  | 'Effect'
  | 'Platform'
  | 'Language'

/** Whether an intrinsic executes during static evaluation or in the residual runtime program. */
export type Phase = 'Runtime' | 'StaticOnly' | 'Mixed'

/** Canonical deterministic order for residual runtime targets. */
export const runtimeTargets: ReadonlyArray<Target.Id> = Object.freeze(
  Target.all.map((target) => target.id),
)

const nativeTargets: ReadonlyArray<Target.Id> = Object.freeze(
  Target.native.map((target) => target.id),
)

/** Normalizes an availability set to the compiler-owned target order. */
export const normalizeRuntimeTargets = (
  targets: ReadonlyArray<Target.Id>,
): ReadonlyArray<Target.Id> =>
  Object.freeze(runtimeTargets.filter((target) => targets.includes(target)))

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
      readonly _tag: 'ContractRule'
      readonly contract: CallableContract.CallableContract
      readonly post: 'BindRequirement' | 'CatchFailure'
      readonly providerMode?: Constraint.ProviderMode
    }
  | { readonly _tag: 'PlaceRule'; readonly operation: 'Replace' }
  | {
      readonly _tag: 'StaticOnlyRule'
      readonly contract: CallableContract.CallableContract
    }
  | {
      /** One shared owner lane plus one descriptor lane consumed while residualizing. */
      readonly _tag: 'MixedFieldProjectionRule'
      readonly contract: CallableContract.CallableContract
      readonly runtimeOwnerParameter: 0
      readonly staticDescriptorParameter: 1
    }
  | {
      /** Result and parameter types are derived from the owning canonical enum declaration. */
      readonly _tag: 'EnumValueRule'
    }

/** One compiler-provided operation shared by analysis, presentation, and completion. */
export interface Operation {
  readonly _tag: 'IntrinsicOperation'
  readonly id: OperationId
  readonly spelling: string
  readonly typeParameters: ReadonlyArray<TypeParameter>
  readonly parameters: ReadonlyArray<ValueParameter>
  readonly result: string
  readonly unsafe: boolean
  readonly phase: Phase
  readonly admission?: AdmissionCategory
  readonly consumer?: string
  readonly targets: ReadonlyArray<Target.Id>
  readonly invariant?: string
  readonly hostImport?: string

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

const staticValueParameter = (name: string, type: string): ValueParameter =>
  Object.freeze({ name, type, phase: 'Static' })

const upperInitial = (value: string): string =>
  `${value.slice(0, 1).toUpperCase()}${value.slice(1)}`

const intrinsicSpelling = (family: string, operation: string): string => {
  if (Scalar.isSpelling(family)) return `${family}${upperInitial(operation)}`
  if (family === 'Effect' && operation === 'suspendEffect') return 'suspendEffect'
  if (family === 'Effect' && operation.startsWith('bindRequirement')) return operation
  if (family === 'Effect' && operation === 'catchFailure') return operation
  if (family === 'Wake' && operation === 'signal') return 'wake'
  if (family === 'Parking' && operation === 'park') return 'park'
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
  if (
    family === 'RawBuffer' ||
    family === 'Pointer' ||
    family === 'Slot' ||
    family === 'Shared' ||
    family === 'Execution' ||
    family === 'Wake' ||
    family === 'Parking'
  )
    return 'Ownership'
  return 'Language'
}

/** The canonical standard-library consumer of one OS boundary operation. */
const osConsumer = (spelling: string): string => {
  if (spelling === 'monotonicClockNow') return 'silk/os_monotonic_clock.now'
  if (spelling === 'monotonicClockResolution') return 'silk/os_monotonic_clock.getResolution'
  if (spelling === 'monotonicClockWaitUntil') return 'silk/os_monotonic_clock.waitUntil'
  if (spelling === 'randomFill') return 'silk/os_random.fillBytes'
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
  if (family === 'Execution')
    return operation === 'layout' || operation === 'fromAllocation'
      ? 'silk/execution.make'
      : `silk/execution.${operation}`
  if (family === 'Wake' || family === 'Parking') return 'language:external-wake-parking'
  if (family === 'Storage') return 'silk/allocator.allocate'
  if (family === 'Host') return 'silk/writer.writeAll'
  if (family === 'Os') return osConsumer(operation)
  if (family === 'Place') return 'language:place-replacement'
  return `silk/${family.replaceAll(/([a-z])([A-Z])/g, '$1_$2').toLowerCase()}.${operation}`
}

/** The caller obligation of each unsafe pointer primitive, keyed by operation name. */
const pointerInvariants: ReadonlyMap<string, string> = new Map([
  ['at', 'caller proves the pointer and the offset result address elements of one live allocation'],
  [
    'atMut',
    'caller proves the pointer and the offset result address elements of one live allocation',
  ],
  [
    'requalify',
    'caller proves every strengthened pointer qualifier, including alignment, nullness, extent and write access',
  ],
  ['readUnaligned', 'caller proves live readable storage containing an initialized Copy T'],
  ['writeUnaligned', 'caller proves live writable storage for a Copy T'],
  ['read', 'caller proves the pointer is non-null, aligned, and addresses an initialized T'],
  ['write', 'caller proves the pointer is non-null, aligned, and addresses writable storage for T'],
])

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
  readonly targets?: ReadonlyArray<Target.Id>
}): BuiltinOperation => {
  const spelling = intrinsicSpelling(options.actor, options.name)
  let invariant: string | undefined
  if (options.unsafe) {
    switch (options.actor) {
      case 'RawBuffer':
        invariant =
          'caller proves raw-buffer bounds, ownership, and initializedness required by the operation'
        break
      case 'Pointer':
        invariant = pointerInvariants.get(options.name)
        break
      case 'Slot':
        invariant =
          'caller proves the selected slot is in bounds and has the initializedness state required by the operation'
        break
      case 'Shared':
        invariant =
          'caller proves the allocation came from the exact shared layout specialization and transfers it and the value exactly once'
        break
      case 'Execution':
        invariant =
          'caller proves the allocation came from the exact execution package layout specialization and transfers it, the body, and the fixed endpoint exactly once'
        break
    }
  }
  return Object.freeze({
    _tag: 'IntrinsicOperation',
    id: operationId('Intrinsic', spelling),
    spelling,
    typeParameters: Object.freeze((options.typeParameters ?? []).map(typeParameter)),
    parameters: Object.freeze(Array.from(options.parameters)),
    callParameters: Object.freeze(Array.from(options.callParameters ?? options.semanticParameters)),
    result: options.result,
    unsafe: options.unsafe ?? false,
    phase: 'Runtime',
    admission: admission(options.actor),
    consumer: consumer(options.actor, options.name),
    targets: normalizeRuntimeTargets(options.targets ?? runtimeTargets),
    ...(invariant === undefined ? {} : { invariant }),
    ...(options.actor === 'Host' && options.name === 'write'
      ? { hostImport: 'silk_standard_stream_write_v1' }
      : {}),

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

const contractEffect = (options: {
  readonly name: string
  readonly typeParameters: ReadonlyArray<string>
  readonly parameters: ReadonlyArray<ValueParameter>
  readonly result: string
  readonly contract: CallableContract.CallableContract
  readonly post: Extract<Rule, { readonly _tag: 'ContractRule' }>['post']
  readonly providerMode?: Constraint.ProviderMode
  readonly targets?: ReadonlyArray<Target.Id>
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
    phase: 'Runtime',
    admission: admission('Effect'),
    consumer: consumer('Effect', options.name),
    targets: normalizeRuntimeTargets(options.targets ?? runtimeTargets),
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

// Intrinsic headers quantify their borrowed inputs and retained environments. A call
// instantiates these declaration-owned regions before validating its selected contract.
const contractLifetime = (name: string): Lifetime.Bound =>
  Lifetime.bound({ module: 'Intrinsic', name }, 0, 'call')

const rawElement = Type.parameter({ module: 'silk/core', name: '$RawStorage' }, 0, 'T')
const rawTypeParameters = Object.freeze([rawElement])
const pointerElement = Type.parameter({ module: 'Intrinsic', name: '$Pointer' }, 0, 'T')
const pointerTypeParameters = Object.freeze([pointerElement])
const pointerSource = Type.parameter({ module: 'Intrinsic', name: '$PointerQualifiers' }, 0, 'From')
const pointerDestination = Type.parameter(
  { module: 'Intrinsic', name: '$PointerQualifiers' },
  1,
  'To',
)
const sharedElement = Type.parameter({ module: 'Intrinsic', name: '$LocalShared' }, 0, 'T')
const sharedResult = Type.parameter({ module: 'Intrinsic', name: '$LocalShared' }, 1, 'A')
const sharedTypeParameters = Object.freeze([sharedElement])
const sharedLifecycleTypeParameters = Object.freeze([sharedElement, sharedResult])
const sharedAccessLifetime = Lifetime.bound(
  { module: 'Intrinsic', name: 'SharedWithMut.use' },
  0,
  'access',
)
const executionPackageOwner = Object.freeze({ module: 'Intrinsic', name: '$ExecutionPackage' })
const executionResult = Type.parameter(executionPackageOwner, 0, 'A')
const executionBodyBound = Type.effect(
  executionResult,
  Object.freeze([]),
  { environment: Lifetime.staticLifetime, lifetimeBinders: [] },
  'Take',
)
const executionBody = Type.parameter(
  executionPackageOwner,
  1,
  'F',
  'EffectRepresentation',
  executionBodyBound,
  Object.freeze(['Intrinsic.Detached']),
)
const executionEndpoint = Type.parameter(
  executionPackageOwner,
  2,
  'O',
  'Value',
  undefined,
  Object.freeze(['Intrinsic.Detached']),
)
const executionReadyLifetime = contractLifetime('executionReadyBound')
const executionReadyBound = Type.callable(
  Object.freeze([Type.reference('Shared', executionEndpoint, executionReadyLifetime)]),
  Type.unit,
  { environment: Lifetime.staticLifetime, lifetimeBinders: [executionReadyLifetime] },
  'Shared',
)
const executionReady = Type.parameter(
  executionPackageOwner,
  3,
  'R',
  'CallableRepresentation',
  executionReadyBound,
  Object.freeze(['Intrinsic.Detached', 'Intrinsic.NonParking']),
)
const executionPackageTypeParameters = Object.freeze([
  executionResult,
  executionBody,
  executionEndpoint,
  executionReady,
])
const representedExecutionBody = Type.represented(
  executionBodyBound,
  executionBodyBound,
  Type.representationParameterArgument(executionBody),
)
const representedExecutionReady = Type.represented(
  executionReadyBound,
  executionReadyBound,
  Type.representationParameterArgument(executionReady),
)
const executionDriveOwner = Object.freeze({ module: 'Intrinsic', name: '$ExecutionDrive' })
const drivenResult = Type.parameter(executionDriveOwner, 0, 'A')
const driveBranch = Type.parameter(executionDriveOwner, 1, 'D')
const completionBound = Type.callable(
  Object.freeze([driveBranch, drivenResult]),
  Type.unit,
  { environment: contractLifetime('completionBound'), lifetimeBinders: [] },
  'Take',
)
const completionCallback = Type.parameter(
  executionDriveOwner,
  2,
  'C',
  'CallableRepresentation',
  completionBound,
  Object.freeze(['Intrinsic.NonParking']),
)
const suspensionBound = Type.callable(
  Object.freeze([driveBranch, Type.execution(drivenResult)]),
  Type.unit,
  { environment: contractLifetime('suspensionBound'), lifetimeBinders: [] },
  'Take',
)
const suspensionCallback = Type.parameter(
  executionDriveOwner,
  3,
  'S',
  'CallableRepresentation',
  suspensionBound,
  Object.freeze(['Intrinsic.NonParking']),
)
const executionDriveTypeParameters = Object.freeze([
  drivenResult,
  driveBranch,
  completionCallback,
  suspensionCallback,
])
const executionNotifyOwner = Object.freeze({ module: 'Intrinsic', name: '$ExecutionNotifyInitial' })
const notifiedResult = Type.parameter(executionNotifyOwner, 0, 'A')
const representedCompletion = Type.represented(
  completionBound,
  completionBound,
  Type.representationParameterArgument(completionCallback),
)
const representedSuspension = Type.represented(
  suspensionBound,
  suspensionBound,
  Type.representationParameterArgument(suspensionCallback),
)
const parkingOwner = Object.freeze({ module: 'Intrinsic', name: '$ExecutionPark' })
const registrationGuard = Type.parameter(parkingOwner, 0, 'G')
const registrationBound = Type.callable(
  Object.freeze([Type.wake]),
  registrationGuard,
  { environment: contractLifetime('registrationBound'), lifetimeBinders: [] },
  'Take',
)
const registrationCallback = Type.parameter(
  parkingOwner,
  1,
  'F',
  'CallableRepresentation',
  registrationBound,
  Object.freeze(['Intrinsic.NonParking']),
)
const parkingTypeParameters = Object.freeze([registrationGuard, registrationCallback])
const representedRegistration = Type.represented(
  registrationBound,
  registrationBound,
  Type.representationParameterArgument(registrationCallback),
)
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
  const provider =
    mode === 'Take'
      ? bindingProvider
      : Type.reference(mode, bindingProvider, contractLifetime('provider'))
  return CallableContract.make({
    environment: contractLifetime('bindingContract'),
    lifetimeBinders: [],
    lifetimeBounds:
      mode === 'Take'
        ? []
        : [{ longer: contractLifetime('provider'), shorter: contractLifetime('bindingContract') }],
    functionKind: 'Effect',
    binders: bindingTypeParameters,
    parameters: Object.freeze([
      Object.freeze({
        type: Type.effectWithRows(
          bindingSuccess,
          bindingFailureRow,
          { environment: contractLifetime('bindingContract'), lifetimeBinders: [] },
          'Take',
          bindingRequirementRow,
        ),
        mode: 'Take' as const,
      }),
      Object.freeze({ type: provider, mode }),
    ]),
    result: Type.effectWithRows(
      bindingSuccess,
      bindingFailureRow,
      { environment: contractLifetime('bindingContract'), lifetimeBinders: [] },
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
  environment: contractLifetime('catchContract'),
  lifetimeBinders: [],
  functionKind: 'Effect',
  binders: catchTypeParameters,
  parameters: Object.freeze([
    Object.freeze({
      type: Type.effectWithRows(
        catchSuccess,
        catchProtectedFailureRow,
        { environment: contractLifetime('catchContract'), lifetimeBinders: [] },
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
          { environment: contractLifetime('catchContract'), lifetimeBinders: [] },
          'Shared',
          catchHandlerRequirementRow,
        ),
        { environment: contractLifetime('catchContract'), lifetimeBinders: [] },
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
    { environment: contractLifetime('catchContract'), lifetimeBinders: [] },
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
const byteSlice = Type.slice('Shared', 'u8', contractLifetime('byteSlice'))
const mutableI32 = Type.reference('Exclusive', 'i32', contractLifetime('mutableI32'))
const mutableI64 = Type.reference('Exclusive', 'i64', contractLifetime('mutableI64'))
const mutableU32 = Type.reference('Exclusive', 'u32', contractLifetime('mutableU32'))
const mutableU64 = Type.reference('Exclusive', 'u64', contractLifetime('mutableU64'))
const mutableUsize = Type.reference('Exclusive', 'usize', contractLifetime('mutableUsize'))
const mutableHandle = Type.reference('Exclusive', Type.osHandle, contractLifetime('mutableHandle'))

const osEffect = (value: Type.Type): Type.Effect =>
  Type.effect(
    value,
    Object.freeze([]),
    { environment: contractLifetime('osEffect'), lifetimeBinders: [] },
    undefined,
    Object.freeze([]),
  )

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

const osOpen = (options: {
  readonly name: 'fileOpen' | 'directoryOpen'
  readonly operation: 'OsFileOpen' | 'OsDirectoryOpen'
  readonly parameters: ReadonlyArray<ValueParameter>
  readonly semanticParameters: ReadonlyArray<Type.Type>
  readonly invariant: string
}): Operation => {
  const carrierOwner = Object.freeze({
    module: 'Intrinsic',
    name: `$Os.${options.name}`,
  })
  const carrierResult = Type.parameter(carrierOwner, 0, 'R')
  const success = Type.callable(
    Object.freeze([Type.osHandle]),
    carrierResult,
    { environment: contractLifetime('success'), lifetimeBinders: [] },
    'Take',
  )
  const failure = Type.callable(
    Object.freeze([]),
    carrierResult,
    { environment: contractLifetime('failure'), lifetimeBinders: [] },
    'Take',
  )
  return Object.freeze({
    ...builtin({
      actor: 'Os',
      name: options.name,
      operation: options.operation,
      typeParameters: Object.freeze(['R']),
      semanticTypeParameters: Object.freeze([carrierResult]),
      parameters: Object.freeze([
        ...options.parameters,
        valueParameter('success', 'once fn(OsHandle) -> R'),
        valueParameter('failure', 'once fn() -> R'),
      ]),
      semanticParameters: Object.freeze([...options.semanticParameters, success, failure]),
      result: 'Effect<R>',
      semanticResult: osEffect(carrierResult),
      unsafe: true,
      targets: nativeTargets,
    }),
    invariant: options.invariant,
  })
}

const scalarOperation = (scalar: Scalar.Scalar, operation: Scalar.Operation): Operation => {
  let concreteResult: Type.Type
  switch (operation.result) {
    case 'Self':
    case 'OptionSelf':
      concreteResult = scalar.spelling
      break
    case 'Boolean':
      concreteResult = Scalar.boolean.spelling
      break
    case 'OptionTarget':
      concreteResult = Scalar.conversionTarget(operation.code)?.spelling ?? scalar.spelling
      break
    default:
      concreteResult = operation.result
      break
  }
  const checked = operation.result === 'OptionSelf' || operation.result === 'OptionTarget'
  const carrierOwner = Object.freeze({
    module: 'Intrinsic',
    name: `$${scalar.spelling}.${operation.spelling}`,
  })
  const carrierResult = Type.parameter(carrierOwner, 0, 'R')
  const result = checked ? 'R' : concreteResult
  const semanticResult = checked ? carrierResult : concreteResult
  const parameterNames =
    operation.arity === 1 ? Object.freeze(['value']) : Object.freeze(['left', 'right'])
  const semanticParameters =
    operation.parameters ?? Object.freeze(parameterNames.map(() => scalar.spelling))
  const borrowed = operation.code === 'LessThan'
  const contractParameters = borrowed
    ? Object.freeze(
        semanticParameters.map((type) =>
          Type.reference('Shared', type, contractLifetime('contractParameters')),
        ),
      )
    : semanticParameters
  const carrierParameters = checked
    ? Object.freeze([
        valueParameter('present', `once fn(${concreteResult}) -> R`),
        valueParameter('absent', 'once fn() -> R'),
      ])
    : Object.freeze([])
  const semanticCarrierParameters = checked
    ? Object.freeze([
        Type.callable(
          Object.freeze([concreteResult]),
          carrierResult,
          { environment: contractLifetime('semanticCarrierParameters'), lifetimeBinders: [] },
          'Take',
        ),
        Type.callable(
          Object.freeze([]),
          carrierResult,
          { environment: contractLifetime('semanticCarrierParameters'), lifetimeBinders: [] },
          'Take',
        ),
      ])
    : Object.freeze([])
  return builtin({
    actor: scalar.spelling,
    name: operation.spelling,
    operation: operation.code,
    ...(checked
      ? {
          typeParameters: Object.freeze(['R']),
          semanticTypeParameters: Object.freeze([carrierResult]),
        }
      : {}),
    parameters: Object.freeze([
      ...parameterNames.map((name, ordinal) => {
        const type = semanticParameters.at(ordinal) ?? scalar.spelling
        return valueParameter(name, borrowed ? `&${type}` : type)
      }),
      ...carrierParameters,
    ]),
    semanticParameters: Object.freeze([...semanticParameters, ...semanticCarrierParameters]),
    callParameters: Object.freeze([...contractParameters, ...semanticCarrierParameters]),
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
      semanticResult: Type.string(byteSlice.lifetime),
      unsafe: true,
    }),
    invariant:
      'bytes remain live and immutable for the returned view lifetime and contain complete valid UTF-8',
  }),
  builtin({
    actor: 'string',
    name: 'utf8Bytes',
    operation: 'StringUtf8Bytes',
    parameters: Object.freeze([valueParameter('value', 'string')]),
    semanticParameters: Object.freeze([Type.string(byteSlice.lifetime)]),
    result: '&[u8]',
    semanticResult: byteSlice,
  }),
  builtin({
    actor: 'string',
    name: 'byteLength',
    operation: 'StringByteLength',
    parameters: Object.freeze([valueParameter('value', 'string')]),
    semanticParameters: Object.freeze([Type.string(contractLifetime('byteLength'))]),
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
    semanticParameters: Object.freeze([
      Type.string(contractLifetime('equalsExact')),
      Type.string(contractLifetime('equalsExact')),
    ]),
    result: 'bool',
    semanticResult: 'bool',
  }),
])

const stringActor = actor('string', 'Type', Object.freeze([]))

const profileOperations: ReadonlyArray<Operation> = Object.freeze(
  [
    ...[
      'targetArchitecture',
      'targetOperatingSystem',
      'targetAbi',
      'targetObjectFormat',
      'targetEndianness',
    ].map((name) => ({
      name,
      result: "string<'static>",
      type: Type.string(Lifetime.staticLifetime),
      arguments: [],
    })),
    ...['targetPointerBits', 'targetPointerAlignment'].map((name) => ({
      name,
      result: 'u32',
      type: 'u32' as const,
      arguments: [],
    })),
    {
      name: 'profileText',
      result: "string<'static>",
      type: Type.string(Lifetime.staticLifetime),
      arguments: ['key'],
    },
    { name: 'profileFlag', result: 'bool', type: 'bool' as const, arguments: ['key'] },
    { name: 'profileContains', result: 'bool', type: 'bool' as const, arguments: ['key', 'value'] },
  ].map((operation): Operation =>
    Object.freeze({
      _tag: 'IntrinsicOperation',
      id: operationId('Intrinsic', operation.name),
      spelling: operation.name,
      typeParameters: Object.freeze([]),
      parameters: Object.freeze(
        operation.arguments.map((name) => valueParameter(name, "string<'static>")),
      ),
      result: operation.result,
      unsafe: false,
      phase: 'StaticOnly',
      admission: 'Language',
      consumer: `silk/target.${operation.name}`,
      targets: Object.freeze([]),
      rule: Object.freeze({
        _tag: 'StaticOnlyRule',
        contract: CallableContract.make({
          environment: Lifetime.staticLifetime,
          lifetimeBinders: [],
          functionKind: 'Function',
          parameters: operation.arguments.map(() => ({
            type: Type.string(Lifetime.staticLifetime),
            mode: 'Value',
          })),
          result: operation.type,
        }),
      }),
    }),
  ),
)

const staticTextOperation = (
  name: string,
  parameters: ReadonlyArray<ValueParameter>,
  semanticParameters: ReadonlyArray<Type.Type>,
  result: string,
  semanticResult: Type.Type,
): Operation =>
  Object.freeze({
    _tag: 'IntrinsicOperation',
    id: operationId('Intrinsic', name),
    spelling: name,
    typeParameters: Object.freeze([]),
    parameters,
    result,
    unsafe: false,
    phase: 'StaticOnly',
    admission: 'Language',
    consumer: `silk/static_text.${name.replace('staticText', '').replace(/^./, (value) => value.toLowerCase())}`,
    targets: Object.freeze([]),
    rule: Object.freeze({
      _tag: 'StaticOnlyRule',
      contract: CallableContract.make({
        environment: Lifetime.staticLifetime,
        lifetimeBinders: [],
        functionKind: 'Function',
        parameters: semanticParameters.map((type) =>
          Object.freeze({ type, mode: 'Value' as const }),
        ),
        result: semanticResult,
      }),
    }),
  })

const staticTextOperations = Object.freeze([
  staticTextOperation(
    'staticTextByteLength',
    Object.freeze([valueParameter('value', 'string')]),
    Object.freeze([Type.string(contractLifetime('staticTextOperations'))]),
    'usize',
    'usize',
  ),
  staticTextOperation(
    'staticTextByteAt',
    Object.freeze([valueParameter('value', 'string'), valueParameter('index', 'usize')]),
    Object.freeze([Type.string(contractLifetime('staticTextOperations')), 'usize']),
    'u8',
    'u8',
  ),
  staticTextOperation(
    'staticTextConcat',
    Object.freeze([valueParameter('left', 'string'), valueParameter('right', 'string')]),
    Object.freeze([
      Type.string(contractLifetime('staticTextOperations')),
      Type.string(contractLifetime('staticTextOperations')),
    ]),
    'string',
    Type.string(Lifetime.staticLifetime),
  ),
  staticTextOperation(
    'staticTextSlice',
    Object.freeze([
      valueParameter('value', 'string'),
      valueParameter('start', 'usize'),
      valueParameter('end', 'usize'),
    ]),
    Object.freeze([Type.string(contractLifetime('staticTextOperations')), 'usize', 'usize']),
    'string',
    Type.string(Lifetime.staticLifetime),
  ),
])

const staticGenericOperation = (input: {
  readonly name: string
  readonly typeParameters: ReadonlyArray<Type.Parameter>
  readonly parameters: ReadonlyArray<ValueParameter>
  readonly semanticParameters: ReadonlyArray<Type.Type>
  readonly result: string
  readonly semanticResult: Type.Type
  readonly consumer: string
}): Operation =>
  Object.freeze({
    _tag: 'IntrinsicOperation',
    id: operationId('Intrinsic', input.name),
    spelling: input.name,
    typeParameters: Object.freeze(
      input.typeParameters.map((parameter) => typeParameter(parameter.name)),
    ),
    parameters: input.parameters,
    result: input.result,
    unsafe: false,
    phase: 'StaticOnly',
    admission: 'Language',
    consumer: input.consumer,
    targets: Object.freeze([]),
    rule: Object.freeze({
      _tag: 'StaticOnlyRule',
      contract: CallableContract.make({
        environment: Lifetime.staticLifetime,
        lifetimeBinders: [],
        functionKind: 'Function',
        binders: input.typeParameters,
        parameters: input.semanticParameters.map((type) =>
          Object.freeze({ type, mode: 'Value' as const }),
        ),
        result: input.semanticResult,
      }),
    }),
  })

const reflectionOwner = Type.parameter({ module: 'Intrinsic', name: 'reflect' }, 0, 'Owner')
const reflectionValue = Type.parameter({ module: 'Intrinsic', name: 'reflect' }, 1, 'Value')
const projectionOwner = Type.parameter({ module: 'Intrinsic', name: 'borrowField' }, 0, 'Owner')
const projectionValue = Type.parameter({ module: 'Intrinsic', name: 'borrowField' }, 1, 'Value')
const sequenceElement = Type.parameter(
  { module: 'Intrinsic', name: 'staticSequence' },
  0,
  'Element',
)

const reflectionOperations = Object.freeze([
  staticGenericOperation({
    name: 'reflectType',
    typeParameters: Object.freeze([reflectionOwner]),
    parameters: Object.freeze([]),
    semanticParameters: Object.freeze([]),
    result: 'Type<Owner>',
    semanticResult: Type.typeDescriptor(reflectionOwner),
    consumer: 'silk/reflect.type',
  }),
  staticGenericOperation({
    name: 'reflectFields',
    typeParameters: Object.freeze([reflectionOwner]),
    parameters: Object.freeze([]),
    semanticParameters: Object.freeze([]),
    result: 'Fields<Owner>',
    semanticResult: Type.fieldsDescriptor(reflectionOwner),
    consumer: 'silk/reflect.fields',
  }),
  staticGenericOperation({
    name: 'reflectTypeKind',
    typeParameters: Object.freeze([reflectionOwner]),
    parameters: Object.freeze([valueParameter('descriptor', 'Type<Owner>')]),
    semanticParameters: Object.freeze([Type.typeDescriptor(reflectionOwner)]),
    result: 'u8',
    semanticResult: 'u8',
    consumer: 'silk/reflect.typeKind',
  }),
  staticGenericOperation({
    name: 'reflectFieldKind',
    typeParameters: Object.freeze([reflectionOwner, reflectionValue]),
    parameters: Object.freeze([valueParameter('field', 'Field<Owner, Value>')]),
    semanticParameters: Object.freeze([Type.fieldDescriptor(reflectionOwner, reflectionValue)]),
    result: 'u8',
    semanticResult: 'u8',
    consumer: 'silk/reflect.fieldKind',
  }),
  staticGenericOperation({
    name: 'reflectFieldLabel',
    typeParameters: Object.freeze([reflectionOwner, reflectionValue]),
    parameters: Object.freeze([valueParameter('field', 'Field<Owner, Value>')]),
    semanticParameters: Object.freeze([Type.fieldDescriptor(reflectionOwner, reflectionValue)]),
    result: 'string',
    semanticResult: Type.string(contractLifetime('reflectFieldLabel')),
    consumer: 'silk/reflect.fieldLabel',
  }),
  staticGenericOperation({
    name: 'reflectFieldOrdinal',
    typeParameters: Object.freeze([reflectionOwner, reflectionValue]),
    parameters: Object.freeze([valueParameter('field', 'Field<Owner, Value>')]),
    semanticParameters: Object.freeze([Type.fieldDescriptor(reflectionOwner, reflectionValue)]),
    result: 'usize',
    semanticResult: 'usize',
    consumer: 'silk/reflect.fieldOrdinal',
  }),
])

const borrowFieldOperation: Operation = Object.freeze({
  _tag: 'IntrinsicOperation',
  id: operationId('Intrinsic', 'borrowField'),
  spelling: 'borrowField',
  typeParameters: Object.freeze([typeParameter('Owner'), typeParameter('Value')]),
  parameters: Object.freeze([
    valueParameter('owner', '&Owner'),
    staticValueParameter('field', 'Field<Owner, Value>'),
  ]),
  result: '&Value',
  unsafe: false,
  phase: 'Mixed',
  admission: 'Language',
  consumer: 'silk/reflect.borrowField',
  targets: Object.freeze([]),

  rule: Object.freeze({
    _tag: 'MixedFieldProjectionRule',
    contract: CallableContract.make({
      environment: Lifetime.staticLifetime,
      lifetimeBinders: [],
      functionKind: 'Function',
      binders: Object.freeze([projectionOwner, projectionValue]),
      parameters: Object.freeze([
        Object.freeze({
          type: Type.reference('Shared', projectionOwner, contractLifetime('borrowFieldOperation')),
          mode: 'Value',
        }),
        Object.freeze({
          type: Type.fieldDescriptor(projectionOwner, projectionValue),
          mode: 'Value',
        }),
      ]),
      result: Type.reference('Shared', projectionValue, contractLifetime('borrowFieldOperation')),
    }),
    runtimeOwnerParameter: 0,
    staticDescriptorParameter: 1,
  }),
})

const staticSequenceOperations = Object.freeze([
  staticGenericOperation({
    name: 'staticSequenceEmpty',
    typeParameters: Object.freeze([sequenceElement]),
    parameters: Object.freeze([]),
    semanticParameters: Object.freeze([]),
    result: 'StaticSequence<Element>',
    semanticResult: Type.staticSequence(sequenceElement),
    consumer: 'silk/static_sequence.empty',
  }),
  staticGenericOperation({
    name: 'staticSequenceAppend',
    typeParameters: Object.freeze([sequenceElement]),
    parameters: Object.freeze([
      valueParameter('self', 'StaticSequence<Element>'),
      valueParameter('value', 'Element'),
    ]),
    semanticParameters: Object.freeze([Type.staticSequence(sequenceElement), sequenceElement]),
    result: 'StaticSequence<Element>',
    semanticResult: Type.staticSequence(sequenceElement),
    consumer: 'silk/static_sequence.append',
  }),
  staticGenericOperation({
    name: 'staticSequenceConcat',
    typeParameters: Object.freeze([sequenceElement]),
    parameters: Object.freeze([
      valueParameter('left', 'StaticSequence<Element>'),
      valueParameter('right', 'StaticSequence<Element>'),
    ]),
    semanticParameters: Object.freeze([
      Type.staticSequence(sequenceElement),
      Type.staticSequence(sequenceElement),
    ]),
    result: 'StaticSequence<Element>',
    semanticResult: Type.staticSequence(sequenceElement),
    consumer: 'silk/static_sequence.concat',
  }),
  staticGenericOperation({
    name: 'staticSequenceLength',
    typeParameters: Object.freeze([sequenceElement]),
    parameters: Object.freeze([valueParameter('self', 'StaticSequence<Element>')]),
    semanticParameters: Object.freeze([Type.staticSequence(sequenceElement)]),
    result: 'usize',
    semanticResult: 'usize',
    consumer: 'silk/static_sequence.length',
  }),
  staticGenericOperation({
    name: 'staticSequenceAt',
    typeParameters: Object.freeze([sequenceElement]),
    parameters: Object.freeze([
      valueParameter('self', 'StaticSequence<Element>'),
      valueParameter('index', 'usize'),
    ]),
    semanticParameters: Object.freeze([Type.staticSequence(sequenceElement), 'usize']),
    result: 'Element',
    semanticResult: sequenceElement,
    consumer: 'silk/static_sequence.at',
  }),
])

const enumValueOperation: Operation = Object.freeze({
  _tag: 'IntrinsicOperation',
  id: operationId('Intrinsic', 'enumValue'),
  spelling: 'enumValue',
  typeParameters: Object.freeze([]),
  parameters: Object.freeze([valueParameter('value', '<owning enum>')]),
  result: '<owning enum representation>',
  unsafe: false,
  phase: 'Runtime',
  admission: 'Representation',
  consumer: 'language:scalar-enum-value',
  targets: runtimeTargets,
  rule: Object.freeze({ _tag: 'EnumValueRule' }),
})

const replaceOperation: Operation = Object.freeze({
  _tag: 'IntrinsicOperation',
  id: operationId('Intrinsic', 'replace'),
  spelling: 'replace',
  typeParameters: Object.freeze([typeParameter('T')]),
  parameters: Object.freeze([valueParameter('place', '&mut T'), valueParameter('value', 'T')]),
  result: 'T',
  unsafe: false,
  phase: 'Runtime',
  admission: admission('Place'),
  consumer: consumer('Place', 'replace'),
  targets: runtimeTargets,
  rule: Object.freeze({ _tag: 'PlaceRule', operation: 'Replace' }),
})

const intrinsicOperations = Object.freeze([
  ...Scalar.all().flatMap(scalarOperations),
  ...stringOperations,
  ...Object.freeze([
    osBuiltin({
      name: 'monotonicClockNow',
      operation: 'OsMonotonicClockNow',
      parameters: Object.freeze([
        valueParameter('seconds', '&mut i64'),
        valueParameter('nanoseconds', '&mut i64'),
      ]),
      semanticParameters: Object.freeze([mutableI64, mutableI64]),
      result: 'Effect<bool>',
      semanticResult: 'bool',
      invariant:
        'outputs are initialized only on success and form a canonical mark on one monotonic timeline',
    }),
    osBuiltin({
      name: 'monotonicClockResolution',
      operation: 'OsMonotonicClockResolution',
      parameters: Object.freeze([valueParameter('nanoseconds', '&mut u64')]),
      semanticParameters: Object.freeze([mutableU64]),
      result: 'Effect<bool>',
      semanticResult: 'bool',
      invariant:
        'output is initialized only on success and is a positive whole-nanosecond resolution',
    }),
    osBuiltin({
      name: 'monotonicClockWaitUntil',
      operation: 'OsMonotonicClockWaitUntil',
      parameters: Object.freeze([
        valueParameter('seconds', 'i64'),
        valueParameter('nanoseconds', 'i64'),
      ]),
      semanticParameters: Object.freeze(['i64', 'i64']),
      result: 'Effect<bool>',
      semanticResult: 'bool',
      invariant:
        'deadline is a canonical non-negative mark from the selected monotonic timeline and success means it has been reached',
    }),
    osBuiltin({
      name: 'randomFill',
      operation: 'OsRandomFill',
      parameters: Object.freeze([valueParameter('output', '&mut [u8]')]),
      semanticParameters: Object.freeze([
        Type.slice('Exclusive', 'u8', contractLifetime('randomFill')),
      ]),
      result: 'Effect<bool>',
      semanticResult: 'bool',
      invariant:
        'true means the complete initialized output contains fresh cryptographically secure bytes; false exposes no recoverable output',
    }),
    osOpen({
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
      invariant:
        'root is an absolute native path; path is normalized provider-absolute; status outputs are initialized; traversal rejects symlinks and namespace escape; success transfers one live handle only to the selected carrier',
    }),
    osBuiltin({
      name: 'fileRead',
      operation: 'OsFileRead',
      parameters: Object.freeze([
        valueParameter('handle', '&mut OsHandle'),
        valueParameter('output', '&mut [u8]'),
        valueParameter('count', '&mut usize'),
        valueParameter('reason', '&mut i32'),
        valueParameter('nativeCode', '&mut u32'),
      ]),
      semanticParameters: Object.freeze([
        mutableHandle,
        Type.slice('Exclusive', 'u8', contractLifetime('fileRead')),
        mutableUsize,
        mutableI32,
        mutableU32,
      ]),
      result: 'Effect<bool>',
      semanticResult: 'bool',
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
        valueParameter('count', '&mut usize'),
        valueParameter('reason', '&mut i32'),
        valueParameter('nativeCode', '&mut u32'),
      ]),
      semanticParameters: Object.freeze([
        mutableHandle,
        byteSlice,
        'usize',
        mutableUsize,
        mutableI32,
        mutableU32,
      ]),
      result: 'Effect<bool>',
      semanticResult: 'bool',
      invariant:
        'handle is a live file; input is initialized; success reports the exact transferred byte count and may be partial',
    }),
    osOpen({
      name: 'directoryOpen',
      operation: 'OsDirectoryOpen',
      parameters: Object.freeze([
        valueParameter('root', '&[u8]'),
        valueParameter('path', '&[u8]'),
        valueParameter('reason', '&mut i32'),
        valueParameter('nativeCode', '&mut u32'),
      ]),
      semanticParameters: Object.freeze([byteSlice, byteSlice, mutableI32, mutableU32]),
      invariant:
        'root and path satisfy confined traversal; status outputs are initialized; success transfers one live handle only to the selected carrier',
    }),
    osBuiltin({
      name: 'directoryNext',
      operation: 'OsDirectoryNext',
      parameters: Object.freeze([
        valueParameter('handle', '&mut OsHandle'),
        valueParameter('output', '&mut [u8]'),
        valueParameter('count', '&mut usize'),
        valueParameter('kind', '&mut i32'),
        valueParameter('requiredCapacity', '&mut usize'),
        valueParameter('reason', '&mut i32'),
        valueParameter('nativeCode', '&mut u32'),
      ]),
      semanticParameters: Object.freeze([
        mutableHandle,
        Type.slice('Exclusive', 'u8', contractLifetime('directoryNext')),
        mutableUsize,
        mutableI32,
        mutableUsize,
        mutableI32,
        mutableU32,
      ]),
      result: 'Effect<bool>',
      semanticResult: 'bool',
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
        valueParameter('count', '&mut usize'),
        valueParameter('requiredCapacity', '&mut usize'),
        valueParameter('reason', '&mut i32'),
        valueParameter('nativeCode', '&mut u32'),
      ]),
      semanticParameters: Object.freeze([
        byteSlice,
        byteSlice,
        byteSlice,
        Type.slice('Exclusive', 'u8', contractLifetime('directoryCreateUnique')),
        mutableUsize,
        mutableUsize,
        mutableI32,
        mutableU32,
      ]),
      result: 'Effect<bool>',
      semanticResult: 'bool',
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
        valueParameter('count', '&mut usize'),
        valueParameter('reason', '&mut i32'),
        valueParameter('nativeCode', '&mut u32'),
      ]),
      semanticParameters: Object.freeze([
        Type.slice('Exclusive', 'u8', contractLifetime('standardInputRead')),
        mutableUsize,
        mutableI32,
        mutableU32,
      ]),
      result: 'Effect<bool>',
      semanticResult: 'bool',
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
        valueParameter('count', '&mut usize'),
        valueParameter('reason', '&mut i32'),
        valueParameter('nativeCode', '&mut u32'),
      ]),
      semanticParameters: Object.freeze([
        'i32',
        'usize',
        Type.slice('Exclusive', 'u8', contractLifetime('processCapture')),
        mutableUsize,
        mutableI32,
        mutableU32,
      ]),
      result: 'Effect<bool>',
      semanticResult: 'bool',
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
        valueParameter('count', '&mut usize'),
        valueParameter('reason', '&mut i32'),
        valueParameter('nativeCode', '&mut u32'),
      ]),
      semanticParameters: Object.freeze([
        'usize',
        Type.slice('Exclusive', 'u8', contractLifetime('hostArgument')),
        mutableUsize,
        mutableI32,
        mutableU32,
      ]),
      result: 'Effect<bool>',
      semanticResult: 'bool',
      invariant:
        'output is initialized writable storage; success reports the complete argument byte length and copies the prefix that fits, and absence reports the not-found reason',
    }),
    osBuiltin({
      name: 'hostVariable',
      operation: 'OsHostVariable',
      parameters: Object.freeze([
        valueParameter('name', '&[u8]'),
        valueParameter('output', '&mut [u8]'),
        valueParameter('count', '&mut usize'),
        valueParameter('reason', '&mut i32'),
        valueParameter('nativeCode', '&mut u32'),
      ]),
      semanticParameters: Object.freeze([
        byteSlice,
        Type.slice('Exclusive', 'u8', contractLifetime('hostVariable')),
        mutableUsize,
        mutableI32,
        mutableU32,
      ]),
      result: 'Effect<bool>',
      semanticResult: 'bool',
      invariant:
        'output is initialized writable storage; success reports the complete value byte length and copies the prefix that fits, and an unset name reports the not-found reason',
    }),
    osBuiltin({
      name: 'hostWorkingDirectory',
      operation: 'OsHostWorkingDirectory',
      parameters: Object.freeze([
        valueParameter('output', '&mut [u8]'),
        valueParameter('count', '&mut usize'),
        valueParameter('reason', '&mut i32'),
        valueParameter('nativeCode', '&mut u32'),
      ]),
      semanticParameters: Object.freeze([
        Type.slice('Exclusive', 'u8', contractLifetime('hostWorkingDirectory')),
        mutableUsize,
        mutableI32,
        mutableU32,
      ]),
      result: 'Effect<bool>',
      semanticResult: 'bool',
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
      actor: 'Execution',
      name: 'layout',
      operation: 'ExecutionLayout',
      typeParameters: Object.freeze(['A', 'F', 'O', 'R']),
      semanticTypeParameters: executionPackageTypeParameters,
      parameters: Object.freeze([]),
      semanticParameters: Object.freeze([]),
      result: 'Layout',
      semanticResult: Type.layout,
    }),
    builtin({
      actor: 'Execution',
      name: 'fromAllocation',
      operation: 'ExecutionFromAllocation',
      typeParameters: Object.freeze(['A', 'F', 'O', 'R']),
      semanticTypeParameters: executionPackageTypeParameters,
      parameters: Object.freeze([
        valueParameter('allocation', 'Allocation'),
        valueParameter('body', 'F'),
        valueParameter('readyState', 'O'),
        valueParameter('onReady', 'R'),
      ]),
      semanticParameters: Object.freeze([
        Type.allocation,
        representedExecutionBody,
        executionEndpoint,
        representedExecutionReady,
      ]),
      result: 'Execution<A>',
      semanticResult: Type.execution(executionResult),
      unsafe: true,
    }),
    builtin({
      actor: 'Execution',
      name: 'drive',
      operation: 'ExecutionDrive',
      typeParameters: Object.freeze(['A', 'D', 'C', 'S']),
      semanticTypeParameters: executionDriveTypeParameters,
      parameters: Object.freeze([
        valueParameter('execution', 'Execution<A>'),
        valueParameter('branchState', 'D'),
        valueParameter('onComplete', 'C'),
        valueParameter('onSuspend', 'S'),
      ]),
      semanticParameters: Object.freeze([
        Type.execution(drivenResult),
        driveBranch,
        representedCompletion,
        representedSuspension,
      ]),
      result: 'Effect<()>',
      semanticResult: Type.effect(
        Type.unit,
        Object.freeze([]),
        { environment: contractLifetime('drive'), lifetimeBinders: [] },
        'Take',
      ),
    }),
    builtin({
      actor: 'Execution',
      name: 'notifyInitial',
      operation: 'ExecutionNotifyInitial',
      typeParameters: Object.freeze(['A']),
      semanticTypeParameters: Object.freeze([notifiedResult]),
      parameters: Object.freeze([valueParameter('execution', '&mut Execution<A>')]),
      semanticParameters: Object.freeze([
        Type.reference(
          'Exclusive',
          Type.execution(notifiedResult),
          contractLifetime('notifyInitial'),
        ),
      ]),
      result: '()',
      semanticResult: Type.unit,
    }),
    builtin({
      actor: 'Wake',
      name: 'signal',
      operation: 'ExecutionWake',
      parameters: Object.freeze([valueParameter('wake', 'Wake')]),
      semanticParameters: Object.freeze([Type.wake]),
      result: '()',
      semanticResult: Type.unit,
    }),
    builtin({
      actor: 'Parking',
      name: 'park',
      operation: 'ExecutionPark',
      typeParameters: Object.freeze(['G', 'F']),
      semanticTypeParameters: parkingTypeParameters,
      parameters: Object.freeze([valueParameter('register', 'F')]),
      semanticParameters: Object.freeze([representedRegistration]),
      result: 'Effect<()>',
      semanticResult: Type.effect(
        Type.unit,
        Object.freeze([]),
        { environment: contractLifetime('park'), lifetimeBinders: [] },
        'Take',
      ),
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
      semanticParameters: Object.freeze([
        Type.reference('Shared', Type.sharedCore(sharedElement), contractLifetime('clone')),
      ]),
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
        Type.reference('Shared', Type.sharedCore(sharedElement), contractLifetime('withMut')),
        Type.callable(
          Object.freeze([Type.reference('Exclusive', sharedElement, sharedAccessLifetime)]),
          sharedResult,
          { environment: contractLifetime('withMut'), lifetimeBinders: [sharedAccessLifetime] },
          'Take',
        ),
        Type.callable(
          Object.freeze([]),
          sharedResult,
          { environment: contractLifetime('withMut'), lifetimeBinders: [] },
          'Take',
        ),
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
      result: 'Effect<Allocation ! Intrinsic.StorageFailure>',
      semanticResult: Type.effect(
        Type.allocation,
        Object.freeze([Type.storageFailure]),
        { environment: contractLifetime('acquire'), lifetimeBinders: [] },
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
      semanticParameters: Object.freeze([
        'bool',
        Type.slice('Shared', 'u8', contractLifetime('write')),
      ]),
      result: 'Effect<() ! WriterError>',
      semanticResult: Type.effect(
        Type.unit,
        Object.freeze([Type.streamWriteFailure]),
        { environment: contractLifetime('write'), lifetimeBinders: [] },
        undefined,
        Object.freeze([]),
      ),
      targets: nativeTargets,
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
        Type.reference('Shared', Type.rawBuffer(rawElement), contractLifetime('view')),
        'usize',
        'usize',
      ]),
      result: '&[T]',
      semanticResult: Type.slice('Shared', rawElement, contractLifetime('view')),
      unsafe: true,
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
        Type.reference('Exclusive', Type.rawBuffer(rawElement), contractLifetime('viewMut')),
        'usize',
        'usize',
      ]),
      result: '&mut [T]',
      semanticResult: Type.slice('Exclusive', rawElement, contractLifetime('viewMut')),
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
        valueParameter('index', 'usize'),
      ]),
      semanticParameters: Object.freeze([
        Type.reference('Exclusive', Type.rawBuffer(rawElement), contractLifetime('slot')),
        'usize',
      ]),
      result: 'Slot<T>',
      semanticResult: Type.slot(rawElement, contractLifetime('slot')),
      unsafe: true,
    }),
    builtin({
      actor: 'RawBuffer',
      name: 'count',
      operation: 'RawBufferCount',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: rawTypeParameters,
      parameters: Object.freeze([valueParameter('buffer', '&RawBuffer<T>')]),
      semanticParameters: Object.freeze([
        Type.reference('Shared', Type.rawBuffer(rawElement), contractLifetime('count')),
      ]),
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
        Type.reference('Shared', Type.rawBuffer(rawElement), contractLifetime('read')),
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
        Type.reference('Exclusive', Type.rawBuffer(rawElement), contractLifetime('copy')),
        'usize',
        Type.slice('Shared', rawElement, contractLifetime('copy')),
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
        Type.reference('Exclusive', Type.rawBuffer('u8'), contractLifetime('fill')),
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
      actor: 'Pointer',
      name: 'requalify',
      operation: 'PointerRequalify',
      typeParameters: Object.freeze(['From', 'To']),
      semanticTypeParameters: Object.freeze([pointerSource, pointerDestination]),
      parameters: Object.freeze([valueParameter('pointer', 'From')]),
      semanticParameters: Object.freeze([pointerSource]),
      result: 'To',
      semanticResult: pointerDestination,
      unsafe: true,
    }),
    builtin({
      actor: 'Slot',
      name: 'address',
      operation: 'SlotAddress',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: rawTypeParameters,
      parameters: Object.freeze([valueParameter('slot', 'Slot<T>')]),
      semanticParameters: Object.freeze([Type.slot(rawElement, contractLifetime('slotAccess'))]),
      result: '*mut T',
      semanticResult: Type.pointer({
        mutable: true,
        pointee: rawElement,
        nullable: false,
        extent: 'Single',
        alignment: 'Natural',
        addressSpace: 0,
      }),
    }),
    builtin({
      actor: 'Pointer',
      name: 'readUnaligned',
      operation: 'PointerReadUnaligned',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: pointerTypeParameters,
      parameters: Object.freeze([valueParameter('pointer', '*const align(1) T')]),
      semanticParameters: Object.freeze([
        Type.pointer({
          mutable: false,
          pointee: pointerElement,
          nullable: false,
          extent: 'Single',
          alignment: 1,
          addressSpace: 0,
        }),
      ]),
      result: 'T',
      semanticResult: pointerElement,
      unsafe: true,
    }),
    builtin({
      actor: 'Pointer',
      name: 'writeUnaligned',
      operation: 'PointerWriteUnaligned',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: pointerTypeParameters,
      parameters: Object.freeze([
        valueParameter('pointer', '*mut align(1) T'),
        valueParameter('value', 'T'),
      ]),
      semanticParameters: Object.freeze([
        Type.pointer({
          mutable: true,
          pointee: pointerElement,
          nullable: false,
          extent: 'Single',
          alignment: 1,
          addressSpace: 0,
        }),
        pointerElement,
      ]),
      result: '()',
      semanticResult: Type.unit,
      unsafe: true,
    }),
    builtin({
      actor: 'Pointer',
      name: 'null',
      operation: 'PointerNull',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: pointerTypeParameters,
      parameters: Object.freeze([]),
      semanticParameters: Object.freeze([]),
      result: '?*mut T',
      semanticResult: Type.pointer({
        mutable: true,
        pointee: pointerElement,
        nullable: true,
        extent: 'Single',
        alignment: 'Natural',
        addressSpace: 0,
      }),
    }),
    builtin({
      actor: 'Pointer',
      name: 'isNull',
      operation: 'PointerIsNull',
      typeParameters: Object.freeze(['P']),
      semanticTypeParameters: Object.freeze([pointerSource]),
      parameters: Object.freeze([valueParameter('pointer', 'P')]),
      semanticParameters: Object.freeze([pointerSource]),
      result: 'bool',
      semanticResult: 'bool',
    }),
    builtin({
      actor: 'Pointer',
      name: 'fromRef',
      operation: 'PointerFromRef',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: pointerTypeParameters,
      parameters: Object.freeze([valueParameter('value', '&T')]),
      semanticParameters: Object.freeze([
        Type.reference('Shared', pointerElement, contractLifetime('fromRef')),
      ]),
      result: '*const T',
      semanticResult: Type.pointer({
        mutable: false,
        pointee: pointerElement,
        nullable: false,
        extent: 'Single',
        alignment: 'Natural',
        addressSpace: 0,
      }),
    }),
    builtin({
      actor: 'Pointer',
      name: 'fromMutRef',
      operation: 'PointerFromMutRef',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: pointerTypeParameters,
      parameters: Object.freeze([valueParameter('value', '&mut T')]),
      semanticParameters: Object.freeze([
        Type.reference('Exclusive', pointerElement, contractLifetime('fromMutRef')),
      ]),
      result: '*mut T',
      semanticResult: Type.pointer({
        mutable: true,
        pointee: pointerElement,
        nullable: false,
        extent: 'Single',
        alignment: 'Natural',
        addressSpace: 0,
      }),
    }),
    builtin({
      actor: 'Pointer',
      name: 'fromSlice',
      operation: 'PointerFromSlice',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: pointerTypeParameters,
      parameters: Object.freeze([valueParameter('values', '&[T]')]),
      semanticParameters: Object.freeze([
        Type.slice('Shared', pointerElement, contractLifetime('fromSlice')),
      ]),
      result: '?[*]const T',
      semanticResult: Type.pointer({
        mutable: false,
        pointee: pointerElement,
        nullable: true,
        extent: 'Many',
        alignment: 'Natural',
        addressSpace: 0,
      }),
    }),
    builtin({
      actor: 'Pointer',
      name: 'fromMutSlice',
      operation: 'PointerFromMutSlice',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: pointerTypeParameters,
      parameters: Object.freeze([valueParameter('values', '&mut [T]')]),
      semanticParameters: Object.freeze([
        Type.slice('Exclusive', pointerElement, contractLifetime('fromMutSlice')),
      ]),
      result: '?[*]mut T',
      semanticResult: Type.pointer({
        mutable: true,
        pointee: pointerElement,
        nullable: true,
        extent: 'Many',
        alignment: 'Natural',
        addressSpace: 0,
      }),
    }),
    builtin({
      actor: 'Pointer',
      name: 'at',
      operation: 'PointerAt',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: pointerTypeParameters,
      parameters: Object.freeze([
        valueParameter('pointer', '[*]const T'),
        valueParameter('count', 'usize'),
      ]),
      semanticParameters: Object.freeze([
        Type.pointer({
          mutable: false,
          pointee: pointerElement,
          nullable: false,
          extent: 'Many',
          alignment: 'Natural',
          addressSpace: 0,
        }),
        'usize',
      ]),
      result: '*const T',
      semanticResult: Type.pointer({
        mutable: false,
        pointee: pointerElement,
        nullable: false,
        extent: 'Single',
        alignment: 'Natural',
        addressSpace: 0,
      }),
      unsafe: true,
    }),
    builtin({
      actor: 'Pointer',
      name: 'atMut',
      operation: 'PointerAtMut',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: pointerTypeParameters,
      parameters: Object.freeze([
        valueParameter('pointer', '[*]mut T'),
        valueParameter('count', 'usize'),
      ]),
      semanticParameters: Object.freeze([
        Type.pointer({
          mutable: true,
          pointee: pointerElement,
          nullable: false,
          extent: 'Many',
          alignment: 'Natural',
          addressSpace: 0,
        }),
        'usize',
      ]),
      result: '*mut T',
      semanticResult: Type.pointer({
        mutable: true,
        pointee: pointerElement,
        nullable: false,
        extent: 'Single',
        alignment: 'Natural',
        addressSpace: 0,
      }),
      unsafe: true,
    }),
    builtin({
      actor: 'Pointer',
      name: 'read',
      operation: 'PointerRead',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: pointerTypeParameters,
      parameters: Object.freeze([valueParameter('pointer', '*const T')]),
      semanticParameters: Object.freeze([
        Type.pointer({
          mutable: false,
          pointee: pointerElement,
          nullable: false,
          extent: 'Single',
          alignment: 'Natural',
          addressSpace: 0,
        }),
      ]),
      result: 'T',
      semanticResult: pointerElement,
      unsafe: true,
    }),
    builtin({
      actor: 'Pointer',
      name: 'write',
      operation: 'PointerWrite',
      typeParameters: Object.freeze(['T']),
      semanticTypeParameters: pointerTypeParameters,
      parameters: Object.freeze([
        valueParameter('pointer', '*mut T'),
        valueParameter('value', 'T'),
      ]),
      semanticParameters: Object.freeze([
        Type.pointer({
          mutable: true,
          pointee: pointerElement,
          nullable: false,
          extent: 'Single',
          alignment: 'Natural',
          addressSpace: 0,
        }),
        pointerElement,
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
      semanticParameters: Object.freeze([
        Type.slot(rawElement, contractLifetime('slotAccess')),
        rawElement,
      ]),
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
      semanticParameters: Object.freeze([Type.slot(rawElement, contractLifetime('slotAccess'))]),
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
      semanticParameters: Object.freeze([Type.slot(rawElement, contractLifetime('slotAccess'))]),
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
      semanticParameters: Object.freeze([Type.slot(rawElement, contractLifetime('slotAccess'))]),
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
          { environment: contractLifetime('suspendEffect'), lifetimeBinders: [] },
          'Take',
          suspensionRequirementRow,
        ),
      ]),
      result: 'Effect<A ! E ? R>',
      semanticResult: Type.effectWithRows(
        suspensionSuccess,
        suspensionFailureRow,
        { environment: contractLifetime('suspendEffect'), lifetimeBinders: [] },
        'Take',
        suspensionRequirementRow,
      ),
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
  ...profileOperations,
  ...staticTextOperations,
  ...reflectionOperations,
  borrowFieldOperation,
  ...staticSequenceOperations,
  enumValueOperation,
  replaceOperation,
])

const operations = Object.freeze([
  stringActor,
  actor('Intrinsic', 'Namespace', intrinsicOperations),
])

const actorsBySpelling: ReadonlyMap<string, Actor> = new Map(
  operations.map((actor_): readonly [string, Actor] => [actor_.spelling, actor_]),
)

const operationsByActorSpelling: ReadonlyMap<string, ReadonlyMap<string, Operation>> = new Map(
  operations.map((actor_): readonly [string, ReadonlyMap<string, Operation>] => [
    actor_.spelling,
    new Map(
      actor_.operations.map((operation): readonly [string, Operation] => [
        operation.spelling,
        operation,
      ]),
    ),
  ]),
)

const operationsById: ReadonlyMap<string, ReadonlyMap<string, Operation>> = (() => {
  const actors = new Map<string, Map<string, Operation>>()
  for (const operation of intrinsicOperations) {
    const actor = actors.get(operation.id.actor)
    if (actor === undefined)
      actors.set(operation.id.actor, new Map([[operation.id.name, operation]]))
    else actor.set(operation.id.name, operation)
  }
  return actors
})()

/** Every intrinsic actor in stable presentation and completion order. */
export const all = (): ReadonlyArray<Actor> => operations

/** Finds an intrinsic actor by its accepted source spelling. */
export const findActor = (spelling: string): Actor | undefined => actorsBySpelling.get(spelling)

/** Finds an intrinsic operation by actor and member source spelling. */
export const findOperation = (actor_: string, spelling: string): Operation | undefined =>
  operationsByActorSpelling.get(actor_)?.get(spelling) ??
  (Scalar.isSpelling(actor_)
    ? operationsByActorSpelling.get('Intrinsic')?.get(`${actor_}${upperInitial(spelling)}`)
    : undefined)

/** Finds one sealed operation by its canonical compiler identity. */
export const findOperationById = (id: OperationId): Operation | undefined =>
  operationsById.get(id.actor)?.get(id.name)

/** Stable textual form of one sealed operation identity. */
export const operationText = (id: OperationId): string => `${id.actor}.${id.name}`

/** One deterministic audit record spanning source identity and execution surfaces. */
export interface InventoryEntry {
  readonly operation: string
  readonly signature: string
  readonly unsafe: boolean
  readonly phase: Phase
  readonly invariant?: string
  readonly admission: AdmissionCategory
  readonly consumer: string
  readonly hir?: string
  readonly mir?: string
  readonly targets: ReadonlyArray<Target.Id>
  readonly hostImport?: string
}

/** Publishes the closed intrinsic inventory used by verification and release review. */
export const inventory = (): ReadonlyArray<InventoryEntry> =>
  Object.freeze(
    intrinsicOperations.map((operation) => {
      if (operation.admission === undefined || operation.consumer === undefined)
        throw new RangeError(`Intrinsic ${operation.spelling} is missing admission metadata`)
      if (operation.phase === 'Runtime' && operation.targets.length === 0)
        throw new RangeError(`Runtime intrinsic ${operation.spelling} has no execution target`)
      if (operation.phase !== 'Runtime' && operation.targets.length !== 0)
        throw new RangeError(`Non-runtime intrinsic ${operation.spelling} has a runtime target`)
      const staticParameters = operation.parameters.flatMap((parameter, ordinal) =>
        parameter.phase === 'Static' ? [ordinal] : [],
      )
      if (operation.phase === 'Mixed') {
        if (
          operation.rule._tag !== 'MixedFieldProjectionRule' ||
          staticParameters.length !== 1 ||
          staticParameters.at(0) !== operation.rule.staticDescriptorParameter ||
          operation.parameters.at(operation.rule.runtimeOwnerParameter)?.phase === 'Static'
        )
          throw new RangeError(`Mixed intrinsic ${operation.spelling} has an invalid calling shape`)
      } else if (staticParameters.length !== 0) {
        throw new RangeError(`Whole-phase intrinsic ${operation.spelling} declares a static lane`)
      }
      const normalizedTargets = normalizeRuntimeTargets(operation.targets)
      if (
        normalizedTargets.length !== operation.targets.length ||
        normalizedTargets.some((target, index) => operation.targets.at(index) !== target)
      )
        throw new RangeError(`Intrinsic ${operation.spelling} has non-normalized target metadata`)
      let identity: string | undefined
      switch (operation.rule._tag) {
        case 'BuiltinRule':
          identity = operation.rule.operation
          break
        case 'ContractRule':
          identity = `${operation.rule._tag}.${operation.rule.post}`
          break
        case 'EnumValueRule':
          identity = operation.rule._tag
          break
        case 'StaticOnlyRule':
        case 'MixedFieldProjectionRule':
          identity = undefined
          break
        default:
          identity = `${operation.rule._tag}.${operation.rule.operation}`
          break
      }
      return Object.freeze({
        operation: `Intrinsic.${operation.spelling}`,
        signature: signature(operation),
        unsafe: operation.unsafe,
        phase: operation.phase,
        ...(operation.invariant === undefined ? {} : { invariant: operation.invariant }),
        admission: operation.admission,
        consumer: operation.consumer,
        ...(identity === undefined ? {} : { hir: identity, mir: identity }),
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
    .map(
      (parameter) =>
        `${parameter.phase === 'Static' ? 'static ' : ''}${parameter.name}: ${parameter.type}`,
    )
    .join(', ')
  return `${self.unsafe ? 'unsafe ' : ''}fn ${self.id.actor}.${self.spelling}${typeParameters}(${parameters}) -> ${self.result}`
}
