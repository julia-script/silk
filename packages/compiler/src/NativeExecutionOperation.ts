import * as Alignment from '@silk-effect/llvm/Alignment'
import * as LlvmBlock from '@silk-effect/llvm/Block'
import * as Constant from '@silk-effect/llvm/Constant'
import * as FunctionActor from '@silk-effect/llvm/Function'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import type * as LlvmError from '@silk-effect/llvm/LlvmError'
import type * as Value from '@silk-effect/llvm/Value'
import * as Effect from 'effect/Effect'
import { suspensionPointKey } from './Backend.js'
import * as ExecutionPackage from './ExecutionPackage.js'
import * as Hir from './Hir.js'
import * as Layout from './Layout.js'
import * as LayoutVerify from './LayoutVerify.js'
import type { LinearOperation } from './MirLinearization.js'
import * as NativeAggregate from './NativeAggregate.js'
import * as NativeCall from './NativeCall.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import type { Context } from './NativeOperationContext.js'
import * as NativeStorage from './NativeStorage.js'
import * as NativeSuspension from './NativeSuspension.js'
import * as NativeType from './NativeType.js'
import * as SilkType from './Type.js'

type Operation = Extract<
  LinearOperation,
  {
    readonly _tag: 'ExecutionFromAllocation' | 'ExecutionDrive' | 'ExecutionWake' | 'ExecutionPark'
  }
>

const componentOffset = (
  plan: ExecutionPackage.Plan,
  role: ExecutionPackage.Component['role'],
): number | undefined => {
  let cursor = 0
  for (const component of plan.components) {
    cursor = Math.ceil(cursor / component.alignment) * component.alignment
    if (component.role === role) return cursor
    cursor += component.size
  }
  return undefined
}

const targetForCallable = (
  context: Context,
  local: { readonly ordinal: number },
  typeArguments: ReadonlyArray<SilkType.GenericArgument>,
) => {
  const type = context.entry.fn.localTypes.at(local.ordinal)
  if (type?._tag !== 'CallableValue')
    throw new RangeError('LLVM execution callback lost its exact callable identity')
  if (type.target._tag !== 'DeclarationCallableTarget')
    throw new RangeError('LLVM execution callback cannot use a compiler builtin target')
  const declaration = type.target.declaration
  const target = context.declared.find((candidate) =>
    context.program.functions.some(
      (fn) =>
        fn === candidate.fn &&
        candidate.fn.id.module === declaration.module &&
        candidate.fn.id.name === declaration.name &&
        MirMatches(candidate.fn.instance.typeArguments, typeArguments),
    ),
  )
  if (target === undefined) throw new RangeError('LLVM execution callback target is unavailable')
  return Object.freeze({ type, target })
}

const MirMatches = (
  left: ReadonlyArray<SilkType.GenericArgument>,
  right: ReadonlyArray<SilkType.GenericArgument>,
): boolean =>
  left.length === right.length &&
  left.every(
    (argument, ordinal) =>
      SilkType.genericArgumentKey(argument) ===
      SilkType.genericArgumentKey(
        right.at(ordinal) ??
          (() => {
            throw new RangeError('missing generic argument')
          })(),
      ),
  )

const applyCallable = Effect.fnUntraced(function* (
  context: Context,
  local: { readonly _tag: 'Local'; readonly ordinal: number },
  typeArguments: ReadonlyArray<SilkType.GenericArgument>,
  arguments_: ReadonlyArray<Value.Input>,
  tag: string,
) {
  const { type, target } = targetForCallable(context, local, typeArguments)
  const values = NativeStorage.readLocal(context.storage, local)
  let cursor = 0
  const captures = (type.environment?.fields ?? [])
    .map((field) => {
      const lanes = Layout.callableFieldLanes(context.program.layout, field)
      const selected = values.slice(cursor, cursor + lanes.length)
      cursor += lanes.length
      return Object.freeze({ parameterOrdinal: field.parameterOrdinal, values: selected })
    })
    .sort((left, right) => left.parameterOrdinal - right.parameterOrdinal)
    .flatMap((capture) => [...capture.values])
  return yield* NativeCall.callValues(context.call, target, [...arguments_, ...captures], tag)
})

const storePackageValue = Effect.fnUntraced(function* (
  context: Context,
  base: Value.Input,
  local: { readonly _tag: 'Local'; readonly ordinal: number },
  type: SilkType.Type,
  byteOffset: number,
  tag: string,
) {
  const values = NativeStorage.readLocal(context.storage, local)
  const localType = context.entry.fn.localTypes.at(local.ordinal)
  const placements: Array<{
    readonly value: Value.Input
    readonly lane: Layout.CallingLane
    readonly offset: number
  }> = []
  let cursor = 0
  if (localType?._tag === 'CallableValue') {
    for (const field of localType.environment?.fields ?? []) {
      const lanes = Layout.callableFieldLanes(context.program.layout, field)
      for (const lane of lanes) {
        const value = values.at(cursor)
        const offset =
          field.representation === 'Borrow'
            ? 0
            : LayoutVerify.laneOffset(context.program.layout, field.type, lane.path)
        if (value === undefined || offset === undefined)
          throw new RangeError('LLVM execution package environment lost a lane')
        placements.push(Object.freeze({ value, lane, offset: field.offset + offset }))
        cursor += 1
      }
    }
  } else if (localType?._tag === 'EffectValue') {
    for (const field of localType.environment.fields) {
      const lanes = Layout.effectFieldLanes(context.program.layout, field)
      for (const lane of lanes) {
        const value = values.at(cursor)
        const offset =
          field.representation === 'Borrow'
            ? 0
            : LayoutVerify.laneOffset(context.program.layout, field.type, lane.path)
        if (value === undefined || offset === undefined)
          throw new RangeError('LLVM execution package environment lost a lane')
        placements.push(Object.freeze({ value, lane, offset: field.offset + offset }))
        cursor += 1
      }
    }
  } else {
    for (const lane of Layout.callingShape(context.program.layout, type)?.lanes ?? []) {
      const value = values.at(cursor)
      const offset = LayoutVerify.laneOffset(context.program.layout, type, lane.path)
      if (value === undefined || offset === undefined)
        throw new RangeError('LLVM execution package value lost a lane')
      placements.push(Object.freeze({ value, lane, offset }))
      cursor += 1
    }
  }
  if (cursor !== values.length)
    throw new RangeError('LLVM execution package value retained a stale lane')
  for (const [ordinal, placement] of placements.entries()) {
    yield* FunctionBody.store(
      context.body,
      placement.value,
      yield* NativeLanePointer.lanePointer(
        context.lanePointers,
        context.body,
        base,
        byteOffset + placement.offset,
        `${tag}_${ordinal}_ptr`,
      ),
    )
  }
})

interface PackageReadContext {
  readonly body: Context['body']
  readonly program: Context['program']
  readonly lanePointers: Context['lanePointers']
  readonly types: Context['types']
}

const loadPackageValue = Effect.fnUntraced(function* (
  context: PackageReadContext,
  base: Value.Input,
  type: SilkType.Type,
  byteOffset: number,
  tag: string,
) {
  const values: Array<Value.Input> = []
  for (const [ordinal, lane] of (
    Layout.callingShape(context.program.layout, type)?.lanes ?? []
  ).entries()) {
    const offset = LayoutVerify.laneOffset(context.program.layout, type, lane.path)
    if (offset === undefined) throw new RangeError('LLVM execution package lost a value lane')
    values.push(
      yield* FunctionBody.load(
        context.body,
        NativeType.laneType(context.types, lane),
        yield* NativeLanePointer.lanePointer(
          context.lanePointers,
          context.body,
          base,
          byteOffset + offset,
          `${tag}_${ordinal}_ptr`,
        ),
        `${tag}_${ordinal}`,
      ),
    )
  }
  return Object.freeze(values)
})

const exactEffect = (context: Context, package_: ExecutionPackage.Plan) => {
  const represented = package_.specialization.body
  const representation = SilkType.isRepresented(represented)
    ? represented.representation.argument
    : undefined
  const identity =
    representation !== undefined && SilkType.isExactRepresentationArgument(representation)
      ? representation.identity
      : undefined
  const environment =
    identity !== undefined && SilkType.isEffectIdentityArgument(identity)
      ? context.program.layout.effectEnvironments.find(
          (
            candidate,
          ): candidate is Extract<
            Layout.EffectEnvironment,
            { readonly _tag: 'EffectEnvironment' }
          > =>
            candidate._tag === 'EffectEnvironment' &&
            Hir.effectRepresentationIdentity(candidate.site) === identity.identity &&
            identity.owner !== undefined &&
            candidate.instance.declaration.module === identity.owner.declaration.module &&
            candidate.instance.declaration.name === identity.owner.declaration.name &&
            MirMatches(candidate.instance.typeArguments, identity.owner.typeArguments),
        )
      : undefined
  const target =
    environment === undefined
      ? undefined
      : context.declared.find(
          (candidate) =>
            candidate.fn.id.module === environment.instance.declaration.module &&
            candidate.fn.id.name ===
              Hir.effectRunnerId(environment.instance.declaration, environment.site).name &&
            MirMatches(candidate.fn.instance.typeArguments, environment.instance.typeArguments),
        )
  if (environment === undefined || target === undefined)
    throw new RangeError('LLVM execution drive lost its exact body runner')
  return Object.freeze({ environment, target })
}

const bodyOperands = Effect.fnUntraced(function* (
  context: Context,
  package_: ExecutionPackage.Plan,
  base: Value.Input,
  tag: string,
) {
  const { environment, target } = exactEffect(context, package_)
  const bodyOffset = componentOffset(package_, 'BodyEnvironment')
  if (bodyOffset === undefined) throw new RangeError('LLVM execution drive lost body storage')
  const values: Array<Value.Input> = []
  for (const field of environment.fields) {
    for (const [ordinal, lane] of Layout.effectFieldLanes(
      context.program.layout,
      field,
    ).entries()) {
      const offset =
        field.representation === 'Borrow'
          ? 0
          : LayoutVerify.laneOffset(context.program.layout, field.type, lane.path)
      if (offset === undefined) throw new RangeError('LLVM execution body lost a capture lane')
      values.push(
        yield* FunctionBody.load(
          context.body,
          NativeType.laneType(context.types, lane),
          yield* NativeLanePointer.lanePointer(
            context.lanePointers,
            context.body,
            base,
            bodyOffset + field.offset + offset,
            `${tag}_${field.ordinal}_${ordinal}_ptr`,
          ),
          `${tag}_${field.ordinal}_${ordinal}`,
        ),
      )
    }
  }
  return Object.freeze({ environment, target, values: Object.freeze(values) })
})

const notifyReady = Effect.fnUntraced(function* (
  context: Context,
  package_: ExecutionPackage.Plan,
  base: Value.Input,
  tag: string,
) {
  const callbackOffset = componentOffset(package_, 'EndpointCallback')
  const endpointOffset = componentOffset(package_, 'EndpointState')
  const callback = package_.specialization.callback
  const representation = SilkType.isRepresented(callback)
    ? callback.representation.argument
    : undefined
  const identity =
    representation !== undefined &&
    SilkType.isExactRepresentationArgument(representation) &&
    SilkType.isCallableIdentityArgument(representation.identity)
      ? representation.identity
      : undefined
  const targetIdentity =
    identity === undefined ? undefined : Hir.callableTargetFromIdentity(identity.target)
  const environment =
    identity?.environment === undefined
      ? undefined
      : Layout.callableEnvironmentByIdentity(context.program.layout, identity.environment)
  const targetArguments =
    identity === undefined
      ? Object.freeze([])
      : environment === undefined
        ? identity.typeArguments
        : Layout.callableTargetArguments(environment)
  const target =
    targetIdentity?._tag === 'DeclarationCallableTarget'
      ? context.declared.find(
          (candidate) =>
            candidate.fn.id.module === targetIdentity.declaration.module &&
            candidate.fn.id.name === targetIdentity.declaration.name &&
            MirMatches(candidate.fn.instance.typeArguments, targetArguments),
        )
      : undefined
  if (
    callbackOffset === undefined ||
    endpointOffset === undefined ||
    identity === undefined ||
    target === undefined ||
    (identity.environment !== undefined && environment === undefined)
  )
    throw new RangeError('LLVM readiness notification lost its exact package callback authority')
  const captures: Array<{
    readonly parameterOrdinal: number
    readonly values: ReadonlyArray<Value.Input>
  }> = []
  for (const field of environment?.fields ?? []) {
    const values: Array<Value.Input> = []
    for (const [ordinal, lane] of Layout.callableFieldLanes(
      context.program.layout,
      field,
    ).entries()) {
      const offset =
        field.representation === 'Borrow'
          ? 0
          : LayoutVerify.laneOffset(context.program.layout, field.type, lane.path)
      if (offset === undefined) throw new RangeError('LLVM readiness callback lost a capture lane')
      values.push(
        yield* FunctionBody.load(
          context.body,
          NativeType.laneType(context.types, lane),
          yield* NativeLanePointer.lanePointer(
            context.lanePointers,
            context.body,
            base,
            callbackOffset + field.offset + offset,
            `${tag}_capture${field.parameterOrdinal}_${ordinal}_ptr`,
          ),
          `${tag}_capture${field.parameterOrdinal}_${ordinal}`,
        ),
      )
    }
    captures.push(
      Object.freeze({ parameterOrdinal: field.parameterOrdinal, values: Object.freeze(values) }),
    )
  }
  yield* NativeCall.callValues(
    context.call,
    target,
    [
      yield* NativeLanePointer.lanePointer(
        context.lanePointers,
        context.body,
        base,
        endpointOffset,
        `${tag}_endpoint`,
      ),
      ...captures
        .sort((left, right) => left.parameterOrdinal - right.parameterOrdinal)
        .flatMap((capture) => capture.values),
    ],
    tag,
  )
})

const releasePackage = Effect.fnUntraced(function* (
  context: Context,
  package_: ExecutionPackage.Plan,
  base: Value.Input,
  tag: string,
) {
  const cleanup = package_.cleanup
  const allocationOffset = componentOffset(package_, 'AllocationAuthority')
  if (cleanup === undefined || allocationOffset === undefined)
    throw new RangeError('LLVM execution cleanup lost package metadata')
  const callbackOffset = componentOffset(package_, 'EndpointCallback')
  if (callbackOffset !== undefined)
    yield* NativeAggregate.dropThroughPlan(
      context.cleanup,
      cleanup.callback,
      yield* loadPackageValue(
        context,
        base,
        package_.specialization.callback,
        callbackOffset,
        `${tag}_callback_load`,
      ),
      `${tag}_callback`,
    )
  const endpointOffset = componentOffset(package_, 'EndpointState')
  if (endpointOffset !== undefined)
    yield* NativeAggregate.dropThroughPlan(
      context.cleanup,
      cleanup.endpoint,
      yield* loadPackageValue(
        context,
        base,
        package_.specialization.endpoint,
        endpointOffset,
        `${tag}_endpoint_load`,
      ),
      `${tag}_endpoint`,
    )
  yield* NativeAggregate.dropThroughPlan(
    context.cleanup,
    Object.freeze({
      _tag: 'AllocationCleanup' as const,
      type: SilkType.allocation,
      ticket: 'ActiveReclaimTicket' as const,
    }),
    yield* loadPackageValue(
      context,
      base,
      SilkType.allocation,
      allocationOffset,
      `${tag}_allocation_load`,
    ),
    `${tag}_allocation`,
  )
})

const releaseAllocation = Effect.fnUntraced(function* (
  context: Context,
  package_: ExecutionPackage.Plan,
  base: Value.Input,
  tag: string,
) {
  const allocationOffset = componentOffset(package_, 'AllocationAuthority')
  if (allocationOffset === undefined)
    throw new RangeError('LLVM execution lost allocation authority')
  yield* NativeAggregate.dropThroughPlan(
    context.cleanup,
    Object.freeze({
      _tag: 'AllocationCleanup' as const,
      type: SilkType.allocation,
      ticket: 'ActiveReclaimTicket' as const,
    }),
    yield* loadPackageValue(context, base, SilkType.allocation, allocationOffset, `${tag}_load`),
    tag,
  )
})

const dropStoredPackage = Effect.fnUntraced(function* (
  context: NativeAggregate.Context,
  package_: ExecutionPackage.Plan,
  base: Value.Input,
  options: {
    readonly body: boolean
    readonly endpoints: boolean
    readonly allocation: boolean
  },
  tag: string,
) {
  const cleanup = package_.cleanup
  if (cleanup === undefined) throw new RangeError('LLVM execution drop lost package metadata')
  if (options.body) {
    const offset = componentOffset(package_, 'BodyEnvironment')
    if (offset === undefined) throw new RangeError('LLVM execution drop lost body storage')
    yield* NativeAggregate.dropThroughPlan(
      context,
      cleanup.body,
      yield* loadPackageValue(
        context,
        base,
        package_.specialization.body,
        offset,
        `${tag}_body_load`,
      ),
      `${tag}_body`,
    )
  }
  if (options.endpoints) {
    const callbackOffset = componentOffset(package_, 'EndpointCallback')
    if (callbackOffset !== undefined)
      yield* NativeAggregate.dropThroughPlan(
        context,
        cleanup.callback,
        yield* loadPackageValue(
          context,
          base,
          package_.specialization.callback,
          callbackOffset,
          `${tag}_callback_load`,
        ),
        `${tag}_callback`,
      )
    const endpointOffset = componentOffset(package_, 'EndpointState')
    if (endpointOffset !== undefined)
      yield* NativeAggregate.dropThroughPlan(
        context,
        cleanup.endpoint,
        yield* loadPackageValue(
          context,
          base,
          package_.specialization.endpoint,
          endpointOffset,
          `${tag}_endpoint_load`,
        ),
        `${tag}_endpoint`,
      )
  }
  if (options.allocation) {
    const allocationOffset = componentOffset(package_, 'AllocationAuthority')
    if (allocationOffset === undefined)
      throw new RangeError('LLVM execution drop lost allocation storage')
    yield* NativeAggregate.dropThroughPlan(
      context,
      Object.freeze({
        _tag: 'AllocationCleanup' as const,
        type: SilkType.allocation,
        ticket: 'ActiveReclaimTicket' as const,
      }),
      yield* loadPackageValue(
        context,
        base,
        SilkType.allocation,
        allocationOffset,
        `${tag}_allocation_load`,
      ),
      `${tag}_allocation`,
    )
  }
})

const dropFrames = Effect.fnUntraced(function* (
  context: NativeAggregate.Context,
  package_: ExecutionPackage.Plan,
  base: Value.Input,
  tag: string,
) {
  const continuationOffset = componentOffset(package_, 'InitialContinuationSegment')
  if (continuationOffset === undefined) return
  const { body, builder, coroutineFramePop, lanePointers, pointer, program, usizeType } = context
  if (coroutineFramePop === undefined || usizeType === undefined)
    throw new RangeError('LLVM execution frame cleanup lost runtime support')
  const headStorage = yield* FunctionBody.alloca(body, pointer, `${tag}_head_slot`)
  yield* FunctionBody.store(
    body,
    yield* FunctionBody.load(
      body,
      pointer,
      yield* NativeLanePointer.lanePointer(
        lanePointers,
        body,
        base,
        continuationOffset,
        `${tag}_saved_head_ptr`,
      ),
      `${tag}_saved_head`,
    ),
    headStorage,
  )
  const loop = yield* LlvmBlock.make(body, `${tag}_frame_loop`)
  const finish = yield* LlvmBlock.make(body, `${tag}_frame_finish`)
  yield* FunctionBody.branch(body, loop)
  yield* LlvmBlock.setInsertionPoint(body, loop)
  const head = yield* FunctionBody.load(body, pointer, headStorage, `${tag}_head`)
  const address = yield* FunctionBody.cast(body, 'ptrtoint', head, usizeType, `${tag}_head_address`)
  const present = yield* LlvmBlock.make(body, `${tag}_frame_present`)
  yield* FunctionBody.conditionalBranch(
    body,
    yield* FunctionBody.integerCompare(
      body,
      'eq',
      address,
      yield* Constant.integerUnsigned(builder, usizeType, 0n),
      `${tag}_frames_done`,
    ),
    finish,
    present,
  )
  yield* LlvmBlock.setInsertionPoint(body, present)
  const next = yield* FunctionBody.load(body, pointer, head, `${tag}_next`)
  const resume = yield* FunctionBody.load(
    body,
    pointer,
    yield* NativeLanePointer.lanePointer(
      lanePointers,
      body,
      head,
      program.layout.target.pointerSize,
      `${tag}_resume_ptr`,
    ),
    `${tag}_resume`,
  )
  const resumeAddress = yield* FunctionBody.cast(
    body,
    'ptrtoint',
    resume,
    usizeType,
    `${tag}_resume_address`,
  )
  const released = yield* LlvmBlock.make(body, `${tag}_frame_released`)
  let otherwise = present
  for (const [ordinal, generated] of [...context.resumeThunks.values()].entries()) {
    const selected = yield* LlvmBlock.make(body, `${tag}_frame_${ordinal}`)
    const following = yield* LlvmBlock.make(body, `${tag}_frame_${ordinal}_otherwise`)
    if (otherwise !== present) yield* LlvmBlock.setInsertionPoint(body, otherwise)
    const target = yield* Constant.fromGlobal(
      builder,
      yield* FunctionActor.global(builder, generated.handle),
    )
    yield* FunctionBody.conditionalBranch(
      body,
      yield* FunctionBody.integerCompare(
        body,
        'eq',
        resumeAddress,
        yield* FunctionBody.cast(
          body,
          'ptrtoint',
          target,
          usizeType,
          `${tag}_target_${ordinal}_address`,
        ),
        `${tag}_frame_${ordinal}_matches`,
      ),
      selected,
      following,
    )
    yield* LlvmBlock.setInsertionPoint(body, selected)
    for (const field of generated.layout.payload) {
      if (field.access._tag !== 'AffineTransfer') continue
      const values: Array<Value.Input> = []
      const packed = NativeType.packLanes(
        program.layout.target,
        NativeType.lanesFor(context.types, field.type),
        field.offset,
      )
      for (const [laneOrdinal, lane] of packed.entries.entries())
        values.push(
          yield* FunctionBody.load(
            body,
            NativeType.laneType(context.types, lane.lane),
            yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              head,
              lane.offset,
              `${tag}_frame_${ordinal}_${laneOrdinal}_ptr`,
            ),
            `${tag}_frame_${ordinal}_${laneOrdinal}`,
          ),
        )
      yield* NativeAggregate.dropThroughPlan(
        context,
        field.access.cleanup,
        Object.freeze(values),
        `${tag}_frame_${ordinal}_slot${field.slot}`,
      )
    }
    yield* FunctionBody.branch(body, released)
    otherwise = following
  }
  yield* LlvmBlock.setInsertionPoint(body, otherwise)
  yield* FunctionBody.unreachable(body)
  yield* LlvmBlock.setInsertionPoint(body, released)
  yield* FunctionBody.callDirect(body, coroutineFramePop, [head], `${tag}_frame_pop`)
  yield* FunctionBody.store(body, next, headStorage)
  yield* FunctionBody.branch(body, loop)
  yield* LlvmBlock.setInsertionPoint(body, finish)
})

const selectPackage = Effect.fnUntraced(function* (
  context: NativeAggregate.Context,
  base: Value.Input,
  tag: string,
  emitPlan: (
    package_: ExecutionPackage.Plan,
    ordinal: number,
  ) => Effect.Effect<void, LlvmError.LlvmError>,
) {
  const { body, builder, program, usizeType } = context
  if (usizeType === undefined) throw new RangeError('LLVM execution cleanup lost usize')
  const packageOrdinal = yield* FunctionBody.load(
    body,
    usizeType,
    yield* NativeLanePointer.lanePointer(
      context.lanePointers,
      body,
      base,
      program.layout.target.pointerSize,
      `${tag}_package_ptr`,
    ),
    `${tag}_package`,
  )
  const following = yield* LlvmBlock.make(body, `${tag}_following`)
  let otherwise: LlvmBlock.Block | undefined
  for (const [ordinal, package_] of program.layout.executionPackages.plans.entries()) {
    if (otherwise !== undefined) yield* LlvmBlock.setInsertionPoint(body, otherwise)
    const selected = yield* LlvmBlock.make(body, `${tag}_package_${ordinal}`)
    const next = yield* LlvmBlock.make(body, `${tag}_package_${ordinal}_otherwise`)
    yield* FunctionBody.conditionalBranch(
      body,
      yield* FunctionBody.integerCompare(
        body,
        'eq',
        packageOrdinal,
        yield* Constant.integerUnsigned(builder, usizeType, BigInt(ordinal)),
        `${tag}_package_${ordinal}_matches`,
      ),
      selected,
      next,
    )
    yield* LlvmBlock.setInsertionPoint(body, selected)
    yield* emitPlan(package_, ordinal)
    yield* FunctionBody.branch(body, following)
    otherwise = next
  }
  if (otherwise === undefined) throw new RangeError('LLVM execution cleanup has no package plans')
  yield* LlvmBlock.setInsertionPoint(body, otherwise)
  yield* FunctionBody.unreachable(body)
  yield* LlvmBlock.setInsertionPoint(body, following)
})

const selectDrivePackage = Effect.fnUntraced(function* (
  context: NativeAggregate.Context,
  base: Value.Input,
  matching: ReadonlyArray<ExecutionPackage.Plan>,
  tag: string,
  emitPlan: (package_: ExecutionPackage.Plan) => Effect.Effect<void, LlvmError.LlvmError>,
) {
  const { body, builder, program, usizeType } = context
  if (usizeType === undefined) throw new RangeError('LLVM execution drive lost usize')
  const packageOrdinal = yield* FunctionBody.load(
    body,
    usizeType,
    yield* NativeLanePointer.lanePointer(
      context.lanePointers,
      body,
      base,
      program.layout.target.pointerSize,
      `${tag}_selected_package_ptr`,
    ),
    `${tag}_selected_package`,
  )
  const following = yield* LlvmBlock.make(body, `${tag}_selected_following`)
  let otherwise: LlvmBlock.Block | undefined
  for (const package_ of matching) {
    const ordinal = program.layout.executionPackages.plans.findIndex((candidate) =>
      ExecutionPackage.equals(candidate, package_),
    )
    if (ordinal < 0) throw new RangeError('LLVM execution drive lost a matching package ordinal')
    if (otherwise !== undefined) yield* LlvmBlock.setInsertionPoint(body, otherwise)
    const selected = yield* LlvmBlock.make(body, `${tag}_selected_package_${ordinal}`)
    const next = yield* LlvmBlock.make(body, `${tag}_selected_package_${ordinal}_otherwise`)
    yield* FunctionBody.conditionalBranch(
      body,
      yield* FunctionBody.integerCompare(
        body,
        'eq',
        packageOrdinal,
        yield* Constant.integerUnsigned(builder, usizeType, BigInt(ordinal)),
        `${tag}_selected_package_${ordinal}_matches`,
      ),
      selected,
      next,
    )
    yield* LlvmBlock.setInsertionPoint(body, selected)
    yield* emitPlan(package_)
    yield* FunctionBody.branch(body, following)
    otherwise = next
  }
  if (otherwise === undefined) throw new RangeError('LLVM execution drive has no matching packages')
  yield* LlvmBlock.setInsertionPoint(body, otherwise)
  yield* FunctionBody.unreachable(body)
  yield* LlvmBlock.setInsertionPoint(body, following)
})

/** Drops one opaque Execution through its package state and retained continuation authority. */
export const dropExecution = Effect.fnUntraced(function* (
  context: NativeAggregate.Context,
  values: ReadonlyArray<Value.Input>,
  tag: string,
) {
  const base = values.at(0)
  const { body, builder, usizeType } = context
  if (base === undefined || usizeType === undefined)
    throw new RangeError('LLVM Execution cleanup lost its package reference')
  yield* selectPackage(context, base, tag, (package_) =>
    Effect.gen(function* () {
      const statePointer = yield* NativeLanePointer.lanePointer(
        context.lanePointers,
        body,
        base,
        0,
        `${tag}_state_ptr`,
      )
      const state = yield* FunctionBody.load(body, usizeType, statePointer, `${tag}_state`)
      const initial = yield* LlvmBlock.make(body, `${tag}_initial`)
      const notInitial = yield* LlvmBlock.make(body, `${tag}_not_initial`)
      const done = yield* LlvmBlock.make(body, `${tag}_state_done`)
      yield* FunctionBody.conditionalBranch(
        body,
        yield* FunctionBody.integerCompare(
          body,
          'eq',
          state,
          yield* Constant.integerUnsigned(builder, usizeType, 0n),
          `${tag}_is_initial`,
        ),
        initial,
        notInitial,
      )
      yield* LlvmBlock.setInsertionPoint(body, initial)
      yield* FunctionBody.store(
        body,
        yield* Constant.integerUnsigned(builder, usizeType, 6n),
        statePointer,
      )
      yield* dropStoredPackage(
        context,
        package_,
        base,
        { body: true, endpoints: true, allocation: true },
        `${tag}_initial`,
      )
      yield* FunctionBody.branch(body, done)

      yield* LlvmBlock.setInsertionPoint(body, notInitial)
      const pending = yield* LlvmBlock.make(body, `${tag}_pending`)
      const inactive = yield* LlvmBlock.make(body, `${tag}_inactive`)
      const running = yield* FunctionBody.integerCompare(
        body,
        'eq',
        state,
        yield* Constant.integerUnsigned(builder, usizeType, 1n),
        `${tag}_is_running`,
      )
      const notifying = yield* FunctionBody.integerCompare(
        body,
        'eq',
        state,
        yield* Constant.integerUnsigned(builder, usizeType, 3n),
        `${tag}_is_notifying`,
      )
      yield* FunctionBody.conditionalBranch(
        body,
        yield* FunctionBody.binary(body, 'or', running, notifying, `${tag}_is_pending`),
        pending,
        inactive,
      )
      yield* LlvmBlock.setInsertionPoint(body, pending)
      yield* FunctionBody.store(
        body,
        yield* Constant.integerUnsigned(builder, usizeType, 7n),
        statePointer,
      )
      yield* FunctionBody.branch(body, done)

      yield* LlvmBlock.setInsertionPoint(body, inactive)
      const dormant = yield* FunctionBody.integerCompare(
        body,
        'eq',
        state,
        yield* Constant.integerUnsigned(builder, usizeType, 2n),
        `${tag}_is_dormant`,
      )
      const eligible = yield* FunctionBody.integerCompare(
        body,
        'eq',
        state,
        yield* Constant.integerUnsigned(builder, usizeType, 4n),
        `${tag}_is_eligible`,
      )
      const release = yield* LlvmBlock.make(body, `${tag}_release`)
      const invalid = yield* LlvmBlock.make(body, `${tag}_invalid_state`)
      yield* FunctionBody.conditionalBranch(
        body,
        yield* FunctionBody.binary(body, 'or', dormant, eligible, `${tag}_is_inactive`),
        release,
        invalid,
      )
      yield* LlvmBlock.setInsertionPoint(body, invalid)
      yield* FunctionBody.unreachable(body)
      yield* LlvmBlock.setInsertionPoint(body, release)
      const controlOffset = componentOffset(package_, 'WakeControl')
      let cancelled = eligible
      if (controlOffset !== undefined) {
        const phasePointer = yield* NativeLanePointer.lanePointer(
          context.lanePointers,
          body,
          base,
          controlOffset,
          `${tag}_phase_ptr`,
        )
        const phase = yield* FunctionBody.load(body, usizeType, phasePointer, `${tag}_phase`)
        cancelled = yield* FunctionBody.integerCompare(
          body,
          'eq',
          phase,
          yield* Constant.integerUnsigned(builder, usizeType, 6n),
          `${tag}_wake_already_cancelled`,
        )
        yield* FunctionBody.store(
          body,
          yield* Constant.integerUnsigned(builder, usizeType, 6n),
          phasePointer,
        )
      }
      yield* FunctionBody.store(
        body,
        yield* Constant.integerUnsigned(builder, usizeType, 6n),
        statePointer,
      )
      yield* dropStoredPackage(
        context,
        package_,
        base,
        { body: false, endpoints: true, allocation: false },
        `${tag}_inactive`,
      )
      yield* dropFrames(context, package_, base, `${tag}_inactive`)
      const releaseAllocationBlock = yield* LlvmBlock.make(body, `${tag}_release_allocation`)
      const retainAllocationBlock = yield* LlvmBlock.make(body, `${tag}_retain_allocation`)
      yield* FunctionBody.conditionalBranch(
        body,
        yield* FunctionBody.binary(body, 'or', eligible, cancelled, `${tag}_release_now`),
        releaseAllocationBlock,
        retainAllocationBlock,
      )
      yield* LlvmBlock.setInsertionPoint(body, releaseAllocationBlock)
      yield* dropStoredPackage(
        context,
        package_,
        base,
        { body: false, endpoints: false, allocation: true },
        `${tag}_final`,
      )
      yield* FunctionBody.branch(body, done)
      yield* LlvmBlock.setInsertionPoint(body, retainAllocationBlock)
      yield* FunctionBody.branch(body, done)
      yield* LlvmBlock.setInsertionPoint(body, done)
    }),
  )
})

/** Drops one affine Wake, cancelling or finally discharging its generation authority. */
export const dropWake = Effect.fnUntraced(function* (
  context: NativeAggregate.Context,
  values: ReadonlyArray<Value.Input>,
  tag: string,
) {
  const base = values.at(0)
  const { body, builder, usizeType } = context
  if (base === undefined || usizeType === undefined)
    throw new RangeError('LLVM Wake cleanup lost its package reference')
  yield* selectPackage(context, base, tag, (package_) =>
    Effect.gen(function* () {
      const controlOffset = componentOffset(package_, 'WakeControl')
      if (controlOffset === undefined) return yield* Effect.die('Wake package lacks control state')
      const phasePointer = yield* NativeLanePointer.lanePointer(
        context.lanePointers,
        body,
        base,
        controlOffset,
        `${tag}_phase_ptr`,
      )
      const phase = yield* FunctionBody.load(body, usizeType, phasePointer, `${tag}_phase`)
      const late = yield* LlvmBlock.make(body, `${tag}_late`)
      const cancel = yield* LlvmBlock.make(body, `${tag}_cancel`)
      const done = yield* LlvmBlock.make(body, `${tag}_done`)
      yield* FunctionBody.conditionalBranch(
        body,
        yield* FunctionBody.integerCompare(
          body,
          'eq',
          phase,
          yield* Constant.integerUnsigned(builder, usizeType, 6n),
          `${tag}_is_late`,
        ),
        late,
        cancel,
      )
      yield* LlvmBlock.setInsertionPoint(body, late)
      yield* dropStoredPackage(
        context,
        package_,
        base,
        { body: false, endpoints: false, allocation: true },
        `${tag}_late`,
      )
      yield* FunctionBody.branch(body, done)
      yield* LlvmBlock.setInsertionPoint(body, cancel)
      yield* FunctionBody.store(
        body,
        yield* Constant.integerUnsigned(builder, usizeType, 6n),
        phasePointer,
      )
      yield* FunctionBody.branch(body, done)
      yield* LlvmBlock.setInsertionPoint(body, done)
    }),
  )
})

export const emit = Effect.fnUntraced(function* (context: Context, operation: Operation) {
  const { body, builder, i32, lanePointers, pointer, program, storage, usizeType } = context
  if (usizeType === undefined) throw new RangeError('LLVM execution lowering requires usize')
  switch (operation._tag) {
    case 'ExecutionFromAllocation': {
      const allocation = NativeStorage.readLocal(storage, operation.allocation)
      const baseAddress = allocation.at(0)
      const bytes = allocation.at(1)
      const alignment = allocation.at(2)
      if (baseAddress === undefined || bytes === undefined || alignment === undefined)
        throw new RangeError('LLVM execution initialization lost allocation authority')
      const bytesMismatch = yield* FunctionBody.integerCompare(
        body,
        'ne',
        bytes,
        yield* Constant.integerUnsigned(builder, usizeType, BigInt(operation.plan.size)),
        `execution${operation.destination.ordinal}_bytes_mismatch`,
      )
      const alignmentMismatch = yield* FunctionBody.integerCompare(
        body,
        'ne',
        alignment,
        yield* Constant.integerUnsigned(builder, usizeType, BigInt(operation.plan.alignment)),
        `execution${operation.destination.ordinal}_alignment_mismatch`,
      )
      const invalid = yield* FunctionBody.binary(
        body,
        'or',
        bytesMismatch,
        alignmentMismatch,
        `execution${operation.destination.ordinal}_invalid`,
      )
      const rejected = yield* LlvmBlock.make(body, `execution${operation.destination.ordinal}_trap`)
      const accepted = yield* LlvmBlock.make(
        body,
        `execution${operation.destination.ordinal}_accepted`,
      )
      yield* FunctionBody.conditionalBranch(body, invalid, rejected, accepted)
      yield* LlvmBlock.setInsertionPoint(body, rejected)
      yield* FunctionBody.unreachable(body)
      yield* LlvmBlock.setInsertionPoint(body, accepted)
      const base = yield* FunctionBody.cast(
        body,
        'inttoptr',
        baseAddress,
        pointer,
        `execution${operation.destination.ordinal}_base`,
      )
      const storeWord = Effect.fnUntraced(function* (offset: number, value: Value.Input) {
        yield* FunctionBody.store(
          body,
          value,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            base,
            offset,
            `execution${operation.destination.ordinal}_${offset}_ptr`,
          ),
        )
      })
      const packageOrdinal = program.layout.executionPackages.plans.findIndex((candidate) =>
        ExecutionPackage.equals(candidate, operation.plan),
      )
      if (packageOrdinal < 0)
        throw new RangeError('LLVM execution initialization lost its package ordinal')
      const zero = yield* Constant.integerUnsigned(builder, usizeType, 0n)
      yield* storeWord(0, zero)
      yield* storeWord(
        program.layout.target.pointerSize,
        yield* Constant.integerUnsigned(builder, usizeType, BigInt(packageOrdinal)),
      )
      for (const role of ['WakeControl', 'InitialContinuationSegment'] as const) {
        const offset = componentOffset(operation.plan, role)
        if (offset === undefined) continue
        for (let word = 0; word < 4; word += 1)
          yield* storeWord(offset + word * program.layout.target.pointerSize, zero)
      }
      const allocationOffset = componentOffset(operation.plan, 'AllocationAuthority')
      const bodyOffset = componentOffset(operation.plan, 'BodyEnvironment')
      if (allocationOffset === undefined || bodyOffset === undefined)
        throw new RangeError('LLVM execution initialization lost package components')
      const allocationLanes = Layout.callingShape(program.layout, SilkType.allocation)?.lanes ?? []
      for (const [ordinal, lane] of allocationLanes.entries()) {
        const value = allocation.at(ordinal)
        const offset = LayoutVerify.laneOffset(program.layout, SilkType.allocation, lane.path)
        if (value === undefined || offset === undefined)
          throw new RangeError('LLVM execution initialization lost allocation lane')
        yield* storeWord(allocationOffset + offset, value)
      }
      yield* storePackageValue(
        context,
        base,
        operation.body,
        operation.plan.specialization.body,
        bodyOffset,
        `execution${operation.destination.ordinal}_body`,
      )
      const endpointOffset = componentOffset(operation.plan, 'EndpointState')
      if (endpointOffset !== undefined)
        yield* storePackageValue(
          context,
          base,
          operation.endpoint,
          operation.plan.specialization.endpoint,
          endpointOffset,
          `execution${operation.destination.ordinal}_endpoint`,
        )
      const callbackOffset = componentOffset(operation.plan, 'EndpointCallback')
      if (callbackOffset !== undefined)
        yield* storePackageValue(
          context,
          base,
          operation.callback,
          operation.plan.specialization.callback,
          callbackOffset,
          `execution${operation.destination.ordinal}_callback`,
        )
      storage.locals.set(operation.destination.ordinal, Object.freeze([base]))
      return
    }
    case 'ExecutionPark': {
      const transfer = context.call.transferPointer
      const region = context.suspensionRegions.get(operation)
      const packages = program.layout.executionPackages.plans.filter(
        (candidate) => candidate.readinessStorage,
      )
      if (
        transfer === undefined ||
        region?._tag !== 'RunSuspendableEffectRegion' ||
        packages.length === 0
      )
        throw new RangeError('LLVM park lost external transfer authority')
      const baseAddress = yield* FunctionBody.load(
        body,
        usizeType,
        yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          transfer,
          program.layout.target.pointerSize * 3,
          `park${operation.destination.ordinal}_active_ptr`,
        ),
        `park${operation.destination.ordinal}_active`,
      )
      const base = yield* FunctionBody.cast(
        body,
        'inttoptr',
        baseAddress,
        pointer,
        `park${operation.destination.ordinal}_base`,
      )
      const controlStorage = yield* FunctionBody.alloca(
        body,
        pointer,
        `park${operation.destination.ordinal}_control_slot`,
      )
      const storedPackage = yield* FunctionBody.load(
        body,
        usizeType,
        yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          base,
          program.layout.target.pointerSize,
          `park${operation.destination.ordinal}_package_ptr`,
        ),
        `park${operation.destination.ordinal}_package`,
      )
      const packageSelected = yield* LlvmBlock.make(
        body,
        `park${operation.destination.ordinal}_package_selected`,
      )
      let packageOtherwise: LlvmBlock.Block | undefined
      for (const package_ of packages) {
        const ordinal = program.layout.executionPackages.plans.findIndex((candidate) =>
          ExecutionPackage.equals(candidate, package_),
        )
        const control = componentOffset(package_, 'WakeControl')
        if (ordinal < 0 || control === undefined)
          throw new RangeError('LLVM park lost a wake-control package ordinal')
        if (packageOtherwise !== undefined)
          yield* LlvmBlock.setInsertionPoint(body, packageOtherwise)
        const selected = yield* LlvmBlock.make(
          body,
          `park${operation.destination.ordinal}_package_${ordinal}`,
        )
        const otherwise = yield* LlvmBlock.make(
          body,
          `park${operation.destination.ordinal}_package_${ordinal}_otherwise`,
        )
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            storedPackage,
            yield* Constant.integerUnsigned(builder, usizeType, BigInt(ordinal)),
            `park${operation.destination.ordinal}_package_${ordinal}_matches`,
          ),
          selected,
          otherwise,
        )
        yield* LlvmBlock.setInsertionPoint(body, selected)
        yield* FunctionBody.store(
          body,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            base,
            control,
            `park${operation.destination.ordinal}_package_${ordinal}_control`,
          ),
          controlStorage,
        )
        yield* FunctionBody.branch(body, packageSelected)
        packageOtherwise = otherwise
      }
      if (packageOtherwise === undefined)
        throw new RangeError('LLVM park lost every wake-control package')
      yield* LlvmBlock.setInsertionPoint(body, packageOtherwise)
      yield* FunctionBody.unreachable(body)
      yield* LlvmBlock.setInsertionPoint(body, packageSelected)
      const phasePointer = yield* FunctionBody.load(
        body,
        pointer,
        controlStorage,
        `park${operation.destination.ordinal}_phase_ptr`,
      )
      const generationPointer = yield* NativeLanePointer.lanePointer(
        lanePointers,
        body,
        phasePointer,
        program.layout.target.pointerSize,
        `park${operation.destination.ordinal}_generation_ptr`,
      )
      const generation = yield* FunctionBody.load(
        body,
        usizeType,
        generationPointer,
        `park${operation.destination.ordinal}_generation`,
      )
      yield* FunctionBody.store(
        body,
        yield* FunctionBody.binary(
          body,
          'add',
          generation,
          yield* Constant.integerUnsigned(builder, usizeType, 1n),
          `park${operation.destination.ordinal}_next_generation`,
        ),
        generationPointer,
      )
      yield* FunctionBody.store(
        body,
        yield* Constant.integerUnsigned(builder, usizeType, 1n),
        phasePointer,
      )
      const guard = yield* applyCallable(
        context,
        operation.register,
        operation.registrationTypeArguments,
        [base],
        `park${operation.destination.ordinal}_register`,
      )
      storage.locals.set(operation.guard.ordinal, guard)
      const phase = yield* FunctionBody.load(
        body,
        usizeType,
        phasePointer,
        `park${operation.destination.ordinal}_phase`,
      )
      const latched = yield* FunctionBody.integerCompare(
        body,
        'eq',
        phase,
        yield* Constant.integerUnsigned(builder, usizeType, 2n),
        `park${operation.destination.ordinal}_latched`,
      )
      const keepLatched = yield* LlvmBlock.make(
        body,
        `park${operation.destination.ordinal}_keep_latched`,
      )
      const dormant = yield* LlvmBlock.make(body, `park${operation.destination.ordinal}_dormant`)
      const relinquish = yield* LlvmBlock.make(
        body,
        `park${operation.destination.ordinal}_relinquish`,
      )
      yield* FunctionBody.conditionalBranch(body, latched, keepLatched, dormant)
      yield* LlvmBlock.setInsertionPoint(body, keepLatched)
      yield* FunctionBody.branch(body, relinquish)
      yield* LlvmBlock.setInsertionPoint(body, dormant)
      yield* FunctionBody.store(
        body,
        yield* Constant.integerUnsigned(builder, usizeType, 3n),
        phasePointer,
      )
      yield* FunctionBody.branch(body, relinquish)
      yield* LlvmBlock.setInsertionPoint(body, relinquish)
      yield* NativeCall.retainRelay(context.call, region, `park${operation.destination.ordinal}`)
      yield* NativeSuspension.returnStep(
        context.call.returns,
        2n,
        Object.freeze([]),
        `park${operation.destination.ordinal}_external`,
      )
      yield* LlvmBlock.setInsertionPoint(
        body,
        context.suspension.resumeBlocks.get(suspensionPointKey(region.point)) ??
          (() => {
            throw new RangeError('LLVM park lost its verified resume label')
          })(),
      )
      yield* NativeSuspension.restoreRelayPayload(
        context.suspension,
        region,
        `park${operation.destination.ordinal}`,
      )
      yield* NativeAggregate.dropThroughPlan(
        context.cleanup,
        operation.guardCleanup,
        NativeStorage.readLocal(storage, operation.guard),
        `park${operation.destination.ordinal}_guard`,
      )
      storage.locals.set(operation.destination.ordinal, Object.freeze([]))
      return
    }
    case 'ExecutionDrive': {
      const executionType = context.entry.fn.localTypes.at(operation.execution.ordinal)
      const executionResult =
        executionType?._tag === 'Nominal' && SilkType.isExecution(executionType.type)
          ? SilkType.typeArgumentAt(executionType.type, 0)
          : undefined
      const matchingPackages = program.layout.executionPackages.plans.filter(
        (package_) =>
          executionResult !== undefined &&
          SilkType.equals(package_.specialization.result, executionResult),
      )
      if (matchingPackages.length === 0)
        throw new RangeError('LLVM execution drive lost every result package specialization')
      const base = NativeStorage.readLocal(storage, operation.execution).at(0)
      if (base === undefined)
        throw new RangeError('LLVM execution drive lost its package reference')
      const emitPackage = Effect.fnUntraced(function* (package_: ExecutionPackage.Plan) {
        const continuationOffset = componentOffset(package_, 'InitialContinuationSegment')
        const childThunkType = context.childThunkType
        const resumeThunkType = context.resumeThunkType
        if (
          base === undefined ||
          continuationOffset === undefined ||
          childThunkType === undefined ||
          resumeThunkType === undefined
        )
          throw new RangeError('LLVM execution drive lost independent suspension storage')
        const baseAddress = yield* FunctionBody.cast(
          body,
          'ptrtoint',
          base,
          usizeType,
          `drive${operation.destination.ordinal}_base_address`,
        )
        const transfer = yield* FunctionBody.alloca(
          body,
          context.suspension.i8,
          `drive${operation.destination.ordinal}_transfer`,
          {
            count: yield* Constant.integerUnsigned(
              builder,
              i32,
              BigInt(Math.max(context.transferStorageSize, 1)),
            ),
            alignment: yield* Alignment.fromByteUnits(program.layout.target.pointerAlignment),
          },
        )
        const statePointer = yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          base,
          0,
          `drive${operation.destination.ordinal}_state_ptr`,
        )
        const packagePointer = yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          base,
          program.layout.target.pointerSize,
          `drive${operation.destination.ordinal}_package_ptr`,
        )
        const packageOrdinal = program.layout.executionPackages.plans.findIndex((candidate) =>
          ExecutionPackage.equals(candidate, package_),
        )
        if (packageOrdinal < 0) throw new RangeError('LLVM execution drive lost package ordinal')
        const storedPackage = yield* FunctionBody.load(
          body,
          usizeType,
          packagePointer,
          `drive${operation.destination.ordinal}_package`,
        )
        const state = yield* FunctionBody.load(
          body,
          usizeType,
          statePointer,
          `drive${operation.destination.ordinal}_state`,
        )
        const initial = yield* FunctionBody.integerCompare(
          body,
          'eq',
          state,
          yield* Constant.integerUnsigned(builder, usizeType, 0n),
          `drive${operation.destination.ordinal}_initial`,
        )
        const eligible = yield* FunctionBody.integerCompare(
          body,
          'eq',
          state,
          yield* Constant.integerUnsigned(builder, usizeType, 4n),
          `drive${operation.destination.ordinal}_eligible`,
        )
        const validState = yield* FunctionBody.binary(
          body,
          'or',
          initial,
          eligible,
          `drive${operation.destination.ordinal}_valid_state`,
        )
        const validPackage = yield* FunctionBody.integerCompare(
          body,
          'eq',
          storedPackage,
          yield* Constant.integerUnsigned(builder, usizeType, BigInt(packageOrdinal)),
          `drive${operation.destination.ordinal}_valid_package`,
        )
        const valid = yield* FunctionBody.binary(
          body,
          'and',
          validState,
          validPackage,
          `drive${operation.destination.ordinal}_valid`,
        )
        const accepted = yield* LlvmBlock.make(
          body,
          `drive${operation.destination.ordinal}_accepted`,
        )
        const rejected = yield* LlvmBlock.make(
          body,
          `drive${operation.destination.ordinal}_rejected`,
        )
        yield* FunctionBody.conditionalBranch(body, valid, accepted, rejected)
        yield* LlvmBlock.setInsertionPoint(body, rejected)
        yield* FunctionBody.unreachable(body)
        yield* LlvmBlock.setInsertionPoint(body, accepted)
        yield* FunctionBody.store(
          body,
          baseAddress,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            transfer,
            program.layout.target.pointerSize * 3,
            `drive${operation.destination.ordinal}_active`,
          ),
        )
        yield* FunctionBody.store(
          body,
          yield* Constant.integerUnsigned(builder, usizeType, 1n),
          statePointer,
        )
        const statusStorage = yield* FunctionBody.alloca(
          body,
          i32,
          `drive${operation.destination.ordinal}_status_slot`,
        )
        const initialBlock = yield* LlvmBlock.make(
          body,
          `drive${operation.destination.ordinal}_start`,
        )
        const resumeBlock = yield* LlvmBlock.make(
          body,
          `drive${operation.destination.ordinal}_resume`,
        )
        const loop = yield* LlvmBlock.make(body, `drive${operation.destination.ordinal}_loop`)
        const operationFollowing = yield* LlvmBlock.make(
          body,
          `drive${operation.destination.ordinal}_following`,
        )
        const headPointer = yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          transfer,
          program.layout.target.pointerSize,
          `drive${operation.destination.ordinal}_head_ptr`,
        )
        const appendPointerPointer = yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          transfer,
          program.layout.target.pointerSize * 2,
          `drive${operation.destination.ordinal}_append_ptr_ptr`,
        )
        yield* FunctionBody.conditionalBranch(body, initial, initialBlock, resumeBlock)

        yield* LlvmBlock.setInsertionPoint(body, initialBlock)
        const nullPointer = yield* Constant.nullValue(builder, pointer)
        yield* FunctionBody.store(body, nullPointer, headPointer)
        yield* FunctionBody.store(body, headPointer, appendPointerPointer)
        const executable = yield* bodyOperands(
          context,
          package_,
          base,
          `drive${operation.destination.ordinal}_body`,
        )
        const started = executable.target.suspendable
          ? yield* FunctionBody.callDirect(
              body,
              executable.target.handle,
              [
                ...executable.values,
                transfer,
                nullPointer,
                yield* Constant.integerUnsigned(builder, i32, 0n),
              ],
              `drive${operation.destination.ordinal}_started`,
            )
          : yield* FunctionBody.callDirect(
              body,
              executable.target.handle,
              executable.values,
              `drive${operation.destination.ordinal}_started`,
            )
        if (executable.target.resultLaneCount > 0 && started === undefined)
          throw new RangeError('LLVM execution body returned no outcome')
        const outcomeLanes = NativeType.lanesFor(context.types, executable.target.fn.result)
        const startedValues: Array<Value.Input> = []
        if (started !== undefined) {
          if (executable.target.resultLaneCount === 1 && !executable.target.suspendable)
            startedValues.push(started)
          else
            for (let ordinal = 0; ordinal < executable.target.resultLaneCount; ordinal += 1)
              startedValues.push(
                yield* FunctionBody.extractValue(
                  body,
                  started,
                  [ordinal + (executable.target.suspendable ? 1 : 0)],
                  `drive${operation.destination.ordinal}_initial_result${ordinal}`,
                ),
              )
        }
        const packedOutcome = NativeType.packLanes(
          program.layout.target,
          outcomeLanes,
          context.suspension.transferResultOffset,
        )
        for (const [ordinal, lane] of packedOutcome.entries.entries()) {
          const value = startedValues.at(ordinal)
          if (value === undefined) throw new RangeError('LLVM execution body lost an outcome lane')
          yield* FunctionBody.store(
            body,
            value,
            yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              transfer,
              lane.offset,
              `drive${operation.destination.ordinal}_initial_result${ordinal}_ptr`,
            ),
          )
        }
        const startedStatus =
          executable.target.suspendable && started !== undefined
            ? yield* FunctionBody.extractValue(
                body,
                started,
                [0],
                `drive${operation.destination.ordinal}_initial_status`,
              )
            : yield* Constant.integerUnsigned(builder, i32, 0n)
        yield* FunctionBody.store(body, startedStatus, statusStorage)
        yield* FunctionBody.branch(body, loop)

        yield* LlvmBlock.setInsertionPoint(body, resumeBlock)
        const savedHead = yield* FunctionBody.load(
          body,
          pointer,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            base,
            continuationOffset,
            `drive${operation.destination.ordinal}_saved_head_ptr`,
          ),
          `drive${operation.destination.ordinal}_saved_head`,
        )
        const savedNext = yield* FunctionBody.load(
          body,
          pointer,
          savedHead,
          `drive${operation.destination.ordinal}_saved_next`,
        )
        yield* FunctionBody.store(body, savedNext, headPointer)
        yield* FunctionBody.store(body, headPointer, appendPointerPointer)
        const resumeFunction = yield* FunctionBody.load(
          body,
          pointer,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            savedHead,
            program.layout.target.pointerSize,
            `drive${operation.destination.ordinal}_resume_fn_ptr`,
          ),
          `drive${operation.destination.ordinal}_resume_fn`,
        )
        const resumed = yield* FunctionBody.call(
          body,
          resumeThunkType,
          resumeFunction,
          [transfer, savedHead],
          `drive${operation.destination.ordinal}_resumed`,
        )
        if (resumed === undefined) throw new RangeError('LLVM execution resume returned no status')
        yield* FunctionBody.store(body, resumed, statusStorage)
        yield* FunctionBody.branch(body, loop)

        yield* LlvmBlock.setInsertionPoint(body, loop)
        const status = yield* FunctionBody.load(
          body,
          i32,
          statusStorage,
          `drive${operation.destination.ordinal}_status`,
        )
        const external = yield* LlvmBlock.make(
          body,
          `drive${operation.destination.ordinal}_external`,
        )
        const notExternal = yield* LlvmBlock.make(
          body,
          `drive${operation.destination.ordinal}_not_external`,
        )
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            status,
            yield* Constant.integerUnsigned(builder, i32, 2n),
            `drive${operation.destination.ordinal}_is_external`,
          ),
          external,
          notExternal,
        )

        yield* LlvmBlock.setInsertionPoint(body, notExternal)
        const child = yield* LlvmBlock.make(body, `drive${operation.destination.ordinal}_child`)
        const completed = yield* LlvmBlock.make(
          body,
          `drive${operation.destination.ordinal}_completed_step`,
        )
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            status,
            yield* Constant.integerUnsigned(builder, i32, 1n),
            `drive${operation.destination.ordinal}_is_child`,
          ),
          child,
          completed,
        )
        yield* LlvmBlock.setInsertionPoint(body, child)
        const childFunction = yield* FunctionBody.load(
          body,
          pointer,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            transfer,
            0,
            `drive${operation.destination.ordinal}_child_ptr`,
          ),
          `drive${operation.destination.ordinal}_child_fn`,
        )
        const childStatus = yield* FunctionBody.call(
          body,
          childThunkType,
          childFunction,
          [transfer],
          `drive${operation.destination.ordinal}_child_step`,
        )
        if (childStatus === undefined)
          throw new RangeError('LLVM execution child returned no status')
        yield* FunctionBody.store(body, childStatus, statusStorage)
        yield* FunctionBody.branch(body, loop)

        yield* LlvmBlock.setInsertionPoint(body, completed)
        const head = yield* FunctionBody.load(
          body,
          pointer,
          headPointer,
          `drive${operation.destination.ordinal}_head`,
        )
        const finish = yield* LlvmBlock.make(body, `drive${operation.destination.ordinal}_finish`)
        const resumeParent = yield* LlvmBlock.make(
          body,
          `drive${operation.destination.ordinal}_resume_parent`,
        )
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            yield* FunctionBody.cast(
              body,
              'ptrtoint',
              head,
              usizeType,
              `drive${operation.destination.ordinal}_head_address`,
            ),
            yield* Constant.integerUnsigned(builder, usizeType, 0n),
            `drive${operation.destination.ordinal}_at_root`,
          ),
          finish,
          resumeParent,
        )
        yield* LlvmBlock.setInsertionPoint(body, resumeParent)
        const next = yield* FunctionBody.load(
          body,
          pointer,
          head,
          `drive${operation.destination.ordinal}_next_head`,
        )
        yield* FunctionBody.store(body, next, headPointer)
        yield* FunctionBody.store(body, headPointer, appendPointerPointer)
        const parentResume = yield* FunctionBody.load(
          body,
          pointer,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            head,
            program.layout.target.pointerSize,
            `drive${operation.destination.ordinal}_parent_resume_ptr`,
          ),
          `drive${operation.destination.ordinal}_parent_resume`,
        )
        const parentStatus = yield* FunctionBody.call(
          body,
          resumeThunkType,
          parentResume,
          [transfer, head],
          `drive${operation.destination.ordinal}_parent_step`,
        )
        if (parentStatus === undefined)
          throw new RangeError('LLVM execution parent returned no status')
        yield* FunctionBody.store(body, parentStatus, statusStorage)
        yield* FunctionBody.branch(body, loop)

        yield* LlvmBlock.setInsertionPoint(body, finish)
        const outcome: Array<Value.Input> = []
        for (const [ordinal, lane] of packedOutcome.entries.entries())
          outcome.push(
            yield* FunctionBody.load(
              body,
              NativeType.laneType(context.types, lane.lane),
              yield* NativeLanePointer.lanePointer(
                lanePointers,
                body,
                transfer,
                lane.offset,
                `drive${operation.destination.ordinal}_outcome${ordinal}_ptr`,
              ),
              `drive${operation.destination.ordinal}_outcome${ordinal}`,
            ),
          )
        const outcomeTag = outcome.at(0)
        if (outcomeTag === undefined)
          throw new RangeError('LLVM execution body lost its outcome tag')
        const succeeded = yield* LlvmBlock.make(
          body,
          `drive${operation.destination.ordinal}_succeeded`,
        )
        const failed = yield* LlvmBlock.make(body, `drive${operation.destination.ordinal}_failed`)
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            outcomeTag,
            yield* Constant.integerSigned(builder, i32, 0n),
            `drive${operation.destination.ordinal}_success`,
          ),
          succeeded,
          failed,
        )
        yield* LlvmBlock.setInsertionPoint(body, failed)
        yield* FunctionBody.unreachable(body)
        yield* LlvmBlock.setInsertionPoint(body, succeeded)
        const resultValues = Object.freeze(outcome.slice(1))
        storage.locals.set(operation.result.ordinal, resultValues)
        yield* NativeAggregate.dropThroughPlan(
          context.cleanup,
          operation.suspensionCleanup,
          NativeStorage.readLocal(storage, operation.onSuspend),
          `drive${operation.destination.ordinal}_unused_suspend`,
        )
        yield* FunctionBody.store(
          body,
          yield* Constant.integerUnsigned(builder, usizeType, 5n),
          statePointer,
        )
        yield* releasePackage(
          context,
          package_,
          base,
          `drive${operation.destination.ordinal}_complete`,
        )
        storage.locals.set(
          operation.destination.ordinal,
          yield* applyCallable(
            context,
            operation.onComplete,
            operation.completionTypeArguments,
            [...NativeStorage.readLocal(storage, operation.branch), ...resultValues],
            `drive${operation.destination.ordinal}_on_complete`,
          ),
        )
        yield* FunctionBody.branch(body, operationFollowing)

        yield* LlvmBlock.setInsertionPoint(body, external)
        const transferredHead = yield* FunctionBody.load(
          body,
          pointer,
          headPointer,
          `drive${operation.destination.ordinal}_transferred_head`,
        )
        const transferredAppend = yield* FunctionBody.load(
          body,
          pointer,
          appendPointerPointer,
          `drive${operation.destination.ordinal}_transferred_append`,
        )
        yield* FunctionBody.store(
          body,
          transferredHead,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            base,
            continuationOffset,
            `drive${operation.destination.ordinal}_store_head`,
          ),
        )
        yield* FunctionBody.store(
          body,
          transferredAppend,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            base,
            continuationOffset + program.layout.target.pointerSize,
            `drive${operation.destination.ordinal}_store_append`,
          ),
        )
        yield* NativeAggregate.dropThroughPlan(
          context.cleanup,
          operation.completionCleanup,
          NativeStorage.readLocal(storage, operation.onComplete),
          `drive${operation.destination.ordinal}_unused_complete`,
        )
        const suspendedResult = yield* applyCallable(
          context,
          operation.onSuspend,
          operation.suspensionTypeArguments,
          [...NativeStorage.readLocal(storage, operation.branch), base],
          `drive${operation.destination.ordinal}_on_suspend`,
        )
        const controlOffset = componentOffset(package_, 'WakeControl')
        if (controlOffset === undefined) {
          yield* FunctionBody.store(
            body,
            yield* Constant.integerUnsigned(builder, usizeType, 2n),
            statePointer,
          )
        } else {
          const phasePointer = yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            base,
            controlOffset,
            `drive${operation.destination.ordinal}_phase_ptr`,
          )
          const phase = yield* FunctionBody.load(
            body,
            usizeType,
            phasePointer,
            `drive${operation.destination.ordinal}_phase`,
          )
          const following = yield* LlvmBlock.make(
            body,
            `drive${operation.destination.ordinal}_suspended`,
          )
          const destroyedAfterSuspend = yield* LlvmBlock.make(
            body,
            `drive${operation.destination.ordinal}_destroyed_after_suspend`,
          )
          const retainedAfterSuspend = yield* LlvmBlock.make(
            body,
            `drive${operation.destination.ordinal}_retained_after_suspend`,
          )
          const stateAfterSuspend = yield* FunctionBody.load(
            body,
            usizeType,
            statePointer,
            `drive${operation.destination.ordinal}_state_after_suspend`,
          )
          yield* FunctionBody.conditionalBranch(
            body,
            yield* FunctionBody.integerCompare(
              body,
              'eq',
              stateAfterSuspend,
              yield* Constant.integerUnsigned(builder, usizeType, 7n),
              `drive${operation.destination.ordinal}_destroy_pending_after_suspend`,
            ),
            destroyedAfterSuspend,
            retainedAfterSuspend,
          )
          yield* LlvmBlock.setInsertionPoint(body, destroyedAfterSuspend)
          yield* FunctionBody.store(
            body,
            yield* Constant.integerUnsigned(builder, usizeType, 6n),
            phasePointer,
          )
          yield* FunctionBody.store(
            body,
            yield* Constant.integerUnsigned(builder, usizeType, 6n),
            statePointer,
          )
          yield* dropStoredPackage(
            context.cleanup,
            package_,
            base,
            { body: false, endpoints: true, allocation: false },
            `drive${operation.destination.ordinal}_destroyed_after_suspend`,
          )
          yield* dropFrames(
            context.cleanup,
            package_,
            base,
            `drive${operation.destination.ordinal}_destroyed_after_suspend`,
          )
          const releaseDestroyedAllocation = yield* LlvmBlock.make(
            body,
            `drive${operation.destination.ordinal}_release_destroyed_allocation`,
          )
          const retainDestroyedAllocation = yield* LlvmBlock.make(
            body,
            `drive${operation.destination.ordinal}_retain_destroyed_allocation`,
          )
          yield* FunctionBody.conditionalBranch(
            body,
            yield* FunctionBody.integerCompare(
              body,
              'eq',
              phase,
              yield* Constant.integerUnsigned(builder, usizeType, 2n),
              `drive${operation.destination.ordinal}_destroyed_wake_consumed`,
            ),
            releaseDestroyedAllocation,
            retainDestroyedAllocation,
          )
          yield* LlvmBlock.setInsertionPoint(body, releaseDestroyedAllocation)
          yield* releaseAllocation(
            context,
            package_,
            base,
            `drive${operation.destination.ordinal}_destroyed_allocation`,
          )
          yield* FunctionBody.branch(body, following)
          yield* LlvmBlock.setInsertionPoint(body, retainDestroyedAllocation)
          yield* FunctionBody.branch(body, following)

          yield* LlvmBlock.setInsertionPoint(body, retainedAfterSuspend)
          const notify = yield* LlvmBlock.make(body, `drive${operation.destination.ordinal}_notify`)
          const dormant = yield* LlvmBlock.make(
            body,
            `drive${operation.destination.ordinal}_dormant`,
          )
          yield* FunctionBody.conditionalBranch(
            body,
            yield* FunctionBody.integerCompare(
              body,
              'eq',
              phase,
              yield* Constant.integerUnsigned(builder, usizeType, 2n),
              `drive${operation.destination.ordinal}_latched`,
            ),
            notify,
            dormant,
          )
          yield* LlvmBlock.setInsertionPoint(body, dormant)
          yield* FunctionBody.store(
            body,
            yield* Constant.integerUnsigned(builder, usizeType, 3n),
            phasePointer,
          )
          yield* FunctionBody.store(
            body,
            yield* Constant.integerUnsigned(builder, usizeType, 2n),
            statePointer,
          )
          yield* FunctionBody.branch(body, following)
          yield* LlvmBlock.setInsertionPoint(body, notify)
          yield* FunctionBody.store(
            body,
            yield* Constant.integerUnsigned(builder, usizeType, 3n),
            statePointer,
          )
          yield* FunctionBody.store(
            body,
            yield* Constant.integerUnsigned(builder, usizeType, 4n),
            phasePointer,
          )
          yield* notifyReady(context, package_, base, `drive${operation.destination.ordinal}_ready`)
          const stateAfterNotify = yield* FunctionBody.load(
            body,
            usizeType,
            statePointer,
            `drive${operation.destination.ordinal}_state_after_notify`,
          )
          const destroyedAfterNotify = yield* LlvmBlock.make(
            body,
            `drive${operation.destination.ordinal}_destroyed_after_notify`,
          )
          const eligibleAfterNotify = yield* LlvmBlock.make(
            body,
            `drive${operation.destination.ordinal}_eligible_after_notify`,
          )
          yield* FunctionBody.conditionalBranch(
            body,
            yield* FunctionBody.integerCompare(
              body,
              'eq',
              stateAfterNotify,
              yield* Constant.integerUnsigned(builder, usizeType, 7n),
              `drive${operation.destination.ordinal}_destroy_pending_after_notify`,
            ),
            destroyedAfterNotify,
            eligibleAfterNotify,
          )
          yield* LlvmBlock.setInsertionPoint(body, destroyedAfterNotify)
          yield* dropFrames(
            context.cleanup,
            package_,
            base,
            `drive${operation.destination.ordinal}_destroyed_after_notify`,
          )
          yield* releasePackage(
            context,
            package_,
            base,
            `drive${operation.destination.ordinal}_destroyed_after_notify`,
          )
          yield* FunctionBody.branch(body, following)
          yield* LlvmBlock.setInsertionPoint(body, eligibleAfterNotify)
          yield* FunctionBody.store(
            body,
            yield* Constant.integerUnsigned(builder, usizeType, 5n),
            phasePointer,
          )
          yield* FunctionBody.store(
            body,
            yield* Constant.integerUnsigned(builder, usizeType, 4n),
            statePointer,
          )
          yield* FunctionBody.branch(body, following)
          yield* LlvmBlock.setInsertionPoint(body, following)
        }
        storage.locals.set(operation.destination.ordinal, suspendedResult)
        yield* FunctionBody.branch(body, operationFollowing)
        yield* LlvmBlock.setInsertionPoint(body, operationFollowing)
      })
      if (matchingPackages.length === 1) {
        const selected = matchingPackages.at(0)
        if (selected === undefined) throw new RangeError('LLVM execution drive lost its package')
        yield* emitPackage(selected)
        return
      }
      yield* selectDrivePackage(
        context.cleanup,
        base,
        matchingPackages,
        `drive${operation.destination.ordinal}`,
        emitPackage,
      )
      return
    }
    case 'ExecutionWake': {
      const packages = program.layout.executionPackages.plans.filter(
        (candidate) => candidate.readinessStorage,
      )
      const base = NativeStorage.readLocal(storage, operation.wake).at(0)
      if (packages.length === 0 || base === undefined)
        throw new RangeError('LLVM Wake lost its exact package authority')
      const emitPackage = Effect.fnUntraced(function* (package_: ExecutionPackage.Plan) {
        const controlOffset = componentOffset(package_, 'WakeControl')
        if (controlOffset === undefined)
          throw new RangeError('LLVM Wake package lost its control authority')
        const phasePointer = yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          base,
          controlOffset,
          `wake${operation.destination.ordinal}_phase_ptr`,
        )
        const statePointer = yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          base,
          0,
          `wake${operation.destination.ordinal}_state_ptr`,
        )
        const phase = yield* FunctionBody.load(
          body,
          usizeType,
          phasePointer,
          `wake${operation.destination.ordinal}_phase`,
        )
        const registering = yield* LlvmBlock.make(
          body,
          `wake${operation.destination.ordinal}_registering`,
        )
        const notRegistering = yield* LlvmBlock.make(
          body,
          `wake${operation.destination.ordinal}_not_registering`,
        )
        const following = yield* LlvmBlock.make(
          body,
          `wake${operation.destination.ordinal}_following`,
        )
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            phase,
            yield* Constant.integerUnsigned(builder, usizeType, 1n),
            `wake${operation.destination.ordinal}_is_registering`,
          ),
          registering,
          notRegistering,
        )
        yield* LlvmBlock.setInsertionPoint(body, registering)
        yield* FunctionBody.store(
          body,
          yield* Constant.integerUnsigned(builder, usizeType, 2n),
          phasePointer,
        )
        yield* FunctionBody.branch(body, following)

        yield* LlvmBlock.setInsertionPoint(body, notRegistering)
        const dormant = yield* LlvmBlock.make(body, `wake${operation.destination.ordinal}_dormant`)
        const notDormant = yield* LlvmBlock.make(
          body,
          `wake${operation.destination.ordinal}_not_dormant`,
        )
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            phase,
            yield* Constant.integerUnsigned(builder, usizeType, 3n),
            `wake${operation.destination.ordinal}_is_dormant`,
          ),
          dormant,
          notDormant,
        )
        yield* LlvmBlock.setInsertionPoint(body, dormant)
        yield* FunctionBody.store(
          body,
          yield* Constant.integerUnsigned(builder, usizeType, 3n),
          statePointer,
        )
        yield* FunctionBody.store(
          body,
          yield* Constant.integerUnsigned(builder, usizeType, 4n),
          phasePointer,
        )
        yield* notifyReady(context, package_, base, `wake${operation.destination.ordinal}_ready`)
        const stateAfterNotify = yield* FunctionBody.load(
          body,
          usizeType,
          statePointer,
          `wake${operation.destination.ordinal}_state_after_notify`,
        )
        const destroyed = yield* LlvmBlock.make(
          body,
          `wake${operation.destination.ordinal}_destroyed`,
        )
        const eligibleBlock = yield* LlvmBlock.make(
          body,
          `wake${operation.destination.ordinal}_eligible`,
        )
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            stateAfterNotify,
            yield* Constant.integerUnsigned(builder, usizeType, 7n),
            `wake${operation.destination.ordinal}_destroy_pending`,
          ),
          destroyed,
          eligibleBlock,
        )
        yield* LlvmBlock.setInsertionPoint(body, destroyed)
        yield* dropFrames(
          context.cleanup,
          package_,
          base,
          `wake${operation.destination.ordinal}_destroy`,
        )
        yield* releasePackage(
          context,
          package_,
          base,
          `wake${operation.destination.ordinal}_destroy`,
        )
        yield* FunctionBody.branch(body, following)
        yield* LlvmBlock.setInsertionPoint(body, eligibleBlock)
        yield* FunctionBody.store(
          body,
          yield* Constant.integerUnsigned(builder, usizeType, 5n),
          phasePointer,
        )
        yield* FunctionBody.store(
          body,
          yield* Constant.integerUnsigned(builder, usizeType, 4n),
          statePointer,
        )
        yield* FunctionBody.branch(body, following)

        yield* LlvmBlock.setInsertionPoint(body, notDormant)
        const cancelled = yield* LlvmBlock.make(
          body,
          `wake${operation.destination.ordinal}_cancelled`,
        )
        const invalid = yield* LlvmBlock.make(body, `wake${operation.destination.ordinal}_invalid`)
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            phase,
            yield* Constant.integerUnsigned(builder, usizeType, 6n),
            `wake${operation.destination.ordinal}_is_cancelled`,
          ),
          cancelled,
          invalid,
        )
        yield* LlvmBlock.setInsertionPoint(body, cancelled)
        yield* releaseAllocation(
          context,
          package_,
          base,
          `wake${operation.destination.ordinal}_late_release`,
        )
        yield* FunctionBody.branch(body, following)
        yield* LlvmBlock.setInsertionPoint(body, invalid)
        yield* FunctionBody.unreachable(body)
        yield* LlvmBlock.setInsertionPoint(body, following)
      })
      if (packages.length === 1) {
        const selected = packages.at(0)
        if (selected === undefined) throw new RangeError('LLVM Wake lost its package')
        yield* emitPackage(selected)
      } else {
        yield* selectDrivePackage(
          context.cleanup,
          base,
          packages,
          `wake${operation.destination.ordinal}`,
          emitPackage,
        )
      }
      storage.locals.set(operation.destination.ordinal, Object.freeze([]))
      return
    }
  }
})
