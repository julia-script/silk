import * as Alignment from '@silklang/llvm/Alignment'
import * as LlvmBlock from '@silklang/llvm/Block'
import type * as Builder from '@silklang/llvm/Builder'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionActor from '@silklang/llvm/Function'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import type * as LlvmType from '@silklang/llvm/Type'
import * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import { suspensionPointKey } from './Backend.js'
import type * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import * as NativeCall from './NativeCall.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import type * as NativeLoweringContext from './NativeLoweringContext.js'
import * as NativeStorage from './NativeStorage.js'
import * as NativeType from './NativeType.js'

export interface ReturnContext {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly i32: LlvmType.Type
  readonly pointer: LlvmType.Type
  readonly entry: NativeLoweringContext.DeclaredFunction
  readonly invocationFrameStorage?: Value.Input
  readonly coroutineFramePop?: FunctionActor.Function
  readonly types: NativeType.LoweringContext
}

/** Emits one suspension-step ABI return and releases a completed invocation frame. */
export const returnStep = Effect.fnUntraced(function* (
  context: ReturnContext,
  status: bigint,
  values: ReadonlyArray<Value.Input>,
  tag: string,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  if (!context.entry.suspendable)
    throw new RangeError('LLVM synchronous function cannot return a suspension step')
  if (status === 0n && context.invocationFrameStorage !== undefined) {
    if (context.coroutineFramePop === undefined)
      throw new RangeError('LLVM suspension step lost private frame release')
    yield* FunctionBody.callDirect(
      context.body,
      context.coroutineFramePop,
      [
        yield* FunctionBody.load(
          context.body,
          context.pointer,
          context.invocationFrameStorage,
          `${tag}_invocation_frame`,
        ),
      ],
      `${tag}_release_invocation_frame`,
    )
  }
  const padded = [...values]
  const resultLanes = NativeType.lanesFor(context.types, context.entry.fn.result)
  while (padded.length < resultLanes.length) {
    const lane = resultLanes.at(padded.length)
    if (lane === undefined) break
    padded.push(
      yield* Constant.nullValue(context.builder, NativeType.laneType(context.types, lane)),
    )
  }
  yield* FunctionBody.returnValue(
    context.body,
    yield* FunctionBody.buildAggregate(
      context.body,
      context.entry.emittedResultType,
      Object.freeze([
        yield* Constant.integerUnsigned(context.builder, context.i32, status),
        ...padded.slice(0, resultLanes.length),
      ]),
      tag,
    ),
  )
})

/** Flattens the logical calling lanes retained across one suspension boundary. */
export const logicalLanes = (
  fn: Mir.MirFunction,
  locals: ReadonlyArray<Mir.LocalId>,
  types: NativeType.LoweringContext,
): ReadonlyArray<Layout.CallingLane> =>
  Object.freeze(
    locals.flatMap((local) => {
      const type = fn.localTypes.at(local.ordinal)
      if (type === undefined) throw new RangeError(`LLVM suspension lost local %${local.ordinal}`)
      return NativeType.lanesFor(types, type)
    }),
  )

export interface ThunkContext {
  readonly builder: Builder.Builder
  readonly program: Mir.Module
  readonly i8: LlvmType.Type
  readonly i32: LlvmType.Type
  readonly pointer: LlvmType.Type
  readonly usizeType?: LlvmType.Type
  readonly lanePointers: NativeLanePointer.Context
  readonly declared: ReadonlyArray<NativeLoweringContext.DeclaredFunction>
  readonly originThunks: ReadonlyMap<
    string,
    {
      readonly handle: FunctionActor.Function
      readonly region: Mir.SuspendEffectRegion
      readonly owner: NativeLoweringContext.DeclaredFunction
    }
  >
  readonly resumeThunks: ReadonlyMap<
    string,
    {
      readonly handle: FunctionActor.Function
      readonly region: Mir.RunSuspendableEffectRegion
      readonly owner: NativeLoweringContext.DeclaredFunction
      readonly frame: Mir.CoroutineFrameTargetLayout
      readonly layout: Mir.CoroutineFrameTargetStateLayout
    }
  >
  readonly types: NativeType.LoweringContext
  readonly transferHeaderSize: number
  readonly transferResultOffset: number
  readonly transferStorageSize: number
  readonly driver?: FunctionActor.Function
  readonly machine?: NativeLoweringContext.DeclaredFunction
  readonly childThunkType?: LlvmType.Type
  readonly resumeThunkType?: LlvmType.Type
}

/** Emits child, resume, and machine-driver suspension thunks. */
export const emitThunks = Effect.fnUntraced(function* (context: ThunkContext) {
  const {
    builder,
    program,
    i8,
    i32,
    pointer,
    usizeType,
    lanePointers,
    declared,
    originThunks,
    resumeThunks,
    types,
    transferHeaderSize,
    transferResultOffset,
    transferStorageSize,
    driver,
    machine,
    childThunkType,
    resumeThunkType,
  } = context
  for (const origin of originThunks.values()) {
    yield* FunctionActor.buildBody(
      builder,
      origin.handle,
      Effect.fnUntraced(function* (body) {
        yield* LlvmBlock.make(body, 'entry')
        const transfer = yield* Value.argument(body, 0)
        const target = declared.find((candidate) =>
          origin.region.deferred.instance !== undefined
            ? Mir.matchesInstanceKey(candidate.fn, origin.region.deferred.instance)
            : origin.region.deferred.declaration !== undefined &&
              Mir.matchesInstance(
                candidate.fn,
                origin.region.deferred.declaration,
                origin.region.deferred.typeArguments,
              ),
        )
        if (target === undefined) throw new RangeError('LLVM child thunk lost deferred runner')
        const argumentLanes = logicalLanes(
          origin.owner.fn,
          NativeCall.operationInputs(origin.region.operation),
          types,
        )
        const packed = NativeType.packLanes(program.layout.target, argumentLanes)
        const arguments_: Array<Value.Input> = []
        for (const [ordinal, lane] of packed.entries.entries()) {
          arguments_.push(
            yield* FunctionBody.load(
              body,
              NativeType.laneType(types, lane.lane),
              yield* NativeLanePointer.lanePointer(
                lanePointers,
                body,
                transfer,
                transferHeaderSize + lane.offset,
                `child_argument${ordinal}_ptr`,
              ),
              `child_argument${ordinal}`,
            ),
          )
        }
        const result = yield* FunctionBody.callDirect(
          body,
          target.handle,
          target.suspendable
            ? [
                ...arguments_,
                transfer,
                yield* Constant.nullValue(builder, pointer),
                yield* Constant.integerUnsigned(builder, i32, 0n),
              ]
            : arguments_,
          'child_step',
        )
        if (target.resultLaneCount > 0 && result === undefined)
          throw new RangeError('LLVM child thunk lost result')
        let status: Value.Input | undefined
        if (target.suspendable) {
          if (result === undefined) {
            status = undefined
          } else {
            status = yield* FunctionBody.extractValue(body, result, [0], 'child_status')
          }
        } else {
          status = yield* Constant.integerUnsigned(builder, i32, 0n)
        }
        if (status === undefined) throw new RangeError('LLVM child thunk lost status')
        const resultLanes = NativeType.lanesFor(types, target.fn.result)
        const resultPacked = NativeType.packLanes(
          program.layout.target,
          resultLanes,
          transferResultOffset,
        )
        for (const [ordinal, lane] of resultPacked.entries.entries()) {
          let value: Value.Input | undefined
          if (resultLanes.length === 1 && !target.suspendable) {
            value = result
          } else if (result === undefined) {
            value = undefined
          } else {
            value = yield* FunctionBody.extractValue(
              body,
              result,
              [target.suspendable ? ordinal + 1 : ordinal],
              `child_result${ordinal}`,
            )
          }
          if (value === undefined) throw new RangeError('LLVM child thunk lost result lane')
          yield* FunctionBody.store(
            body,
            yield* FunctionBody.freeze(body, value, `child_result${ordinal}_stable`),
            yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              transfer,
              lane.offset,
              `child_result${ordinal}_ptr`,
            ),
          )
        }
        yield* FunctionBody.returnValue(body, status)
      }),
    )
  }

  for (const resume of resumeThunks.values()) {
    yield* FunctionActor.buildBody(
      builder,
      resume.handle,
      Effect.fnUntraced(function* (body) {
        yield* LlvmBlock.make(body, 'entry')
        const transfer = yield* Value.argument(body, 0)
        const frame = yield* Value.argument(body, 1)
        const ordinal = [...resumeThunks.values()]
          .filter((candidate) => candidate.owner === resume.owner)
          .sort((left, right) =>
            suspensionPointKey(left.region.point).localeCompare(
              suspensionPointKey(right.region.point),
            ),
          )
          .indexOf(resume)
        if (ordinal < 0) throw new RangeError('LLVM resume thunk lost dispatch identity')
        const parameters = resume.owner.fn.localTypes
          .slice(0, resume.owner.fn.parameterCount)
          .flatMap((type) => NativeType.lanesFor(types, type))
        const result = yield* FunctionBody.callDirect(
          body,
          resume.owner.handle,
          [
            ...(yield* Effect.forEach(parameters, (lane) =>
              Constant.nullValue(builder, NativeType.laneType(types, lane)),
            )),
            transfer,
            frame,
            yield* Constant.integerUnsigned(builder, i32, BigInt(ordinal + 1)),
          ],
          'resume_step',
        )
        if (result === undefined) throw new RangeError('LLVM resume thunk lost step result')
        const status = yield* FunctionBody.extractValue(body, result, [0], 'resume_status')
        const resultPacked = NativeType.packLanes(
          program.layout.target,
          NativeType.lanesFor(types, resume.owner.fn.result),
          transferResultOffset,
        )
        for (const [laneOrdinal, lane] of resultPacked.entries.entries()) {
          const value = yield* FunctionBody.extractValue(
            body,
            result,
            [laneOrdinal + 1],
            `resume_result${laneOrdinal}`,
          )
          yield* FunctionBody.store(
            body,
            yield* FunctionBody.freeze(body, value, `resume_result${laneOrdinal}_stable`),
            yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              transfer,
              lane.offset,
              `resume_result${laneOrdinal}_ptr`,
            ),
          )
        }
        yield* FunctionBody.returnValue(body, status)
      }),
    )
  }

  if (driver !== undefined && machine !== undefined) {
    yield* FunctionActor.buildBody(
      builder,
      driver,
      Effect.fnUntraced(function* (body) {
        yield* LlvmBlock.make(body, 'entry')
        const arguments_: Array<Value.Input> = []
        for (let ordinal = 0; ordinal < machine.parameterTypes.length - 3; ordinal += 1)
          arguments_.push(yield* Value.argument(body, ordinal))
        const transfer = yield* FunctionBody.alloca(body, i8, 'suspend_transfer', {
          count: yield* Constant.integerUnsigned(
            builder,
            i32,
            BigInt(Math.max(transferStorageSize, 1)),
          ),
          alignment: yield* Alignment.fromByteUnits(program.layout.target.pointerAlignment),
        })
        const nullPointer = yield* Constant.nullValue(builder, pointer)
        yield* FunctionBody.store(
          body,
          nullPointer,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            transfer,
            program.layout.target.pointerSize,
            'suspend_initial_head_ptr',
          ),
        )
        const initial = yield* FunctionBody.callDirect(
          body,
          machine.handle,
          [...arguments_, transfer, nullPointer, yield* Constant.integerUnsigned(builder, i32, 0n)],
          'suspend_initial',
        )
        if (initial === undefined) throw new RangeError('LLVM suspension driver lost initial step')
        const initialStatus = yield* FunctionBody.extractValue(
          body,
          initial,
          [0],
          'suspend_initial_status',
        )
        const initialComplete = yield* LlvmBlock.make(body, 'suspend_initial_complete')
        const drive = yield* LlvmBlock.make(body, 'suspend_drive')
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            initialStatus,
            yield* Constant.integerUnsigned(builder, i32, 0n),
            'suspend_initial_done',
          ),
          initialComplete,
          drive,
        )
        const returnMachineResult = Effect.fnUntraced(function* (
          values: ReadonlyArray<Value.Input>,
          tag: string,
        ) {
          if (machine.resultLaneCount === 0) return yield* FunctionBody.returnVoid(body)
          if (machine.resultLaneCount === 1) {
            const value = values.at(0)
            if (value === undefined) throw new RangeError('LLVM driver lost scalar result')
            return yield* FunctionBody.returnValue(body, value)
          }
          return yield* FunctionBody.returnValue(
            body,
            yield* FunctionBody.buildAggregate(body, machine.resultType, values, tag),
          )
        })
        yield* LlvmBlock.setInsertionPoint(body, initialComplete)
        const initialValues: Array<Value.Input> = []
        for (let ordinal = 0; ordinal < machine.resultLaneCount; ordinal += 1)
          initialValues.push(
            yield* FunctionBody.extractValue(
              body,
              initial,
              [ordinal + 1],
              `suspend_initial_result${ordinal}`,
            ),
          )
        yield* returnMachineResult(Object.freeze(initialValues), 'suspend_initial_result')
        yield* LlvmBlock.setInsertionPoint(body, drive)
        const child = yield* FunctionBody.load(
          body,
          pointer,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            transfer,
            0,
            'suspend_child_ptr',
          ),
          'suspend_child',
        )
        if (childThunkType === undefined || resumeThunkType === undefined)
          throw new RangeError('LLVM driver lost private thunk types')
        const childStatus = yield* FunctionBody.call(
          body,
          childThunkType,
          child,
          [transfer],
          'suspend_child_status',
        )
        if (childStatus === undefined) throw new RangeError('LLVM driver child returned void')
        const childTransferred = yield* LlvmBlock.make(body, 'suspend_child_transferred')
        const childCompleted = yield* LlvmBlock.make(body, 'suspend_child_completed')
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            childStatus,
            yield* Constant.integerUnsigned(builder, i32, 0n),
            'suspend_child_done',
          ),
          childCompleted,
          childTransferred,
        )
        yield* LlvmBlock.setInsertionPoint(body, childTransferred)
        yield* FunctionBody.branch(body, drive)
        yield* LlvmBlock.setInsertionPoint(body, childCompleted)
        const parentPointer = yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          transfer,
          program.layout.target.pointerSize,
          'suspend_parent_ptr',
        )
        const parent = yield* FunctionBody.load(body, pointer, parentPointer, 'suspend_parent')
        const finish = yield* LlvmBlock.make(body, 'suspend_finish')
        const resumeParent = yield* LlvmBlock.make(body, 'suspend_resume_parent')
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            yield* FunctionBody.cast(
              body,
              'ptrtoint',
              parent,
              usizeType ?? i32,
              'suspend_parent_addr',
            ),
            yield* Constant.integerUnsigned(builder, usizeType ?? i32, 0n),
            'suspend_has_no_parent',
          ),
          finish,
          resumeParent,
        )
        yield* LlvmBlock.setInsertionPoint(body, resumeParent)
        const nextParent = yield* FunctionBody.load(
          body,
          pointer,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            parent,
            0,
            'suspend_next_parent_ptr',
          ),
          'suspend_next_parent',
        )
        yield* FunctionBody.store(body, nextParent, parentPointer)
        const appendPointerPointer = yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          transfer,
          program.layout.target.pointerSize * 2,
          'suspend_append_ptr_ptr',
        )
        yield* FunctionBody.store(body, parentPointer, appendPointerPointer)
        const resumeFunction = yield* FunctionBody.load(
          body,
          pointer,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            parent,
            program.layout.target.pointerSize,
            'suspend_resume_ptr',
          ),
          'suspend_resume',
        )
        const resumedStatus = yield* FunctionBody.call(
          body,
          resumeThunkType,
          resumeFunction,
          [transfer, parent],
          'suspend_resumed_status',
        )
        if (resumedStatus === undefined) throw new RangeError('LLVM resume returned void')
        const resumeTransferred = yield* LlvmBlock.make(body, 'suspend_resume_transferred')
        const resumeCompleted = yield* LlvmBlock.make(body, 'suspend_resume_completed')
        yield* FunctionBody.conditionalBranch(
          body,
          yield* FunctionBody.integerCompare(
            body,
            'eq',
            resumedStatus,
            yield* Constant.integerUnsigned(builder, i32, 0n),
            'suspend_resume_done',
          ),
          resumeCompleted,
          resumeTransferred,
        )
        yield* LlvmBlock.setInsertionPoint(body, resumeTransferred)
        yield* FunctionBody.branch(body, drive)
        yield* LlvmBlock.setInsertionPoint(body, resumeCompleted)
        yield* FunctionBody.branch(body, childCompleted)
        yield* LlvmBlock.setInsertionPoint(body, finish)
        const finalValues: Array<Value.Input> = []
        const finalPacked = NativeType.packLanes(
          program.layout.target,
          NativeType.lanesFor(types, machine.fn.result),
          transferResultOffset,
        )
        for (const [ordinal, lane] of finalPacked.entries.entries())
          finalValues.push(
            yield* FunctionBody.load(
              body,
              NativeType.laneType(types, lane.lane),
              yield* NativeLanePointer.lanePointer(
                lanePointers,
                body,
                transfer,
                lane.offset,
                `suspend_final_result${ordinal}_ptr`,
              ),
              `suspend_final_result${ordinal}`,
            ),
          )
        yield* returnMachineResult(Object.freeze(finalValues), 'suspend_final_result')
      }),
    )
  }
})

export interface OperationContext {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly program: Mir.Module
  readonly i8: LlvmType.Type
  readonly i32: LlvmType.Type
  readonly pointer: LlvmType.Type
  readonly entry: NativeLoweringContext.DeclaredFunction
  readonly lanePointers: NativeLanePointer.Context
  readonly types: NativeType.LoweringContext
  readonly transferHeaderSize: number
  readonly transferResultOffset: number
  readonly transferPointer?: Value.Input
  readonly resumeFrame?: Value.Input
  readonly originThunks: ReadonlyMap<string, { readonly handle: FunctionActor.Function }>
  readonly resumeThunks: ReadonlyMap<
    string,
    {
      readonly handle: FunctionActor.Function
      readonly layout: Mir.CoroutineFrameTargetStateLayout
    }
  >
  readonly suspensionRegions: ReadonlyMap<Mir.Operation, Mir.SuspensionRegion>
  readonly resumeBlocks: ReadonlyMap<string, LlvmBlock.Block>
  readonly storage: NativeStorage.Context
  readonly returns: ReturnContext
}

const storeMutable = Effect.fnUntraced(function* (
  context: OperationContext,
  root: Mir.LocalId,
  values: ReadonlyArray<Value.Input>,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  yield* NativeStorage.storeMutable(context.storage, root, values)
})

/** Restores every retained relay payload lane at its verified resume label. */
export const restoreRelayPayload = Effect.fnUntraced(function* (
  context: OperationContext,
  region: Mir.RunSuspendableEffectRegion,
  name: string,
) {
  const { body, entry, lanePointers, program, resumeFrame, resumeThunks, storage, types } = context
  if (resumeFrame === undefined) throw new RangeError('LLVM relay restore lost its frame argument')
  const generated = resumeThunks.get(suspensionPointKey(region.point))
  if (generated === undefined) throw new RangeError('LLVM relay restore lost generated control')
  for (const field of generated.layout.payload) {
    const type = entry.fn.localTypes.at(field.local.ordinal)
    const targets = storage.mutableStorage.get(field.local.ordinal)
    if (type === undefined || targets === undefined)
      throw new RangeError('LLVM relay payload has no mutable restore storage')
    const packed = NativeType.packLanes(
      program.layout.target,
      NativeType.lanesFor(types, type),
      field.offset,
    )
    const values: Array<Value.Input> = []
    for (const [ordinal, lane] of packed.entries.entries()) {
      const target = targets.at(ordinal)
      if (target === undefined) throw new RangeError('LLVM relay restore lost a payload lane')
      const value = yield* FunctionBody.load(
        body,
        NativeType.laneType(types, lane.lane),
        yield* NativeLanePointer.lanePointer(
          lanePointers,
          body,
          resumeFrame,
          lane.offset,
          `${name}_restore${field.slot}_${ordinal}_ptr`,
        ),
        `${name}_restore${field.slot}_${ordinal}`,
      )
      yield* FunctionBody.store(body, value, target)
      values.push(value)
    }
    storage.locals.set(field.local.ordinal, Object.freeze(values))
  }
})

const originateTransfer = Effect.fnUntraced(function* (
  context: OperationContext,
  region: Mir.SuspendEffectRegion,
  arguments_: ReadonlyArray<Value.Input>,
  name: string,
) {
  const {
    builder,
    body,
    entry,
    lanePointers,
    types,
    originThunks,
    program,
    transferHeaderSize,
    transferPointer,
  } = context
  if (transferPointer === undefined)
    throw new RangeError('LLVM suspension origin lost transfer storage')
  const generated = originThunks.get(suspensionPointKey(region.point))
  if (generated === undefined) throw new RangeError('LLVM suspension origin lost thunk')
  yield* FunctionBody.store(
    body,
    yield* Constant.fromGlobal(builder, yield* FunctionActor.global(builder, generated.handle)),
    yield* NativeLanePointer.lanePointer(lanePointers, body, transferPointer, 0, `${name}_child`),
  )
  yield* FunctionBody.store(
    body,
    yield* NativeLanePointer.lanePointer(
      lanePointers,
      body,
      transferPointer,
      program.layout.target.pointerSize,
      `${name}_head`,
    ),
    yield* NativeLanePointer.lanePointer(
      lanePointers,
      body,
      transferPointer,
      program.layout.target.pointerSize * 2,
      `${name}_append_ptr`,
    ),
  )
  const packed = NativeType.packLanes(
    program.layout.target,
    logicalLanes(entry.fn, NativeCall.operationInputs(region.operation), types),
  )
  if (packed.entries.length !== arguments_.length)
    throw new RangeError('LLVM suspension origin argument shape disagrees with its thunk')
  for (const [ordinal, lane] of packed.entries.entries()) {
    const value = arguments_.at(ordinal)
    if (value === undefined) throw new RangeError('LLVM suspension origin lost argument')
    yield* FunctionBody.store(
      body,
      value,
      yield* NativeLanePointer.lanePointer(
        lanePointers,
        body,
        transferPointer,
        transferHeaderSize + lane.offset,
        `${name}_argument${ordinal}`,
      ),
    )
  }
  yield* returnStep(context.returns, 1n, Object.freeze([]), `${name}_originated`)
})

export const emitOrigin = Effect.fnUntraced(function* (
  context: OperationContext,
  operation: Extract<
    Mir.Operation,
    { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'CatchEffect' }
  >,
  arguments_: ReadonlyArray<Value.Input>,
  name: string,
) {
  const { body, builder, storage: nativeStorage, suspensionRegions, types } = context
  const suspension = suspensionRegions.get(operation)
  if (suspension?._tag !== 'SuspendEffectRegion') return false
  yield* originateTransfer(context, suspension, arguments_, name)
  yield* LlvmBlock.setInsertionPoint(
    body,
    yield* LlvmBlock.make(body, `${name}_unreachable_continuation`),
  )
  const outcomeValues = Object.freeze(
    yield* Effect.forEach(NativeType.lanesFor(types, operation.outcomeType), (lane) =>
      Constant.nullValue(builder, NativeType.laneType(types, lane)),
    ),
  )
  const destinationValues = Object.freeze(
    yield* Effect.forEach(NativeType.lanesFor(types, operation.type), (lane) =>
      Constant.nullValue(builder, NativeType.laneType(types, lane)),
    ),
  )
  nativeStorage.locals.set(operation.outcome.ordinal, outcomeValues)
  nativeStorage.locals.set(operation.destination.ordinal, destinationValues)
  return true
})

export const joinOutcome = Effect.fnUntraced(function* (
  context: OperationContext,
  operation: Extract<
    Mir.Operation,
    { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'CatchEffect' }
  >,
  completedValues: ReadonlyArray<Value.Input>,
  name: string,
) {
  const {
    body,
    builder,
    entry,
    i8,
    i32,
    lanePointers,
    program,
    resumeBlocks,
    resumeFrame,
    resumeThunks,
    suspensionRegions,
    storage: nativeStorage,
    transferPointer,
    transferResultOffset,
    types,
  } = context
  const suspension = suspensionRegions.get(operation)
  const descriptor =
    suspension?._tag === 'RunSuspendableEffectRegion' ? suspension.relay.state : undefined
  if (descriptor === undefined) return completedValues
  if (transferPointer === undefined || resumeFrame === undefined)
    throw new RangeError('LLVM coroutine resume lost private arguments')
  const generated = resumeThunks.get(suspensionPointKey(descriptor.point))
  const resumeBlock = resumeBlocks.get(suspensionPointKey(descriptor.point))
  if (generated === undefined || resumeBlock === undefined)
    throw new RangeError('LLVM coroutine resume lost generated control')
  yield* storeMutable(context, operation.outcome, completedValues)
  const following = yield* LlvmBlock.make(body, `${name}_joined`)
  yield* FunctionBody.branch(body, following)
  yield* LlvmBlock.setInsertionPoint(body, resumeBlock)
  for (const field of generated.layout.payload) {
    const type = entry.fn.localTypes.at(field.local.ordinal)
    const storage = nativeStorage.mutableStorage.get(field.local.ordinal)
    if (type === undefined || storage === undefined)
      throw new RangeError('LLVM coroutine payload has no mutable restore storage')
    const packed = NativeType.packLanes(
      program.layout.target,
      NativeType.lanesFor(types, type),
      field.offset,
    )
    for (const [ordinal, lane] of packed.entries.entries()) {
      const target = storage.at(ordinal)
      if (target === undefined) throw new RangeError('LLVM restore lost payload lane')
      yield* FunctionBody.store(
        body,
        yield* FunctionBody.load(
          body,
          NativeType.laneType(types, lane.lane),
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            resumeFrame,
            lane.offset,
            `${name}_restore${field.slot}_${ordinal}`,
          ),
          `${name}_restored${field.slot}_${ordinal}`,
        ),
        target,
      )
    }
  }
  const outcomeStorage = nativeStorage.mutableStorage.get(operation.outcome.ordinal)
  if (outcomeStorage === undefined)
    throw new RangeError('LLVM coroutine outcome has no restore storage')
  const outcomePacked = NativeType.packLanes(
    program.layout.target,
    NativeType.lanesFor(types, operation.outcomeType),
    transferResultOffset,
  )
  for (const [ordinal, lane] of outcomePacked.entries.entries()) {
    const target = outcomeStorage.at(ordinal)
    if (target === undefined) throw new RangeError('LLVM resume lost outcome lane')
    yield* FunctionBody.store(
      body,
      yield* FunctionBody.load(
        body,
        NativeType.laneType(types, lane.lane),
        yield* FunctionBody.getElementPtr(
          body,
          i8,
          transferPointer,
          [yield* Constant.integerUnsigned(builder, i32, BigInt(lane.offset))],
          `${name}_resume_outcome${ordinal}_ptr`,
        ),
        `${name}_resume_outcome${ordinal}`,
      ),
      target,
    )
  }
  yield* FunctionBody.branch(body, following)
  yield* LlvmBlock.setInsertionPoint(body, following)
  for (const field of generated.layout.payload) {
    const type = entry.fn.localTypes.at(field.local.ordinal)
    const storage = nativeStorage.mutableStorage.get(field.local.ordinal)
    if (type === undefined || storage === undefined)
      throw new RangeError('LLVM joined continuation lost payload storage')
    const values: Array<Value.Input> = []
    for (const [ordinal, lane] of NativeType.lanesFor(types, type).entries()) {
      const source = storage.at(ordinal)
      if (source === undefined) throw new RangeError('LLVM joined payload lost lane')
      values.push(
        yield* FunctionBody.load(
          body,
          NativeType.laneType(types, lane),
          source,
          `${name}_joined_payload${field.slot}_${ordinal}`,
        ),
      )
    }
    nativeStorage.locals.set(field.local.ordinal, Object.freeze(values))
  }
  const joined: Array<Value.Input> = []
  for (const [ordinal, lane] of NativeType.lanesFor(types, operation.outcomeType).entries()) {
    const source = outcomeStorage.at(ordinal)
    if (source === undefined) throw new RangeError('LLVM joined outcome lost storage')
    joined.push(
      yield* FunctionBody.load(
        body,
        NativeType.laneType(types, lane),
        source,
        `${name}_joined${ordinal}`,
      ),
    )
  }
  return Object.freeze(joined)
})
