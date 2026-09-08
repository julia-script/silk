import type * as Builder from '@silklang/llvm/Builder'
import * as Block from '@silklang/llvm/Block'
import * as Constant from '@silklang/llvm/Constant'
import type * as FunctionActor from '@silklang/llvm/Function'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as Intrinsic from '@silklang/llvm/Intrinsic'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import type * as LlvmType from '@silklang/llvm/Type'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as Mir from './Mir.js'
import type * as NativeLoweringContext from './NativeLoweringContext.js'

/** Source function handles for the selected, already verified storage contract. */
export interface NativeExecutionStorage {
  readonly diagnosticCauseType?: LlvmType.Type
  readonly create: FunctionActor.Function
  readonly acquire: FunctionActor.Function
  readonly release: FunctionActor.Function
  readonly destroy: FunctionActor.Function
}

/** Binds source implementations directly, without knowing provider module or symbol names. */
export const make = (
  program: Mir.Module,
  declared: ReadonlyArray<NativeLoweringContext.DeclaredFunction>,
): NativeExecutionStorage | undefined => {
  const selected = program.executionStorage
  if (selected === undefined) return undefined
  const handle = (
    operation: 'create' | 'acquire' | 'release' | 'destroy',
  ): FunctionActor.Function => {
    const fn = declared.find((entry) => Mir.matchesInstanceKey(entry.fn, selected[operation].key))
    if (fn === undefined || fn.suspendable)
      throw new RangeError('Selected execution storage lost its synchronous source implementation')
    return fn.handle
  }
  const create = declared.find((entry) => Mir.matchesInstanceKey(entry.fn, selected.create.key))
  const diagnosticCauseType =
    create?.diagnosticParameter === undefined
      ? undefined
      : create.parameterTypes.at(create.diagnosticParameter + 1)
  if (Mir.hasDiagnosticObservation(program) && diagnosticCauseType === undefined)
    throw new RangeError('Storage bootstrap lost its diagnostic cause signature')
  return Object.freeze({
    ...(diagnosticCauseType === undefined ? {} : { diagnosticCauseType }),
    create: handle('create'),
    acquire: handle('acquire'),
    release: handle('release'),
    destroy: handle('destroy'),
  })
}

/** The fifth transfer word carries the invocation's storage state. */
export const stateOffset = (pointerSize: number): number => pointerSize * 4

export interface Context {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly pointer: LlvmType.Type
  readonly usizeType: LlvmType.Type
  readonly storage: NativeExecutionStorage
}

/** Storage bootstrap calls disable observation to avoid invoking a storage-dependent observer. */
export const invoke = Effect.fnUntraced(function* (
  context: Pick<Context, 'builder' | 'body' | 'pointer' | 'storage'>,
  operation: 'create' | 'acquire' | 'release' | 'destroy',
  arguments_: ReadonlyArray<Value.Input>,
  tag: string,
) {
  return yield* FunctionBody.callDirect(
    context.body,
    context.storage[operation],
    context.storage.diagnosticCauseType === undefined
      ? arguments_
      : [
          ...arguments_,
          yield* Constant.nullValue(context.builder, context.pointer),
          yield* Constant.nullValue(context.builder, context.storage.diagnosticCauseType),
        ],
    tag,
  )
})

/** Lazily acquires state and traps on private storage exhaustion without changing source rows. */
export const ensure = Effect.fnUntraced(function* (
  context: Context,
  slot: Value.Input,
  tag: string,
): Effect.fn.Return<Value.Input, LlvmError.LlvmError> {
  const { builder, body, pointer, usizeType } = context
  const current = yield* FunctionBody.load(body, pointer, slot, `${tag}_current`)
  const create = yield* Block.make(body, `${tag}_create`)
  const ready = yield* Block.make(body, `${tag}_ready`)
  yield* FunctionBody.conditionalBranch(
    body,
    yield* FunctionBody.integerCompare(
      body,
      'eq',
      yield* FunctionBody.cast(body, 'ptrtoint', current, usizeType, `${tag}_current_address`),
      yield* Constant.integerUnsigned(builder, usizeType, 0n),
      `${tag}_absent`,
    ),
    create,
    ready,
  )
  yield* Block.setInsertionPoint(body, create)
  const acquired = yield* invoke(context, 'create', [], `${tag}_acquired`)
  if (acquired === undefined) throw new RangeError('Storage creation lost its result')
  const refused = yield* Block.make(body, `${tag}_refused`)
  const publish = yield* Block.make(body, `${tag}_publish`)
  yield* FunctionBody.conditionalBranch(
    body,
    yield* FunctionBody.integerCompare(
      body,
      'eq',
      yield* FunctionBody.cast(body, 'ptrtoint', acquired, usizeType, `${tag}_acquired_address`),
      yield* Constant.integerUnsigned(builder, usizeType, 0n),
      `${tag}_exhausted`,
    ),
    refused,
    publish,
  )
  yield* Block.setInsertionPoint(body, refused)
  yield* Intrinsic.call(body, 'trap', [], [])
  yield* FunctionBody.unreachable(body)
  yield* Block.setInsertionPoint(body, publish)
  yield* FunctionBody.store(body, acquired, slot)
  yield* FunctionBody.branch(body, ready)
  yield* Block.setInsertionPoint(body, ready)
  return yield* FunctionBody.load(body, pointer, slot, `${tag}_state`)
})

/** Consumes a live state once; an unstarted invocation owns no state and performs no call. */
export const destroy = Effect.fnUntraced(function* (
  context: Context,
  slot: Value.Input,
  tag: string,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  const { builder, body, pointer, usizeType } = context
  const state = yield* FunctionBody.load(body, pointer, slot, `${tag}_state`)
  const empty = yield* Constant.nullValue(builder, pointer)
  const release = yield* Block.make(body, `${tag}_release`)
  const done = yield* Block.make(body, `${tag}_done`)
  yield* FunctionBody.conditionalBranch(
    body,
    yield* FunctionBody.integerCompare(
      body,
      'eq',
      yield* FunctionBody.cast(body, 'ptrtoint', state, usizeType, `${tag}_state_address`),
      yield* Constant.integerUnsigned(builder, usizeType, 0n),
      `${tag}_absent`,
    ),
    done,
    release,
  )
  yield* Block.setInsertionPoint(body, release)
  yield* FunctionBody.store(body, empty, slot)
  yield* invoke(context, 'destroy', [state], `${tag}_destroy`)
  yield* FunctionBody.branch(body, done)
  yield* Block.setInsertionPoint(body, done)
})
