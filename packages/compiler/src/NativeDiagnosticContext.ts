import type * as NativeDiagnosticOutcome from './NativeDiagnosticOutcome.js'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as LlvmType from '@silklang/llvm/Type'
import type * as Builder from '@silklang/llvm/Builder'
import * as Block from '@silklang/llvm/Block'
import * as Constant from '@silklang/llvm/Constant'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as NativeDiagnosticFailure from './NativeDiagnosticFailure.js'

/** One invocation's current lexical observer reference; never module or thread-local storage. */
export interface NativeDiagnosticContext {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly pointer: LlvmType.Type
  readonly byte: LlvmType.Type
  readonly word: LlvmType.Type
  readonly recordType: LlvmType.Type
  readonly callbackType: LlvmType.Type
  readonly current: Value.Input
  readonly causeType: LlvmType.Type
  /** Borrowed input retained by the caller; copied by value, never a pointer to its stack slot. */
  readonly incomingCause: Value.Input
  readonly cause: Value.Input
  readonly outcomes: Map<number, NativeDiagnosticOutcome.NativeDiagnosticOutcome>
  /** Interned artifact-lifetime text shared by repeated sites in this function. */
  readonly literals: Map<string, readonly [Value.Input, Value.Input]>
  /** Callback dispatch invalidates cached source values at the enclosing operation join. */
  readonly sourceState: { dirty: boolean }
}

/** Creates the invocation-local slot initialized from the internal call ABI. */
export const make = Effect.fnUntraced(function* (
  builder: Builder.Builder,
  body: FunctionBody.FunctionBody,
  pointer: LlvmType.Type,
  byte: LlvmType.Type,
  word: LlvmType.Type,
  initial: Value.Input,
  initialCause?: Value.Input,
): Effect.fn.Return<NativeDiagnosticContext, LlvmError.LlvmError> {
  const causeType = yield* NativeDiagnosticFailure.type({ builder, pointer, word })
  const recordType = yield* LlvmType.structure(builder, [
    pointer,
    pointer,
    pointer,
    pointer,
    causeType,
  ])
  const callbackType = yield* LlvmType.functionType(builder, word, [
    pointer,
    pointer,
    byte,
    word,
    word,
    pointer,
    word,
    pointer,
    word,
  ])
  const current = yield* FunctionBody.alloca(body, pointer, 'diagnostic_observer_slot')
  yield* FunctionBody.store(body, initial, current)
  const incomingCause = initialCause ?? (yield* Constant.nullValue(builder, causeType))
  const cause = yield* FunctionBody.alloca(body, causeType, 'diagnostic_cause_slot')
  yield* FunctionBody.store(body, incomingCause, cause)
  return Object.freeze({
    builder,
    body,
    pointer,
    byte,
    word,
    recordType,
    callbackType,
    current,
    causeType,
    incomingCause,
    cause,
    outcomes: new Map<number, NativeDiagnosticOutcome.NativeDiagnosticOutcome>(),
    literals: new Map<string, readonly [Value.Input, Value.Input]>(),
    sourceState: { dirty: false },
  })
})

/** Borrows the selected cause for a nested call without creating an owned reference. */
export const currentCause = Effect.fnUntraced(function* (self: NativeDiagnosticContext) {
  return yield* FunctionBody.load(self.body, self.causeType, self.cause, 'diagnostic_cause')
})

/** Loads the observer selected at this call site, including any intervening lexical scope. */
export const current = Effect.fnUntraced(function* (self: NativeDiagnosticContext) {
  return yield* FunctionBody.load(self.body, self.pointer, self.current, 'diagnostic_observer')
})

/** Borrows the selected failure for terminal observation; unrelated or absent contexts stay silent. */
export const unhandled = Effect.fnUntraced(function* (self: NativeDiagnosticContext) {
  const failure = yield* NativeDiagnosticFailure.unpack(self, yield* currentCause(self))
  const selected = yield* FunctionBody.integerCompare(
    self.body,
    'eq',
    yield* FunctionBody.cast(
      self.body,
      'ptrtoint',
      failure.observer,
      self.word,
      'unhandled_cause_owner_address',
    ),
    yield* FunctionBody.cast(
      self.body,
      'ptrtoint',
      yield* current(self),
      self.word,
      'unhandled_current_observer_address',
    ),
    'unhandled_selected_observer',
  )
  return yield* NativeDiagnosticFailure.unhandled(
    {
      ...failure,
      observer: yield* FunctionBody.select(
        self.body,
        selected,
        failure.observer,
        yield* Constant.nullValue(self.builder, self.pointer),
        'unhandled_observer',
      ),
    },
    self,
  )
})

/** Addresses dispatch, state, captures, previous observer, or previous cause in a descriptor. */
export const field = Effect.fnUntraced(function* (
  self: NativeDiagnosticContext,
  record: Value.Input,
  ordinal: 0 | 1 | 2 | 3 | 4,
) {
  const index = yield* LlvmType.integer(self.builder, 32)
  return yield* FunctionBody.getElementPtr(self.body, self.recordType, record, [
    yield* Constant.integerUnsigned(self.builder, index, 0n),
    yield* Constant.integerUnsigned(self.builder, index, BigInt(ordinal)),
  ])
})

/** Dispatches to the observer that owns a retained context, independently of lexical selection. */
export const emitAt = Effect.fnUntraced(function* (
  self: NativeDiagnosticContext,
  observer: Value.Input,
  event: 0 | 1 | 2 | 3 | 4 | 5 | 6,
  first: Value.Input,
  second: Value.Input,
  identity: readonly [Value.Input, Value.Input],
  origin: readonly [Value.Input, Value.Input],
) {
  const { body, builder, pointer, word } = self
  const enabled = yield* Block.make(body, 'diagnostic_enabled')
  const disabled = yield* Block.make(body, 'diagnostic_disabled')
  const following = yield* Block.make(body, 'diagnostic_complete')
  const zero = yield* Constant.integerUnsigned(builder, word, 0n)
  yield* FunctionBody.conditionalBranch(
    body,
    yield* FunctionBody.integerCompare(
      body,
      'ne',
      yield* FunctionBody.cast(body, 'ptrtoint', observer, word, 'diagnostic_observer_address'),
      zero,
    ),
    enabled,
    disabled,
  )
  yield* Block.setInsertionPoint(body, enabled)
  const dispatch = yield* FunctionBody.load(body, pointer, yield* field(self, observer, 0))
  const state = yield* FunctionBody.load(body, pointer, yield* field(self, observer, 1))
  const captures = yield* FunctionBody.load(body, pointer, yield* field(self, observer, 2))
  const result = yield* FunctionBody.call(
    body,
    self.callbackType,
    dispatch,
    [
      captures,
      state,
      yield* Constant.integerUnsigned(builder, self.byte, BigInt(event)),
      first,
      second,
      ...identity,
      ...origin,
    ],
    'diagnostic_result',
  )
  if (result === undefined) throw new RangeError('Diagnostic callback returned no handle')
  yield* FunctionBody.branch(body, following)
  yield* Block.setInsertionPoint(body, disabled)
  yield* FunctionBody.branch(body, following)
  yield* Block.setInsertionPoint(body, following)
  const joined = yield* FunctionBody.phi(body, word, 'diagnostic_handle')
  yield* FunctionBody.addPhiIncoming(body, joined, result, enabled)
  yield* FunctionBody.addPhiIncoming(body, joined, zero, disabled)
  yield* FunctionBody.sealPhi(body, joined)
  const value = yield* FunctionBody.phiValue(body, joined)
  self.sourceState.dirty = true
  return value
})

/** Dispatches a semantic event to the current observer; disabled observation yields zero. */
export const emit = Effect.fnUntraced(function* (
  self: NativeDiagnosticContext,
  event: 0 | 1 | 2 | 3 | 4 | 5 | 6,
  first: Value.Input,
  second: Value.Input,
  identity: readonly [Value.Input, Value.Input],
  origin: readonly [Value.Input, Value.Input],
) {
  return yield* emitAt(self, yield* current(self), event, first, second, identity, origin)
})

/** Borrows the active cause only from the selected observer's pool for fatal reporting. */
export const fatal = Effect.fnUntraced(function* (
  self: NativeDiagnosticContext,
  reason: readonly [Value.Input, Value.Input],
  origin: readonly [Value.Input, Value.Input],
) {
  const observer = yield* current(self)
  const cause = yield* NativeDiagnosticFailure.unpack(self, yield* currentCause(self))
  const zero = yield* Constant.integerUnsigned(self.builder, self.word, 0n)
  const selected = yield* FunctionBody.select(
    self.body,
    yield* FunctionBody.integerCompare(
      self.body,
      'eq',
      yield* FunctionBody.cast(
        self.body,
        'ptrtoint',
        observer,
        self.word,
        'fatal_observer_address',
      ),
      yield* FunctionBody.cast(
        self.body,
        'ptrtoint',
        cause.observer,
        self.word,
        'fatal_cause_owner_address',
      ),
      'fatal_cause_owner_matches',
    ),
    cause.handle,
    zero,
    'fatal_cause_handle',
  )
  return yield* emitAt(self, observer, 6, selected, zero, reason, origin)
})
