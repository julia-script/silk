import type * as LlvmError from '@silklang/llvm/LlvmError'
import * as Block from '@silklang/llvm/Block'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as Intrinsic from '@silklang/llvm/Intrinsic'
import * as LlvmType from '@silklang/llvm/Type'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as NativeDiagnosticContext from './NativeDiagnosticContext.js'

/** One owned diagnostic reference with immutable fallback metadata and its originating observer. */
export interface NativeDiagnosticFailure {
  readonly observer: Value.Input
  readonly handle: Value.Input
  readonly identity: readonly [Value.Input, Value.Input]
  readonly origin: readonly [Value.Input, Value.Input]
}

type Context = NativeDiagnosticContext.NativeDiagnosticContext

/** Metadata carries observer and static text addresses, independently of payload storage. */
export const type = Effect.fnUntraced(function* (
  context: Pick<Context, 'builder' | 'pointer' | 'word'>,
): Effect.fn.Return<LlvmType.Type, LlvmError.LlvmError> {
  const { builder, pointer, word } = context
  return yield* LlvmType.structure(builder, [pointer, word, pointer, word, pointer, word])
})

/** Serializes the value without retaining it; the caller must transfer, not duplicate, ownership. */
export const pack = Effect.fnUntraced(function* (self: NativeDiagnosticFailure, context: Context) {
  return yield* FunctionBody.buildAggregate(
    context.body,
    yield* type(context),
    [self.observer, self.handle, ...self.identity, ...self.origin],
    'diagnostic_failure',
  )
})

/** Restores transferred metadata without interpreting the opaque source handle. */
export const unpack = Effect.fnUntraced(function* (
  context: Context,
  value: Value.Input,
): Effect.fn.Return<NativeDiagnosticFailure, LlvmError.LlvmError> {
  const field = Effect.fnUntraced(function* (ordinal: number) {
    return yield* FunctionBody.extractValue(
      context.body,
      value,
      [ordinal],
      `diagnostic_failure${ordinal}`,
    )
  })
  return Object.freeze({
    observer: yield* field(0),
    handle: yield* field(1),
    identity: Object.freeze([yield* field(2), yield* field(3)] as const),
    origin: Object.freeze([yield* field(4), yield* field(5)] as const),
  })
})

/** Produces one optional source context; identity and origin survive a refused allocation. */
export const produce = Effect.fnUntraced(function* (
  context: Context,
  identity: NativeDiagnosticFailure['identity'],
  origin: NativeDiagnosticFailure['origin'],
): Effect.fn.Return<NativeDiagnosticFailure, LlvmError.LlvmError> {
  const observer = yield* NativeDiagnosticContext.current(context)
  const zero = yield* Constant.integerUnsigned(context.builder, context.word, 0n)
  const handle = yield* NativeDiagnosticContext.emitAt(
    context,
    observer,
    0,
    zero,
    zero,
    identity,
    origin,
  )
  return Object.freeze({ observer, handle, identity, origin })
})

const emptyText = Effect.fnUntraced(function* (context: Context) {
  return Object.freeze([
    yield* Constant.nullValue(context.builder, context.pointer),
    yield* Constant.integerUnsigned(context.builder, context.word, 0n),
  ] as const)
})

/** Returns another owned reference while preserving the borrowed input and fallback metadata. */
export const retain = Effect.fnUntraced(function* (
  self: NativeDiagnosticFailure,
  context: Context,
) {
  const text = yield* emptyText(context)
  const handle = yield* NativeDiagnosticContext.emitAt(
    context,
    self.observer,
    3,
    self.handle,
    text[1],
    text,
    text,
  )
  return Object.freeze({ ...self, handle })
})

/** Consumes exactly one reference through its originating observer, including a zero handle. */
export const release = Effect.fnUntraced(function* (
  self: NativeDiagnosticFailure,
  context: Context,
) {
  const text = yield* emptyText(context)
  yield* NativeDiagnosticContext.emitAt(context, self.observer, 4, self.handle, text[1], text, text)
})

/** Creates an immutable replacement with a caller frame; the original remains owned by the caller. */
export const propagate = Effect.fnUntraced(function* (
  self: NativeDiagnosticFailure,
  context: Context,
  frame: NativeDiagnosticFailure['identity'],
) {
  const text = yield* emptyText(context)
  const handle = yield* NativeDiagnosticContext.emitAt(
    context,
    self.observer,
    1,
    self.handle,
    text[1],
    frame,
    text,
  )
  return Object.freeze({ ...self, handle })
})

/** Combines borrowed contexts from one observer without consuming either input. */
export const withCause = Effect.fnUntraced(function* (
  self: NativeDiagnosticFailure,
  context: Context,
  cause: NativeDiagnosticFailure,
) {
  const { body, word } = context
  const accepted = yield* Block.make(body, 'diagnostic_cause_owner')
  const invalid = yield* Block.make(body, 'diagnostic_cause_invalid_owner')
  yield* FunctionBody.conditionalBranch(
    body,
    yield* FunctionBody.integerCompare(
      body,
      'eq',
      yield* FunctionBody.cast(body, 'ptrtoint', self.observer, word, 'diagnostic_primary_owner'),
      yield* FunctionBody.cast(body, 'ptrtoint', cause.observer, word, 'diagnostic_cause_owner'),
    ),
    accepted,
    invalid,
  )
  yield* Block.setInsertionPoint(body, invalid)
  // A source handle is meaningful only to its originating observer. Crossing this boundary
  // is a compiler ownership defect; never pass another pool's handle to the callback.
  yield* Intrinsic.call(body, 'trap', [], [])
  yield* FunctionBody.unreachable(body)
  yield* Block.setInsertionPoint(body, accepted)
  const text = yield* emptyText(context)
  const handle = yield* NativeDiagnosticContext.emitAt(
    context,
    self.observer,
    2,
    self.handle,
    cause.handle,
    text,
    text,
  )
  return Object.freeze({ ...self, handle })
})

/** Observes a borrowed selected failure after payload cleanup; the caller still owns its handle. */
export const unhandled = Effect.fnUntraced(function* (
  self: NativeDiagnosticFailure,
  context: Context,
) {
  return yield* NativeDiagnosticContext.emitAt(
    context,
    self.observer,
    5,
    self.handle,
    yield* Constant.integerUnsigned(context.builder, context.word, 0n),
    self.identity,
    self.origin,
  )
})
