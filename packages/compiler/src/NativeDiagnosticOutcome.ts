import * as Block from '@silklang/llvm/Block'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as NativeDiagnosticFailure from './NativeDiagnosticFailure.js'
import type * as NativeResult from './NativeResult.js'
import type * as Mir from './Mir.js'
import * as NativeDiagnosticContext from './NativeDiagnosticContext.js'

/** Independent storage for an outcome's owned metadata, never for its source payload. */
export interface NativeDiagnosticOutcome {
  readonly storage: Value.Input
}

type Context = NativeDiagnosticContext.NativeDiagnosticContext

/** Replaces an owned failure with an outward frame while preserving its immutable identity. */
export const propagate = Effect.fnUntraced(function* (
  self: NativeDiagnosticOutcome,
  context: Context,
  frame: NativeDiagnosticFailure.NativeDiagnosticFailure['identity'],
) {
  const failure = yield* NativeDiagnosticFailure.unpack(context, yield* borrow(self, context))
  const propagated = yield* NativeDiagnosticFailure.propagate(failure, context, frame)
  yield* replace(self, context, yield* NativeDiagnosticFailure.pack(propagated, context))
})

/** Produces a failure and attaches the selected same-observer recovery cause, if present. */
export const produce = Effect.fnUntraced(function* (
  self: NativeDiagnosticOutcome,
  context: Context,
  identity: NativeDiagnosticFailure.NativeDiagnosticFailure['identity'],
  origin: NativeDiagnosticFailure.NativeDiagnosticFailure['origin'],
) {
  const failure = yield* NativeDiagnosticFailure.produce(context, identity, origin)
  const cause = yield* NativeDiagnosticFailure.unpack(
    context,
    yield* NativeDiagnosticContext.currentCause(context),
  )
  const owner = yield* FunctionBody.cast(
    context.body,
    'ptrtoint',
    failure.observer,
    context.word,
    'produced_observer',
  )
  const causeOwner = yield* FunctionBody.cast(
    context.body,
    'ptrtoint',
    cause.observer,
    context.word,
    'produced_cause_observer',
  )
  const attach = yield* Block.make(context.body, 'produced_with_cause')
  const plain = yield* Block.make(context.body, 'produced_without_cause')
  const following = yield* Block.make(context.body, 'produced_complete')
  const sameOwner = yield* FunctionBody.integerCompare(
    context.body,
    'eq',
    owner,
    causeOwner,
    'produced_same_observer',
  )
  const observed = yield* FunctionBody.integerCompare(
    context.body,
    'ne',
    owner,
    yield* Constant.integerUnsigned(context.builder, context.word, 0n),
    'produced_observed',
  )
  yield* FunctionBody.conditionalBranch(
    context.body,
    yield* FunctionBody.binary(context.body, 'and', sameOwner, observed, 'produced_has_cause'),
    attach,
    plain,
  )
  yield* Block.setInsertionPoint(context.body, attach)
  const attached = yield* NativeDiagnosticFailure.withCause(failure, context, cause)
  yield* NativeDiagnosticFailure.release(failure, context)
  yield* replace(self, context, yield* NativeDiagnosticFailure.pack(attached, context))
  yield* FunctionBody.branch(context.body, following)
  yield* Block.setInsertionPoint(context.body, plain)
  yield* replace(self, context, yield* NativeDiagnosticFailure.pack(failure, context))
  yield* FunctionBody.branch(context.body, following)
  yield* Block.setInsertionPoint(context.body, following)
})

/** Transfers a complete call result into its owning local before exposing payload lanes. */
export const accept = Effect.fnUntraced(function* (
  context: Context | undefined,
  local: Mir.LocalId,
  result: NativeResult.NativeResult,
) {
  const outcome = context?.outcomes.get(local.ordinal)
  if (result.diagnostic !== undefined) {
    if (context === undefined || outcome === undefined)
      throw new RangeError('Native call result lost its diagnostic outcome owner')
    yield* replace(outcome, context, result.diagnostic)
  } else if (context !== undefined && outcome !== undefined) yield* release(outcome, context)
  return result.values
})

/** Releases a completed recovery's owned local; unobserved functions have no metadata storage. */
export const releaseLocal = Effect.fnUntraced(function* (
  context: Context | undefined,
  local: Mir.LocalId,
) {
  if (context === undefined) return
  const outcome = context.outcomes.get(local.ordinal)
  if (outcome === undefined) throw new RangeError('Recovery lost its owned diagnostic outcome')
  yield* release(outcome, context)
})

/** Consumes metadata when an independent execution delivers its payload to its outcome callback. */
export const consume = Effect.fnUntraced(function* (
  context: Context | undefined,
  result: NativeResult.NativeResult,
) {
  if (result.diagnostic !== undefined) {
    if (context === undefined)
      throw new RangeError('Native outcome consumption lost its diagnostic context')
    yield* NativeDiagnosticFailure.release(
      yield* NativeDiagnosticFailure.unpack(context, result.diagnostic),
      context,
    )
  }
  return result.values
})

/** Initializes newly acquired storage; resuming an existing frame must not call this. */
export const initialize = Effect.fnUntraced(function* (
  self: NativeDiagnosticOutcome,
  context: Context,
) {
  yield* FunctionBody.store(
    context.body,
    yield* Constant.nullValue(context.builder, context.causeType),
    self.storage,
  )
})

/** Borrows the metadata while the slot retains ownership. */
export const borrow = Effect.fnUntraced(function* (
  self: NativeDiagnosticOutcome,
  context: Context,
) {
  return yield* FunctionBody.load(
    context.body,
    context.causeType,
    self.storage,
    'outcome_diagnostic',
  )
})

/** Moves the reference out and clears ownership before any source callback can execute. */
export const take = Effect.fnUntraced(function* (self: NativeDiagnosticOutcome, context: Context) {
  const value = yield* borrow(self, context)
  yield* initialize(self, context)
  return value
})

/** Publishes a transferred reference, then releases the old reference through its own observer. */
export const replace = Effect.fnUntraced(function* (
  self: NativeDiagnosticOutcome,
  context: Context,
  value: Value.Input,
) {
  const previous = yield* take(self, context)
  yield* FunctionBody.store(context.body, value, self.storage)
  yield* NativeDiagnosticFailure.release(
    yield* NativeDiagnosticFailure.unpack(context, previous),
    context,
  )
})

/** Clears ownership before releasing the reference; empty slots perform no observer callback. */
export const release = Effect.fnUntraced(function* (
  self: NativeDiagnosticOutcome,
  context: Context,
) {
  const previous = yield* take(self, context)
  yield* NativeDiagnosticFailure.release(
    yield* NativeDiagnosticFailure.unpack(context, previous),
    context,
  )
})

/** Releases references owned by a lexical observer before that observer's state is destroyed. */
export const releaseForObserver = Effect.fnUntraced(function* (
  self: NativeDiagnosticOutcome,
  context: Context,
  observer: Value.Input,
) {
  const value = yield* borrow(self, context)
  const owner = yield* FunctionBody.extractValue(context.body, value, [0], 'outcome_observer')
  const selected = yield* Block.make(context.body, 'outcome_release_selected')
  const following = yield* Block.make(context.body, 'outcome_release_following')
  yield* FunctionBody.conditionalBranch(
    context.body,
    yield* FunctionBody.integerCompare(
      context.body,
      'eq',
      yield* FunctionBody.cast(
        context.body,
        'ptrtoint',
        owner,
        context.word,
        'outcome_owner_address',
      ),
      yield* FunctionBody.cast(
        context.body,
        'ptrtoint',
        observer,
        context.word,
        'outcome_scope_address',
      ),
    ),
    selected,
    following,
  )
  yield* Block.setInsertionPoint(context.body, selected)
  yield* release(self, context)
  yield* FunctionBody.branch(context.body, following)
  yield* Block.setInsertionPoint(context.body, following)
})
