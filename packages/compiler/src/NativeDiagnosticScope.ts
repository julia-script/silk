import * as NativeDiagnosticOutcome from './NativeDiagnosticOutcome.js'
import type * as Builder from '@silklang/llvm/Builder'
import * as Block from '@silklang/llvm/Block'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionActor from '@silklang/llvm/Function'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as Mir from './Mir.js'
import * as NativeCallable from './NativeCallable.js'
import * as NativeDiagnosticContext from './NativeDiagnosticContext.js'
import type * as NativeLoweringContext from './NativeLoweringContext.js'
import type * as NativeOperationContext from './NativeOperationContext.js'
import * as NativeStorage from './NativeStorage.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import * as NativeType from './NativeType.js'

export interface NativeDiagnosticScope {
  readonly record: Value.Input
  readonly callback: FunctionActor.Function
}

interface Context extends NativeCallable.Context {
  readonly builder: Builder.Builder
  readonly entry: NativeLoweringContext.DeclaredFunction
  readonly declared: ReadonlyArray<NativeLoweringContext.DeclaredFunction>
  readonly diagnostic: NativeDiagnosticContext.NativeDiagnosticContext
}

/** Preallocates synchronous descriptors and adapters borrowing the represented callback owner. */
export const prepare = Effect.fnUntraced(function* (context: Context) {
  const { builder, body, entry, diagnostic, types } = context
  const scopes = new Map<number, NativeDiagnosticScope>()
  for (const operation of entry.linear.flatMap((block) => block.operations)) {
    if (operation._tag !== 'EnterDiagnosticScope') continue
    const type = entry.fn.localTypes.at(operation.observer.ordinal)
    if (type?._tag !== 'CallableValue' || type.target?._tag !== 'DeclarationCallableTarget')
      throw new RangeError('Diagnostic observer lost its declared callable identity')
    const targetId = type.target.declaration
    const arguments_ =
      type.environment?.callable.typeArguments ??
      type.storage?.realization.targetArguments ??
      type.typeArguments ??
      []
    const target = context.declared.find((candidate) =>
      Mir.matchesInstance(candidate.fn, targetId, arguments_),
    )
    if (target === undefined || target.suspendable || target.diagnosticParameter === undefined)
      throw new RangeError('Diagnostic observer lost its direct internal target')
    const captureLanes = NativeType.lanesFor(types, type)
    const callback = yield* FunctionActor.declare(
      builder,
      `${entry.symbol}$diagnostic${operation.scope.ordinal}`,
      diagnostic.callbackType,
      { linkage: 'internal' },
    )
    yield* FunctionActor.buildBody(
      builder,
      callback,
      Effect.fnUntraced(function* (callbackBody) {
        yield* Block.make(callbackBody, 'entry')
        const environment = yield* Value.argument(callbackBody, 0)
        const values: Array<Value.Input> = []
        for (const [ordinal, lane] of captureLanes.entries()) {
          const offset = NativeType.addressLaneOffset(context.program.layout, type, lane, ordinal)
          if (offset === undefined)
            throw new RangeError('Diagnostic callback lost its capture address layout')
          values.push(
            yield* FunctionBody.load(
              callbackBody,
              NativeType.laneType(types, lane),
              yield* NativeLanePointer.lanePointer(
                context.lanePointers,
                callbackBody,
                environment,
                offset,
                `observer_capture${ordinal}_ptr`,
              ),
              `observer_capture${ordinal}`,
            ),
          )
        }
        const groups = yield* NativeCallable.capturedArguments(
          { ...context, body: callbackBody },
          type,
          values,
          'observer',
        )
        const arguments_: Array<ReadonlyArray<Value.Input>> = []
        for (const ordinals of [[1], [2], [3], [4], [5, 6], [7, 8]])
          arguments_.push(
            yield* Effect.forEach(ordinals, (ordinal) => Value.argument(callbackBody, ordinal)),
          )
        const operands = Mir.applyOperands(
          groups.map((group) => ({
            parameterOrdinal: group.parameterOrdinal,
            items: group.values,
          })),
          arguments_,
        )
        if (operands.length !== target.diagnosticParameter)
          throw new RangeError('Diagnostic callback adapter lost its source argument lanes')
        const result = yield* FunctionBody.callDirect(
          callbackBody,
          target.handle,
          [
            ...operands,
            yield* Constant.nullValue(builder, diagnostic.pointer),
            yield* Constant.nullValue(builder, diagnostic.causeType),
          ],
          'observer_result',
        )
        if (result === undefined) throw new RangeError('Diagnostic callback returned no handle')
        yield* FunctionBody.returnValue(callbackBody, result)
      }),
    )
    scopes.set(
      operation.scope.ordinal,
      Object.freeze({
        record: entry.suspendable
          ? yield* Constant.nullValue(builder, diagnostic.pointer)
          : yield* FunctionBody.alloca(
              body,
              diagnostic.recordType,
              `diagnostic_scope${operation.scope.ordinal}`,
            ),
        callback,
      }),
    )
  }
  return scopes
})

/** Installs or leaves one lexical descriptor without moving the state owner out of its storage. */
export const emit = Effect.fnUntraced(function* (
  context: NativeOperationContext.Context,
  operation: Extract<
    import('./MirLinearization.js').LinearOperation,
    { readonly _tag: 'EnterDiagnosticScope' | 'LeaveDiagnosticScope' }
  >,
) {
  const diagnostic = context.termination.diagnostic
  const scope = context.diagnosticScopes.get(operation.scope.ordinal)
  if (diagnostic === undefined || scope === undefined)
    throw new RangeError('Diagnostic scope lost its invocation storage')
  const { body, builder } = context
  if (operation._tag === 'LeaveDiagnosticScope') {
    for (const outcome of diagnostic.outcomes.values())
      yield* NativeDiagnosticOutcome.releaseForObserver(outcome, diagnostic, scope.record)

    yield* FunctionBody.store(
      body,
      yield* FunctionBody.load(
        body,
        diagnostic.pointer,
        yield* NativeDiagnosticContext.field(diagnostic, scope.record, 3),
      ),
      diagnostic.current,
    )
    yield* FunctionBody.store(
      body,
      yield* FunctionBody.load(
        body,
        diagnostic.causeType,
        yield* NativeDiagnosticContext.field(diagnostic, scope.record, 4),
      ),
      diagnostic.cause,
    )
    return
  }
  yield* NativeStorage.ensureAddressRoot(context.storage, operation.state)
  const state = context.storage.addressStorage.get(operation.state.ordinal)
  if (state === undefined) throw new RangeError('Diagnostic state lost its stable address')
  yield* NativeStorage.ensureAddressRoot(context.storage, operation.observer)
  const captures = context.storage.addressStorage.get(operation.observer.ordinal)
  if (captures === undefined) throw new RangeError('Diagnostic callback lost its stable address')
  const fields = [
    yield* Constant.fromGlobal(builder, yield* FunctionActor.global(builder, scope.callback)),
    state,
    captures,
    yield* NativeDiagnosticContext.current(diagnostic),
    yield* NativeDiagnosticContext.currentCause(diagnostic),
  ]
  for (const ordinal of [0, 1, 2, 3, 4] as const) {
    const value = fields.at(ordinal)
    if (value === undefined) throw new RangeError('Diagnostic descriptor lost a field')
    yield* FunctionBody.store(
      body,
      value,
      yield* NativeDiagnosticContext.field(diagnostic, scope.record, ordinal),
    )
  }
  yield* FunctionBody.store(body, scope.record, diagnostic.current)
})
