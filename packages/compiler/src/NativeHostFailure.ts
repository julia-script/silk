import type * as Builder from '@silklang/llvm/Builder'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import type * as Mir from './Mir.js'
import type * as NativeLoweringContext from './NativeLoweringContext.js'
import type * as NativeSuspension from './NativeSuspension.js'
import * as NativeReturn from './NativeReturn.js'
import * as NativeType from './NativeType.js'
import * as NativeDiagnosticOutcome from './NativeDiagnosticOutcome.js'
import * as NativeDiagnosticText from './NativeDiagnosticText.js'
import * as NativeTermination from './NativeTermination.js'

/** Failure-return state for allocation boundaries. */
export interface Context {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly entry: NativeLoweringContext.DeclaredFunction
  readonly types: NativeType.LoweringContext
  readonly suspension: NativeSuspension.ReturnContext
  readonly termination: NativeTermination.FunctionContext
}

/** Emits one host-boundary failure in the function's synchronous or suspension ABI. */
export const emit = Effect.fnUntraced(function* (
  context: Context,
  operation: Extract<Mir.Operation, { readonly _tag: 'Allocate' }>,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  const lanes = NativeType.lanesFor(context.types, operation.propagationType)
  const values: Array<Value.Input> = []
  for (const [ordinal, lane] of lanes.entries()) {
    values.push(
      yield* Constant.integerUnsigned(
        context.builder,
        NativeType.laneType(context.types, lane),
        ordinal === 0 ? BigInt(operation.failureTag) : 0n,
      ),
    )
  }
  let metadata: Value.Input | undefined
  if (context.entry.diagnosticResult !== undefined) {
    const diagnostic = context.suspension.diagnostic
    if (diagnostic === undefined)
      throw new RangeError('Allocation failure lost its diagnostic context')
    // Allocation refusal returns immediately. This stack slot cannot survive a suspension
    // or escape the invocation; the completed result takes its reference before cleanup.
    const outcome = {
      storage: yield* FunctionBody.alloca(
        context.body,
        diagnostic.causeType,
        `allocation_failure${operation.destination.ordinal}`,
      ),
    }
    yield* NativeDiagnosticOutcome.initialize(outcome, diagnostic)
    yield* NativeDiagnosticOutcome.produce(
      outcome,
      diagnostic,
      yield* NativeDiagnosticText.literal(
        diagnostic,
        NativeTermination.identityOf(operation.propagationType.type, operation.failureTag),
        `${context.entry.symbol}.allocation${operation.destination.ordinal}.identity`,
      ),
      yield* NativeDiagnosticText.literal(
        diagnostic,
        NativeDiagnosticText.origin(
          context.termination.module,
          context.entry.fn,
          operation.provenance.span,
        ),
        `${context.entry.symbol}.allocation${operation.destination.ordinal}.origin`,
      ),
    )
    metadata = yield* NativeDiagnosticOutcome.take(outcome, diagnostic)
  }
  yield* NativeReturn.completeResult(
    context.suspension,
    { values: Object.freeze(values), ...(metadata === undefined ? {} : { diagnostic: metadata }) },
    `host_failure${operation.destination.ordinal}`,
  )
})
