import type * as FunctionActor from '@silk-effect/llvm/Function'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import type * as LlvmError from '@silk-effect/llvm/LlvmError'
import type * as Value from '@silk-effect/llvm/Value'
import * as Effect from 'effect/Effect'
import type * as Mir from './Mir.js'

export interface DeclaredTarget {
  readonly handle: FunctionActor.Function
  readonly resultLaneCount: number
  readonly suspendable: boolean
}

export interface SynchronousContext {
  readonly body: FunctionBody.FunctionBody
  readonly addressRoots: ReadonlySet<number>
  readonly reloadAddressRoot: (root: number) => Effect.Effect<void, LlvmError.LlvmError>
}

/** Calls one synchronous native target and unpacks its ABI result lanes. */
export const callSynchronous = Effect.fnUntraced(function* (
  context: SynchronousContext,
  target: DeclaredTarget,
  arguments_: ReadonlyArray<Value.Input>,
  name: string,
): Effect.fn.Return<ReadonlyArray<Value.Input>, LlvmError.LlvmError> {
  if (target.suspendable)
    throw new RangeError('LLVM synchronous helper selected a suspendable target')
  const result = yield* FunctionBody.callDirect(context.body, target.handle, arguments_, name)
  for (const root of [...context.addressRoots].sort((left, right) => left - right))
    yield* context.reloadAddressRoot(root)
  if (target.resultLaneCount === 0) return Object.freeze([])
  if (result === undefined) throw new RangeError('Backend call produced no value')
  if (target.resultLaneCount === 1) return Object.freeze([result])
  const values: Array<Value.Input> = []
  for (let lane = 0; lane < target.resultLaneCount; lane += 1)
    values.push(yield* FunctionBody.extractValue(context.body, result, [lane], `${name}_${lane}`))
  return Object.freeze(values)
})

/** Runtime inputs consumed by an Effect execution operation. */
export const operationInputs = (
  operation: Extract<
    Mir.Operation,
    { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'ReifyEffect' }
  >,
): ReadonlyArray<Mir.LocalId> =>
  operation._tag === 'RunEffect'
    ? Object.freeze(operation.arguments)
    : Object.freeze([operation.effect, ...operation.arguments])
