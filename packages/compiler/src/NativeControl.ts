import type * as LlvmBlock from '@silk-effect/llvm/Block'
import type * as Builder from '@silk-effect/llvm/Builder'
import * as Constant from '@silk-effect/llvm/Constant'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import type * as LlvmError from '@silk-effect/llvm/LlvmError'
import type * as LlvmType from '@silk-effect/llvm/Type'
import type * as Value from '@silk-effect/llvm/Value'
import * as Effect from 'effect/Effect'
import type * as Mir from './Mir.js'
import type { LinearTerminator } from './MirLinearization.js'

export interface Context {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly i32: LlvmType.Type
  readonly blocks: ReadonlyMap<number, LlvmBlock.Block>
  readonly readScalar: (local: Mir.LocalId) => Value.Input
  readonly readLocal: (local: Mir.LocalId) => ReadonlyArray<Value.Input>
}

/** Resolves one MIR control target to its declared LLVM block. */
export const targetBlock = (
  blocks: ReadonlyMap<number, LlvmBlock.Block>,
  target: Mir.RegionId,
  operation: string,
): LlvmBlock.Block => {
  const block = blocks.get(target.ordinal)
  if (block === undefined) throw new RangeError(`${operation} targets a missing block`)
  return block
}

export const jump = (
  context: Context,
  terminator: Extract<LinearTerminator, { readonly _tag: 'Jump' }>,
): Effect.Effect<void, LlvmError.LlvmError> =>
  FunctionBody.branch(context.body, targetBlock(context.blocks, terminator.target, 'Backend jump'))

export const branch = Effect.fnUntraced(function* (
  context: Context,
  terminator: Extract<LinearTerminator, { readonly _tag: 'Branch' }>,
  ordinal: number,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  const zero = yield* Constant.integerSigned(context.builder, context.i32, 0n)
  const condition = yield* FunctionBody.integerCompare(
    context.body,
    'ne',
    context.readScalar(terminator.condition),
    zero,
    `c${ordinal}`,
  )
  yield* FunctionBody.conditionalBranch(
    context.body,
    condition,
    targetBlock(context.blocks, terminator.taken, 'Backend branch'),
    targetBlock(context.blocks, terminator.otherwise, 'Backend branch'),
  )
})

export const matchBranch = Effect.fnUntraced(function* (
  context: Context,
  terminator: Extract<LinearTerminator, { readonly _tag: 'MatchBranch' }>,
  blockOrdinal: number,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  const tag = context.readLocal(terminator.scrutinee).at(0)
  if (tag === undefined) throw new RangeError('LLVM union match has no tag lane')
  const expected = yield* Constant.integerSigned(
    context.builder,
    context.i32,
    BigInt(terminator.memberOrdinal),
  )
  const condition = yield* FunctionBody.integerCompare(
    context.body,
    'eq',
    tag,
    expected,
    `match${blockOrdinal}_member`,
  )
  yield* FunctionBody.conditionalBranch(
    context.body,
    condition,
    targetBlock(context.blocks, terminator.taken, 'LLVM match branch'),
    targetBlock(context.blocks, terminator.otherwise, 'LLVM match branch'),
  )
})
