import type * as LlvmBlock from '@silk-effect/llvm/Block'
import type * as Builder from '@silk-effect/llvm/Builder'
import * as Constant from '@silk-effect/llvm/Constant'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import * as Intrinsic from '@silk-effect/llvm/Intrinsic'
import type * as LlvmError from '@silk-effect/llvm/LlvmError'
import type * as LlvmType from '@silk-effect/llvm/Type'
import type * as Value from '@silk-effect/llvm/Value'
import * as Effect from 'effect/Effect'
import * as CleanupPlan from './CleanupPlan.js'
import * as Mir from './Mir.js'
import type { LinearTerminator } from './MirLinearization.js'
import * as NativeAggregate from './NativeAggregate.js'
import * as NativeDebug from './NativeDebug.js'
import type * as NativeLoweringContext from './NativeLoweringContext.js'
import * as NativeSuspension from './NativeSuspension.js'
import * as NativeType from './NativeType.js'

export interface Context {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly i32: LlvmType.Type
  readonly types: NativeType.LoweringContext
  readonly blocks: ReadonlyMap<number, LlvmBlock.Block>
  readonly locals: ReadonlyMap<number, ReadonlyArray<Value.Input>>
  readonly entry: NativeLoweringContext.DeclaredFunction
  readonly cleanup: NativeAggregate.Context
  readonly failure: NativeAggregate.FailureContext
  readonly suspension: NativeSuspension.ReturnContext
  readonly debug: NativeDebug.LocationContext
}

const read = (context: Context, local: Mir.LocalId): ReadonlyArray<Value.Input> => {
  const found = context.locals.get(local.ordinal)
  if (found === undefined) throw new RangeError(`Backend read undefined local %${local.ordinal}`)
  return found
}

const scalar = (context: Context, local: Mir.LocalId): Value.Input => {
  const values = read(context, local)
  const first = values.at(0)
  if (values.length !== 1 || first === undefined)
    throw new RangeError(`Backend expected scalar local %${local.ordinal}`)
  return first
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
    scalar(context, terminator.condition),
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

export const enumMatchBranch = Effect.fnUntraced(function* (
  context: Context,
  terminator: Extract<LinearTerminator, { readonly _tag: 'EnumMatchBranch' }>,
  blockOrdinal: number,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  const value = scalar(context, terminator.scrutinee)
  const lane = NativeType.lanesFor(context.types, terminator.type).at(0)
  if (lane === undefined) throw new RangeError('LLVM enum match lost its scalar lane')
  const type = NativeType.laneType(context.types, lane)
  const expected =
    terminator.representation.signedness === 'Signed'
      ? yield* Constant.integerSigned(context.builder, type, terminator.discriminant)
      : yield* Constant.integerUnsigned(context.builder, type, terminator.discriminant)
  const condition = yield* FunctionBody.integerCompare(
    context.body,
    'eq',
    value,
    expected,
    `enum_match${blockOrdinal}_member`,
  )
  yield* FunctionBody.conditionalBranch(
    context.body,
    condition,
    targetBlock(context.blocks, terminator.taken, 'LLVM enum match branch'),
    targetBlock(context.blocks, terminator.otherwise, 'LLVM enum match branch'),
  )
})

export const matchBranch = Effect.fnUntraced(function* (
  context: Context,
  terminator: Extract<LinearTerminator, { readonly _tag: 'MatchBranch' }>,
  blockOrdinal: number,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  const tag = read(context, terminator.scrutinee).at(0)
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

/** Emits one complete MIR terminator, including propagation cleanup and suspension return ABI. */
export const emit = Effect.fnUntraced(function* (
  context: Context,
  terminator: LinearTerminator,
  blockOrdinal: number,
  blockId: Mir.RegionId,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  const { builder, body, i32, entry } = context
  const readLocal = (local: Mir.LocalId) => read(context, local)
  const readScalar = (local: Mir.LocalId) => scalar(context, local)
  const block = Object.freeze({ id: blockId })
  switch (terminator._tag) {
    case 'PropagateEffectFailure': {
      const source = readLocal(terminator.source)
      const sourceTag = terminator.sourceType._tag === 'Union' ? source.at(0) : undefined
      let mappedTag: Value.Input
      if (terminator.sourceType._tag === 'Nominal') {
        mappedTag = yield* Constant.integerSigned(
          builder,
          i32,
          BigInt(terminator.tagMappings.at(0)?.target ?? -1),
        )
      } else {
        if (sourceTag === undefined)
          throw new RangeError('Effect failure propagation lost its tag lane')
        mappedTag = yield* Constant.integerSigned(builder, i32, -1n)
        for (const [ordinal, mapping] of terminator.tagMappings.entries()) {
          const matches = yield* FunctionBody.integerCompare(
            body,
            'eq',
            sourceTag,
            yield* Constant.integerSigned(builder, i32, BigInt(mapping.source)),
            `effect_failure_propagation${terminator.source.ordinal}_${ordinal}`,
          )
          mappedTag = yield* FunctionBody.select(
            body,
            matches,
            yield* Constant.integerSigned(builder, i32, BigInt(mapping.target)),
            mappedTag,
            `effect_failure_propagation${terminator.source.ordinal}_${ordinal}_tag`,
          )
        }
      }
      for (const release of terminator.releases ?? []) {
        if (!CleanupPlan.hasEffect(release.cleanup)) continue
        yield* NativeAggregate.dropThroughPlan(
          context.cleanup,
          release.cleanup,
          readLocal(release.local),
          `propagation_release${release.local.ordinal}`,
        )
      }
      const returned: Array<Value.Input> = [
        mappedTag,
        ...(yield* NativeAggregate.failurePayload(
          context.failure,
          source,
          Mir.semanticType(terminator.sourceType),
          sourceTag,
          terminator.propagationType.type,
          terminator.tagMappings,
          `effect_failure_propagation${terminator.source.ordinal}_payload`,
        )),
      ]
      if (entry.suspendable) {
        yield* NativeSuspension.returnStep(
          context.suspension,
          0n,
          Object.freeze(returned),
          'propagated_selective_failure_step',
        )
      } else {
        yield* FunctionBody.returnValue(
          body,
          returned.length === 1
            ? (returned.at(0) ?? mappedTag)
            : yield* FunctionBody.buildAggregate(
                body,
                entry.resultType,
                Object.freeze(returned.slice(0, terminator.propagationLaneCount)),
                'propagated_selective_failure',
              ),
        )
      }
      break
    }
    case 'Return': {
      const returned = readLocal(terminator.value)
      if (entry.suspendable) {
        yield* NativeSuspension.returnStep(
          context.suspension,
          0n,
          returned,
          `complete_value_b${block.id.ordinal}`,
        )
        break
      }
      const instruction =
        returned.length === 0
          ? yield* FunctionBody.returnVoid(body)
          : returned.length === 1
            ? yield* FunctionBody.returnValue(body, readScalar(terminator.value))
            : yield* FunctionBody.returnValue(
                body,
                yield* FunctionBody.buildAggregate(
                  body,
                  entry.resultType,
                  returned,
                  `return_value_b${block.id.ordinal}`,
                ),
              )
      yield* NativeDebug.locate(context.debug, terminator.provenance.span, instruction)
      break
    }
    case 'Jump': {
      yield* jump(context, terminator)
      break
    }
    case 'Branch': {
      yield* branch(context, terminator, blockOrdinal)
      break
    }
    case 'MatchBranch': {
      yield* matchBranch(context, terminator, block.id.ordinal)
      break
    }
    case 'EnumMatchBranch': {
      yield* enumMatchBranch(context, terminator, block.id.ordinal)
      break
    }
    case 'Trap': {
      yield* Intrinsic.call(body, 'trap', [], [])
      const instruction = yield* FunctionBody.unreachable(body)
      yield* NativeDebug.locate(context.debug, terminator.provenance.span, instruction)
      break
    }
  }
})
