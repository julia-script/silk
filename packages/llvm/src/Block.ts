import * as Effect from 'effect/Effect'
import type * as ByteString from './ByteString.js'
import type * as FunctionBody from './FunctionBody.js'
import * as FunctionBodyState from './internal/FunctionBodyState.js'
import type * as Handle from './internal/Handle.js'
import { SilkError } from './SilkError.js'

/**
 * Opaque identity for a basic block inside one open function-body transaction.
 *
 * @category blocks
 * @since 0.0.0
 */
export interface Block extends Handle.Handle<'Block'> {}

/**
 * Creates a basic block in declaration order inside an open function-body transaction.
 *
 * **Details**
 *
 * The first block becomes the insertion point automatically. Later blocks are created without
 * moving that point; call {@link setInsertionPoint} before appending their instructions. Every
 * created block must have a terminator before the enclosing `Function.buildBody` can commit.
 *
 * JavaScript labels are encoded as UTF-8; byte strings preserve an exact block label.
 *
 * **Gotchas**
 *
 * A closed body or a body used from a different fiber fails with {@link SilkError}.
 *
 * **Example** (Building multiple blocks)
 *
 * Create two blocks and terminate both before committing the function.
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Block from '@silk-effect/llvm/Block'
 * import * as Builder from '@silk-effect/llvm/Builder'
 * import * as FunctionActor from '@silk-effect/llvm/Function'
 * import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
 * import * as Type from '@silk-effect/llvm/Type'
 *
 * await Effect.runPromise(Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   const voidType = yield* Type.voidType(builder)
 *   const signature = yield* Type.functionType(builder, voidType, [])
 *   const fn = yield* FunctionActor.declare(builder, 'two_blocks', signature)
 *   yield* FunctionActor.buildBody(builder, fn, Effect.fn('Example.twoBlocks')(function* (body) {
 *     yield* Block.make(body, 'entry')
 *     const exit = yield* Block.make(body, 'exit')
 *     yield* FunctionBody.branch(body, exit)
 *     yield* Block.setInsertionPoint(body, exit)
 *     yield* FunctionBody.returnVoid(body)
 *   }))
 * }))
 * ```
 *
 * @category blocks
 * @since 0.0.0
 */
export const make = Effect.fn('Block.make')(function* (
  body: FunctionBody.FunctionBody,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Block, SilkError> {
  return yield* FunctionBodyState.mutate(body, 'Block.make', (draft) =>
    FunctionBodyState.makeBlock(draft, name),
  )
})

/**
 * Directs subsequent instructions to this non-terminated block.
 *
 * @category blocks
 * @since 0.0.0
 */
export const setInsertionPoint = Effect.fn('Block.setInsertionPoint')(function* (
  body: FunctionBody.FunctionBody,
  self: Block,
): Effect.fn.Return<void, SilkError> {
  yield* FunctionBodyState.mutate(body, 'Block.setInsertionPoint', (draft) =>
    FunctionBodyState.setCursor(draft, self),
  )
})

/**
 * Returns the block's zero-based function-local index.
 *
 * @category blocks
 * @since 0.0.0
 */
export const index = Effect.fn('Block.index')(function* (
  body: FunctionBody.FunctionBody,
  self: Block,
): Effect.fn.Return<number, SilkError> {
  return yield* FunctionBodyState.mutate(body, 'Block.index', (draft) =>
    FunctionBodyState.resolveBlock(draft, self, 'Block.index'),
  )
})

/**
 * Reads the block's byte-exact optional label.
 *
 * @category blocks
 * @since 0.0.0
 */
export const name = Effect.fn('Block.name')(function* (
  body: FunctionBody.FunctionBody,
  self: Block,
): Effect.fn.Return<ByteString.ByteString, SilkError> {
  return yield* FunctionBodyState.mutate(body, 'Block.name', (draft) => {
    const block = draft.blocks[FunctionBodyState.resolveBlock(draft, self, 'Block.name')]
    if (block === undefined) {
      throw new SilkError({
        operation: 'Block.name',
        message: 'Block table entry is missing',
        cause: self,
      })
    }
    return block.name
  })
})

/**
 * Returns predecessor blocks discovered from committed terminators in stable index order.
 *
 * @category blocks
 * @since 0.0.0
 */
export const predecessors = Effect.fn('Block.predecessors')(function* (
  body: FunctionBody.FunctionBody,
  self: Block,
): Effect.fn.Return<ReadonlyArray<Block>, SilkError> {
  return yield* FunctionBodyState.mutate(body, 'Block.predecessors', (draft) => {
    const block = draft.blocks[FunctionBodyState.resolveBlock(draft, self, 'Block.predecessors')]
    if (block === undefined) {
      throw new SilkError({
        operation: 'Block.predecessors',
        message: 'Block table entry is missing',
        cause: self,
      })
    }
    return Object.freeze(
      [...block.predecessors].flatMap((index) => {
        const handle = draft.blockHandles[index]
        return handle === undefined ? [] : [handle]
      }),
    )
  })
})
