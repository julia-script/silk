import * as Attribute from '@silklang/llvm/Attribute'
import * as Block from '@silklang/llvm/Block'
import type * as Builder from '@silklang/llvm/Builder'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionActor from '@silklang/llvm/Function'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type { LlvmError } from '@silklang/llvm/LlvmError'
import * as Value from '@silklang/llvm/Value'
import * as Type from '@silklang/llvm/Type'
import * as Effect from 'effect/Effect'

/** Per-module fatal personality for the admitted native Itanium unwind ABI. */
export interface NativeForeignGuard {
  readonly personality: Constant.Constant
  readonly trap: FunctionActor.Function
}

/** Defines a personality that terminates in either phase before an exception can cross Silk. */
export const make = Effect.fn('NativeForeignGuard.make')(function* (
  builder: Builder.Builder,
): Effect.fn.Return<NativeForeignGuard, LlvmError> {
  const pointer = yield* Type.pointer(builder)
  const i32 = yield* Type.integer(builder, 32)
  const trap = yield* FunctionActor.declare(
    builder,
    'llvm.trap',
    yield* Type.functionType(builder, yield* Type.voidType(builder), []),
  )
  const personality = yield* FunctionActor.declare(
    builder,
    '__silk_foreign_personality',
    yield* Type.functionType(builder, i32, [
      i32,
      i32,
      yield* Type.integer(builder, 64),
      pointer,
      pointer,
    ]),
    {
      linkage: 'internal',
      attributes: yield* Attribute.functionSet(builder, {
        functionAttributes: yield* Attribute.set(builder, [
          yield* Attribute.flag(builder, 'noinline'),
          yield* Attribute.flag(builder, 'nounwind'),
          yield* Attribute.flag(builder, 'noreturn'),
        ]),
      }),
    },
  )
  yield* FunctionActor.buildBody(
    builder,
    personality,
    Effect.fnUntraced(function* (body) {
      yield* Block.make(body, 'entry')
      yield* FunctionBody.callDirect(body, trap, [])
      yield* FunctionBody.unreachable(body)
    }),
  )
  return Object.freeze({
    personality: yield* Constant.fromGlobal(
      builder,
      yield* FunctionActor.global(builder, personality),
    ),
    trap,
  })
})

/** Wraps one foreign symbol in a non-inlined frame with an enforced fatal unwind path. */
export const wrap = Effect.fn('NativeForeignGuard.wrap')(function* (
  self: NativeForeignGuard,
  builder: Builder.Builder,
  target: FunctionActor.Function,
  ordinal: number,
  parameterCount: number,
): Effect.fn.Return<FunctionActor.Function, LlvmError> {
  const properties = yield* FunctionActor.properties(builder, target)
  const groups =
    properties.attributes === undefined
      ? undefined
      : yield* Attribute.functionSetEntries(builder, properties.attributes)
  const functions =
    groups === undefined ? [] : yield* Attribute.entries(builder, groups.functionAttributes)
  const attributes = yield* Attribute.functionSet(builder, {
    ...groups,
    functionAttributes: yield* Attribute.set(builder, [
      ...functions,
      yield* Attribute.flag(builder, 'noinline'),
      yield* Attribute.flag(builder, 'nounwind'),
    ]),
  })
  const guard = yield* FunctionActor.declare(
    builder,
    `__silk_foreign_guard.${ordinal}`,
    properties.type,
    { linkage: 'internal', personality: self.personality, attributes },
  )
  const callee = yield* Constant.fromGlobal(builder, yield* FunctionActor.global(builder, target))
  yield* FunctionActor.buildBody(
    builder,
    guard,
    Effect.fnUntraced(function* (body) {
      yield* Block.make(body, 'entry')
      const normal = yield* Block.make(body, 'returned')
      const unwind = yield* Block.make(body, 'foreign_unwind')
      const args: Array<Value.Input> = []
      for (let index = 0; index < parameterCount; index += 1)
        args.push(yield* Value.argument(body, index))
      const result = yield* FunctionBody.invoke(
        body,
        properties.type,
        callee,
        args,
        normal,
        unwind,
        'result',
        properties.attributes === undefined ? {} : { attributes: properties.attributes },
      )
      yield* Block.setInsertionPoint(body, normal)
      if (result === undefined) yield* FunctionBody.returnVoid(body)
      else yield* FunctionBody.returnValue(body, result)
      yield* Block.setInsertionPoint(body, unwind)
      yield* FunctionBody.cleanupLandingPad(body, 'exception')
      yield* FunctionBody.callDirect(body, self.trap, [])
      yield* FunctionBody.unreachable(body)
    }),
  )
  return guard
})
