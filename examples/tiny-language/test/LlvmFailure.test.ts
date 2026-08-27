import { assert, it } from '@effect/vitest'
import * as Block from '@silklang/llvm/Block'
import * as Builder from '@silklang/llvm/Builder'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionActor from '@silklang/llvm/Function'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as IrText from '@silklang/llvm/IrText'
import * as Type from '@silklang/llvm/Type'
import * as Effect from 'effect/Effect'

it.effect('rolls back an unterminated body and permits a clean retry', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make({ sourceFilename: 'transaction.tiny' })
    const i32 = yield* Type.integer(builder, 32)
    const signature = yield* Type.functionType(builder, i32, [])
    const answer = yield* FunctionActor.declare(builder, 'answer', signature)

    const failure = yield* Effect.flip(
      FunctionActor.buildBody(
        builder,
        answer,
        Effect.fnUntraced(function* (body) {
          yield* Block.make(body, 'entry')
        }),
      ),
    )
    assert.strictEqual(failure._tag, 'LlvmError')
    assert.include(failure.operation, 'FunctionBody')
    assert.include(yield* IrText.render(builder), 'declare i32 @answer()')

    yield* FunctionActor.buildBody(
      builder,
      answer,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const value = yield* Constant.integerSigned(builder, i32, 42)
        yield* FunctionBody.returnValue(body, value)
      }),
    )
    const restored = yield* IrText.render(builder)
    assert.include(restored, 'define i32 @answer()')
    assert.include(restored, 'ret i32 42')
  }),
)
