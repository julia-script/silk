import * as Effect from 'effect/Effect'
import * as Alias from '../dist/Alias.js'
import * as Attribute from '../dist/Attribute.js'
import * as Bitcode from '../dist/Bitcode.js'
import * as Builder from '../dist/Builder.js'
import * as Constant from '../dist/Constant.js'
import * as FunctionActor from '../dist/Function.js'
import * as IrText from '../dist/IrText.js'
import * as Type from '../dist/Type.js'
import * as Variable from '../dist/Variable.js'

const output = await Effect.runPromise(
  Effect.gen(function* () {
    const builder = yield* Builder.make({
      sourceFilename: 'decl.ll',
      dataLayout: 'e-p:64:64-i32:32',
      targetTriple: 'aarch64-unknown-linux',
    })
    const i32 = yield* Type.integer(builder, 32)
    const one = yield* Constant.integerUnsigned(builder, i32, 1)
    const answer = yield* Variable.make(builder, 'answer', i32, {
      initializer: one,
      constant: true,
      linkage: 'internal',
    })
    const answerPointer = yield* Constant.fromGlobal(
      builder,
      yield* Variable.global(builder, answer),
    )
    yield* Alias.make(builder, 'answer_alias', i32, answerPointer)
    const functionType = yield* Type.functionType(builder, i32, [i32])
    const nounwind = yield* Attribute.flag(builder, 'nounwind')
    const attributes = yield* Attribute.functionSet(builder, {
      functionAttributes: yield* Attribute.set(builder, [nounwind]),
    })
    yield* FunctionActor.declare(builder, 'compute', functionType, { attributes })
    return process.argv[2] === 'bitcode'
      ? Buffer.from(yield* Bitcode.encode(builder))
      : Buffer.from(yield* IrText.render(builder))
  }),
)

process.stdout.write(output)
