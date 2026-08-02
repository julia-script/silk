import * as Effect from 'effect/Effect'
import * as Bitcode from '../dist/Bitcode.js'
import * as Builder from '../dist/Builder.js'
import * as IrText from '../dist/IrText.js'

const result = await Effect.runPromise(
  Effect.gen(function* () {
    const builder = yield* Builder.make({ sourceFilename: 'smoke.ll' })
    const text = yield* IrText.render(builder)
    const bitcode = yield* Bitcode.encode(builder)
    return {
      text,
      bitcode: Buffer.from(bitcode).toString('hex'),
    }
  }),
)

process.stdout.write(JSON.stringify(result))
