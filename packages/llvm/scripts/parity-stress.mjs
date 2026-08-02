import * as Effect from 'effect/Effect'
import * as Bitcode from '../dist/Bitcode.js'
import * as Block from '../dist/Block.js'
import * as Builder from '../dist/Builder.js'
import * as Constant from '../dist/Constant.js'
import * as FunctionActor from '../dist/Function.js'
import * as FunctionBody from '../dist/FunctionBody.js'
import * as Metadata from '../dist/Metadata.js'
import * as Type from '../dist/Type.js'

const before = process.memoryUsage().heapUsed
const result = await Effect.runPromise(
  Effect.gen(function* () {
    const builder = yield* Builder.make({ strip: false })
    const widest = yield* Type.integer(builder, 0xff_ffff)
    const i32 = yield* Type.integer(builder, 32)
    const zero = yield* Constant.integerUnsigned(builder, i32, 0)
    const one = yield* Constant.integerUnsigned(builder, i32, 1)
    const signature = yield* Type.functionType(builder, i32, [])
    const fn = yield* FunctionActor.declare(builder, 'stress', signature)
    yield* FunctionActor.buildBody(
      builder,
      fn,
      Effect.fn('ParityStress.body')(function* (body) {
        yield* Block.make(body, 'entry')
        let value = zero
        for (let index = 0; index < 5_000; index += 1) {
          value = yield* FunctionBody.binary(body, 'add', value, one)
        }
        yield* FunctionBody.returnValue(body, value)
      }),
    )

    let tail = yield* Metadata.tuple(builder)
    for (let index = 0; index < 5_000; index += 1)
      tail = yield* Metadata.distinctTuple(builder, [tail])
    yield* Metadata.named(builder, 'stress.chain', [tail])

    const forward = yield* Metadata.forward(builder, 'node')
    if (forward === undefined) throw new Error('metadata forwards are disabled')
    const cycle = yield* Metadata.distinctTuple(builder, [forward])
    yield* Metadata.resolveForward(builder, forward, cycle)
    yield* Metadata.named(builder, 'stress.cycle', [cycle])

    return { widest: widest.index, bytes: (yield* Bitcode.encode(builder)).byteLength }
  }),
)
const heapGrowthBytes = process.memoryUsage().heapUsed - before
if (heapGrowthBytes > 512 * 1024 * 1024)
  throw new Error(`stress heap grew by ${heapGrowthBytes} bytes`)
process.stdout.write(`${JSON.stringify({ ...result, heapGrowthBytes })}\n`)
