// Opt-in construction/GC probe: node --expose-gc scripts/construction-benchmark.mjs [functions] [instructions]
import { performance } from 'node:perf_hooks'
import * as Effect from 'effect/Effect'
import * as Block from '../dist/Block.js'
import * as Builder from '../dist/Builder.js'
import * as Constant from '../dist/Constant.js'
import * as FunctionActor from '../dist/Function.js'
import * as FunctionBody from '../dist/FunctionBody.js'
import * as Type from '../dist/Type.js'

const functionCount = Number(process.argv[2] ?? 100)
const instructionCount = Number(process.argv[3] ?? 1000)
if (![functionCount, instructionCount].every((value) => Number.isSafeInteger(value) && value > 0))
  throw new Error('function and instruction counts must be positive integers')
if (globalThis.gc === undefined) throw new Error('run this benchmark with node --expose-gc')
const start = performance.now()
const report = (phase) => {
  const before = performance.now()
  globalThis.gc?.()
  return {
    phase,
    elapsedMs: performance.now() - start,
    gcMs: performance.now() - before,
    heapBytes: process.memoryUsage().heapUsed,
  }
}
const retained = await Effect.runPromise(
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const i32 = yield* Type.integer(builder, 32)
    const signature = yield* Type.functionType(builder, i32, [])
    const zero = yield* Constant.integerUnsigned(builder, i32, 0)
    const one = yield* Constant.integerUnsigned(builder, i32, 1)
    const handles = []
    for (let index = 0; index < functionCount; index += 1) {
      const fn = yield* FunctionActor.declare(builder, `fn.${index}`, signature)
      yield* FunctionActor.buildBody(
        builder,
        fn,
        Effect.fnUntraced(function* (body) {
          yield* Block.make(body, 'entry')
          let value = zero
          for (let ordinal = 0; ordinal < instructionCount; ordinal += 1) {
            value = yield* FunctionBody.binary(body, 'add', value, one, `value.${ordinal}`)
          }
          handles.push(value)
          yield* FunctionBody.returnValue(body, value)
        }),
      )
    }
    yield* Effect.log(report('committed-with-one-handle-per-function'))
    return { builder, handles }
  }),
)
Effect.runSync(Effect.log(report('completed')))
retained.handles.length = 0
Effect.runSync(Effect.log(report('released-handles')))
// Keep the committed module alive through both GC probes.
Effect.runSync(Effect.log({ builder: retained.builder._tag, functionCount, instructionCount }))
