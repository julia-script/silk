import { writeFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { performance } from 'node:perf_hooks'
import { fileURLToPath } from 'node:url'
import * as Effect from 'effect/Effect'
import * as Bitcode from '../dist/Bitcode.js'
import * as Block from '../dist/Block.js'
import * as Builder from '../dist/Builder.js'
import * as Constant from '../dist/Constant.js'
import * as FunctionActor from '../dist/Function.js'
import * as FunctionBody from '../dist/FunctionBody.js'
import * as Metadata from '../dist/Metadata.js'
import * as Type from '../dist/Type.js'

const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const samples = Number(process.env.PARITY_BENCHMARK_SAMPLES ?? 7)

const median = (values) =>
  [...values].sort((left, right) => left - right)[Math.floor(values.length / 2)]

const measure = async (name, workload) => {
  await workload()
  const durations = []
  for (let index = 0; index < samples; index += 1) {
    const start = performance.now()
    await workload()
    durations.push(performance.now() - start)
  }
  return {
    name,
    samples: durations.map((value) => Number(value.toFixed(3))),
    medianMs: Number(median(durations).toFixed(3)),
  }
}

const makeFunction = Effect.fn('ParityBenchmark.makeFunction')(
  function* (
    /** @type {Builder.Builder} */ builder,
    /** @type {string} */ name,
    /** @type {number} */ instructionCount,
  ) {
    const i32 = yield* Type.integer(builder, 32)
    const signature = yield* Type.functionType(builder, i32, [])
    const fn = yield* FunctionActor.declare(builder, name, signature)
    const zero = yield* Constant.integerUnsigned(builder, i32, 0)
    const one = yield* Constant.integerUnsigned(builder, i32, 1)
    yield* FunctionActor.buildBody(
      builder,
      fn,
      Effect.fn('ParityBenchmark.functionBody')(
        function* (/** @type {FunctionBody.FunctionBody} */ body) {
          yield* Block.make(body, 'entry')
          let value = zero
          for (let index = 0; index < instructionCount; index += 1) {
            value = yield* FunctionBody.binary(body, 'add', value, one, `value.${index}`)
          }
          yield* FunctionBody.returnValue(body, value)
        },
      ),
    )
  },
)

const workloads = {
  'interning-heavy': () =>
    Effect.runPromise(
      Effect.gen(function* () {
        const builder = yield* Builder.make()
        for (let index = 0; index < 4_000; index += 1) {
          const type = yield* Type.integer(builder, (index % 256) + 1)
          yield* Constant.integerUnsigned(builder, type, BigInt(index % 2))
        }
        yield* Bitcode.encode(builder)
      }),
    ),
  'one-large-function': () =>
    Effect.runPromise(
      Effect.gen(function* () {
        const builder = yield* Builder.make()
        yield* makeFunction(builder, 'large', 1_000)
        yield* Bitcode.encode(builder)
      }),
    ),
  'many-small-functions': () =>
    Effect.runPromise(
      Effect.gen(function* () {
        const builder = yield* Builder.make()
        for (let index = 0; index < 150; index += 1) {
          yield* makeFunction(builder, `small.${index}`, 2)
        }
        yield* Bitcode.encode(builder)
      }),
    ),
  'control-flow-heavy': () =>
    Effect.runPromise(
      Effect.gen(function* () {
        const builder = yield* Builder.make()
        const i32 = yield* Type.integer(builder, 32)
        const signature = yield* Type.functionType(builder, i32, [])
        const fn = yield* FunctionActor.declare(builder, 'cfg', signature)
        const zero = yield* Constant.integerUnsigned(builder, i32, 0)
        yield* FunctionActor.buildBody(
          builder,
          fn,
          Effect.fn('ParityBenchmark.controlFlowBody')(
            function* (/** @type {FunctionBody.FunctionBody} */ body) {
              const blocks = []
              for (let index = 0; index < 250; index += 1) {
                blocks.push(yield* Block.make(body, `block.${index}`))
              }
              for (let index = 0; index < blocks.length - 1; index += 1) {
                yield* Block.setInsertionPoint(body, blocks[index])
                yield* FunctionBody.branch(body, blocks[index + 1])
              }
              yield* Block.setInsertionPoint(body, blocks[blocks.length - 1])
              yield* FunctionBody.returnValue(body, zero)
            },
          ),
        )
        yield* Bitcode.encode(builder)
      }),
    ),
  'metadata-heavy': () =>
    Effect.runPromise(
      Effect.gen(function* () {
        const builder = yield* Builder.make({ strip: false })
        const nodes = []
        for (let index = 0; index < 2_000; index += 1) {
          nodes.push(
            yield* Metadata.tuple(builder, [yield* Metadata.string(builder, `node.${index}`)]),
          )
        }
        yield* Metadata.named(builder, 'benchmark.nodes', nodes)
        yield* Bitcode.encode(builder)
      }),
    ),
  'large-blobs': () =>
    Effect.runPromise(
      Effect.gen(function* () {
        const builder = yield* Builder.make()
        yield* Builder.appendModuleAssembly(builder, `;${'x'.repeat(1_000_000)}`)
        yield* Bitcode.encode(builder)
      }),
    ),
}

const instructionCandidate = (wrapper) => () =>
  Effect.runPromise(
    Effect.gen(function* () {
      const builder = yield* Builder.make()
      yield* makeFunction(builder, 'candidate', 500)
      yield* wrapper(function* () {
        return yield* Bitcode.encode(builder)
      })()
    }),
  )

const traced = (body) => Effect.fn('ParityBenchmark.bitstreamCandidate')(body)
const untraced = (body) => Effect.fnUntraced(body)

const results = []
for (const [name, workload] of Object.entries(workloads))
  results.push(await measure(name, workload))
const candidates = [
  await measure('bitstream-and-instruction-loop/traced', instructionCandidate(traced)),
  await measure('bitstream-and-instruction-loop/untraced', instructionCandidate(untraced)),
]
const tracedMedian = candidates[0].medianMs
const untracedMedian = candidates[1].medianMs
const improvementPercent = Number(
  (((tracedMedian - untracedMedian) / tracedMedian) * 100).toFixed(2),
)
const output = {
  schemaVersion: 1,
  runtime: process.version,
  architecture: process.arch,
  sampleCount: samples,
  workloads: results,
  candidates,
  decision: {
    materialThresholdPercent: 10,
    improvementPercent,
    retainEffectFnUntraced: improvementPercent >= 10,
    productionDecision:
      'Production actor operations remain Effect.fn; byte packing stays imperative behind Bitcode.encode.',
  },
}

if (process.argv.includes('--write')) {
  writeFileSync(
    resolve(packageRoot, 'parity/BENCHMARKS.json'),
    `${JSON.stringify(output, undefined, 2)}\n`,
  )
}
process.stdout.write(`${JSON.stringify(output)}\n`)
