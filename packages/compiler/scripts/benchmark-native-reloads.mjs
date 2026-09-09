// Opt-in cold backend probe: build the workspace, then run this script with an optional size.
import { performance } from 'node:perf_hooks'
import * as Effect from 'effect/Effect'
import * as Analysis from '../dist/Analysis.js'
import * as SourceFile from '../dist/SourceFile.js'
import * as SourceResolver from '../dist/SourceResolver.js'

const size = Number(process.argv[2] ?? 40)
if (!Number.isSafeInteger(size) || size < 1) throw new Error('size must be a positive integer')

// These locals are dead before the later joins. Reloading every mutable root at every join
// creates quadratic IR growth even though the source and useful work grow linearly.
const source = `pub fn main() -> i32 {
  ${Array.from(
    { length: size },
    (_, index) => `let mut x${index} = ${index} x${index} = x${index} + 1 drop x${index}`,
  ).join('\n')}
  let mut result = 0
  ${Array.from({ length: size }, (_, index) => `if result < ${index} { result = result + 1 }`).join(
    '\n',
  )}
  return result
}`

await Effect.runPromise(
  Effect.gen(function* () {
    const start = performance.now()
    const snapshot = yield* Analysis.makeRealized({
      root: SourceFile.make('bench/reloads', new TextEncoder().encode(source)),
      configuration: {
        profile: { target: 'aarch64-apple-darwin', artifact: 'object', runtime: { kind: 'none' } },
        composition: {
          runtimes: [],
          defaults: [],
          retention: [{ module: 'bench/reloads', declaration: 'main' }],
          requirements: [],
          components: [],
        },
      },
    }).pipe(Effect.provide(SourceResolver.empty))
    const diagnostics = Analysis.diagnostics(snapshot)
    if (diagnostics.length > 0)
      return yield* Effect.die(
        new Error('benchmark source has diagnostics', { cause: diagnostics }),
      )
    const analyzed = performance.now()
    const artifact = yield* Analysis.codegen(snapshot, { mode: 'release' })
    yield* Effect.log({
      size,
      analysisMs: analyzed - start,
      codegenMs: performance.now() - analyzed,
      loads: artifact.ir.split('\n').filter((line) => line.includes(' = load ')).length,
      irBytes: Buffer.byteLength(artifact.ir),
      heapBytes: process.memoryUsage().heapUsed,
    })
  }),
)
