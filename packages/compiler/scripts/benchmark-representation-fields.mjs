// Opt-in declaration-heavy planning probe. Build the workspace, then run in a fresh process.
// Arguments: unrelated declarations (default 1500), queries (default 2000).
import { createHash } from 'node:crypto'
import { performance } from 'node:perf_hooks'
import * as Effect from 'effect/Effect'
import * as Schema from 'effect/Schema'
import * as ModuleClosure from '../dist/ModuleClosure.js'
import * as NameResolution from '../dist/NameResolution.js'
import * as RepresentationField from '../dist/RepresentationField.js'
import * as SourceFile from '../dist/SourceFile.js'
import * as SourceResolver from '../dist/SourceResolver.js'
import * as Type from '../dist/Type.js'

const unrelated = Number(process.argv[2] ?? 1500)
const queries = Number(process.argv[3] ?? 2000)
if (!Number.isSafeInteger(unrelated) || unrelated < 0)
  throw new Error('unrelated must be a non-negative integer')
if (!Number.isSafeInteger(queries) || queries < 1)
  throw new Error('queries must be a positive integer')

const module = 'bench/representation-fields'
const source = `${Array.from(
  { length: unrelated },
  (_, index) => `struct Unrelated${index} { value: i32 }`,
).join('\n')}
struct Inner<F: fn<'static>(i32) -> i32> { operation: F }
struct Outer<F: fn<'static>(i32) -> i32> { first: Inner<F> second: Inner<F> }`

await Effect.runPromise(
  Effect.gen(function* () {
    const closure = yield* ModuleClosure.load({
      root: SourceFile.make(module, new TextEncoder().encode(source)),
    }).pipe(Effect.provide(SourceResolver.empty))
    const index = NameResolution.analyze(closure).index
    if (index.diagnostics.length > 0)
      return yield* Effect.die(new Error('benchmark source has declaration diagnostics'))
    const instance = Type.nominal(module, 'Outer')
    const query = () => ({
      plans: RepresentationField.plansOf(index, instance),
      fields: RepresentationField.resolveFields(index, [instance]),
    })
    const firstStart = performance.now()
    const first = query()
    const firstQueryMs = performance.now() - firstStart
    const start = performance.now()
    for (let i = 0; i < queries; i++) query()
    const queriesMs = performance.now() - start
    const encoded = yield* Schema.encodeEffect(Schema.fromJsonString(Schema.Unknown))(first)
    yield* Effect.log({
      unrelated,
      queries,
      firstQueryMs,
      queriesMs,
      resultSha256: createHash('sha256').update(encoded).digest('hex'),
    })
  }),
)
