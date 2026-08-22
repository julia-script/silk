import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
import * as Instances from '../src/Instances.js'
import * as Layout from '../src/Layout.js'
import * as LayoutEncode from '../src/LayoutEncode.js'
import * as Lower from '../src/Lower.js'
import * as Mir from '../src/Mir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'
import * as OpaqueRealization from '../src/OpaqueRealization.js'
import type * as Ownership from '../src/Ownership.js'
import * as Target from '../src/Target.js'
import { unreachable } from './support/raise.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const lowerStored = Effect.fnUntraced(function* (name: string, source: string) {
  const snapshot = yield* Analysis.ofSourceRealized(
    name,
    ascii(source),
    Target.wasm32UnknownUnknown.id,
  )
  const catalog = Layout.catalog(Target.wasm32UnknownUnknown, snapshot.index, snapshot.instances)
  const layout = Layout.plan(catalog, snapshot.instances, snapshot.index)
  const ownership = Analysis.ownershipOf(snapshot, name) ?? unreachable('expected module ownership')
  const module = Lower.lowerProgram(
    snapshot.instances,
    new Map<string, Ownership.ModuleOwnership>([[name, ownership]]),
    layout,
    snapshot.index,
    OpaqueRealization.catalogOf(snapshot),
  )
  return { module, layout }
})

it.effect(
  'carries one stored callable fact from construction through projection and invocation',
  () =>
    Effect.gen(function* () {
      const source = `struct Parser<F: fn(i32) -> i32> { parse: F }
fn decode(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 {
  let parser = Parser { parse: decode }
  return parser.parse(1)
}`
      const { module } = yield* lowerStored('stored-callable-mir/named', source)
      const operations = module.functions.flatMap(MirVerification.operations)
      const construction = operations.find((operation) => operation._tag === 'Construct')
      const invocation = operations.find((operation) => operation._tag === 'ApplyCallable')
      const projected =
        invocation?._tag === 'ApplyCallable' && invocation.callable !== undefined
          ? module.functions
              .flatMap((fn) => fn.localTypes)
              .find(
                (type) =>
                  type._tag === 'CallableValue' &&
                  type.storage !== undefined &&
                  type.storage.realization.target._tag === 'Declaration' &&
                  type.storage.realization.target.name === 'decode',
              )
          : undefined

      assert.strictEqual(construction?._tag, 'Construct')
      const constructionStorage =
        construction?._tag === 'Construct' ? construction.fields.at(0)?.stored : undefined
      assert.strictEqual(
        constructionStorage?._tag === 'StoredCallableField'
          ? constructionStorage.realization.target._tag
          : undefined,
        'Declaration',
      )
      assert.strictEqual(projected?._tag, 'CallableValue')
      assert.strictEqual(
        projected?._tag === 'CallableValue' ? projected.storage?._tag : undefined,
        'StoredCallableField',
      )
      assert.strictEqual(invocation?._tag, 'ApplyCallable')
      assert.strictEqual(
        invocation?._tag === 'ApplyCallable' ? invocation.realization : undefined,
        'Environment',
      )
      assert.deepEqual(MirVerification.verify(module), [])
    }),
)

it.effect('resolves stored owned-capture cleanup before MIR', () =>
  Effect.gen(function* () {
    const source = `struct Token { value: i32 }
struct Holder<F: once fn(i32) -> i32> { step: F }
fn read(value: i32, token: Token) -> i32 { return value }
pub fn main() -> i32 {
  let token = Token { value: 1 }
  let holder = Holder { step: read(move token) }
  return 0
}`
    const { module } = yield* lowerStored('stored-callable-mir/cleanup', source)
    const cleanups = module.functions
      .flatMap(MirVerification.operations)
      .flatMap((operation) => (operation._tag === 'Drop' ? [operation.cleanup] : []))

    assert.notInclude(
      cleanups.map((cleanup) => cleanup._tag),
      'RepresentedCallableCleanup',
    )
    assert.include(
      cleanups.flatMap((cleanup) =>
        cleanup._tag === 'StructCleanup'
          ? cleanup.fields.map((field) => field.cleanup._tag)
          : [cleanup._tag],
      ),
      'CallableCleanup',
    )
    assert.deepEqual(MirVerification.verify(module), [])
  }),
)

it.effect('keeps nested layouts, instance keys, symbols, and MIR text deterministic', () =>
  Effect.gen(function* () {
    const source = `import silk.i32 as i32
struct Parser<F: fn(i32) -> i32> { parse: F }
struct Boxed<F: fn(i32) -> i32> { inner: Parser<F> }
fn box<F: fn(i32) -> i32>(inner: Parser<F>) -> Boxed<F> {
  return Boxed<F> { inner: move inner }
}
pub fn main() -> i32 {
  let parser = Parser { parse: i32.add(1) }
  let boxed = box(move parser)
  return boxed.inner.parse(2)
}`
    const first = yield* lowerStored('stored-callable-mir/determinism', source)
    const second = yield* lowerStored('stored-callable-mir/determinism', source)
    const facts = (snapshot: typeof first) => ({
      layout: LayoutEncode.encode(snapshot.layout),
      instances: snapshot.module.functions.map((fn) => Instances.keyText(fn.instance)),
      symbols: snapshot.module.functions.map((fn) =>
        Backend.symbolFor(fn, Mir.machineEntry(snapshot.module)),
      ),
      mir: MirEncoding.encode(snapshot.module),
    })
    const encoded = facts(first)

    assert.deepEqual(encoded, facts(second))
    assert.include(encoded.instances.join('\n'), 'target=declaration:silk/i32.add')
    assert.include(encoded.symbols.join('\n'), 'silk_stored_callable_mir_determinism_box__')
    assert.include(encoded.mir, 'stored=silk/i32.add')
    assert.include(encoded.mir, 'read-place %5.#0.#0')
    assert.deepEqual(MirVerification.verify(first.module), [])
  }),
)
