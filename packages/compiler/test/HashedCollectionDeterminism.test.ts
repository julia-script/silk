import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

/**
 * One `HashSeed` fixes one iteration order — in every engine.
 *
 * A test that builds one collection twice inside one engine does not establish this. It would pass
 * for a map whose order followed allocation addresses, as long as the addresses repeated, and it
 * would pass for a map whose bucket index narrowed a 64-bit hash through a pointer-wide remainder,
 * which answers one way where `usize` is 64 bits and another where it is 32.
 *
 * So the order is pinned as a value: each program folds the keys in the order the collection
 * presents them into one number, checks that number against the order this seed is known to give,
 * and answers 42 only if it matches. That answer is then required from the bootstrap evaluator, from
 * the direct WebAssembly backend — where `usize` is 32 bits — and from native LLVM, where it is 64.
 * Three engines answering 42 is three runs agreeing on one order, and agreeing with the recorded one.
 */

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyzed = (name: string, source: string, target?: string) =>
  Analysis.ofSourceRealized(name, ascii(source), target)

const messages = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.message)

const describe = (outcome: unknown): string =>
  JSON.stringify(outcome, (_, value) => (typeof value === 'bigint' ? value.toString() : value))

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-hashed-determinism-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

const threeEngineValue = (name: string, source: string, artifact: string) =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed(name, source, 'wasm32-unknown-unknown')
    assert.deepEqual(messages(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
    const bootstrap = evaluated._tag === 'Completed' ? evaluated.result.value : undefined

    // The same snapshot evaluated a second time: one engine, two runs, before the other two engines
    // are asked the same question.
    const repeated = Analysis.evaluate(snapshot)
    assert.strictEqual(repeated._tag, 'Completed', describe(repeated))
    const again = repeated._tag === 'Completed' ? repeated.result.value : undefined

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    const direct = (instance.exports.silk_main as () => number)()

    const compiled = yield* Driver.compile({
      compilation: { root: SourceFile.make(name, ascii(source)) },
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
      profile: 'release',
      destination: join(destinationRoot, artifact),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(compiled._tag, 'Compiled')
    const run =
      compiled._tag === 'Compiled'
        ? spawnSync(compiled.path, [], { encoding: 'utf8' })
        : { status: undefined, stderr: 'native compilation did not produce an artifact' }

    return Object.freeze({ bootstrap, again, direct, native: run.status, stderr: run.stderr })
  })

const agrees = (
  outcome: { bootstrap: unknown; again: unknown; direct: unknown; native: unknown; stderr: string },
  expected: number,
) => {
  assert.strictEqual(outcome.bootstrap, expected, 'bootstrap evaluator')
  assert.strictEqual(outcome.again, expected, 'bootstrap evaluator, second run')
  assert.strictEqual(outcome.direct, expected, 'direct WebAssembly')
  assert.strictEqual(outcome.native, expected, `native LLVM: ${outcome.stderr}`)
}

/**
 * The order twelve keys take under each seed, as a digest of the whole 64-bit fold. Pinned rather
 * than merely compared between engines: agreement alone would survive a change that made every
 * engine wrong in the same new way, and a program that records this map's order needs the order
 * itself to be stable, not just uniform.
 */
const ORDER_UNDER_12345 = 971199974
const ORDER_UNDER_6789 = 434552010

const mapImports = `import silk.hash { HashKey, HashSeed, Word }
import silk.hash_map { HashMap, bucketCount, insert, keyAt, length, make, occupiedAt }
import silk.option { Option, Some, None }`

/**
 * Folds the keys in bucket order into one number. The fold is order-sensitive — a key's contribution
 * depends on how many keys follow it — so two different orders over one set of keys are two
 * different numbers, which a test that only compared the set of keys would miss.
 */
const foldOrder = (map: string, into: string): string =>
  `  let mut ${into}Index = usize.ZERO
  let mut ${into} = u64.toU64(0)
  while ${into}Index < bucketCount<Word, i32>(&${map}) {
    if occupiedAt<Word, i32>(&${map}, ${into}Index) {
      let held = keyAt<Word, i32>(&${map}, ${into}Index)
      ${into} = u64.wrappingAdd(u64.wrappingMultiply(${into}, 131), held.value)
    }
    ${into}Index = ${into}Index + usize.ONE
  }`

/** Twelve keys spread far enough apart that they do not land on consecutive buckets. */
const fill = (map: string): string =>
  `  let mut ${map}Key = 0
  while ${map}Key < 12 {
    let previous = run insert<Word, i32>(&mut ${map}, HashKey.word(i32.toU64(${map}Key * 7 + 1)), ${map}Key)
      |> Effect.provideMut(&mut allocator)
    drop previous
    ${map}Key = ${map}Key + 1
  }`

/**
 * The fold is compared to its expected value inside the program rather than returned, because a
 * native run reports its answer as a process exit status and only the low byte of one survives.
 * Comparing in Silk keeps the whole 64-bit fold in the assertion; each engine answers 42 only if it
 * folded the very same order.
 */
const ordered = (seed: number, digest: number): string =>
  `${mapImports}

effect fn build() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let mut map = make<Word, i32>(HashKey.seed(${seed}))
${fill('map')}
${foldOrder('map', 'folded')}
  if u64.toI32(u64.remainder(folded, 1000000007)) != ${digest} { return 1 }
  return 42
}

effect fn recover(error: OutOfMemory) -> i32 { return 99 }

pub fn main() -> i32 { return run Effect.catch(build(), recover) }`

it.effect(
  'presents one order under one seed on every engine',
  () =>
    Effect.gen(function* () {
      // The number itself is pinned, not merely the agreement. Agreement alone would survive a
      // change that made every engine wrong in the same new way; the constant makes an order change
      // visible as a change, which is what a program that stores this map's order in a file needs.
      const outcome = yield* threeEngineValue(
        'hashed-determinism/seeded-order',
        ordered(12345, ORDER_UNDER_12345),
        'seeded-order',
      )
      agrees(outcome, 42)
    }),
  60_000,
)

it.effect(
  'presents a different order under a different seed',
  () =>
    Effect.gen(function* () {
      // The seed is what fixes the order, rather than the order being fixed by the insertion
      // sequence and the seed riding along unused: one changed seed, one changed order, and the
      // three engines still agree with each other.
      const outcome = yield* threeEngineValue(
        'hashed-determinism/other-seed',
        ordered(6789, ORDER_UNDER_6789),
        'other-seed',
      )
      agrees(outcome, 42)
      assert.notStrictEqual(ORDER_UNDER_6789, ORDER_UNDER_12345, 'the seed changed the order')
    }),
  60_000,
)
