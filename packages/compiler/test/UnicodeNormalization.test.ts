import { readFileSync } from 'node:fs'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

/**
 * The acceptance criteria of #42's normalization half, asserted on returned values.
 *
 * Every program here returns a number the test checks on the bootstrap evaluator and compiled
 * WebAssembly, so a passing case means the value came back, not that compilation finished. Native
 * execution parity is carried by the differential corpus in `DriverNativeAcceptance.test.ts`.
 */

const packageRoot = join(dirname(fileURLToPath(import.meta.url)), '..')

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const diagnosticSummary = (snapshot: Analysis.Snapshot) =>
  Analysis.diagnostics(snapshot).map((diagnostic) => `${diagnostic.code} ${diagnostic.message}`)

/** Runs one program on the evaluator and on compiled wasm. */
const onEveryEngine = (name: string, source: string, expected: number) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(name, ascii(source), 'wasm32-unknown-unknown')
    assert.deepEqual(diagnosticSummary(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', `${name} did not evaluate`)
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(
      evaluated.result.value,
      BigInt(expected),
      `${name} on the bootstrap evaluator`,
    )

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual(
      (instance.exports.silk_main as () => number)(),
      expected,
      `${name} on WebAssembly`,
    )
  })

/**
 * The scenario at `openspec/specs/bootstrap-string/spec.md:113-115`, which had no path to equality
 * before this module existed: the two spellings are unequal, and normalizing makes them equal.
 */
const spelledTwoWays = `import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effects as Effect
import silk.string { String, view }
import silk.unicode { normalizeNfc }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let precomposed = "\\u{e9}"
  let decomposed = "e\\u{301}"

  // Before normalizing, ordinary equality reports them unequal, which is the guarantee this issue
  // must not disturb.
  if precomposed == decomposed { return 1 }

  // After normalizing, each is the precomposed spelling by that same exact equality, so the two
  // are equal to each other. Each owner is read before the next is built: a wasm backend defect
  // unrelated to Unicode invalidates the first of two live owned Strings, and the direct
  // owner-to-owner comparison is asserted below on the engines where it is sound.
  let composedLeft = run normalizeNfc(precomposed) |> Effect.provideMut(&mut allocator)
  if view(&composedLeft) == precomposed {} else { return 2 }
  let composedRight = run normalizeNfc(decomposed) |> Effect.provideMut(&mut allocator)
  if view(&composedRight) == precomposed {} else { return 3 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'makes a precomposed and a decomposed é equal after NFC and unequal before it',
  () => onEveryEngine('unicode/spelled-two-ways', spelledTwoWays, 42),
  120_000,
)

/**
 * The same scenario asserted the direct way: the two normalized owners compared with each other.
 *
 * This runs on the bootstrap evaluator here — and natively through the corpus entry
 * `unicode-compared-directly` in `DriverNativeAcceptance.test.ts` — but not on WebAssembly,
 * because two owned `String` views compared in one expression give the wrong answer there. That
 * defect has nothing to do with normalization — `String.copy("abc")` twice and comparing the two
 * views reproduces it with no Unicode in the program — so it is reported separately rather than
 * worked around silently here.
 */
const comparedDirectly = `import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effects as Effect
import silk.string { String, view }
import silk.unicode { normalizeNfc }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let left = run normalizeNfc("\\u{e9}") |> Effect.provideMut(&mut allocator)
  let right = run normalizeNfc("e\\u{301}") |> Effect.provideMut(&mut allocator)
  if view(&left) == view(&right) {} else { return 1 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'compares the two normalized owners directly on the evaluator',
  () =>
    Effect.gen(function* () {
      const name = 'unicode/compared-directly'
      const snapshot = yield* Analysis.ofSourceRealized(name, ascii(comparedDirectly))
      assert.deepEqual(diagnosticSummary(snapshot), [])
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)
    }),
  120_000,
)

const decomposing = `import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effects as Effect
import silk.string { String, view, ownedByteLength }
import silk.unicode { normalizeNfd }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()

  // A precomposed scalar becomes its base plus its combining mark.
  let simple = run normalizeNfd("\\u{e9}") |> Effect.provideMut(&mut allocator)
  if view(&simple) == "e\\u{301}" {} else { return 1 }
  if ownedByteLength(&simple) == 3 {} else { return 2 }

  // A scalar whose decomposition recurses: U+1E14 is E-macron-grave, and the macron form itself
  // decomposes, so the full form is three scalars rather than two.
  let nested = run normalizeNfd("\\u{1e14}") |> Effect.provideMut(&mut allocator)
  if view(&nested) == "E\\u{304}\\u{300}" {} else { return 3 }

  // Two marks on one base come back in combining-class order regardless of how they were written.
  let reordered = run normalizeNfd("q\\u{307}\\u{323}") |> Effect.provideMut(&mut allocator)
  if view(&reordered) == "q\\u{323}\\u{307}" {} else { return 4 }

  // Hangul decomposes arithmetically rather than through the table.
  let hangul = run normalizeNfd("\\u{ac01}") |> Effect.provideMut(&mut allocator)
  if view(&hangul) == "\\u{1100}\\u{1161}\\u{11a8}" {} else { return 5 }

  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'decomposes to the canonical scalar sequence in canonical order',
  () => onEveryEngine('unicode/decomposing', decomposing, 42),
  120_000,
)

const composing = `import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effects as Effect
import silk.string { String, view }
import silk.unicode { normalizeNfc }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()

  // Recomposition puts a decomposed sequence back together.
  let simple = run normalizeNfc("E\\u{304}\\u{300}") |> Effect.provideMut(&mut allocator)
  if view(&simple) == "\\u{1e14}" {} else { return 1 }

  // A composition exclusion stays decomposed: U+0958 has a canonical decomposition but is listed
  // in CompositionExclusions, so NFC must not rebuild it.
  let excluded = run normalizeNfc("\\u{915}\\u{93c}") |> Effect.provideMut(&mut allocator)
  if view(&excluded) == "\\u{915}\\u{93c}" {} else { return 2 }

  // A singleton decomposition is likewise never recomposed: U+212B ANGSTROM SIGN decomposes to
  // U+00C5 and NFC leaves it there.
  let singleton = run normalizeNfc("\\u{212b}") |> Effect.provideMut(&mut allocator)
  if view(&singleton) == "\\u{c5}" {} else { return 3 }

  // A mark blocked from its starter by an equal-or-higher combining class is not absorbed.
  let blocked = run normalizeNfc("q\\u{323}\\u{307}") |> Effect.provideMut(&mut allocator)
  if view(&blocked) == "q\\u{323}\\u{307}" {} else { return 4 }

  // Hangul composes arithmetically, jamo by jamo.
  let hangul = run normalizeNfc("\\u{1100}\\u{1161}\\u{11a8}") |> Effect.provideMut(&mut allocator)
  if view(&hangul) == "\\u{ac01}" {} else { return 5 }

  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'composes canonically and respects composition exclusions',
  () => onEveryEngine('unicode/composing', composing, 42),
  120_000,
)

/**
 * Ordinary `string` equality is untouched by this work.
 *
 * The check is deliberately about strings this module never sees: normalization has to be something
 * source asks for, so a program that does not ask for it must compare exactly the same bytes it
 * always did.
 */
const exactEquality = `pub fn main() -> i32 {
  // Canonically equivalent spellings stay unequal.
  if "\\u{e9}" == "e\\u{301}" { return 1 }
  // So do compatibility-equivalent ones, which are out of scope entirely.
  if "\\u{fb01}" == "fi" { return 2 }
  // And case differences, which no equality folds.
  if "SILK" == "silk" { return 3 }
  // Marks in a different order are a different string until something reorders them.
  if "q\\u{307}\\u{323}" == "q\\u{323}\\u{307}" { return 4 }
  // Identical bytes stay equal, and inequality is not simply always true.
  if "\\u{e9}" == "\\u{e9}" {} else { return 5 }
  if "e\\u{301}" != "e\\u{301}" { return 6 }
  return 42
}`

it.effect(
  'leaves ordinary string equality exact and normalization-free',
  () => onEveryEngine('unicode/exact-equality', exactEquality, 42),
  120_000,
)

/**
 * The Unicode data version is a named value a program can read and a test can check on its own,
 * which is requirement 6. It is data: nothing about a type or an ABI mentions it.
 */
it.effect('names the Unicode data version and checks it on its own', () =>
  Effect.gen(function* () {
    const source = `import silk.usize as usize
import silk.unicode { dataVersion }
import silk.string { byteLength }

pub fn main() -> i32 {
  if dataVersion() == "17.0.0" {} else { return 1 }
  return usize.toI32(byteLength(dataVersion()))
}`
    const snapshot = yield* Analysis.ofSourceRealized('unicode/data-version', ascii(source))
    assert.deepEqual(diagnosticSummary(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 6n)

    // And the version the module reports is the version the vendored database was pinned at, so the
    // constant cannot drift away from the data behind it.
    const pinned = readFileSync(join(packageRoot, 'unicode-data', 'VERSION'), 'utf8').trim()
    assert.strictEqual(pinned, '17.0.0')
  }),
)

/**
 * The decomposer's pending buffer is a fixed four slots, which is only safe while no scalar in the
 * pinned database decomposes further than that. A Unicode version that decomposed more deeply would
 * fail here rather than silently overrun.
 */
it.effect('bounds the decomposition buffer against the generated tables', () =>
  Effect.gen(function* () {
    const source = `import silk.usize as usize
import silk.unicode { longestDecomposition }

pub fn main() -> i32 {
  return usize.toI32(longestDecomposition())
}`
    const snapshot = yield* Analysis.ofSourceRealized('unicode/longest', ascii(source))
    assert.deepEqual(diagnosticSummary(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    const longest = Number(evaluated.result.value)
    assert.strictEqual(longest, 4)

    const module = readFileSync(join(packageRoot, 'stdlib', 'silk', 'unicode.silk'), 'utf8')
    const capacity = /const PENDING_CAPACITY: usize = (\d+)/.exec(module)
    assert.isNotNull(capacity)
    assert.isAtLeast(
      Number(capacity?.[1]),
      longest,
      'the pending buffer is smaller than the longest decomposition in the tables',
    )
  }),
)

/**
 * Normalization allocates, so it declares that it allocates. A caller with no memory to give gets
 * the existing typed failure back rather than a partial result.
 */
const allocationFailure = `import silk.core { Allocator }
import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effects as Effect
import silk.layout { Layout }
import silk.string { String, view }
import silk.unicode { normalizeNfc }

struct QuotaAllocator { remaining: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  if self.remaining == 0 { fail OutOfMemoryError {} }
  self.remaining = self.remaining - 1
  let mut inner = SystemAllocator.make()
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
  return run pending
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = QuotaAllocator { remaining: 0 }
  let composing = normalizeNfc("e\\u{301}") |> Effect.provideMut(&mut allocator)
  let composed = run composing
  return 1
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 42 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('reports the typed allocation failure rather than a partial normalization', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'unicode/allocation-failure',
      ascii(allocationFailure),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(diagnosticSummary(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)
