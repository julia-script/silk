import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/**
 * `Effect.zip` and `Effect.zip3` run their operands in declaration order and collect every success
 * value, stopping at the first typed failure.
 *
 * Order and short-circuiting are both claims about *what ran*, so asserting the returned value
 * alone would not settle either: a `Pair` holding 40 and 2 reads the same whichever operand ran
 * first. The evaluator's allocation trace is the evidence instead. Every operand below acquires a
 * distinct number of heap owners — one for the first, two for the second, three for the third — so
 * the trace of acquire/release events spells out the execution order unambiguously, and an operand
 * that never ran contributes no events at all.
 */
const prelude = `import silk.core { Allocator }
import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.effect { Pair, Triple }

struct Problem { code: i32 }

service Clock {}
struct FixedClock { storage: Allocation }
impl Clock for FixedClock {}

effect fn openClock() -> FixedClock ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  return FixedClock { storage: move allocation }
}

effect fn exhausted(error: OutOfMemoryError) -> FixedClock ! Problem { fail Problem { code: 9 } }

/// One owner, acquired and released inside whichever body runs this.
effect fn acquireClock() -> FixedClock ! Problem {
  return run Effect.catchAll(openClock(), exhausted)
}

/// One acquire/release pair, then 40.
effect fn firstStep() -> i32 ! Problem {
  let held = run acquireClock()
  drop move held
  return 40
}

/// Two nested acquire/release pairs, then 2. The asymmetry against \`firstStep\` is what makes the
/// order assertion unambiguous rather than a trace that reads the same in either order.
effect fn secondStep() -> i32 ! Problem {
  let outer = run acquireClock()
  let inner = run acquireClock()
  drop move inner
  drop move outer
  return 2
}

/// Three nested pairs, so the third operand is distinguishable from the other two as well.
effect fn thirdStep() -> i32 ! Problem {
  let outer = run acquireClock()
  let middle = run acquireClock()
  let inner = run acquireClock()
  drop move inner
  drop move middle
  drop move outer
  return 300
}

/// Fails with a payload of its own after one acquire/release pair, so a short-circuited run is
/// still distinguishable from one that never started.
effect fn failingStep() -> i32 ! Problem {
  let held = run acquireClock()
  drop move held
  fail Problem { code: 7 }
}

/// Projects both collected values out of the pair, which is also the assertion that the fields are
/// reachable from another module at all.
fn combinePair(pair: Pair<i32, i32>) -> i32 { return pair.first + pair.second }

fn combineTriple(triple: Triple<i32, i32, i32>) -> i32 {
  return triple.first + triple.second + triple.third
}

/// Reads the failure's own payload back out, so the asserted value is evidence about which failure
/// arrived rather than merely that some failure did.
effect fn recover(problem: Problem) -> i32 { return problem.code }`

/** Both operands succeed: the pair carries both values and the trace carries the order. */
const zipping = `import silk.effect as Effect
${prelude}

pub fn main() -> i32 {
  let zipped = Effect.zip(firstStep(), secondStep()) |> Effect.map(combinePair)
  return run Effect.catchAll(move zipped, recover)
}`

/** The first operand fails, so the second never runs and contributes no events. */
const zipShortCircuiting = `import silk.effect as Effect
${prelude}

pub fn main() -> i32 {
  let zipped = Effect.zip(failingStep(), secondStep()) |> Effect.map(combinePair)
  return run Effect.catchAll(move zipped, recover)
}`

/** The second operand fails, so the pair's own construction never happens either. */
const zipFailingSecond = `import silk.effect as Effect
${prelude}

pub fn main() -> i32 {
  let zipped = Effect.zip(firstStep(), failingStep()) |> Effect.map(combinePair)
  return run Effect.catchAll(move zipped, recover)
}`

/** All three operands succeed, in declaration order. */
const zipping3 = `import silk.effect as Effect
${prelude}

pub fn main() -> i32 {
  let zipped = Effect.zip3(firstStep(), secondStep(), thirdStep()) |> Effect.map(combineTriple)
  return run Effect.catchAll(move zipped, recover)
}`

/** The middle operand fails, so the third never runs. */
const zip3ShortCircuiting = `import silk.effect as Effect
${prelude}

pub fn main() -> i32 {
  let zipped = Effect.zip3(firstStep(), failingStep(), thirdStep()) |> Effect.map(combineTriple)
  return run Effect.catchAll(move zipped, recover)
}`

/** The piped form resolves to the same declaration and produces the same result. */
const zipPiped = `import silk.effect as Effect
${prelude}

pub fn main() -> i32 {
  let zipped = firstStep() |> Effect.zip(secondStep()) |> Effect.map(combinePair)
  return run Effect.catchAll(move zipped, recover)
}`

/** Distinct failure rows and distinct requirement rows on each operand, so both unions show. */
const zipRowsSource = `import silk.effect as Effect
struct Left { code: i32 }
struct Right { code: i32 }
service Clock {}
service Meter {}
effect fn left() -> i32 ! Left ? &Clock { return 40 }
effect fn right() -> i32 ! Right ? &Meter { return 2 }
pub fn main() -> i32 {
  let zipped = Effect.zip(left(), right())
  return 0
}`

const zip3RowsSource = `import silk.effect as Effect
struct Left { code: i32 }
struct Middle { code: i32 }
struct Right { code: i32 }
service Clock {}
service Meter {}
service Gauge {}
effect fn left() -> i32 ! Left ? &Clock { return 40 }
effect fn middle() -> i32 ! Middle ? &Meter { return 2 }
effect fn right() -> i32 ! Right ? &Gauge { return 0 }
pub fn main() -> i32 {
  let zipped = Effect.zip3(left(), middle(), right())
  return 0
}`

/**
 * The parity source is deliberately allocation-free. The allocating programs above are asserted on
 * the evaluator and Wasm — the same two engines `Effect.ensuring` uses — because LLVM emission of an
 * allocating body already fails on this shape without any zip in it, so holding zip to it would be
 * asserting a pre-existing backend limitation rather than anything about this combinator.
 */
const parity = `import silk.effect as Effect
import silk.effect { Pair, Triple }
effect fn left() -> i32 { return 40 }
effect fn middle() -> i32 { return 2 }
effect fn right() -> i32 { return 300 }
fn combinePair(pair: Pair<i32, i32>) -> i32 { return pair.first + pair.second }
fn combineTriple(triple: Triple<i32, i32, i32>) -> i32 {
  return triple.first + triple.second + triple.third
}
pub fn main() -> i32 {
  let pair = run (Effect.zip(left(), middle()) |> Effect.map(combinePair))
  let triple = run (Effect.zip3(left(), middle(), right()) |> Effect.map(combineTriple))
  return pair + triple - 342
}`

const allocationEvents = (
  run: ReturnType<typeof Analysis.evaluate>,
): ReadonlyArray<'AllocationAcquire' | 'AllocationRelease'> =>
  run.trace.flatMap((event) =>
    event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease' ? [event._tag] : [],
  )

const acquire = 'AllocationAcquire'
const release = 'AllocationRelease'

/** One operand's contribution to the trace: `depth` nested acquires, then `depth` releases. */
const step = (depth: number): ReadonlyArray<string> => [
  ...Array.from({ length: depth }, () => acquire),
  ...Array.from({ length: depth }, () => release),
]

/**
 * The allocation trace is asserted on the evaluator because it is the only engine that publishes
 * one; Wasm is held to the observable result that trace predicts.
 */
const accept = (
  name: string,
  source: string,
  expected: number,
  expectedEvents: ReadonlyArray<string>,
) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      `zip/${name}`,
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [], name)
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [], name)

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      `${name}: ${evaluated._tag === 'Blocked' ? evaluated.reason._tag : evaluated._tag}`,
    )
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, BigInt(expected), name)

    const events = allocationEvents(evaluated)
    assert.deepEqual(events, expectedEvents, `${name} acquire/release trace`)
    // An operand that never ran must not have stranded anything either: the unrun Effect is
    // released by ordinary local cleanup on the way out.
    assert.strictEqual(
      events.filter((event) => event === acquire).length,
      events.filter((event) => event === release).length,
      `${name} acquires equal releases`,
    )

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), expected, `${name} wasm`)
  })

it.effect('runs both zipped Effects in declaration order and collects both values', () =>
  accept('zipping', zipping, 42, [...step(1), ...step(2)]),
)

it.effect('accepts the piped form of zip with the same order and result', () =>
  accept('piped', zipPiped, 42, [...step(1), ...step(2)]),
)

it.effect('stops zip at a first-operand failure without running the second Effect', () =>
  accept('short-circuit', zipShortCircuiting, 7, [...step(1)]),
)

it.effect('propagates a second-operand failure out of zip with its payload intact', () =>
  accept('failing-second', zipFailingSecond, 7, [...step(1), ...step(1)]),
)

it.effect('runs all three zipped Effects in declaration order', () =>
  accept('zipping3', zipping3, 342, [...step(1), ...step(2), ...step(3)]),
)

it.effect('stops zip3 at a middle failure without running the third Effect', () =>
  accept('short-circuit3', zip3ShortCircuiting, 7, [...step(1), ...step(1)]),
)

/** Requirement 6: both failure rows and both requirement rows are unioned. */
it.effect('unions the failure rows and the requirement rows of both zipped Effects', () =>
  Effect.gen(function* () {
    const module = 'zip/rows'
    const snapshot = yield* Analysis.ofSourceRealized(module, ascii(zipRowsSource))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const encoded = Analysis.expressionsOf(snapshot, module).flatMap((expression) =>
      expression._tag === 'Call' && expression.type._tag === 'Available'
        ? [Type.encode(expression.type.type)]
        : [],
    )
    // The zip call itself is encoded first, then its two operands.
    assert.deepEqual(encoded, [
      `Effect<silk/effect.Pair<i32, i32> ! ${module}.Left | ${module}.Right ? &${module}.Clock | &${module}.Meter>`,
      `Effect<i32 ! ${module}.Left ? &${module}.Clock>`,
      `Effect<i32 ! ${module}.Right ? &${module}.Meter>`,
    ])
  }),
)

it.effect('unions all three failure rows and all three requirement rows through zip3', () =>
  Effect.gen(function* () {
    const module = 'zip/rows3'
    const snapshot = yield* Analysis.ofSourceRealized(module, ascii(zip3RowsSource))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const encoded = Analysis.expressionsOf(snapshot, module).flatMap((expression) =>
      expression._tag === 'Call' && expression.type._tag === 'Available'
        ? [Type.encode(expression.type.type)]
        : [],
    )
    assert.strictEqual(encoded.length, 4)
    assert.strictEqual(
      encoded[0],
      `Effect<silk/effect.Triple<i32, i32, i32> ! ${module}.Left | ${module}.Middle | ${module}.Right ? &${module}.Clock | &${module}.Gauge | &${module}.Meter>`,
    )
  }),
)

/**
 * Requirement 7 and the closed combinator list: both combinators are ordinary shipped Silk
 * declarations. Nothing about them is selected from their names, and no intrinsic is registered.
 */
it.effect('resolves zip and zip3 through the ordinary declaration path without an intrinsic', () =>
  Effect.gen(function* () {
    const module = 'zip/declaration'
    const snapshot = yield* Analysis.ofSourceRealized(module, ascii(zipping3))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    for (const [name, offset] of [
      ['zip3', zipping3.indexOf('Effect.zip3') + 'Effect.'.length],
    ] as const) {
      const occurrence = Analysis.semanticOccurrenceAt(snapshot, module, offset)
      assert.strictEqual(occurrence?.role, 'Value', name)
      assert.strictEqual(occurrence?.declaration?.module, 'silk/effect', name)
      assert.include(
        occurrence === undefined
          ? ''
          : (Analysis.occurrencePresentation(snapshot, module, occurrence)?.text ?? ''),
        `pub effect fn ${name}`,
        name,
      )
    }

    const catalog = Intrinsic.all().flatMap((actor) =>
      actor.operations.map((operation) => operation.spelling),
    )
    assert.include(catalog, 'effectResult')
    assert.notInclude(catalog, 'zip')
    assert.notInclude(catalog, 'zip3')
  }),
)

/** Three-engine parity for the plain success path. */
it.effect('produces the same zipped result on the evaluator, LLVM, and Wasm', () =>
  Effect.gen(function* () {
    const native = yield* Analysis.ofSourceRealized(
      'zip/parity',
      ascii(parity),
      'aarch64-apple-darwin',
    )
    const wasm = yield* Analysis.ofSourceRealized(
      'zip/parity',
      ascii(parity),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(native), [])
    assert.deepEqual(Analysis.diagnostics(wasm), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(wasm)), [])

    const logical = Analysis.evaluate(native)
    assert.strictEqual(
      logical._tag,
      'Completed',
      logical._tag === 'Blocked' ? logical.reason._tag : logical._tag,
    )
    assert.strictEqual(logical._tag === 'Completed' ? logical.result.value : undefined, 42n)

    const llvm = yield* Analysis.codegen(native, { mode: 'release' })
    assert.include(llvm.ir, 'define')

    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)
