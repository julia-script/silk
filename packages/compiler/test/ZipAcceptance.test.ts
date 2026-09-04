import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/**
 * `Effect.zip` and `Effect.zip3` run their operands in declaration order and collect every success
 * value, stopping at the first typed failure.
 *
 * Runtime order and short-circuiting are pinned by the native corpus; structured claims here inspect
 * the compiler representation directly.
 */
const prelude = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.layout { Layout }
import silk.effect { Pair, Triple }

struct Problem { code: i32 }

service Clock {}
struct FixedClock { storage: Allocation }
impl Clock for FixedClock {}

effect fn openClock() -> FixedClock ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
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

/** All three operands succeed, in declaration order. */
const zipping3 = `import silk.effect { Effect }
${prelude}

pub fn main() -> i32 {
  let zipped = Effect.zip3(firstStep(), secondStep(), thirdStep()) |> Effect.map(combineTriple)
  return run Effect.catchAll(move zipped, recover)
}`

/** Distinct failure rows and distinct requirement rows on each operand, so both unions show. */
const zipRowsSource = `import silk.effect { Effect }
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

const zip3RowsSource = `import silk.effect { Effect }
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
    assert.notInclude(catalog, 'effectResult')
    assert.notInclude(catalog, 'zip')
    assert.notInclude(catalog, 'zip3')
  }),
)
