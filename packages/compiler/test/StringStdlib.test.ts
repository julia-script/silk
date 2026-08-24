import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as SourceFile from '../src/SourceFile.js'
import * as Stdlib from '../src/Stdlib.js'
import * as Json from './support/Json.js'
import * as Projections from './support/projections.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const diagnosticSummary = (snapshot: Analysis.Snapshot) =>
  Analysis.diagnostics(snapshot).map((diagnostic) => ({
    code: diagnostic.code,
    message: diagnostic.message,
    sourceId: diagnostic.span.sourceId,
    start: diagnostic.span.start,
  }))

const validation = `import silk.usize as usize
import silk.string { InvalidUtf8, fromUtf8, byteLength }
import silk.result { Result, Success, Failure }

fn observe(result: Result<string, InvalidUtf8>) -> i32 {
  return match move result {
    Result<string, InvalidUtf8> { value: outcome } => match move outcome {
      Success<string> { value } => usize.toI32(byteLength(value))
      Failure<InvalidUtf8> { error } => usize.toI32(error.offset) + 100
    }
  }
}

pub fn main() -> i32 {
  let ascii = observe(fromUtf8(b"silk"))
  let mixed = observe(fromUtf8(b"A\\xc2\\xa2\\xe2\\x82\\xac\\xf0\\x90\\x8d\\x88"))
  let stray = observe(fromUtf8(b"a\\x80"))
  let truncated = observe(fromUtf8(b"a\\xe2\\x82"))
  let overlong = observe(fromUtf8(b"a\\xe0\\x80\\x80"))
  let surrogate = observe(fromUtf8(b"a\\xed\\xa0\\x80"))
  let above = observe(fromUtf8(b"a\\xf4\\x90\\x80\\x80"))
  return ascii + mixed + stray + truncated + overlong + surrogate + above
}`

it.effect('validates complete UTF-8 and reports the first invalid byte offset', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized('string-stdlib/validation', ascii(validation))
    assert.deepEqual(diagnosticSummary(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    const mir = Analysis.mirOf(snapshot)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      JSON.stringify(
        {
          evaluated,
          functions:
            mir._tag === 'Available'
              ? mir.value.functions.map((fn) => `${fn.id.module}.${fn.id.name}`)
              : [],
        },
        Json.bigIntReplacer,
      ),
    )
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 522n)
  }),
)

it.effect('exposes unchecked UTF-8 construction as an unsafe source callable', () =>
  Effect.gen(function* () {
    const safe = yield* Analysis.ofSourceRealized(
      'string-stdlib/unchecked-safe',
      ascii(`import silk.string { byteLength, fromUtf8Unchecked }
import silk.usize { toI32 }
pub fn main() -> i32 { return toI32(byteLength(fromUtf8Unchecked(b"silk"))) }`),
    )
    assert.deepEqual(
      diagnosticSummary(safe).map(({ code }) => code),
      ['SEM0082'],
    )

    const acknowledged = yield* Analysis.ofSourceRealized(
      'string-stdlib/unchecked-acknowledged',
      ascii(`import silk.string { byteLength, fromUtf8Unchecked }
import silk.usize { toI32 }
pub fn main() -> i32 { return toI32(byteLength(unsafe fromUtf8Unchecked(b"silk"))) }`),
    )
    assert.deepEqual(diagnosticSummary(acknowledged), [])
    const evaluated = Analysis.evaluate(acknowledged)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated, Json.bigIntReplacer))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 4n)
  }),
)

const owned = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.string {
  copy,
  append,
  view,
  ownedByteLength,
  ownedUtf8Bytes
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorService()
  let copying = copy("h\\u{e9}") |> Effect.provideMut(&mut allocator)
  let mut value = run copying
  let appending = append(&mut value, " \\u{1f642}") |> Effect.provideMut(&mut allocator)
  let appended = run appending
  if view(&value) == "h\\u{e9} \\u{1f642}" {} else { return 1 }
  if ownedByteLength(&value) == 8 {} else { return 2 }
  if ownedUtf8Bytes(&value).length == 8 {} else { return 3 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('copies, appends, views, and drops owned String through ordinary Bytes storage', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized('string-stdlib/owned', ascii(owned))
    assert.deepEqual(diagnosticSummary(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    const mir = Analysis.mirOf(snapshot)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      JSON.stringify(
        {
          evaluated,
          functions:
            mir._tag === 'Available'
              ? mir.value.functions.map((fn) => `${fn.id.module}.${fn.id.name}`)
              : [],
        },
        Json.bigIntReplacer,
      ),
    )
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.isTrue(evaluated.trace.some((event) => event._tag === 'AllocationRelease'))
  }),
)

const appendRollback = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.string { String, copy, append, view, ownedByteLength }

struct QuotaAllocator { remaining: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  if self.remaining == 0 { fail OutOfMemoryError {} }
  self.remaining = self.remaining - 1
  let mut inner = Allocator.systemAllocatorService()
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
  return run pending
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

effect fn ignore(error: OutOfMemoryError) -> () { return () }
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

effect fn build() -> i32 ! OutOfMemoryError {
  // One allocation, spent by the copy. Appending grows the copied storage in place rather than
  // copying it into fresh storage first, so starving the append takes one fewer than it used to.
  let mut allocator = QuotaAllocator { remaining: 1 }
  let copying = copy("ok") |> Effect.provideMut(&mut allocator)
  let mut value = run copying
  let recovered = run Effect.catchAll(
    append(&mut value, "\\u{1f642}") |> Effect.provideMut(&mut allocator),
    ignore
  )
  if view(&value) == "ok" {} else { return 1 }
  if ownedByteLength(&value) == 2 {} else { return 2 }
  return 42
}

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('rolls append back when growth cannot allocate', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'string-stdlib/append-rollback',
      ascii(appendRollback),
    )
    assert.deepEqual(diagnosticSummary(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated, Json.bigIntReplacer))
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
  }),
)

const scalars = `import silk.u32 as u32
import silk.usize as usize
import silk.string {
  ScalarCursor,
  ScalarStep,
  scalarCursor,
  cursorByteOffset,
  nextScalar,
  scalarValue,
  scalarByteOffset,
  nextCursor
}
import silk.option { Option, Some, None }
import silk.char { toU32 as charToU32 }

fn walk(value: string, cursor: ScalarCursor, expectedOffset: usize) -> u32 {
  if cursorByteOffset(&cursor) == expectedOffset {} else { return u32.toU32(1) }
  return match move nextScalar(value, move cursor) {
    Some<ScalarStep> { value: step } => continueWalk(value, move step)
    None nothing => u32.toU32(0)
  }
}

fn continueWalk(value: string, step: ScalarStep) -> u32 {
  let offset = scalarByteOffset(&step)
  let scalar = charToU32(scalarValue(&step))
  let next = nextCursor(move step)
  return scalar + walk(value, move next, offset + stringWidth(scalar))
}

fn stringWidth(scalar: u32) -> usize {
  if scalar < u32.toU32(128) { return usize.toUsize(1) }
  if scalar < u32.toU32(2048) { return usize.toUsize(2) }
  if scalar < u32.toU32(65536) { return usize.toUsize(3) }
  return usize.toUsize(4)
}

pub fn main() -> i32 {
  return u32.toI32(walk("A\\u{a2}\\u{20ac}\\u{10348}", scalarCursor(), usize.toUsize(0)))
}`

it.effect('traverses mixed-width Unicode scalars with explicit byte offsets', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized('string-stdlib/scalars', ascii(scalars))
    assert.deepEqual(diagnosticSummary(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated, Json.bigIntReplacer))
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 74_967n)
  }),
)

it.effect('ships String as navigable ordinary source with private storage', () =>
  Effect.gen(function* () {
    const source = `import silk.string { String, copy, view }
fn inspect(value: &String) -> string { return view(value) }
pub fn main() -> i32 { return 42 }`
    const snapshot = yield* Analysis.ofSourceRealized('string-stdlib/navigation', ascii(source))
    assert.deepEqual(diagnosticSummary(snapshot), [])

    const callOffset = source.lastIndexOf('view')
    const occurrence = Analysis.semanticOccurrenceAt(
      snapshot,
      'string-stdlib/navigation',
      callOffset,
    )
    assert.strictEqual(occurrence?.declaration?.module, 'silk/string')
    assert.isDefined(occurrence?.declaration?.selectionSpan)

    const canonical = Projections.syntaxOf(snapshot, 'silk/string')?.source
    assert.isDefined(canonical)
    const canonicalText =
      canonical === undefined ? '' : new TextDecoder().decode(SourceFile.toUint8Array(canonical))
    assert.include(canonicalText, 'pub struct String')
    assert.include(canonicalText, 'bytes: Bytes')
    assert.notInclude(canonicalText, 'pub bytes: Bytes')

    const archived = Stdlib.find('silk/string')
    assert.strictEqual(archived?.namespace, 'String')
    assert.deepEqual(
      archived?.bytes,
      canonical === undefined ? undefined : SourceFile.toUint8Array(canonical),
    )
    assert.isFalse(Intrinsic.all().some((actor) => actor.spelling === 'String'))

    const forged = yield* Analysis.ofSourceRealized(
      'string-stdlib/private-storage',
      ascii(`import silk.string { String }
fn expose(value: &String) -> usize { return value.bytes.values.length }
pub fn main() -> i32 { return 42 }`),
    )
    assert.include(
      Analysis.diagnostics(forged).map((diagnostic) => diagnostic.code),
      'SEM0028',
    )
  }),
)
