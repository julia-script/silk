import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Scalar from '../src/Scalar.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Stdlib from '../src/Stdlib.js'
import * as Driver from './support/TestDriver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const decoder = new TextDecoder()

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-number-text-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

/**
 * The eight fixed-width integer modules, taken from the catalog rather than spelled out, so a later
 * integer type inherits the same demands. `usize` and `isize` carry a pointer width, so no one
 * bound is correct for every target; they are exercised separately at values every target holds.
 */
const fixedWidthIntegers = Scalar.integers().filter((scalar) => scalar.width._tag === 'FixedWidth')

const pointerWidthSpellings = ['usize', 'isize'] as const

const sourceText = (spelling: string): string => {
  const bytes = Stdlib.sources.get(`silk/${spelling}`)
  assert.isDefined(bytes, `silk/${spelling}`)
  return decoder.decode(bytes)
}

const rangeOf = (spelling: string): { minimum: bigint; maximum: bigint } => {
  const scalar = fixedWidthIntegers.find((candidate) => candidate.spelling === spelling)
  assert.isDefined(scalar, spelling)
  return scalar === undefined
    ? { minimum: 0n, maximum: 0n }
    : { minimum: Scalar.range(scalar, 64).minimum, maximum: Scalar.range(scalar, 64).maximum }
}

const isSigned = (spelling: string): boolean => spelling.startsWith('i')

/** The widening every check funnels through, so a `u8` and a `u64` are compared the same way. */
const widen = (spelling: string): string => (isSigned(spelling) ? 'toI64' : 'toU64')

/** Comparison, widening, and failure narrowing, shared by every generated check. */
const prelude = `${Scalar.integers()
  .map((scalar) => `import silk.${scalar.spelling} as ${scalar.spelling}`)
  .join('\n')}
import silk.string { String, copy, append, appendOwned, ownedUtf8Bytes, utf8Bytes }
import silk.format { ParseError, NotANumber, OutOfRange }
import silk.result { Result, Success, Failure }

fn sameText(text: &String, expected: string) -> bool {
  let actual = ownedUtf8Bytes(text)
  let wanted = utf8Bytes(expected)
  if actual.length != wanted.length { return false }
  let mut index = usize.ZERO
  while index < actual.length {
    if actual[index] != wanted[index] { return false }
    index = index + usize.ONE
  }
  return true
}

fn sameUnsigned(value: u64, expected: u64) -> i32 {
  if value == expected { return 0 }
  return 1
}

fn sameSigned(value: i64, expected: i64) -> i32 {
  if value == expected { return 0 }
  return 1
}

fn isOutOfRange(error: ParseError) -> i32 {
  return match move error {
    ParseError { reason } => match move reason {
      NotANumber { offset } => 1
      OutOfRange nothing => 0
    }
  }
}

fn isNotANumberAt(error: ParseError, expected: usize) -> i32 {
  return match move error {
    ParseError { reason } => match move reason {
      NotANumber { offset } => sameUnsigned(usize.toU64(offset), usize.toU64(expected))
      OutOfRange nothing => 1
    }
  }
}
`

interface Check {
  readonly name: string
  readonly effectful: boolean
  readonly declaration: string
}

/** `spelling.toText(value)` renders exactly `expected`. */
const textCheck = (name: string, spelling: string, value: string, expected: string): Check => ({
  name,
  effectful: true,
  declaration: `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
effect fn ${name}() -> i32 ! OutOfMemoryError ? &mut Allocator {
  let text = run ${spelling}.toText(${value})
  if sameText(&text, "${expected}") { return 0 }
  return 1
}`,
})

/** `spelling.parse(text)` succeeds at exactly `expected`, compared after widening. */
const valueCheck = (name: string, spelling: string, text: string, expected: string): Check => ({
  name,
  effectful: false,
  declaration: `import silk.format { ParseError }
import silk.result { Result }
fn ${name}() -> i32 {
  return match move ${spelling}.parse("${text}") {
    Result<${spelling}, ParseError> { value: outcome } => match move outcome {
      Success<${spelling}> { value } => ${isSigned(spelling) ? 'sameSigned' : 'sameUnsigned'}(${spelling}.${widen(spelling)}(value), ${expected})
      Failure<ParseError> { error } => 1
    }
  }
}`,
})

/** `spelling.parse(text)` fails, and the reason is the one named. */
const failureCheck = (
  name: string,
  spelling: string,
  text: string,
  reason:
    | { readonly _tag: 'OutOfRange' }
    | { readonly _tag: 'NotANumber'; readonly offset: number },
): Check => {
  let failure: string
  if (reason._tag === 'OutOfRange') {
    failure = 'isOutOfRange(move error)'
  } else {
    const offset = reason.offset === 0 ? 'usize.ZERO' : reason.offset
    failure = `isNotANumberAt(move error, ${offset})`
  }
  return {
    name,
    effectful: false,
    declaration: `import silk.format { OutOfRange }
import silk.format { ParseError }
import silk.result { Result }
import silk.usize as usize
fn ${name}() -> i32 {
  return match move ${spelling}.parse("${text}") {
    Result<${spelling}, ParseError> { value: outcome } => match move outcome {
      Success<${spelling}> { value } => 1
      Failure<ParseError> { error } => ${failure}
    }
  }
}`,
  }
}

/**
 * Assembles the checks into one program that returns the 1-based index of the first check that
 * disagreed, or zero when every check held. An index rather than a count, so a failure on the
 * native backend — where the only channel is the process status — still names which check broke.
 */
const programOf = (checks: ReadonlyArray<Check>): string => {
  assert.isBelow(checks.length, 255, 'a failure index must survive a process exit status')
  const body = checks
    .map(
      (
        check,
        position,
      ) => `  let outcome${position} = ${check.effectful ? 'run ' : ''}${check.name}()
  if outcome${position} != 0 { return ${position + 1} }`,
    )
    .join('\n')
  return `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
${prelude}
${checks.map((check) => check.declaration).join('\n\n')}

effect fn firstFailure() -> i32 ! OutOfMemoryError ? &mut Allocator {
${body}
  return 0
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorService()
  let checking = firstFailure() |> Effect.provideMut(&mut allocator)
  return run checking
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 254 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`
}

/** Every bound check for the eight fixed-width types, driven off the catalog's own range table. */
const boundChecks: ReadonlyArray<Check> = fixedWidthIntegers.flatMap((scalar) => {
  const spelling = scalar.spelling
  const { minimum, maximum } = rangeOf(spelling)
  const signed = isSigned(spelling)
  return [
    textCheck(`${spelling}MaxText`, spelling, `${spelling}.MAX`, maximum.toString()),
    textCheck(`${spelling}MinText`, spelling, `${spelling}.MIN`, minimum.toString()),
    valueCheck(
      `${spelling}MaxValue`,
      spelling,
      maximum.toString(),
      `${spelling}.${widen(spelling)}(${spelling}.MAX)`,
    ),
    valueCheck(
      `${spelling}MinValue`,
      spelling,
      minimum.toString(),
      `${spelling}.${widen(spelling)}(${spelling}.MIN)`,
    ),
    failureCheck(`${spelling}AboveMax`, spelling, (maximum + 1n).toString(), {
      _tag: 'OutOfRange',
    }),
    signed
      ? failureCheck(`${spelling}BelowMin`, spelling, (minimum - 1n).toString(), {
          _tag: 'OutOfRange',
        })
      : // Below zero is not a smaller number for an unsigned type: `-` is simply not part of its
        // text, so the leading byte is where reading stops.
        failureCheck(`${spelling}BelowMin`, spelling, (minimum - 1n).toString(), {
          _tag: 'NotANumber',
          offset: 0,
        }),
  ]
})

/**
 * `usize` and `isize` carry a target-dependent bound, so no one expected text is correct for every
 * engine this program runs on. They are held to the round trip at values every supported pointer
 * width holds, which is what a diagnostic actually renders.
 */
const pointerWidthChecks: ReadonlyArray<Check> = pointerWidthSpellings.flatMap((spelling) => {
  const signed = isSigned(spelling)
  const typed = (literal: string) => `i32.to${spelling === 'usize' ? 'Usize' : 'Isize'}(${literal})`
  return [
    textCheck(`${spelling}ZeroText`, spelling, typed('0'), '0'),
    textCheck(`${spelling}Text`, spelling, typed('40960'), '40960'),
    valueCheck(
      `${spelling}Value`,
      spelling,
      '40960',
      `${spelling}.${widen(spelling)}(${typed('40960')})`,
    ),
    ...(signed
      ? [
          textCheck(`${spelling}NegativeText`, spelling, typed('0 - 40960'), '-40960'),
          valueCheck(
            `${spelling}NegativeValue`,
            spelling,
            '-40960',
            `${spelling}.${widen(spelling)}(${typed('0 - 40960')})`,
          ),
        ]
      : [failureCheck(`${spelling}Negative`, spelling, '-1', { _tag: 'NotANumber', offset: 0 })]),
  ]
})

const boundsProgram = programOf([...boundChecks, ...pointerWidthChecks])

/**
 * Requirements 1, 2, 5 and 8, plus the first three acceptance criteria. The text of every bound,
 * the round trip through `parse`, and the out-of-range rejection are checked in one program so the
 * three engines are compared on identical work; the answer is the index of the first disagreement.
 */
it.effect(
  'renders and reads every integer bound identically on all three engines',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'number-text/bounds',
        ascii(boundsProgram),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        JSON.stringify(evaluated, (_, value) =>
          typeof value === 'bigint' ? value.toString() : value,
        ),
      )
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 0n, 'first failing check index')

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 0)

      const compiled = yield* Driver.compile({
        compilation: { root: SourceFile.make('number-text/bounds', ascii(boundsProgram)) },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        profile: 'release',
        destination: join(destinationRoot, 'bounds'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled')
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(run.status, 0, run.stderr)
    }),
  120_000,
)

/**
 * Requirements 4 and 5. The reason a parse failed is data the caller can narrow, not one opaque
 * error: text that is not a number reports where reading stopped, and a well-formed number that
 * does not fit reports the range instead.
 */
const failureProgram = programOf([
  failureCheck('emptyText', 'u8', '', { _tag: 'NotANumber', offset: 0 }),
  failureCheck('letters', 'u8', 'abc', { _tag: 'NotANumber', offset: 0 }),
  failureCheck('trailingLetter', 'u8', '12x', { _tag: 'NotANumber', offset: 2 }),
  failureCheck('interiorSpace', 'u8', '1 2', { _tag: 'NotANumber', offset: 1 }),
  failureCheck('leadingPlus', 'i32', '+1', { _tag: 'NotANumber', offset: 0 }),
  failureCheck('signAlone', 'i32', '-', { _tag: 'NotANumber', offset: 1 }),
  failureCheck('letterAfterSign', 'i32', '-x', { _tag: 'NotANumber', offset: 1 }),
  // Wider than the widest accumulator, so the rejection comes from the digit loop rather than from
  // the narrowing conversion that follows it.
  failureCheck('aboveEveryType', 'u8', '18446744073709551616', { _tag: 'OutOfRange' }),
  failureCheck('farAboveEveryType', 'i32', '-99999999999999999999999', { _tag: 'OutOfRange' }),
  valueCheck('leadingZeros', 'u8', '007', 'u8.toU64(i32.toU8(7))'),
  valueCheck('zero', 'i32', '0', 'i32.toI64(0)'),
  valueCheck('negativeZero', 'i32', '-0', 'i32.toI64(0)'),
])

it.effect('reports which byte stopped a read and which values do not fit', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'number-text/failures',
      ascii(failureProgram),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 0n, 'first failing check index')

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 0)
  }),
)

/**
 * Requirement 7 and the issue's own example, verbatim in shape: a message that no program could
 * write before, because both numbers in it are runtime values. Allocation is balanced, so composing
 * a message leaves nothing behind.
 */
const messageProgram = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.i32 as i32
import silk.usize as usize
import silk.string { String, copy, append, appendOwned, ownedUtf8Bytes, utf8Bytes }

pub effect fn report(code: i32, line: usize) -> String ! OutOfMemoryError ? &mut Allocator {
  let mut message = run copy("LEX")
  let coded = run appendOwned(&mut message, run i32.toText(code))
  let spaced = run append(&mut message, " at line ")
  let lined = run appendOwned(&mut message, run usize.toText(line))
  return move message
}

fn sameText(text: &String, expected: string) -> bool {
  let actual = ownedUtf8Bytes(text)
  let wanted = utf8Bytes(expected)
  if actual.length != wanted.length { return false }
  let mut index = usize.ZERO
  while index < actual.length {
    if actual[index] != wanted[index] { return false }
    index = index + usize.ONE
  }
  return true
}

effect fn compose() -> i32 ! OutOfMemoryError ? &mut Allocator {
  let first = run report(1, 12)
  if sameText(&first, "LEX1 at line 12") {} else { return 1 }
  let second = run report(0 - 7, 4096)
  if sameText(&second, "LEX-7 at line 4096") {} else { return 2 }
  return 0
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorService()
  let composing = compose() |> Effect.provideMut(&mut allocator)
  return run composing
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 254 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('composes a diagnostic message from runtime values and releases every allocation', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'number-text/message',
      ascii(messageProgram),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 0n)

    const acquires = evaluated.trace.filter((event) => event._tag === 'AllocationAcquire')
    const releases = evaluated.trace.filter((event) => event._tag === 'AllocationRelease')
    assert.isAbove(acquires.length, 0)
    assert.strictEqual(releases.length, acquires.length)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 0)
  }),
)

/**
 * Requirement 6. Rendering allocates, so it must be able to say it could not: an allocator that
 * refuses hands the caller the ordinary `OutOfMemoryError` typed failure rather than a partial string.
 */
const exhaustedProgram = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.i32 as i32
import silk.layout { Layout }
struct QuotaAllocator { remaining: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  if self.remaining == 0 { fail OutOfMemoryError {} }
  self.remaining = self.remaining - 1
  let mut inner = Allocator.systemAllocatorService()
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
  return run pending
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = QuotaAllocator { remaining: 0 }
  let rendering = i32.toText(1234) |> Effect.provideMut(&mut allocator)
  let text = run rendering
  return 1
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 42 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('hands back OutOfMemoryError when a rendering cannot allocate, owning nothing', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'number-text/exhausted',
      ascii(exhaustedProgram),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(
      evaluated.trace.filter((event) => event._tag === 'AllocationAcquire'),
      [],
    )

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

/**
 * Requirements 1 and 2 as a surface demand rather than a behavioural one: the pair exists on every
 * integer module the catalog knows, spelled the same way, so a later integer type cannot ship
 * without it. Reading the declaration back also pins the effect row requirement 6 asks for.
 */
it('declares the same rendering and reading pair on every integer module', () => {
  const spellings = [
    ...fixedWidthIntegers.map((scalar) => scalar.spelling),
    ...pointerWidthSpellings,
  ]
  assert.strictEqual(spellings.length, 10)
  for (const spelling of spellings) {
    const source = sourceText(spelling)
    assert.include(source, 'import silk.allocator { Allocator }')
    assert.include(source, 'import silk.allocator { OutOfMemoryError }')
    assert.include(source, 'import silk.string { String }')
    assert.include(
      source,
      `pub effect fn toText(value: ${spelling}) -> String ! OutOfMemoryError ? &mut Allocator {`,
      `silk/${spelling} declares no toText with the allocating effect row`,
    )
    assert.include(source, 'import silk.format { ParseError }')
    assert.include(source, 'import silk.result { Result }')
    assert.include(
      source,
      `pub fn parse(text: string) -> Result<${spelling}, ParseError> {`,
      `silk/${spelling} declares no parse returning a typed failure`,
    )
  }
})

/** Float rendering is deferred with requirement 9; naming its absence keeps it from reading as an
 * oversight. See the follow-up split out of #40. */
it('declares no float rendering yet, which requirement 9 still owns', () => {
  for (const spelling of ['f32', 'f64']) {
    assert.notInclude(sourceText(spelling), 'pub effect fn toText(')
    assert.notInclude(sourceText(spelling), 'pub fn parse(')
  }
})
