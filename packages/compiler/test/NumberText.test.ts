import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Scalar from '../src/Scalar.js'
import * as StandardStreams from '../src/StandardStreams.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()
const ascii = (value: string): Uint8Array => encoder.encode(value)
const fixedWidthIntegers = Scalar.integers().filter((scalar) => scalar.width._tag === 'FixedWidth')
const integerSpellings = [...fixedWidthIntegers.map((scalar) => scalar.spelling), 'usize', 'isize']

const sourceText = (spelling: string): string => {
  return readFileSync(new URL(`../stdlib/silk/${spelling}.silk`, import.meta.url), 'utf8')
}

const rangeOf = (spelling: string): { minimum: bigint; maximum: bigint } => {
  const scalar = fixedWidthIntegers.find((candidate) => candidate.spelling === spelling)
  assert.isDefined(scalar, spelling)
  return scalar === undefined ? { minimum: 0n, maximum: 0n } : Scalar.range(scalar, 64)
}

const isSigned = (spelling: string): boolean => spelling.startsWith('i')

const defaultCases = [
  ...fixedWidthIntegers.flatMap((scalar) => {
    const range = rangeOf(scalar.spelling)
    return [
      {
        spelling: scalar.spelling,
        expression: `${scalar.spelling}.MIN`,
        expected: `${range.minimum}`,
      },
      {
        spelling: scalar.spelling,
        expression: `${scalar.spelling}.MAX`,
        expected: `${range.maximum}`,
      },
    ]
  }),
  { spelling: 'usize', expression: 'i32.toUsize(40960)', expected: '40960' },
  { spelling: 'isize', expression: 'i32.toIsize(0 - 40960)', expected: '-40960' },
]

const imports = Scalar.integers()
  .map((scalar) => `import silk.${scalar.spelling} as ${scalar.spelling}`)
  .join('\n')

const defaultRenderingProgram = `${imports}
import silk.effect as Effect
import silk.format as Format
import silk.writer as WriterActor
import silk.writer { Writer, WriterError }

effect fn render() -> () ! WriterError ? &mut Writer {
${defaultCases
  .map(
    (entry, index) => `  let value${index} = ${entry.expression}
  run Format.display(&value${index})
  run Writer.writeAll(b"|")`,
  )
  .join('\n')}
  return ()
}

effect fn build() -> i32 ! WriterError {
  let mut writer = WriterActor.stdoutWriterProvider()
  run render() |> Effect.provideMut<Writer>(&mut writer)
  return 42
}
effect fn recover(error: WriterError) -> i32 { return 1 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

const outputBytes = (memory: ReturnType<typeof StandardStreams.memory>): ReadonlyArray<number> =>
  memory.events().flatMap((event) => event.bytes)

it.effect('streams every integer bound through Display without allocating', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'number-text/defaults',
      ascii(defaultRenderingProgram),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const memory = StandardStreams.memory()
    const evaluated = Analysis.evaluate(snapshot, { standardStreams: memory.provider })
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.strictEqual(
      decoder.decode(Uint8Array.from(outputBytes(memory))),
      `${defaultCases.map((entry) => entry.expected).join('|')}|`,
    )
    assert.deepEqual(
      evaluated.trace.filter((event) => event._tag === 'AllocationAcquire'),
      [],
    )

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const wasmMemory = StandardStreams.memory()
    let instance: WebAssembly.Instance | undefined
    instance = new WebAssembly.Instance(
      new WebAssembly.Module(wasm.bytes.slice()),
      StandardStreams.wasmImports(wasmMemory.provider, () => {
        const exported = instance?.exports[StandardStreams.wasmMemoryExport]
        return exported instanceof WebAssembly.Memory ? exported : undefined
      }),
    )
    const main = instance.exports.silk_main
    if (typeof main !== 'function') throw new Error('integer Display Wasm lost silk_main')
    assert.strictEqual(main(), 42)
    assert.strictEqual(
      decoder.decode(Uint8Array.from(outputBytes(wasmMemory))),
      `${defaultCases.map((entry) => entry.expected).join('|')}|`,
    )
  }),
)

const optionRenderingProgram = `import silk.effect as Effect
import silk.format as Format
import silk.format { Alignment, FormatOptions, Sign }
import silk.option as Option
import silk.usize as usize
import silk.writer as WriterActor
import silk.writer { Writer, WriterError }

fn options(width: usize, alignment: Alignment, fill: char, sign: Sign, zeroPad: bool, precision: Option.Option<usize>) -> FormatOptions {
  return FormatOptions {
    width: Option.some<usize>(width), alignment: alignment, fill: fill, sign: sign,
    alternate: true, zeroPad: zeroPad, precision: move precision, color: true,
  }
}

fn accessorsHold() -> bool {
  let defaults = Format.makeDefault()
  if Format.hasWidth(&defaults) { return false }
  if Format.width(&defaults) != usize.ZERO { return false }
  if Format.alignment(&defaults) != Alignment.Default { return false }
  if Format.fill(&defaults) != ' ' { return false }
  if Format.sign(&defaults) != Sign.NegativeOnly { return false }
  if Format.alternate(&defaults) { return false }
  if Format.zeroPad(&defaults) { return false }
  if Format.hasPrecision(&defaults) { return false }
  if Format.precision(&defaults) != usize.ZERO { return false }
  if Format.color(&defaults) { return false }
  let explicit = Format.make(options(12, Alignment.Left, '*', Sign.Space, true, Option.some<usize>(9)))
  if !Format.hasWidth(&explicit) { return false }
  if Format.width(&explicit) != 12 { return false }
  if Format.alignment(&explicit) != Alignment.Left { return false }
  if Format.fill(&explicit) != '*' { return false }
  if Format.sign(&explicit) != Sign.Space { return false }
  if !Format.alternate(&explicit) { return false }
  if !Format.zeroPad(&explicit) { return false }
  if !Format.hasPrecision(&explicit) { return false }
  if Format.precision(&explicit) != 9 { return false }
  return Format.color(&explicit)
}

effect fn render() -> () ! WriterError ? &mut Writer {
  let negative = 0 - 42
  run Format.displayWith(&negative, options(8, Alignment.Right, '*', Sign.NegativeOnly, false, Option.some<usize>(4)))
  run Writer.writeAll(b"|")
  let positive = 42
  run Format.displayWith(&positive, options(6, Alignment.Left, '.', Sign.Always, false, Option.none<usize>()))
  run Writer.writeAll(b"|")
  let zero = 0
  run Format.displayWith(&zero, options(7, Alignment.Center, '\u{e9}', Sign.NegativeOnly, false, Option.some<usize>(3)))
  run Writer.writeAll(b"|")
  run Format.displayWith(&negative, options(7, Alignment.Default, ' ', Sign.NegativeOnly, true, Option.none<usize>()))
  run Writer.writeAll(b"|")
  let seven = 0 - 7
  run Format.displayWith(&seven, options(5, Alignment.Center, '\u{b7}', Sign.NegativeOnly, false, Option.none<usize>()))
  run Writer.writeAll(b"|")
  run Format.displayWith(&positive, options(35, Alignment.Right, '_', Sign.NegativeOnly, false, Option.none<usize>()))
  return ()
}

effect fn build() -> i32 ! WriterError {
  if !accessorsHold() { return 2 }
  let mut writer = WriterActor.stdoutWriterProvider()
  run render() |> Effect.provideMut<Writer>(&mut writer)
  return 42
}
effect fn recover(error: WriterError) -> i32 { return 1 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('honors width, alignment, multibyte fill, sign, zero padding, and precision', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'number-text/options',
      ascii(optionRenderingProgram),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const memory = StandardStreams.memory()
    const evaluated = Analysis.evaluate(snapshot, { standardStreams: memory.provider })
    assert.strictEqual(evaluated._tag, 'Completed')
    assert.strictEqual(
      decoder.decode(Uint8Array.from(outputBytes(memory))),
      `***-0042|+42...|éé000éé|-000042|·-7··|${'_'.repeat(33)}42`,
    )
  }),
)

const parsingProgram = `${imports}
import silk.format { NotANumber, OutOfRange, ParseError }
import silk.result { Result }

fn parsed<T>(result: Result<T, ParseError>) -> bool {
  return match move result {
    Result<T, ParseError>.Success { value } => true
    Result<T, ParseError>.Failure { error } => false
  }
}
fn outOfRange<T>(result: Result<T, ParseError>) -> bool {
  return match move result {
    Result<T, ParseError>.Success { value } => false
    Result<T, ParseError>.Failure { error } => match move error.reason {
      NotANumber { offset } => false
      OutOfRange nothing => true
    }
  }
}
fn notANumberAt<T>(result: Result<T, ParseError>, expected: usize) -> bool {
  return match move result {
    Result<T, ParseError>.Success { value } => false
    Result<T, ParseError>.Failure { error } => match move error.reason {
      NotANumber { offset } => offset == expected
      OutOfRange nothing => false
    }
  }
}
pub fn main() -> i32 {
${defaultCases
  .map(
    (entry, index) =>
      `  if !parsed<${entry.spelling}>(${entry.spelling}.parse("${entry.expected}")) { return ${index + 1} }`,
  )
  .join('\n')}
${fixedWidthIntegers
  .flatMap((scalar, index) => {
    const range = rangeOf(scalar.spelling)
    return [
      `  if !outOfRange<${scalar.spelling}>(${scalar.spelling}.parse("${range.maximum + 1n}")) { return ${40 + index * 2} }`,
      isSigned(scalar.spelling)
        ? `  if !outOfRange<${scalar.spelling}>(${scalar.spelling}.parse("${range.minimum - 1n}")) { return ${41 + index * 2} }`
        : `  if !notANumberAt<${scalar.spelling}>(${scalar.spelling}.parse("-1"), usize.ZERO) { return ${41 + index * 2} }`,
    ]
  })
  .join('\n')}
  if !notANumberAt<u8>(u8.parse(""), usize.ZERO) { return 70 }
  if !notANumberAt<u8>(u8.parse("abc"), usize.ZERO) { return 71 }
  if !notANumberAt<u8>(u8.parse("12x"), 2) { return 72 }
  if !notANumberAt<u8>(u8.parse("1 2"), usize.ONE) { return 73 }
  if !notANumberAt<i32>(i32.parse("+1"), usize.ZERO) { return 74 }
  if !notANumberAt<i32>(i32.parse("-"), usize.ONE) { return 75 }
  if !notANumberAt<i32>(i32.parse("-x"), usize.ONE) { return 76 }
  if !outOfRange<u8>(u8.parse("18446744073709551616")) { return 77 }
  if !outOfRange<i32>(i32.parse("-99999999999999999999999")) { return 78 }
  if !parsed<u8>(u8.parse("007")) { return 79 }
  if !parsed<i32>(i32.parse("-0")) { return 80 }
  return 42
}`

it.effect('preserves complete decimal parsing and range failures', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'number-text/parsing',
      ascii(parsingProgram),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it('removes allocating integer rendering without a compatibility path', () => {
  assert.strictEqual(integerSpellings.length, 10)
  for (const spelling of integerSpellings) {
    const source = sourceText(spelling)
    assert.notInclude(source, 'pub effect fn toText(')
    assert.notInclude(source, 'import silk.allocator { Allocator }')
    assert.notInclude(source, 'import silk.allocator { OutOfMemoryError }')
    assert.notInclude(source, 'import silk.string { String }')
    assert.include(source, `pub fn parse(text: string) -> Result<${spelling}, ParseError> {`)
  }
})

it('declares one inline Display witness per integer and no allocating renderer', () => {
  const source = readFileSync(new URL('../stdlib/silk/format.silk', import.meta.url), 'utf8')
  for (const spelling of integerSpellings) assert.include(source, `impl Display for ${spelling} {`)
  assert.notInclude(source, 'unsignedText(')
  assert.notInclude(source, 'signedText(')
  assert.notInclude(source, 'OutOfMemoryError')
  assert.notInclude(source, '? &mut Allocator')
})

it.effect('rejects the removed toText operation as an unknown module member', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'number-text/no-to-text',
      ascii(`import silk.i32 as i32
pub fn main() -> i32 { let text = i32.toText(42) return 0 }`),
      'wasm32-unknown-unknown',
    )
    const diagnostics = Analysis.diagnostics(snapshot)
    assert.deepEqual(
      diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0014'],
    )
  }),
)
