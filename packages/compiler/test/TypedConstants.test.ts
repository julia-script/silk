import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Diagnostic from '../src/Diagnostic.js'
import * as FormattedDocument from '../src/FormattedDocument.js'
import * as Hir from '../src/Hir.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as SyntaxFormatter from '../src/SyntaxFormatter.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

const library = `pub const answer: i32 = 40
pub const step: i32 = 2
pub const enabled: bool = true
pub const ratio: f32 = 1.5`

const source = `import constants as values { answer }

const local: i32 = 0

pub fn main() -> i32 {
  if values.enabled {
    return answer + values.step + local
  }
  return 0
}`

it.effect('parses and formats typed constant declarations losslessly', () =>
  Effect.gen(function* () {
    const syntax = Parser.parse(
      Lexer.lex(
        SourceFile.make(
          'constants/syntax',
          encoder.encode('pub   const answer:i32=40\nconst enabled : bool = true'),
        ),
      ),
    )
    assert.deepEqual(syntax.lexicalDiagnostics, [])
    assert.deepEqual(syntax.parserDiagnostics, [])
    assert.deepEqual(
      syntax.root.children.flatMap((child) => (child._tag === 'SyntaxNode' ? [child.kind] : [])),
      ['ConstantDeclaration', 'ConstantDeclaration'],
    )
    assert.strictEqual(
      syntax.tokens.find((token) => token.kind === 'ConstKeyword')?.kind,
      'ConstKeyword',
    )
    const formatted = yield* SyntaxFormatter.format(syntax)
    assert.strictEqual(
      decoder.decode(FormattedDocument.toUint8Array(formatted)),
      'pub const answer: i32 = 40\n\nconst enabled: bool = true\n',
    )
  }),
)

it.effect('resolves typed constants and lowers only immediate values', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.makeRealized({
      root: SourceFile.make('main', encoder.encode(source)),
    }).pipe(
      Effect.provide(SourceResolver.memory(new Map([['constants', encoder.encode(library)]]))),
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)

    const native = yield* Analysis.codegen(snapshot, { mode: 'release' })
    assert.isAbove(native.bitcode.length, 0)
    const wasmSnapshot = yield* Analysis.makeRealized({
      root: SourceFile.make('main', encoder.encode(source)),
      target: 'wasm32-unknown-unknown',
    }).pipe(
      Effect.provide(SourceResolver.memory(new Map([['constants', encoder.encode(library)]]))),
    )
    assert.deepEqual(Analysis.diagnostics(wasmSnapshot), [])
    const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
    assert.isAbove(wasm.bytes.length, 0)

    const hir = Hir.encode(Analysis.rootAnalysis(snapshot).hir)
    assert.notInclude(hir, 'Constant')
    assert.include(hir, 'literal 40 : i32')
    assert.include(hir, 'literal true : bool')

    const reference = Analysis.semanticOccurrenceAt(snapshot, 'main', source.indexOf('answer +'))
    assert.strictEqual(reference?.role, 'Value')
    assert.strictEqual(reference?.resolution._tag, 'Available')
    assert.strictEqual(reference?.declaration?.module, 'constants')
    assert.strictEqual(
      reference === undefined
        ? undefined
        : Analysis.occurrencePresentation(snapshot, 'main', reference)?.text,
      'pub const answer: i32',
    )
  }),
)

it.effect('accepts computed constant initializers while rejecting mismatched values', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'constants/invalid',
      encoder.encode(`const wrong: bool = 1
const computed: i32 = 40 + 2
const answer: i32 = 42
pub fn main() -> i32 { return move answer }`),
    )
    const invalid = Analysis.diagnostics(snapshot).filter(
      (diagnostic) => diagnostic.code === 'SEM0086',
    )
    assert.strictEqual(invalid.length, 2)
  }),
)

it.effect('evaluates computed primitive constants across module boundaries', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.makeRealized({
      root: SourceFile.make(
        'constants/computed-main',
        encoder.encode(`import constants.computed { answer }
pub fn main() -> i32 { return answer }`),
      ),
    }).pipe(
      Effect.provide(
        SourceResolver.memory(
          new Map([
            [
              'constants/computed',
              encoder.encode(`pub const base: i32 = 40
pub const answer: i32 = base + 2`),
            ],
          ]),
        ),
      ),
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('reports one deterministic cycle diagnostic for cyclic constants', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'constants/cycle',
      encoder.encode(`const first: i32 = second
const second: i32 = first
pub fn main() -> i32 { return 0 }`),
    )

    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [Diagnostic.staticEvaluationCycleCode],
    )
  }),
)

/**
 * A string constant holds the literal's decoded bytes, so its reference lowers to the very static
 * datum the equivalent `let` binding lowers to. Identical HIR data ids are that byte equality.
 */
const stringConstantSource = (initializer: string, binding: string) =>
  `import silk.usize as usize
import silk.string { String }

const pattern: string = ${initializer}

pub fn main() -> i32 {
  let inline = ${binding}
  return usize.toI32(String.byteLength(pattern)) + usize.toI32(String.byteLength(inline))
}`

const staticStringIds = (hir: string): ReadonlyArray<string> =>
  hir.split('\n').flatMap((line) => line.match(/static-string (\S+)/)?.slice(1, 2) ?? [])

it.effect('gives an escaped string constant the same value as the equivalent let binding', () =>
  Effect.gen(function* () {
    const source = stringConstantSource('"\\\\d+"', '"\\\\d+"')
    const snapshot = yield* Analysis.ofSourceRealized('constants/string', encoder.encode(source))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const ids = staticStringIds(Hir.encode(Analysis.rootAnalysis(snapshot).hir))
    assert.deepEqual(ids, ['text:5c642b', 'text:5c642b'])

    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 6n)
  }),
)

it.effect('gives a raw string constant the same value as the equivalent let binding', () =>
  Effect.gen(function* () {
    // `r"\d+"` and `"\\d+"` decode to the same three bytes, so the constant and the escaped
    // binding must share one static datum.
    const source = stringConstantSource('r"\\d+"', '"\\\\d+"')
    const snapshot = yield* Analysis.ofSourceRealized('constants/raw', encoder.encode(source))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const ids = staticStringIds(Hir.encode(Analysis.rootAnalysis(snapshot).hir))
    assert.deepEqual(ids, ['text:5c642b', 'text:5c642b'])

    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 6n)
  }),
)

it.effect('compiles the raw-string example block that motivated the raw literal form', () =>
  Effect.gen(function* () {
    // Reproduced verbatim from the Example block of issue #13.
    const example = String.raw`const decimalPattern: string = r"\d+\.\d+"
const windowsPath: string = r"C:\Users\build\output"

const helpText: string = r"""
Usage: silk build [options]
  --target \path\to\dir
"""

pub fn main() -> i32 { return 0 }`
    const snapshot = yield* Analysis.ofSourceRealized('constants/example', encoder.encode(example))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect('resolves a public string constant across a module boundary', () =>
  Effect.gen(function* () {
    const strings = 'pub const greeting: string = "hi"'
    const main = `import silk.usize as usize
import strings { greeting }
import silk.string { String }

pub fn main() -> i32 { return usize.toI32(String.byteLength(greeting)) }`
    const snapshot = yield* Analysis.makeRealized({
      root: SourceFile.make('main', encoder.encode(main)),
    }).pipe(Effect.provide(SourceResolver.memory(new Map([['strings', encoder.encode(strings)]]))))

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 2n)

    const reference = Analysis.semanticOccurrenceAt(snapshot, 'main', main.indexOf('greeting)'))
    assert.strictEqual(reference?.declaration?.module, 'strings')
    assert.strictEqual(
      reference === undefined
        ? undefined
        : Analysis.occurrencePresentation(snapshot, 'main', reference)?.text,
      'pub const greeting: string',
    )
  }),
)

it.effect('accepts constant references while retaining string initializer restrictions', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'constants/string-invalid',
      encoder.encode(`const source: string = "a"
const copied: string = source
const bytes: string = b"a"
const escape: string = "\\q"
pub fn main() -> i32 { return 0 }`),
    )
    const messages = Analysis.diagnostics(snapshot)
      .filter((diagnostic) => diagnostic.code === 'SEM0086')
      .map((diagnostic) => diagnostic.message)
    assert.deepEqual(messages, [
      'Invalid constant: a byte-string literal does not produce a string',
      'Invalid constant: unknown escape sequence',
    ])
  }),
)

it.effect('checks usize constants against the selected target even when unused', () =>
  Effect.gen(function* () {
    const wide = '4294967296'
    const input = `const wide: usize = ${wide}
pub fn main() -> i32 { return 0 }`
    const native = yield* Analysis.ofSourceRealized(
      'constants/usize',
      encoder.encode(input),
      'aarch64-apple-darwin',
    )
    assert.notInclude(
      Analysis.diagnostics(native).map((diagnostic) => diagnostic.code),
      Diagnostic.usizeTargetOutOfRangeCode,
    )

    const wasm = yield* Analysis.ofSourceRealized(
      'constants/usize',
      encoder.encode(input),
      'wasm32-unknown-unknown',
    )
    const overflow = Analysis.diagnostics(wasm).filter(
      (diagnostic) => diagnostic.code === Diagnostic.usizeTargetOutOfRangeCode,
    )
    assert.strictEqual(overflow.length, 1)
    assert.strictEqual(overflow[0]?.span.start, input.indexOf(wide))
  }),
)

it.effect(
  'rejects calling and assigning constants while permitting a borrowed materialization',
  () =>
    Effect.gen(function* () {
      const input = `const answer: i32 = 42
pub fn main() -> i32 {
  let called = answer()
  let borrowed = &mut answer
  answer = 1
  return called
}`
      const snapshot = yield* Analysis.ofSourceRealized(
        'constants/operations',
        encoder.encode(input),
      )
      const codes = Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)
      assert.include(codes, Diagnostic.nonCallableApplicationCode)
      assert.notInclude(codes, Diagnostic.invalidBorrowPositionCode)
      assert.include(codes, Diagnostic.invalidAssignmentPlaceCode)
      const occurrence = Analysis.semanticOccurrenceAt(
        snapshot,
        'constants/operations',
        input.indexOf('answer()'),
      )
      assert.strictEqual(occurrence?.resolution._tag, 'Available')
      assert.strictEqual(occurrence?.declaration?.module, 'constants/operations')
      assert.strictEqual(
        occurrence === undefined
          ? undefined
          : Analysis.occurrencePresentation(snapshot, 'constants/operations', occurrence)?.text,
        'const answer: i32',
      )
    }),
)
