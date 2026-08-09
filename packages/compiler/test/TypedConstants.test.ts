import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Diagnostic from '../src/Diagnostic.js'
import * as FormattedDocument from '../src/FormattedDocument.js'
import * as Formatter from '../src/Formatter.js'
import * as Hir from '../src/Hir.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

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
    const formatted = yield* Formatter.format(syntax)
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
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)

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

it.effect('rejects non-literal and mismatched constant initializers at their declarations', () =>
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
    assert.strictEqual(invalid.length, 3)
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

it.effect('rejects constant storage operations while preserving call-site navigation', () =>
  Effect.gen(function* () {
    const input = `const answer: i32 = 42
pub fn main() -> i32 {
  let called = answer()
  let borrowed = &mut answer
  answer = 1
  return called
}`
    const snapshot = yield* Analysis.ofSourceRealized('constants/operations', encoder.encode(input))
    const codes = Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)
    assert.include(codes, Diagnostic.nonCallableApplicationCode)
    assert.include(codes, Diagnostic.invalidBorrowPositionCode)
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
