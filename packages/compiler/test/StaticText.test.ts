import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Lexer from '../src/Lexer.js'
import * as LiteralForm from '../src/LiteralForm.js'
import * as Mir from '../src/Mir.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as StaticText from '../src/StaticText.js'
import * as SyntaxTree from '../src/SyntaxTree.js'
import * as Projections from './support/projections.js'

const encoder = new TextEncoder()

const source = `pub fn main() -> i32 {
  let text = """hé
"""
  let bytes = b"""life
"""
  let repeated = "hé\\n"
  if Intrinsic.stringByteLength(text) != 4 { return 0 }
  if bytes.length != 5 { return 1 }
  if Intrinsic.stringByteLength(repeated) != Intrinsic.stringByteLength(text) { return 2 }
  return 42
}`

const formOf = (value: string): LiteralForm.LiteralForm => {
  const form = LiteralForm.recognize(encoder.encode(value))
  if (form === undefined) throw new Error(`expected a recognized literal form: ${value}`)
  return form
}

it('lexes and parses literal spelling losslessly', () => {
  const file = SourceFile.make('static/lossless', encoder.encode('"hé\\n" b"life\\x0a"'))
  const lexical = Lexer.lex(file)
  assert.deepEqual(
    lexical.tokens.map((token) => token.kind),
    ['TextLiteral', 'Whitespace', 'ByteStringLiteral', 'EndOfFile'],
  )
  assert.deepEqual(lexical.diagnostics, [])

  const parsed = Parser.parse(
    Lexer.lex(
      SourceFile.make(
        'static/parse',
        encoder.encode('pub fn main() -> i32 { let value = "hé\\n" return value.length }'),
      ),
    ),
  )
  const nodes = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> =>
    Object.freeze([
      node,
      ...node.children.flatMap((child) => (SyntaxTree.isNode(child) ? nodes(child) : [])),
    ])
  const literals = nodes(parsed.root).filter((node) => node.kind === 'StaticTextLiteralExpression')
  assert.strictEqual(literals.length, 1)
  assert.deepEqual(parsed.lexicalDiagnostics, [])
  assert.deepEqual(parsed.parserDiagnostics, [])
})

it('decodes UTF-8 and exact bytes atomically', () => {
  const textSpelling = '"hé\\n\\u{1f642}"'
  const text = StaticText.decode(Array.from(encoder.encode(textSpelling)), formOf(textSpelling))
  assert.strictEqual(text._tag, 'Decoded')
  if (text._tag === 'Decoded') {
    assert.deepEqual(text.data.bytes, Array.from(encoder.encode('hé\n🙂')))
    assert.strictEqual(text.data.utf8, true)
  }
  const byteSpelling = 'b"\\x00\\xff"'
  assert.deepEqual(
    StaticText.decode(Array.from(encoder.encode(byteSpelling)), formOf(byteSpelling)),
    {
      _tag: 'Decoded',
      data: {
        _tag: 'StaticData',
        id: 'bytes:00ff',
        kind: 'Bytes',
        bytes: [0, 255],
        utf8: false,
      },
    },
  )
  assert.strictEqual(
    StaticText.decode(Array.from(encoder.encode('b"\\u{100}"')), formOf('b"\\u{100}"'))._tag,
    'Invalid',
  )
  assert.strictEqual(
    StaticText.decode(Array.from(encoder.encode('"\\q"')), formOf('"\\q"'))._tag,
    'Invalid',
  )
})

it('decodes multiline content exactly without dedenting and normalizes only physical CRLF', () => {
  const spelling = '"""\r\n  first  \r\n second\n"""'
  const decoded = StaticText.decode(Array.from(encoder.encode(spelling)), formOf(spelling))
  assert.strictEqual(decoded._tag, 'Decoded')
  if (decoded._tag === 'Decoded') {
    assert.deepEqual(decoded.data.bytes, Array.from(encoder.encode('\n  first  \n second\n')))
  }

  const explicit = 'b"""\\r\\n"""'
  const explicitDecoded = StaticText.decode(Array.from(encoder.encode(explicit)), formOf(explicit))
  assert.strictEqual(explicitDecoded._tag, 'Decoded')
  if (explicitDecoded._tag === 'Decoded') assert.deepEqual(explicitDecoded.data.bytes, [13, 10])

  const continuation = '"""before\\\nafter"""'
  assert.strictEqual(
    StaticText.decode(Array.from(encoder.encode(continuation)), formOf(continuation))._tag,
    'Invalid',
  )
})

it('parses all four literal forms as pipeline operands', () => {
  const parsed = Parser.parse(
    Lexer.lex(
      SourceFile.make(
        'static/pipelines',
        encoder.encode(`import silk.string { String }
pub fn main() -> i32 {
  let a = "abc" |> String.uppercase
  let b = b"abc" |> String.uppercase
  let c = """abc""" |> String.uppercase
  let d = b"""abc""" |> String.uppercase
  return 0
}`),
      ),
    ),
  )
  const nodes = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> =>
    Object.freeze([
      node,
      ...node.children.flatMap((child) => (SyntaxTree.isNode(child) ? nodes(child) : [])),
    ])
  assert.strictEqual(
    nodes(parsed.root).filter((node) => node.kind === 'PipelineExpression').length,
    4,
  )
  assert.deepEqual(parsed.lexicalDiagnostics, [])
  assert.deepEqual(parsed.parserDiagnostics, [])
})

it.effect('recovers malformed escapes before the following declaration', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'static/recovery',
      encoder.encode(`fn broken() -> i32 { let bad = "\\q" return 0 }
pub fn main() -> i32 { return 42 }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0085'],
    )
    assert.strictEqual(Projections.hirOf(snapshot, 'static/recovery')?.functions.length, 2)
  }),
)

it.effect('keeps lexical literal sentinels out of parser and semantic diagnostic cascades', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'static/lexical-sentinel',
      encoder.encode(`pub fn main() -> i32 {
  let bad = future"value"
  return 42
}`),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['LEX0002'],
    )
    assert.strictEqual(Projections.hirOf(snapshot, 'static/lexical-sentinel')?.functions.length, 1)
  }),
)

it.effect('keeps static bytes, reuse, lengths, and backend placement in parity', () =>
  Effect.gen(function* () {
    const native = yield* Analysis.ofSourceRealized(
      'static/parity',
      encoder.encode(source),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(native), [])
    const mir = Analysis.loweredMir(native)
    assert.deepEqual(
      mir.staticData?.map((data) => ({ id: data.id, bytes: data.bytes })),
      [
        { id: 'bytes:6c6966650a', bytes: [108, 105, 102, 101, 10] },
        { id: 'text:68c3a90a', bytes: [104, 195, 169, 10] },
      ],
    )
    assert.isFalse(
      mir.functions.some((fn) =>
        Mir.operations(fn).some((operation) => operation._tag === 'Allocate'),
      ),
    )
    assert.strictEqual(native.layout._tag, 'Available')
    if (native.layout._tag === 'Available') {
      assert.deepEqual(
        native.layout.value.staticData?.map((placement) => placement.lengthBits),
        [64, 64],
      )
    }
    const evaluated = Analysis.evaluate(native)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      JSON.stringify(
        {
          evaluated,
          staticData: native.layout._tag === 'Available' ? native.layout.value.staticData : [],
        },
        (_, value) => (typeof value === 'bigint' ? value.toString() : value),
      ),
    )
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
    const llvm = yield* Analysis.codegen(native, { mode: 'release' })
    assert.include(llvm.ir, 'constant [5 x i8] c"life\\0A"')

    const wasm = yield* Analysis.ofSourceRealized(
      'static/parity-wasm',
      encoder.encode(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(wasm), [])
    assert.strictEqual(wasm.layout._tag, 'Available')
    if (wasm.layout._tag === 'Available') {
      assert.deepEqual(
        wasm.layout.value.staticData?.map((placement) => placement.lengthBits),
        [32, 32],
      )
    }
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    assert.include(artifact.wat, '(data')
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    const main = instance.exports.silk_main
    if (typeof main !== 'function') throw new Error('static-text program lost silk_main')
    assert.strictEqual(main(), 42)
  }),
)
