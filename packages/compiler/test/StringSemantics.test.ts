import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as Type from '../src/Type.js'
import { elaborate } from './support/elaborate.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyze = (source: string) =>
  elaborate(Parser.parse(Lexer.lex(SourceFile.make('string/Semantics', ascii(source)))))

it('rejects implicit string access and conversions with stable diagnostics', () => {
  const result = analyze(`struct String {}
fn takeBytes(value: &[u8]) -> () { return () }
fn takeMutableBytes(value: &mut [u8]) -> () { return () }
fn takeString(value: string) -> () { return () }
fn takeOwned(value: String) -> () { return () }
fn index(value: string) -> u8 { return value[0] }
fn genericLength(value: string) -> usize { return value.length }
fn implicitBytes(value: string) -> () { return takeBytes(value) }
fn mutableBytes(value: string) -> () { return takeMutableBytes(value) }
fn implicitOwned(value: string) -> () { return takeOwned(value) }
fn implicitView(value: String) -> () { return takeString(value) }
pub fn main() -> i32 { return 0 }`)

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => ({
      code: diagnostic.code,
      reason: diagnostic.reason._tag,
    })),
    [
      { code: 'SEM0032', reason: 'IndexOnNonArray' },
      { code: 'SEM0026', reason: 'ProjectionOnNonStruct' },
      { code: 'SEM0012', reason: 'ArgumentTypeMismatch' },
      { code: 'SEM0012', reason: 'ArgumentTypeMismatch' },
      { code: 'SEM0012', reason: 'ArgumentTypeMismatch' },
      { code: 'SEM0012', reason: 'ArgumentTypeMismatch' },
    ],
  )
})

it.effect('elaborates text and byte literals with distinct semantic types', () =>
  Effect.gen(function* () {
    const source = `pub fn main() -> () {
  let text = "hé"
  let bytes = b"h\\xc3\\xa9"
  return ()
}`
    const snapshot = yield* Analysis.ofSource('string/literals', new TextEncoder().encode(source))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const literals = Analysis.expressionsOf(snapshot, 'string/literals').filter(
      (expression) => expression._tag === 'StaticText',
    )
    assert.strictEqual(literals.length, 2)
    const text = literals.find((literal) => literal.data?.kind === 'Text')
    const bytes = literals.find((literal) => literal.data?.kind === 'Bytes')
    assert.strictEqual(text?.type._tag, 'Available')
    assert.strictEqual(bytes?.type._tag, 'Available')
    if (text?.type._tag === 'Available') {
      assert.isTrue(Type.isString(text.type.type))
      assert.deepEqual(text.data?.bytes, [104, 195, 169])
      assert.strictEqual(text.syntax.span.sourceId, 'string/literals')
      assert.strictEqual(text.syntax.span.start, source.indexOf('"hé"') - 1)
      assert.strictEqual(
        text.syntax.span.end,
        source.indexOf('"hé"') + new TextEncoder().encode('"hé"').length,
      )
    }
    if (bytes?.type._tag === 'Available') {
      assert.strictEqual(Type.key(bytes.type.type), Type.key(Type.slice('Shared', 'u8')))
      assert.deepEqual(bytes.data?.bytes, [104, 195, 169])
    }

    const hir = Analysis.hirOf(snapshot, 'string/literals')
    assert.isDefined(hir)
    if (hir !== undefined) {
      const expressions = hir.functions.flatMap((fn) =>
        fn.statements.flatMap(Hir.statementExpressions).flatMap(Hir.expressionTree),
      )
      const stringLiteral = expressions.find(
        (expression) => expression._tag === 'StaticStringLiteral',
      )
      const byteLiteral = expressions.find(
        (expression) => expression._tag === 'StaticByteViewLiteral',
      )
      assert.strictEqual(stringLiteral?._tag, 'StaticStringLiteral')
      assert.strictEqual(byteLiteral?._tag, 'StaticByteViewLiteral')
      if (stringLiteral?._tag === 'StaticStringLiteral') {
        assert.isTrue(Type.isString(stringLiteral.type))
      }
      if (byteLiteral?._tag === 'StaticByteViewLiteral') {
        assert.strictEqual(Type.key(byteLiteral.type), Type.key(Type.slice('Shared', 'u8')))
      }
      const encoded = Hir.encode(hir)
      assert.include(
        encoded,
        'static-string text:68c3a9 bytes=68c3a9 length=3 provenance=program : string',
      )
      assert.include(encoded, 'static-bytes bytes:68c3a9 bytes=68c3a9 length=3 : &[u8]')
      assert.deepEqual(Hir.verify(hir), [])

      const fn = hir.functions.at(0)
      if (byteLiteral?._tag === 'StaticByteViewLiteral' && fn !== undefined) {
        const runtimeView: Hir.Expression = Object.freeze({
          _tag: 'RuntimeStringView',
          source: byteLiteral,
          heldLoans: Object.freeze([]),
          type: Type.string,
          span: byteLiteral.span,
        })
        const runtimeModule: Hir.Module = Object.freeze({
          ...hir,
          functions: Object.freeze([
            Object.freeze({
              ...fn,
              statements: Object.freeze([
                Object.freeze({
                  _tag: 'Evaluate',
                  expression: runtimeView,
                  region: fn.entryRegion,
                  span: byteLiteral.span,
                }),
                ...fn.statements,
              ]),
            }),
          ]),
        })
        assert.deepEqual(Hir.expressionChildren(runtimeView), [byteLiteral])
        assert.deepEqual(Hir.verify(runtimeModule), [])
        assert.include(Hir.encode(runtimeModule), 'runtime-string-view loans=none : string')
      }
    }
  }),
)

it.effect('passes borrowed string values through ordinary reference and slice boundaries', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'string/ordinary-borrows',
      new TextEncoder().encode(`fn shared(value: &string) -> () { return () }
fn exclusive(value: &mut string) -> () { return () }
fn many(values: &[string]) -> () { return () }

pub fn main() -> () {
  let mut text = "hello"
  shared(&text)
  exclusive(&mut text)
  let values = ["one", "two"]
  many(&values)
  return ()
}`),
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.strictEqual(Analysis.mirOf(snapshot)._tag, 'Available')
  }),
)
