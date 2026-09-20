import * as AnalysisFixture from './support/AnalysisFixture.js'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Tir from '../src/Tir.js'
import * as Lexer from '../src/Lexer.js'
import * as Lifetime from '../src/Lifetime.js'
import * as Parser from '../src/Parser.js'
import * as SemanticContext from '../src/SemanticContext.js'
import * as SourceFile from '../src/SourceFile.js'
import * as Type from '../src/Type.js'
import { elaborate } from './support/elaborate.js'
import * as Projections from './support/projections.js'

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
      { code: 'SEM0052', reason: 'TypeArgumentInference' },
      { code: 'SEM0052', reason: 'TypeArgumentInference' },
      { code: 'SEM0012', reason: 'ArgumentTypeMismatch' },
      { code: 'SEM0052', reason: 'TypeArgumentInference' },
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
      (expression) =>
        expression._tag === 'StaticStringLiteral' || expression._tag === 'StaticByteViewLiteral',
    )
    assert.strictEqual(literals.length, 2)
    const text = literals.find((literal) => literal._tag === 'StaticStringLiteral')
    const bytes = literals.find((literal) => literal._tag === 'StaticByteViewLiteral')
    if (text?._tag === 'StaticStringLiteral') {
      assert.isTrue(Type.isString(text.type))
      assert.deepEqual(text.data.bytes, [104, 195, 169])
      // Presentation spans are trivia-free: the literal starts at its own opening quote.
      const span = SemanticContext.fromModules(snapshot.closure.modules).spanOf(text.origin.anchor)
      assert.strictEqual(span.sourceId, 'string/literals')
      assert.strictEqual(span.start, source.indexOf('"hé"'))
      assert.strictEqual(span.end, source.indexOf('"hé"') + new TextEncoder().encode('"hé"').length)
    }
    if (bytes?._tag === 'StaticByteViewLiteral') {
      assert.strictEqual(
        Type.key(bytes.type),
        Type.key(Type.slice('Shared', 'u8', Lifetime.staticLifetime)),
      )
      assert.deepEqual(bytes.data.bytes, [104, 195, 169])
    }

    const tir = Projections.tirOf(snapshot, 'string/literals')
    assert.isDefined(tir)
    if (tir !== undefined) {
      const expressions = tir.functions.flatMap((fn) =>
        fn.statements.flatMap(Tir.statementExpressions).flatMap(Tir.expressionTree),
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
        assert.strictEqual(
          Type.key(byteLiteral.type),
          Type.key(Type.slice('Shared', 'u8', Lifetime.staticLifetime)),
        )
      }
      const encoded = Tir.encode(tir)
      assert.include(
        encoded,
        'static-string text:68c3a9 bytes=68c3a9 length=3 provenance=program : string',
      )
      assert.include(encoded, "static-bytes bytes:68c3a9 bytes=68c3a9 length=3 : &'static [u8]")
      assert.deepEqual(Tir.verify(tir), [])

      const fn = tir.functions.at(0)
      if (byteLiteral?._tag === 'StaticByteViewLiteral' && fn !== undefined) {
        const runtimeView: Tir.Expression = Object.freeze({
          _tag: 'RuntimeStringView',
          source: byteLiteral,
          heldLoans: Object.freeze([]),
          type: Type.string(Lifetime.staticLifetime),
          span: byteLiteral.span,
          origin: byteLiteral.origin,
        })
        const runtimeModule: Tir.Module = Object.freeze({
          ...tir,
          functions: Object.freeze([
            Object.freeze({
              ...fn,
              statements: Object.freeze([
                Object.freeze({
                  _tag: 'Evaluate',
                  expression: runtimeView,
                  region: fn.entryRegion,
                  span: byteLiteral.span,
                  origin: byteLiteral.origin,
                }),
                ...fn.statements,
              ]),
            }),
          ]),
        })
        assert.deepEqual(Tir.expressionChildren(runtimeView), [byteLiteral])
        assert.deepEqual(Tir.verify(runtimeModule), [])
        assert.include(Tir.encode(runtimeModule), 'runtime-string-view loans=none : string')
      }
    }
  }),
)

it.effect('passes borrowed string values through ordinary reference and slice boundaries', () =>
  Effect.gen(function* () {
    const snapshot = yield* AnalysisFixture.retainingMain(
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
