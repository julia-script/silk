import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Lexer from '../src/Lexer.js'
import * as SourceFile from '../src/SourceFile.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it('does not retain flow as an effect-function keyword or semantic type alias', () => {
  const lexical = Lexer.lex(SourceFile.make('compat/flow-keyword', ascii('flow fn old() {}')))
  assert.deepEqual(
    lexical.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    [
      'Identifier',
      'FnKeyword',
      'Identifier',
      'LeftParenthesis',
      'RightParenthesis',
      'LeftBrace',
      'RightBrace',
      'EndOfFile',
    ],
  )
  assert.strictEqual('flow' in Type, false)
  assert.strictEqual('isFlow' in Type, false)
})

it.effect('rejects legacy flow declarations and Flow actor calls', () =>
  Effect.gen(function* () {
    const declaration = yield* Analysis.ofSource(
      'compat/flow-declaration',
      ascii('flow fn old() -> I32 { return 1 } pub fn main() -> I32 { return 0 }'),
    )
    const actor = yield* Analysis.ofSource(
      'compat/flow-actor',
      ascii('pub fn main() -> I32 { let value = Flow.catch(1, 2) return 0 }'),
    )
    assert.isAbove(Analysis.diagnostics(declaration).length, 0)
    assert.isAbove(Analysis.diagnostics(actor).length, 0)
  }),
)
