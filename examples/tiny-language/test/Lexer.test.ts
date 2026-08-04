import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Lexer from '../src/Lexer.js'

it.effect('tokenizes a Tiny function with exact source spans', () =>
  Effect.gen(function* () {
    const tokens = yield* Lexer.tokenize('fn main() = 1 + 2 * 3')

    assert.deepStrictEqual(tokens, [
      { kind: 'Fn', lexeme: 'fn', start: 0, end: 2 },
      { kind: 'Identifier', lexeme: 'main', start: 3, end: 7 },
      { kind: 'LeftParen', lexeme: '(', start: 7, end: 8 },
      { kind: 'RightParen', lexeme: ')', start: 8, end: 9 },
      { kind: 'Equal', lexeme: '=', start: 10, end: 11 },
      { kind: 'Integer', lexeme: '1', start: 12, end: 13 },
      { kind: 'Plus', lexeme: '+', start: 14, end: 15 },
      { kind: 'Integer', lexeme: '2', start: 16, end: 17 },
      { kind: 'Star', lexeme: '*', start: 18, end: 19 },
      { kind: 'Integer', lexeme: '3', start: 20, end: 21 },
      { kind: 'Eof', lexeme: '', start: 21, end: 21 },
    ])
  }),
)

it.effect('classifies keywords after scanning one identifier lexeme', () =>
  Effect.gen(function* () {
    const tokens = yield* Lexer.tokenize('\nif then\telse fn value_2')
    assert.deepStrictEqual(
      tokens.map(({ kind, lexeme }) => ({ kind, lexeme })),
      [
        { kind: 'If', lexeme: 'if' },
        { kind: 'Then', lexeme: 'then' },
        { kind: 'Else', lexeme: 'else' },
        { kind: 'Fn', lexeme: 'fn' },
        { kind: 'Identifier', lexeme: 'value_2' },
        { kind: 'Eof', lexeme: '' },
      ],
    )
  }),
)

it.effect('fails at an unsupported character without stalling the cursor', () =>
  Effect.gen(function* () {
    const error = yield* Effect.flip(Lexer.tokenize('@'))
    assert.strictEqual(error._tag, 'LexError')
    assert.strictEqual(error.start, 0)
    assert.strictEqual(error.end, 1)
    assert.strictEqual(error.found, '@')
  }),
)
