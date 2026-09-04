import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Diagnostic from '../src/Diagnostic.js'
import * as Lexer from '../src/Lexer.js'
import * as SourceFile from '../src/SourceFile.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect('lexes `&&` and `||` as one token each', () =>
  Effect.gen(function* () {
    const lexed = Lexer.lex(
      SourceFile.make(
        'short-circuit/tokens',
        ascii('fn both(a: bool, b: bool) -> bool { return a && b || a & b == b }'),
      ),
    )
    const kinds = lexed.tokens
      .filter(
        (token) =>
          token.kind === 'AmpersandAmpersand' ||
          token.kind === 'PipePipe' ||
          token.kind === 'Ampersand' ||
          token.kind === 'Pipe',
      )
      .map((token) => token.kind)
    assert.deepEqual(kinds, ['AmpersandAmpersand', 'PipePipe', 'Ampersand'])
    const compound = lexed.tokens.filter(
      (token) => token.kind === 'AmpersandAmpersand' || token.kind === 'PipePipe',
    )
    for (const token of compound) assert.strictEqual(token.span.end - token.span.start, 2)
    yield* Effect.void
  }),
)

it.effect('rejects a non-bool operand on either side', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'short-circuit/non-bool',
      ascii(`fn left(a: i32, b: bool) -> bool { return a && b }
fn right(a: bool, b: i32) -> bool { return a || b }
pub fn main() -> i32 { return 0 }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [Diagnostic.argumentTypeMismatchCode, Diagnostic.argumentTypeMismatchCode],
    )
  }),
)

it.effect('rejects a use reached after one short-circuit path moves its owner', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'short-circuit/conditional-use-after-move',
      ascii(`struct Flag { value: bool }
fn unwrap(flag: Flag) -> bool { return flag.value }
fn invalid(gate: bool, flag: Flag) -> bool {
  let selected = gate && unwrap(move flag)
  return selected && flag.value
}
pub fn main() -> i32 { return 0 }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [Diagnostic.useAfterMoveCode],
    )
  }),
)
