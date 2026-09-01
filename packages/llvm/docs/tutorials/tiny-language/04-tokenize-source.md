# Build Tiny, a compiled language: Tokenize source

**Lesson 4 of 13** · [Previous: Create the project](./03-consumer-setup.md) ·
[Next: Build an AST and resolve arithmetic precedence](./05-precedence-ast.md)

In this lesson, we will turn Tiny source characters into tokens. The finished lexer will recognize
the whole language's keywords and punctuation, preserve source offsets for later diagnostics, and
fail through Effect's typed error channel when it encounters unsupported input.

Our first visible result will be the token stream for:

```text
fn main() = 1 + 2 * 3
```

## Describe a token

Create `src/Token.ts` with a string-literal union for Tiny's keywords, names, integers,
punctuation, arithmetic, comparisons, and end of input:

```typescript
export type Kind =
  | 'Fn'
  | 'If'
  | 'Then'
  | 'Else'
  | 'Identifier'
  | 'Integer'
  | 'LeftParen'
  | 'RightParen'
  | 'Comma'
  | 'Equal'
  | 'Plus'
  | 'Minus'
  | 'Star'
  | 'Slash'
  | 'Less'
  | 'Greater'
  | 'Eof'
```

Then add this immutable token shape:

```typescript
export interface Token {
  readonly kind: Kind
  readonly lexeme: string
  readonly start: number
  readonly end: number
}
```

`start` includes the first UTF-16 offset and `end` points just after the lexeme. For example, the
`fn` at the beginning of our source spans `[0, 2)`. These offsets match JavaScript string indexes
and can later highlight the offending range in a terminal or browser editor.

Add the keyword table and constructors:

```typescript
const keywords: ReadonlyMap<string, Kind> = new Map([
  ['fn', 'Fn'],
  ['if', 'If'],
  ['then', 'Then'],
  ['else', 'Else'],
])

export const make = (kind: Kind, lexeme: string, start: number, end: number): Token =>
  Object.freeze({ kind, lexeme, start, end })

export const classifyIdentifier = (lexeme: string): Kind => keywords.get(lexeme) ?? 'Identifier'
```

The lexer will scan every word with one rule, then ask `Token.classifyIdentifier` whether that
lexeme is a keyword.

## Add a typed lexical diagnostic

Create `src/Diagnostic.ts`:

```typescript
import * as Data from 'effect/Data'

export class LexError extends Data.TaggedError('LexError')<{
  readonly message: string
  readonly start: number
  readonly end: number
  readonly found: string
}> {}
```

This error describes an expected rejection of Tiny source. We will yield it from `Lexer.tokenize`;
we will not throw it.

## Scan from left to right

Create `src/Lexer.ts` and import the three actors:

```typescript
import * as Effect from 'effect/Effect'
import * as Diagnostic from './Diagnostic.js'
import * as Token from './Token.js'
```

Add the read-only punctuation map and character predicates:

```typescript
const punctuation: ReadonlyMap<string, Token.Kind> = new Map([
  ['(', 'LeftParen'],
  [')', 'RightParen'],
  [',', 'Comma'],
  ['=', 'Equal'],
  ['+', 'Plus'],
  ['-', 'Minus'],
  ['*', 'Star'],
  ['/', 'Slash'],
  ['<', 'Less'],
  ['>', 'Greater'],
])

const isWhitespace = (character: string): boolean =>
  character === ' ' || character === '\n' || character === '\r' || character === '\t'

const isDigit = (character: string): boolean => character >= '0' && character <= '9'

const isIdentifierStart = (character: string): boolean =>
  (character >= 'a' && character <= 'z') ||
  (character >= 'A' && character <= 'Z') ||
  character === '_'

const isIdentifierPart = (character: string): boolean =>
  isIdentifierStart(character) || isDigit(character)
```

Keep `-` separate from integer tokens: the parser will later decide whether it means subtraction
or unary negation.

Now implement the public operation:

```typescript
export const tokenize = Effect.fn('Lexer.tokenize')(function* (
  source: string,
): Effect.fn.Return<ReadonlyArray<Token.Token>, Diagnostic.LexError> {
  const tokens: Array<Token.Token> = []
  let offset = 0

  while (offset < source.length) {
    const character = source.charAt(offset)

    if (isWhitespace(character)) {
      offset += 1
      continue
    }

    const start = offset

    if (isDigit(character)) {
      offset += 1
      while (offset < source.length && isDigit(source.charAt(offset))) offset += 1
      const lexeme = source.slice(start, offset)
      tokens.push(Token.make('Integer', lexeme, start, offset))
      continue
    }

    if (isIdentifierStart(character)) {
      offset += 1
      while (offset < source.length && isIdentifierPart(source.charAt(offset))) offset += 1
      const lexeme = source.slice(start, offset)
      tokens.push(Token.make(Token.classifyIdentifier(lexeme), lexeme, start, offset))
      continue
    }

    const kind = punctuation.get(character)
    if (kind !== undefined) {
      offset += 1
      tokens.push(Token.make(kind, character, start, offset))
      continue
    }

    return yield* new Diagnostic.LexError({
      message: `Unexpected character ${JSON.stringify(character)}`,
      start,
      end: start + 1,
      found: character,
    })
  }

  tokens.push(Token.make('Eof', '', source.length, source.length))
  return Object.freeze(tokens)
})
```

Each successful branch either advances `offset` or returns to the top after a scanning loop. The
failure branch yields immediately. That invariant prevents an unsupported character from trapping
the lexer in an infinite loop.

## Verify the token stream

Create `test/Lexer.test.ts` with `it.effect` and tokenize the first function:

```typescript
const tokens = yield * Lexer.tokenize('fn main() = 1 + 2 * 3')
```

Assert the exact values. The result should be:

```text
Fn[0,2) Identifier[3,7) LeftParen[7,8) RightParen[8,9)
Equal[10,11) Integer[12,13) Plus[14,15) Integer[16,17)
Star[18,19) Integer[20,21) Eof[21,21)
```

Run:

```sh
pnpm typecheck
pnpm test
```

The lexer tests should pass. Notice that whitespace produced no tokens, while EOF records the
source length even though its lexeme is empty.

## Check the failure path

Add a test that flips `Lexer.tokenize('@')` into its error value. It should have `_tag` `LexError`,
`found` equal to `@`, and span `[0, 1)`.

If a token starts at the wrong offset, write the source above a row of numeric positions and check
when `start` is captured. Capture it after skipping whitespace but before scanning the lexeme. If a
test never finishes, inspect every loop branch: it must advance `offset`, `continue` after a scan,
or yield `LexError`.

You now have the first complete compiler stage: deterministic Tiny source in, immutable tokens or
a typed diagnostic out. Next, we will arrange arithmetic tokens into an AST whose shape records
calculation order.

[Previous: Create the project](./03-consumer-setup.md) ·
[Next: Build an AST and resolve arithmetic precedence](./05-precedence-ast.md)
