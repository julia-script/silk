# `@silk-effect/compiler`

`@silk-effect/compiler` contains the first bootstrap layer of the Silk Effect compiler: immutable
source bytes, source-owned spans, tokens, lexical diagnostics, and a deterministic lexer.

```ts
import { Lexer, SourceFile } from '@silk-effect/compiler'

const source = SourceFile.make(
  'memory://example.silk',
  Uint8Array.from('pub fn main() -> I32 {}', (character) => character.charCodeAt(0)),
)
const result = Lexer.lex(source)
```

The same actors are available through explicit deep imports such as
`@silk-effect/compiler/SourceFile` and `@silk-effect/compiler/Lexer`.

## Byte and span conventions

- Source input is an arbitrary byte sequence, not assumed to be valid UTF-8.
- `SourceFile.make` copies its input and attaches a caller-provided logical identity.
- A `SourceSpan` is an owner-qualified half-open byte range `[start, end)`.
- Empty spans represent positions; EOF is `[sourceLength, sourceLength)`.
- A source only returns bytes for a span with the same identity and in-bounds offsets.

## Bootstrap lexer vocabulary

The lexer recognizes ASCII identifiers, the `pub`, `fn`, and `return` keywords, decimal integers,
parentheses, braces, `->`, whitespace, and `//` line comments. Trivia is retained as tokens.
Unsupported bytes form maximal `Invalid` tokens and ordered `LEX0001` diagnostics, so lexing always
makes progress and every input byte can be reconstructed from the non-EOF token spans.

This package intentionally does not yet contain a parser, AST, name resolution, type checking,
HIR, MIR, LLVM lowering, or native compilation. Those belong to later changes after this byte and
token contract has proved useful.

Token families deliberately deferred with those later grammar decisions include string and
character literals, floating-point numbers, general operators, separators, attributes, and any
additional keywords. Until specified, their bytes recover predictably as `Invalid` regions.
