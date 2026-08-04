# `@silk-effect/compiler`

`@silk-effect/compiler` contains the first bootstrap layer of the Silk Effect compiler: immutable
source bytes, source-owned spans, tokens, lexical diagnostics, a deterministic lexer, and a
lossless concrete parser for one public function.

```ts
import { Lexer, Parser, SourceFile, SyntaxTree } from '@silk-effect/compiler'

const source = SourceFile.make(
  'memory://example.silk',
  new TextEncoder().encode('pub fn main() -> I32 { return 42 }'),
)
const lexical = Lexer.lex(source)
const result = Parser.parse(lexical)

console.log(result.root.kind) // SourceFile
console.log(SyntaxTree.tokens(result.root).length === lexical.tokens.length) // true
```

The same actors are available through explicit deep imports such as
`@silk-effect/compiler/SourceFile`, `@silk-effect/compiler/Lexer`, and
`@silk-effect/compiler/Parser`.

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

## First concrete grammar

The parser recognizes exactly this grammatical slice, with whitespace and `//` line comments
allowed between its elements:

```text
File → pub fn Identifier() -> Identifier { return DecimalInteger } EOF
```

The result is a concrete syntax tree (CST), not a semantic AST. Its nodes group the source into a
function declaration, parameter list, return type, block, return statement, and integer literal
expression. Every lexer token—including trivia, invalid tokens, and EOF—remains the same object in
the tree and appears exactly once in source order.

Ordinary source mistakes remain data. A required absent token becomes a `MissingToken` leaf with an
empty span and a `PAR0001` diagnostic. Unexpected concrete input becomes a lossless `Error` node
and a `PAR0002` diagnostic. Lexical diagnostics remain separate on the retained lexical result;
`Parser.parse` does not throw or fail an Effect for these mistakes.

This package intentionally does not yet contain a semantic AST, name resolution, type checking,
HIR, MIR, LLVM lowering, or native compilation. Those belong to later changes after this concrete
grammar and recovery contract have proved useful.

Token families deliberately deferred with those later grammar decisions include string and
character literals, floating-point numbers, general operators, separators, attributes, and any
additional keywords. Until specified, their bytes recover predictably as `Invalid` regions.
