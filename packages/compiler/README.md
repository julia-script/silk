# `@silk-effect/compiler`

`@silk-effect/compiler` contains the first bootstrap layer of the Silk Effect compiler: immutable
source bytes, source-owned spans, tokens, lexical diagnostics, a deterministic lexer, and a
lossless concrete parser and first semantic facts for one public function.

```ts
import { Lexer, Parser, SemanticAnalysis, SourceFile, SyntaxTree } from '@silk-effect/compiler'

const source = SourceFile.make(
  'memory://example.silk',
  new TextEncoder().encode('pub fn main() -> I32 { return 42 }'),
)
const lexical = Lexer.lex(source)
const parse = Parser.parse(lexical)
const result = SemanticAnalysis.analyze(parse)

console.log(parse.root.kind) // SourceFile
console.log(SyntaxTree.tokens(parse.root).length === lexical.tokens.length) // true
console.log(result.declaration.name) // { _tag: 'Present', spelling: 'main', ... }
console.log(result.integerExpression) // { _tag: 'Available', type: 'I32', value: 42, ... }
console.log(result.returnCompatibility) // { _tag: 'Compatible' }
```

The same actors are available through explicit deep imports such as
`@silk-effect/compiler/SourceFile`, `@silk-effect/compiler/Lexer`, and
`@silk-effect/compiler/Parser`. Semantic facts and diagnostics are available through
`@silk-effect/compiler/SemanticAnalysis` and `@silk-effect/compiler/SemanticDiagnostic`.

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

## First semantic facts

`SemanticAnalysis.analyze` retains the exact parse result and describes its single function as
immutable, syntax-provenanced facts. The declaration has a deterministic source-local identity,
public visibility, zero parameters, a present or unavailable name, and a resolved, unresolved, or
unavailable declared return type. `SemanticAnalysis.declarationByName` supports data-first and
pipeable lookup without hiding unavailable syntax behind `undefined`.

This slice recognizes only the exact ASCII type spelling `I32` and positive decimal values from
`0` through `2147483647`. It interprets token bytes without host-number precision loss. A present
unknown type produces `SEM0001`; a present integer above the boundary produces `SEM0002`. Missing
or damaged syntax remains unavailable and belongs to parser diagnostics, so lexical, parser, and
semantic diagnostics remain separate ordered collections.

These are direct semantic facts over the concrete tree—not a semantic AST or a general type
checker. The package intentionally does not yet contain an AST, HIR, MIR, LLVM lowering, or native
compilation. Those layers follow only after this narrow source-to-fact contract proves useful.

Token families deliberately deferred with those later grammar decisions include string and
character literals, floating-point numbers, general operators, separators, attributes, and any
additional keywords. Until specified, their bytes recover predictably as `Invalid` regions.
