# `@silk-effect/compiler`

`@silk-effect/compiler` contains the first bootstrap layer of the Silk Effect compiler: immutable
source bytes, source-owned spans, tokens, lexical diagnostics, a deterministic lexer, and a
lossless concrete parser and ordered semantic facts for one or more public functions.

```ts
import { Lexer, Parser, SemanticAnalysis, SourceFile, SyntaxTree } from '@silk-effect/compiler'

const source = SourceFile.make(
  'memory://example.silk',
  new TextEncoder().encode(`pub fn answer() -> I32 { return 42 }
pub fn main() -> I32 { return 0 }`),
)
const lexical = Lexer.lex(source)
const parse = Parser.parse(lexical)
const result = SemanticAnalysis.analyze(parse)

console.log(parse.root.kind) // SourceFile
console.log(SyntaxTree.tokens(parse.root).length === lexical.tokens.length) // true
console.log(result.functions.length) // 2
console.log(result.functions[0]?.declaration.name) // { _tag: 'Present', spelling: 'answer', ... }
console.log(result.functions[0]?.integerExpression) // { _tag: 'Available', type: 'I32', value: 42, ... }
console.log(SemanticAnalysis.declarationByName(result, 'main')) // { _tag: 'Resolved', ... }
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

## Bootstrap concrete grammar

The parser recognizes exactly this grammatical slice, with whitespace and `//` line comments
allowed between its elements:

```text
File                → FunctionDeclaration+ EOF
FunctionDeclaration → pub fn Identifier() -> Identifier { return DecimalInteger }
```

The result is a concrete syntax tree (CST), not a semantic AST. Its nodes group the source into one
or more direct function declarations in source order. Each declaration contains a parameter list,
return type, block, return statement, and integer literal expression. Every lexer
token—including trivia, invalid tokens, and EOF—remains the same object in the tree and appears
exactly once in source order. A following `pub` also bounds recovery when the prior function is
missing its closing brace.

Ordinary source mistakes remain data. A required absent token becomes a `MissingToken` leaf with an
empty span and a `PAR0001` diagnostic. Unexpected concrete input becomes a lossless `Error` node
and a `PAR0002` diagnostic. Lexical diagnostics remain separate on the retained lexical result;
`Parser.parse` does not throw or fail an Effect for these mistakes.

## Bootstrap semantic facts

`SemanticAnalysis.analyze` retains the exact parse result and publishes an immutable ordered
`functions` collection. Each `FunctionFact` groups one declaration, returned integer expression,
and return compatibility. Declaration identities combine the source identity with the function's
zero-based concrete-source ordinal; missing names do not change later ordinals. Each declaration
also retains public visibility, zero parameters, a present or unavailable name, and a resolved,
unresolved, or unavailable declared return type.

`SemanticAnalysis.declarationByName` supports data-first and pipeable lookup with closed `Resolved`,
`Missing`, and `Ambiguous` outcomes. It never silently selects the first duplicate. Missing recovered
names do not enter lookup, while every present duplicate after the first produces `SEM0003` at the
later name span and retains the original name span in its reason data.

This slice recognizes only the exact ASCII type spelling `I32` and positive decimal values from
`0` through `2147483647`. It interprets token bytes without host-number precision loss. A present
unknown type produces `SEM0001`; a present integer above the boundary produces `SEM0002`. Every
function is analyzed independently. Missing or damaged syntax remains unavailable and belongs to
parser diagnostics, so lexical, parser, and semantic diagnostics remain separate ordered
collections.

These are direct semantic facts over the concrete tree—not a semantic AST or a general type
checker. The package intentionally does not yet contain an AST, HIR, MIR, LLVM lowering, or native
compilation. Calls, reference resolution, and a general scope graph are also intentionally deferred.
Those layers follow only after this narrow source-to-fact contract proves useful.

Token families deliberately deferred with those later grammar decisions include string and
character literals, floating-point numbers, general operators, separators, attributes, and any
additional keywords. Until specified, their bytes recover predictably as `Invalid` regions.
