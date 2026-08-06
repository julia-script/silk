## Context

See `proposal.md` for motivation. The compiler currently lexes the complete source into one token
array before parsing. `<` and `>` are ordinary punctuation tokens, fixed-array types are recognized
by a type-context special case for `Array<...>`, and expression parsing already separates primary
expression recognition from the infix-operator loop. The concrete syntax tree is lossless and the
formatter prints from that tree.

The future template language will eventually need contextual scanning for template tags, template
text, and embedded Silk expressions. This change only protects the grammar boundary needed for that
future work; it does not introduce those scanner modes.

## Goals / Non-Goals

**Goals:**

- Give fixed-array source types an angle-free, recursively composable grammar.
- Make primary-expression `<` a deliberate reservation while preserving relational `<` after a
  left operand.
- Preserve lossless syntax, local parser recovery, canonical formatting, and the existing semantic
  fixed-array representation.
- Make the old source spelling fail visibly instead of creating a compatibility path.

**Non-Goals:**

- Parsing or representing template elements, fragments, attributes, children, or embedded
  expressions.
- Choosing template whitespace, escaping, name-resolution, typing, lowering, or runtime protocols.
- Refactoring the public lexer/parser staging or adding contextual scanner modes.
- Changing fixed-array identity, internal semantic encodings, ownership, layout, HIR, MIR, or
  backend behavior.

## Decisions

### Fixed-array source types use a bracketed prefix form

The source grammar becomes:

```ebnf
fixedArrayType = "[", type, ";", decimalInteger, "]" ;
```

`[T; N]` makes the element type and length visually explicit and composes recursively as
`[[I32; 4]; 3]`. The lexer adds a `Semicolon` punctuation token; the existing bracket tokens are
reused. The parser recognizes this production only where a type is expected, so expression array
literals retain their existing `[first, second]` grammar without lookahead ambiguity.

`T[N]` was rejected because nested dimension order is less obvious and the form looks like an
expression projection. `[T, N]` was rejected because it consumes natural tuple-type territory.
Keeping `Array<T, N>` as an alias was rejected because the project is pre-stable and compatibility
would preserve the exact unrelated angle-bracket syntax this change removes.

### Expression position, not the lexer, distinguishes templates from comparisons

The ordinary lexer continues to emit `Less`, `LessEqual`, `Greater`, and `GreaterEqual`. When the
parser is continuing an existing expression, those tokens retain their current infix meanings.
When the parser instead requires a primary expression, `Less` followed immediately by an
identifier or `Greater` identifies reserved template syntax.

Reserved input is represented with the existing error-syntax machinery and a dedicated
parser-owned diagnostic. The reservation does not add a `TemplateExpression` syntax kind or infer
future template structure. Recovery consumes the reserved opening sequence within the current
expression's synchronization boundaries so a later statement or declaration remains independent.

This contextual parser decision is preferred over lexically classifying `<` as a template opener:
the lexer cannot know whether `a <b` continues `a` or begins a primary expression. The eventual
template implementation may replace the current full-source lexing pipeline with parser-directed
scanner modes behind a unified parse-source boundary, but that architecture is deliberately
deferred.

### Source spelling and semantic encoding remain separate

Only parsed and formatted Silk source adopts `[T; N]`. The structural fixed-array semantic value
continues to contain an element type and numeric length, and existing internal encoders may retain
`Array<T, N>` as an IR/debug notation. This keeps the grammar reservation independent from stable
identity keys and deterministic HIR, MIR, ownership, layout, and backend encodings.

If a future user-facing diagnostic needs source-like type rendering, it should use an explicitly
source-oriented formatter rather than changing the semantic encoder as an incidental consequence
of this syntax migration.

### Formatting follows the concrete bracketed structure

The fixed-array syntax node retains the two brackets, nested type node, semicolon, decimal length,
and trivia. Its canonical document is `[`, the formatted element type, `; `, the length, and `]`.
Nested types recursively use the same document. The formatter continues to reject missing required
syntax rather than inserting a semicolon or bracket.

## Risks / Trade-offs

- **`[T; N]` resembles an array literal** → Type and expression positions already select different
  grammar productions; parser tests will cover both forms at the same surrounding boundaries.
- **Reserved recovery could accidentally consume following code** → Detect only the two committed
  starts (`<Identifier` and `<>`) and test recovery before subsequent statements and declarations.
- **The semantic encoder can display notation that is not valid source** → Treat it explicitly as
  IR/debug notation and keep source rendering owned by syntax and formatting.
- **A standalone public lexer remains insufficient for real template text** → Add no template lexer
  contract now and record a unified parse-source/contextual-scanner boundary as a prerequisite of
  the future implementation.
- **Adding `;` could constrain future statement syntax** → Its meaning is contextual; this change
  assigns it only inside fixed-array type grammar and does not accept statement terminators.

## Migration Plan

1. Add semicolon tokenization and bracketed fixed-array parsing with focused lossless and recovery
   tests.
2. Add reserved-template-start diagnostics and comparison-boundary regression tests.
3. Update fixed-array formatting and formatter idempotence tests.
4. Replace old fixed-array source in compiler fixtures, examples, syntax-inspector presets, and
   syntax-focused specifications. Do not add a compatibility parser branch.
5. Run the repository checks and release-candidate verification required for package-visible syntax
   changes.

Rollback is a direct revert before release; there is no persisted user data or compatibility layer
to migrate.

## Open Questions

- Which parser/scanner interface will own future transitions among Silk expression, template tag,
  and template text modes?
- Which literal escaping and whitespace rules will template text use?
- Which component and intrinsic-element protocol will template expressions target?

These questions are intentionally deferred because none changes the reserved primary-expression
boundary or the bracketed fixed-array grammar.
