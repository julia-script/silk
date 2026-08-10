## Context

See `proposal.md` for motivation and the delta specs for observable behavior. Today the compiler
recognizes `"..."` and `b"..."` directly inside the lexer's measured byte loop, stores only token
kind and span, parses either token as one `StaticTextLiteralExpression`, and asks `StaticText.decode`
to infer delimiter validity from the source slice and a `byteString: boolean`. Unterminated and
malformed literals therefore converge on one semantic diagnostic even though token-boundary
failure is already known during lexing.

The formatter emits a literal token through the ordinary text document node. That renderer assumes
text contains no physical line ending: it adds the full byte length to the current column and only
trims whitespace around formatter-created line nodes. CodeMirror consumes compiler tokens and
already assigns the existing literal kinds a string category. The TextMate grammar has no string
rules, and its checked-in VS Code copy is generated from the package grammar. A Silk-written lexer
is also differentially checked against the TypeScript lexer, so token and lexical-diagnostic
changes must land in both implementations and their cross-engine corpus.

## Goals / Non-Goals

**Goals:**

- Make literal recognition data-driven enough to add future modifier/delimiter combinations
  without another branch matrix in the lexer, decoder, and editor grammars.
- Keep one lossless token and one static-literal syntax node as the seam between source recognition
  and semantic decoding.
- Separate lexical boundary/modifier failures from atomic escape/value decoding failures without
  producing diagnostic cascades.
- Preserve the lexer's single-pass byte-scanning shape and the formatter's deterministic document
  rendering.
- Derive editor behavior from compiler-owned literal form metadata where the consumer permits it,
  then enforce parity where TextMate regexes necessarily duplicate boundary behavior.

**Non-Goals:**

- An owning string type, mutable string storage, allocation policy, or runtime text library.
- Automatic or standard-library dedenting, interpolation, raw strings, C strings, or user-defined
  literal modifiers.
- Adjacent literal concatenation or conversion between text and byte literal categories.
- Preserving source compatibility for identifier-plus-quote sequences or existing diagnostic
  phase assignments.

## Decisions

### 1. A literal-form actor owns introduction metadata and scanning rules

Introduce one compiler actor for literal forms rather than extending `isLiteralStart` and the
`byteString` boolean. Its immutable form values describe the recognized modifier, category
(`Text` or `Bytes`), delimiter width (`1` or `3`), and escape policy. Sibling operations perform
longest introduction recognition, unknown adjacent-modifier recognition, and boundary scanning
over source bytes.

The committed table contains empty/`b` modifiers crossed with one/triple delimiters. Future `r`
and `br` forms can add descriptors with a raw escape policy without changing what triple quotes
mean. The table remains closed: arbitrary identifiers are not accepted as modifiers. Metadata
needed by language tooling is exposed through an explicit compiler subpath; scanning mechanics can
remain private to the compiler.

Alternatives considered:

- **Add four hard-coded lexer branches.** Smaller initially, but repeats the current accidental
  design and makes each future modifier a cross-product change.
- **Create a token kind for every modifier and delimiter width.** Easy to pattern-match, but width
  is source form rather than semantic category and would make token consumers grow combinatorially.
- **Treat any identifier before a quote as a modifier.** Extensible without compiler changes, but
  silently grants semantics to misspellings and prevents deterministic diagnostics.

### 2. Valid literals retain semantic token kinds; damaged introductions use one invalid-literal token

Complete recognized forms continue to produce `TextLiteral` or `ByteStringLiteral`; delimiter
width and exact modifier remain recoverable from the source slice through literal-form
recognition. An unknown modifier or unterminated delimiter produces one `InvalidStaticLiteral`
token covering the committed recovery range plus one lexical diagnostic. The parser accepts that
token into an unavailable static-literal expression without adding an independent parser error,
and elaboration does not attempt semantic decoding or emit a duplicate semantic diagnostic.

Malformed escapes in an otherwise terminated recognized literal keep the valid semantic token
kind. `StaticText.decode` (updated to accept a recognized form instead of a boolean) validates the
body atomically and remains the sole owner of escape, Unicode, UTF-8, and byte-range diagnostics.

This sentinel split makes the phase boundary explicit while preserving one-token lossless syntax
and pipeline recovery.

Alternatives considered:

- **Keep every failure semantic.** Avoids a token addition but leaves editors and downstream
  phases unable to distinguish a malformed value from a token that never found its boundary.
- **Emit a valid token plus a lexical diagnostic.** Preserves expression parsing, but requires
  elaboration to rediscover and suppress the same delimiter failure without a direct diagnostic
  cause on the token.
- **Emit the existing generic `Invalid` token.** Reuses vocabulary but loses the fact that the
  complete damaged range occupies one expression position, encouraging parser cascades.

### 3. Boundary scanning is deterministic and deliberately asymmetric

The scanner chooses the longest opening delimiter, skips the byte following a backslash only for
the purpose of finding an unescaped closing delimiter, and closes at the first unescaped matching
delimiter. A single-line scan stops before CR or LF when no quote closes it; a multiline scan has
no reliable interior synchronization point and therefore runs through end-of-file when no triple
quote closes it. Unknown modifiers use the detected delimiter width to consume the same bounded
range before publishing their lexical diagnostic.

No indentation, declaration, comment, brace, or keyword heuristic terminates multiline content.
That choice sacrifices recovery after a missing triple quote in exchange for making arbitrary
embedded source/configuration data safe and predictable.

### 4. Decoding receives form data and performs semantic normalization once

Decoding strips the recognized modifier and matching delimiter widths, then walks only the body.
It applies the existing escaped text/byte policy identically for one- and three-quote forms. In a
triple-delimited body, a physical CRLF pair contributes one LF; isolated source CR remains invalid,
and escaped `\r\n` contributes CR plus LF because escape expansion is distinct from physical-line
normalization. A backslash followed by a physical CR or LF is a malformed escape and never consumes
following indentation.

Decoded output does not inspect line position and never drops first/last newlines or indentation.
Identity remains derived from the final decoded bytes, so equivalent single-line escape spellings,
multiline physical LF, and normalized CRLF can coalesce as they do today.

Alternatives considered:

- **Normalize every source file before lexing.** Matches Rust's input model, but changes byte spans
  and complicates LSP/editor correspondence with the actual file bytes.
- **Preserve physical CRLF in byte literals.** Seems byte-exact, but lets editor/source-control line
  ending conversion silently change program data and conflicts with canonical formatter output.
- **Dedent from the closing delimiter.** Makes nested source prettier but turns layout into hidden
  data transformation and prevents a literal from visibly spelling its own value.

### 5. Pipeline support remains a property of primary expressions

The parser does not add a literal-specific pipeline path. Both valid literal token kinds already
enter `parsePrimaryExpression`, and the outer expression parser builds `PipelineExpression` after
any complete left operand. The implementation adds regression coverage for all four forms and for
function-reference/callable targets, while elaboration continues to decide compatibility from the
literal's shared `u8` slice type and the target callable signature.

### 6. The formatter gains a verbatim multiline document node

Do not teach the ordinary text document node to contain arbitrary newlines: its flat-width and
column assumptions are useful invariants for grammatical text. Add a verbatim multiline document
variant used for complete literal tokens. Its renderer:

- emits content bytes without indentation, trimming, or dedenting;
- rewrites physical CRLF pairs to LF;
- resets and advances the current column for embedded line endings;
- causes flat-fit checks to fail across an embedded line ending; and
- leaves horizontal whitespace inside the region untouched.

`Formatter.printNode` selects this variant for triple-delimited literal tokens after recognizing
their form. Syntax with `InvalidStaticLiteral` is rejected by the existing complete-syntax gate, so
the formatter never repairs or invents a closing delimiter.

Alternatives considered:

- **Split the literal into formatter hard-line and text nodes.** Would let ordinary trimming and
  indentation mutate semantic body bytes.
- **Keep it as ordinary text and only fix column counting.** Conflates grammatical text with
  protected content and makes flat-fit behavior depend on hidden bytes.

### 7. Editor grammars share form metadata but validate regex parity behaviorally

CodeMirror continues to consume compiler tokens, adding the invalid-literal category and multiline
range tests; it does not implement a second scanner. TextMate rules are generated in longest-first
order from exported literal-form metadata: triple byte/text rules precede single-line byte/text
rules, with nested captures for recognized modifiers, delimiters, and escapes. TextMate begin/end
regexes necessarily repeat escape-aware delimiter matching, so Shiki tokenizer tests cover bodies
containing comment markers, keywords, escaped triple quotes, and physical line endings.

The existing synchronization script regenerates the VS Code grammar, and the structural-equality
test remains the shipping parity authority. Quote auto-closing behavior is unchanged; highlighting
does not implicitly add an editor insertion policy for triple delimiters.

### 8. Canonical and Silk-written lexers land atomically

The TypeScript lexer, token catalog, diagnostics, Silk-written lexer, pressure token records, and
differential fixtures change in the same implementation slice. Determinism coverage includes
recognized forms, unknown modifiers, escaped delimiters, terminated and unterminated forms, LF,
CRLF, and code-like bodies. Cross-engine acceptance verifies that the Silk lexer produces the same
token/diagnostic fingerprint without adding literal-specific compiler or backend intrinsics.

## Risks / Trade-offs

- **[A missing triple delimiter consumes the rest of the file]** → Emit one precise lexical
  diagnostic at the introduction, retain the full invalid token, and avoid unsafe heuristic
  recovery; editor highlighting makes the extent visible immediately.
- **[Unknown-modifier reservation breaks identifier-plus-literal source]** → Treat this as an
  intentional pre-release breaking change and diagnose the modifier specifically rather than with
  a generic parser error.
- **[TextMate escape-aware regexes drift from the byte scanner]** → Generate ordered rules from
  compiler form metadata and keep behavioral tokenizer fixtures plus generated-grammar equality.
- **[Formatter handling of protected whitespace violates global trimming assumptions]** → Isolate
  literal bodies in a dedicated document variant and specify the no-trailing-whitespace policy as
  excluding semantic literal content.
- **[CRLF normalization changes source spelling]** → Normalize in semantic decoding and formatting
  consistently, and test that formatted output preserves decoded identity; exact CRLF remains
  available through escapes.
- **[Lexer hot-path complexity regresses throughput]** → Keep recognition table sizes closed and
  tiny, scan each selected literal body once, and extend existing lexer pressure/benchmark cases.

## Migration Plan

1. Add literal-form metadata and the invalid-literal token/diagnostic vocabulary without enabling
   new source forms in downstream tools.
2. Switch both lexer implementations and differential fixtures together, then enable parser and
   decoder handling for the new tokens/forms.
3. Add the verbatim formatter document path and semantic-preservation/idempotence coverage.
4. Add CodeMirror and TextMate rules, regenerate the checked-in VS Code grammar, and run tokenizer
   parity tests.
5. Run the repository verification sequence and release-candidate checks because compiler token
   and package export surfaces change.

Rollback is a source revert: there is no persisted data migration or compatibility layer. Source
using triple literals or reserved adjacent modifiers will cease to compile after rollback.
