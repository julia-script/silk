## Why

Silk cannot express readable multiline static text or byte data, and its editor grammars do not
currently highlight even the existing single-line literal forms consistently. Defining the literal
surface now gives the lexer, formatter, diagnostics, pipelines, and editor tooling one predictable
contract before accidental behavior becomes a stable language commitment.

## What Changes

- Add escaped multiline text literals delimited by `"""` and escaped multiline byte literals
  introduced by `b"""`.
- Preserve every content character without automatic dedenting or structural newline trimming;
  normalize physical CRLF pairs to LF while retaining explicit `\r\n` escapes as exact CRLF data.
- Keep delimiter width independent from escape policy: multiline forms use the corresponding
  single-line escape rules, physical backslash-newline is invalid, and future recognized modifiers
  such as `r` and `br` can add raw forms without redefining triple quotes.
- Reserve identifier-like modifiers immediately adjacent to a string delimiter, recognize
  modifiers through a closed extensible vocabulary, and report unknown modifiers lexically.
- Make delimiter failures lexical and deterministic: an unterminated single-line literal stops at
  its line ending, while an unterminated multiline literal consumes through end-of-file without
  heuristic recovery.
- Preserve all four current/new literal forms as ordinary primary expressions, including as the
  left operand of `|>`.
- Format complete multiline literals without changing their decoded content, while canonicalizing
  physical line endings and tracking embedded newlines correctly.
- Highlight single-line and multiline text/byte literals, modifiers, delimiters, bodies, and
  escapes in CodeMirror, TextMate consumers, and the generated VS Code grammar.
- Extend lexer pressure and differential coverage for multiline forms, modifiers, CRLF
  normalization, delimiter recovery, pipelines, formatting, and editor tokenization.
- **BREAKING**: adjacent unknown modifier spellings such as `name"value"` become reserved invalid
  literal introductions instead of an identifier followed by a literal, and unterminated-literal
  diagnostics move to the lexical phase.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-lexer`: Recognize extensible single/triple literal introductions and publish bounded,
  deterministic modifier and delimiter diagnostics.
- `bootstrap-syntax`: Preserve multiline literal tokens losslessly and retain every literal form as
  a pipeline-capable primary expression.
- `bootstrap-static-text`: Decode exact multiline text/byte content, CRLF normalization, and
  escapes atomically without implicit dedenting.
- `silk-source-formatting`: Preserve multiline literal content while producing canonical LF output
  and idempotent layout around embedded line breaks.
- `language-codemirror`: Highlight all compiler-recognized literal forms across editor lines.
- `language-textmate`: Scope existing and multiline literal forms consistently in TextMate and the
  generated VS Code grammar.
- `bootstrap-language-pressure-programs`: Keep the Silk lexer and cross-engine differential corpus
  aligned with the expanded canonical literal vocabulary and diagnostics.

## Impact

The change affects compiler token vocabulary and lexing, static-literal decoding and diagnostics,
lossless syntax and pipeline regression coverage, formatter document rendering, the Silk lexer
pressure program, CodeMirror classification, TextMate grammar generation, and the checked-in VS
Code grammar. It adds no owning string type and no implicit or standard-library dedent operation;
an explicit pipeable text transformation remains separate future work.
