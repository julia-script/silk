## Context

Silk already has a lossless, byte-oriented `SyntaxFile`: the lexer retains whitespace and comments,
the parser retains every token, and recovery is represented explicitly through diagnostics, error
nodes, and missing tokens. That is the right reusable input for formatting, but it also means the
formatter must distinguish meaningful source text from trivia and recovery artifacts rather than
formatting an AST that has already discarded them.

The first consumer is `silk format` in `packages/compiler-cli`. A future language server will need
the same canonical output while owning editor-specific concerns such as document versions, UTF-16
positions, minimal text edits, and cursor preservation. The formatter therefore spans the compiler
and CLI packages now, but its reusable boundary must not depend on filesystems, projects, commands,
or the Language Server Protocol.

`DocComment` is currently recognized lexically but otherwise behaves like trivia. This change makes
its attachment rule part of the formatting contract. The project is unreleased, so the formatter
can establish a single canonical form without compatibility options or migration shims.

## Goals / Non-Goals

### Goals

- Provide one deterministic, width-aware formatter for every complete Silk syntax form.
- Keep the public formatter small: a lossless `SyntaxFile` in, immutable formatted bytes and a
  changed flag out, with typed failure for damaged syntax.
- Preserve comment contents while making whitespace, punctuation, line breaking, and blank lines
  canonical.
- Make formatting idempotent and preserve the parsed program independently of semantic analysis.
- Add deterministic project and path selection for write and check-only CLI workflows.
- Leave enough separation for a future LSP adapter to compute editor-native text edits and restore
  cursor or selection state without changing the formatter.

### Non-Goals

- Recovering or formatting lexically or syntactically damaged documents.
- Applying diagnostic fixes, even when a diagnostic already carries an edit.
- Reordering declarations, imports, fields, or other semantically meaningful constructs.
- Semantic analysis, reachability analysis, or workspace-wide diagnostics.
- Public style configuration or project-local formatter configuration.
- LSP document formatting, range formatting, on-type formatting, edit minimization, or cursor
  mapping in this change.

## Decisions

### The compiler package owns a document-local formatter boundary

Add a public `Formatter` actor in `packages/compiler/src/Formatter.ts` and a public
`FormattedDocument` data actor in `packages/compiler/src/FormattedDocument.ts`. The principal API
is a named `Effect.fn('Formatter.format')` operation:

```ts
format(syntaxFile: SyntaxFile): Effect<FormattedDocument, FormatterError>
```

`FormattedDocument` contains immutable bytes and `changed: boolean`. `FormatterError` identifies
damaged syntax and retains the source diagnostics needed by a caller to explain the refusal. The
operation has no service requirements because it performs no I/O and observes no project state.
Both actors receive explicit package exports and namespace exports from the compiler barrel.

The formatter compares its rendered bytes with `SyntaxFile.source.bytes` to derive `changed`; the
caller does not repeat that work. Accepting a `SyntaxFile`, rather than a path or source string,
lets the CLI and future LSP reuse an existing parse and keeps source loading and caching decisions at
their respective boundaries.

Alternatives considered:

- A string-to-string function would be convenient for the CLI, but would duplicate parsing in an
  LSP and obscure typed syntax failures.
- Putting the formatter in `compiler-cli` would couple the reusable logic to filesystem and command
  concerns.
- Returning LSP edits or a cursor map would make a protocol-specific representation part of the
  language core before an LSP exists.

### Strict validation happens before any layout work

`Formatter.format` first validates that the lexical and parser diagnostic collections are empty and
that the tree contains neither error nodes nor missing tokens. If any of those checks fail, it
returns `FormatterError` without producing partial output. Semantic diagnostics are intentionally
absent from the decision because formatting only depends on concrete syntax.

This validation is centralized in the formatter rather than delegated to callers, so the CLI and
future LSP cannot accidentally disagree about what is safe to format. Autofix remains a separate
future pipeline: choose diagnostic edits, apply them to source, parse again, and only then invoke
the strict formatter.

An alternative recovery printer was rejected because silently choosing spellings for missing or
unexpected syntax would make formatting an implicit source repair operation.

### An internal byte-oriented document algebra controls width

Implement a small internal document actor with the concepts needed by Silk's layout: byte text,
hard line, soft line, concatenation, indentation, grouping, and conditional break content. A group
renders flat only when its measured flat form fits the remaining portion of the 100-column target;
otherwise its soft lines break. Conditional break content supplies trailing commas only in broken
multi-item layouts.

The renderer emits canonical ASCII syntax and preserves source byte slices for identifiers,
literals, and comments. This matches the compiler's byte-oriented source model and avoids a lossy
decode/re-encode cycle. Width is exact for the language's currently ASCII syntax. Preserved comments
are indivisible layout atoms and may exceed the target; if Silk later admits non-ASCII identifiers,
display-column measurement can be added inside this private actor without changing the public API.

The renderer uses a bounded flat-width probe and a linear output pass. It must not recursively build
every possible flattened alternative, which would make nested groups exponential.

Alternatives considered:

- Direct string concatenation makes local cases simple but spreads width decisions through every
  syntax printer and makes nested groups inconsistent.
- A third-party pretty-printing dependency is unnecessary for the small algebra and would force the
  byte-oriented source model through a string API.

### Syntax printing is exhaustive and grammar-directed

The formatter dispatches exhaustively over `NodeKind`. It emits required punctuation from the
grammar, copies source spellings for identifiers and literals, and decides optional commas from the
chosen layout rather than preserving arbitrary separator trivia. Complete-syntax validation ensures
that every required token is available.

Compact delimited groups remain on one line when the whole group fits. When they do not fit,
multi-item lists place one item per line at one additional indentation level and include a trailing
comma. Operators and delimiters are printed from the concrete tree in their existing order, so
formatting cannot change grouping or precedence.

Every supported node kind gets formatter coverage. Adding a parser node without a printer branch is
a type-checking failure, not a runtime fallback to original text.

### Trivia is classified once before syntax layout

Whitespace tokens do not print directly. A trivia-classification pass reduces them to the few
signals the layout needs:

- whether a comment follows syntax on the same source line;
- whether a standalone comment belongs before the next syntax element;
- whether at least one author-supplied blank line separates adjacent elements; and
- whether consecutive `///` lines form a documentation block immediately preceding a declaration
  or field with no intervening blank line.

Comment byte contents are copied verbatim except for terminal spaces and tabs, which are removed by
the renderer's no-trailing-whitespace invariant. The formatter may otherwise change only their
indentation and surrounding line breaks. Same-line comments remain trailing comments; standalone
comments remain on standalone lines; attached documentation blocks stay adjacent to their
declaration or field. Ordinary standalone comments do not acquire documentation semantics.

At top level, declarations are separated by exactly one blank line, with attached comments moving
as part of the declaration block. Inside blocks, no blank line is invented, while any run of one or
more author-supplied blank lines is preserved as exactly one. Leading and trailing blank lines are
removed by the canonical file envelope.

Centralizing classification avoids each node printer inventing slightly different attachment
heuristics and gives future semantic tooling one documented rule to mirror.

### Formatted output is checked through properties and representative fixtures

Core tests cover every syntax form with golden expected output, especially width boundaries,
nested groups, trailing commas, comments, documentation blocks, and blank-line normalization.
Property-style assertions are applied to the fixture corpus:

1. formatting a complete file succeeds;
2. parsing the formatted bytes succeeds;
3. the original and formatted concrete programs are equivalent after ignoring trivia and the
   formatter-controlled optional trailing commas; and
4. formatting the result again produces identical bytes with `changed: false`.

Damaged lexical and parser inputs receive explicit refusal tests. This catches printer omissions and
non-idempotent layout choices without making the formatter invoke the parser internally in
production.

### The CLI owns selection, storage, and per-file reporting

Add a `FormatWorkflow` actor under `packages/compiler-cli` for selection and execution, with a thin
`FormatCommand` actor for Effect CLI arguments and terminal rendering. Register the command as
`silk format` with a repeatable positional file-or-directory selection and a `--check` flag.

With no positional selection, the workflow recursively selects exact `.silk` files beneath the
manifest's project source root. Positional paths are resolved relative to the invocation directory,
must resolve within that source root, and may name a `.silk` file or a directory. Directory walks do
not follow directory symlinks; resolved file targets outside the source root are rejected. Duplicate
targets are removed and the final normalized paths are sorted before processing.

The workflow processes files independently and deterministically. Each readable file is parsed and
classified as unchanged, changed, damaged, or failed. In write mode, changed files are replaced via
a temporary sibling and rename so interruption cannot leave a partially written individual file;
there is deliberately no cross-file transaction. A damaged or failed file does not prevent later
selected files from being classified. In `--check` mode no write operation is performed.

The command maps the aggregate result onto the existing CLI exit classes:

- `0` when all selected files are canonical and valid, or become canonical in write mode;
- `1` when check mode finds noncanonical files or either mode encounters damaged syntax; and
- `2` for invalid project/path selection, storage, or write failures.

If multiple classes occur, the highest class wins while all per-file outcomes are reported. This
keeps automation stable without hiding recoverable information from the user.

Alternatives considered:

- Formatting only files reachable from the entry module would leave valid but currently unreachable
  project source noncanonical and would make formatting depend on semantic graph construction.
- Stopping at the first damaged file would make bulk formatting unnecessarily nondeterministic from
  the user's perspective and reduce the usefulness of a check run.
- Whole-project transactional writes add substantial machinery while still not solving failures
  outside the project directory; atomic replacement per file protects the important integrity
  boundary.

### Future LSP integration computes edits outside the formatter

Whole-document LSP formatting will parse the editor snapshot, invoke the same `Formatter.format`,
and diff original bytes against the formatted bytes into minimal non-overlapping `TextEdit` values.
That adapter owns byte-to-LSP position encoding and document versions. The editor applies those edits
and consequently maps cursors and selections using its normal edit-tracking behavior.

Range formatting and on-type formatting may later need context-sensitive boundaries and explicit
selection mapping; they are separate formatter entry points or adapters, not flags on the canonical
whole-document operation. The bytes-plus-changed result is therefore sufficient for current CLI use
and does not pre-commit the language core to an editor protocol.

## Risks / Trade-offs

- **Comment attachment can be surprising at boundaries.** → Keep one trivia classifier, specify the
  same-line/blank-line rules explicitly, and add fixtures around declarations, fields, closing
  delimiters, and end of file.
- **Grammar growth can leave the formatter behind.** → Use exhaustive `NodeKind` dispatch and require
  golden and idempotence coverage for every new syntax form.
- **Width probing can become expensive for deeply nested input.** → Bound flat-width checks by the
  remaining line width and render each selected form once.
- **Byte-oriented width is not full Unicode display width.** → Preserve the byte API and treat
  comments as unbreakable; add display-width logic internally if identifiers later expand beyond
  the current ASCII grammar.
- **A multi-file write can partially succeed.** → Replace each file atomically, report every outcome,
  and provide `--check` for mutation-free CI validation; do not imply project-wide transactions.
- **Synthesizing trailing commas could accidentally alter the parsed program.** → Limit synthesis to
  grammar positions where commas are optional separators and assert normalized concrete equivalence
  after reparsing fixtures.

## Migration Plan

1. Add the internal document renderer, public formatter actors, exhaustive syntax printers, and core
   tests in `packages/compiler`; expose the new actors through explicit barrel and package exports.
2. Add CLI selection, workflow, atomic file replacement, reporting, command registration, and
   integration tests in `packages/compiler-cli`.
3. Document the canonical style and command, run the repository checks, and run the release-candidate
   verification because package exports change.

No source file is rewritten during package installation or upgrade. Projects adopt the canonical
form when a user runs `silk format`; CI can first introduce `silk format --check`. Rolling back the
tool does not require rolling back formatted source because the formatter emits ordinary valid Silk
syntax.
