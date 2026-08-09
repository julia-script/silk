## Context

The compiler already lexes `///` as `DocComment`, preserves all trivia in `SyntaxFile`, retains
leading trivia inside declaration nodes, and exposes semantic declaration identities through
`Analysis.Snapshot`. CodeMirror and TextMate distinguish a whole doc-comment token, while LSP hover
currently wraps only `Presentation.text` in a Markdown code fence. The existing docs application
consumes Markdown but is a site adapter, not the appropriate owner of a source-language
documentation model.

The compiler's normal parse and analysis path is performance-sensitive and must remain independent
of Markdown. Documentation is also required by three independently evolving consumers: editor
highlighting, LSP hover, and generated artifacts.

## Goals / Non-Goals

**Goals:**

- Keep raw documentation attachment source-owned, deterministic, and cheap inside the compiler.
- Put normalization, CommonMark, intra-document links, examples, and documentation IR behind one
  optional package interface.
- Give definition hover, reference hover, editor highlighting, and JSON generation equivalent
  documentation semantics.
- Preserve enough semantic and source provenance for independent formatters and later doctests.

**Non-Goals:**

- Parsing Markdown during lexing, CST parsing, declaration collection, or ordinary analysis.
- Documentation directives that duplicate parameter, return, failure, or requirement facts.
- Documentation diagnostics, strict Markdown validation, or broken-link warnings in the first
  version.
- Executing or compiling fenced examples.
- Publishing a stable JSON compatibility contract during bootstrap.
- Generating HTML or owning a documentation website.

## Decisions

### The compiler owns only raw attachment

Add a `ModuleDocComment` token kind for exact `//!` comments and a concept-oriented `DocBlock`
actor in `@silk-effect/compiler`. `DocBlock` contains its kind, exact documentation tokens, and
source span. Sibling queries extract a leading module block from a `SyntaxFile` or the attached
declaration block from a syntax node. They inspect only direct leading trivia and newline counts;
they do not decode, normalize, or parse text.

Declaration facts already retain their syntax nodes, and semantic occurrences resolve to canonical
declarations. `Analysis` exposes raw documentation queries over those existing identities so tools
do not reconstruct attachment. This keeps syntax as the source of trivia and keeps documentation
out of HIR and MIR.

Alternative: store parsed documentation on declaration facts. Rejected because it would charge
every analysis for optional Markdown work and couple core semantic data to renderer concerns.

### Parsed documentation is an optional package

Create `@silk-effect/documentation`, depending on the compiler rather than the reverse. It owns
singular `Document`, `Project`, and `Json` actors with immutable data-first values and sibling
operations. It normalizes comment markers, parses CommonMark on demand into a package-owned node
vocabulary, recognizes examples, resolves intra-document links from compiler scope facts, and
builds a source-ordered project model.

The package wraps its Markdown implementation in one internal seam. Malformed Markdown is accepted
by CommonMark recovery; if the external parser cannot produce a tree, the package returns one text
node containing the normalized source. No parser error or third-party AST crosses the public
interface.

Alternative: put parsed documentation in `@silk-effect/language`. Rejected because generation and
LSP hover are not editor adapters. Alternative: put the Markdown dependency in the compiler.
Rejected because it expands the core package and weakens the opt-in performance boundary.

### Documentation is Markdown-only

The normalized body is CommonMark. Examples use an `Examples` heading and fenced `silk` blocks.
There are no `@` directives. Parameter documentation attaches to parameter declarations, while
the documentation project derives all signature and contract facts from compiler presentations.
This prevents documentation from becoming a second, stale type system.

Rust-style ``[`Symbol`]`` forms are recognized after Markdown parsing. Resolution uses the same
module scope as the documented declaration. A successful link records the canonical declaration
identity; an unavailable or ambiguous link becomes ordinary inline code. This is deliberately not
a diagnostic path.

### Consumers adapt the shared model

LSP hover obtains the semantic subject, looks up its raw block through `Analysis`, asks the
documentation package for a parsed document, and renders the compiler-derived signature followed
by the entire Markdown body. Examples are not truncated. Intrinsics and anonymous-expression
fallbacks remain signature-only because they have no source-owned documentation block.

CodeMirror uses documentation-provided source ranges layered inside compiler-classified comment
tokens. TextMate uses nested repository patterns for equivalent lexical styling; it cannot provide
semantic link resolution. Both retain stable outer doc-comment categories for themes that do not
style the nested ranges.

### JSON is the canonical generation artifact

`Project` maps the analyzed module closure into documentation-specific module and declaration
values. Signatures contain canonical text plus typed segments and semantic link targets rather than
serializing compiler presentation objects. Documents use the package-owned block and inline node
vocabulary. Provenance contains logical module identities and byte spans, never absolute paths.

`Json.encode` emits fixed object-field order, source-ordered arrays, two-space indentation, and one
final newline. Default generation excludes private declarations; an option includes them. The root
contains an experimental format marker but no compatibility promise or migration machinery.

Alternative: generate Markdown. Rejected because it makes Markdown the privileged output and asks
other formatters to reverse-engineer rendered text. Alternative: serialize compiler and mdast
objects. Rejected because that leaks internal schemas and couples external formatters to unrelated
implementations.

### CLI generation uses existing project analysis

`silk doc` follows the project discovery, target selection, source resolution, reporting, and
atomic-destination conventions already used by project workflows. It analyzes one canonical target
because documentation contracts are source-semantic; it does not invoke a backend. The command
encodes a complete `Project` before atomically committing the destination, so rejected analysis
cannot leave partial JSON.

## Risks / Trade-offs

- **[CommonMark AST breadth makes the public model large]** → Own only the stable node vocabulary
  required to represent CommonMark and map third-party nodes internally; do not expose parser types.
- **[Rich documentation highlighting can add editor latency]** → Parse only doc-comment ranges in
  the editor adapter and keep ordinary compiler analysis unchanged.
- **[Full examples can make hover large]** → Preserve the agreed complete-hover behavior initially
  and revisit truncation only with usage evidence.
- **[Silent unresolved links can hide typos]** → Preserve readable inline code now; a later opt-in
  documentation lint can be added without changing parsing or generation.
- **[Experimental JSON changes break early formatters]** → Mark the artifact experimental and add a
  formal version and JSON Schema only when external formatter support is intentionally published.
- **[One-target documentation may omit target-conditional facts later]** → Bootstrap has no such
  source surface; evolve the project model when the language gains it rather than inventing a
  premature multi-target merge.

## Migration Plan

1. Extend lexical and formatting support for `//!` while preserving `///` behavior.
2. Add raw `DocBlock` extraction and facade queries without changing existing hover output.
3. Add the optional documentation package and its total parser/project/JSON model.
4. Switch hover and editor adapters to the shared documentation model.
5. Add `silk doc`, package exports, documentation, changesets, and release-candidate validation.

Each step is additive during bootstrap. Rolling back the optional package and adapters leaves raw
comment tokens and existing signature hover intact.
