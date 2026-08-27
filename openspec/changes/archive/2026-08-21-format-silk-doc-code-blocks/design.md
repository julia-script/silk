## Context

The compiler-owned formatter currently accepts a lossless `SyntaxFile`, validates complete outer
syntax, and prints comments as opaque byte slices. The CLI and LSP both invoke that operation, which
is why their whole-document results agree. Parsed documentation lives in the optional
`@silk-lang/documentation` package: it normalizes `///` and `//!` tokens, parses CommonMark, and
maps document nodes back to source bytes. That package depends on the compiler, deliberately keeping
Markdown out of ordinary compiler parsing and analysis.

A fenced body is not one contiguous source substring after documentation markers are considered.
Every physical body line is separated in the `.silk` file by a newline, indentation, and another
`///` or `//!` marker; nested CommonMark containers add another prefix. Replacing a Markdown node's
apparent start/end range as plain text would therefore delete comment markers or container syntax.
The documentation owner must participate in reconstructing the source spelling.

The existing standard library also establishes two distinct contracts: exact `silk` examples are
complete modules, while `silk,ignore` permits illustrative fragments. See the modified
`silk-source-formatting` specification for the observable behavior.

## Goals / Non-Goals

**Goals:**

- Keep one pure, document-local canonical formatter shared by filesystem and editor adapters.
- Reuse the existing CommonMark interpretation and source provenance instead of recognizing fences
  with a second ad hoc Markdown scanner.
- Keep compiler parsing, analysis, and backend packages free of a Markdown dependency.
- Preserve the exact outer documentation structure while replacing active fence bodies in the
  actual `.silk` source.
- Retain strict all-or-nothing behavior for each source artifact and source-locate embedded damage.

**Non-Goals:**

- Formatting standalone Markdown files, generated documentation JSON, rendered documentation, or
  prose outside source-owned documentation comments.
- Formatting `silk,ignore` fragments, inferring wrappers for snippets, or adding fragment parsing.
- Type checking, compiling, or executing fenced examples; doctest remains the semantic consumer.
- Range formatting, on-type formatting, minimal LSP edits, cursor mapping, or formatter options.
- Treating unattached `///` tokens as documentation when the existing attachment rules do not.

## Decisions

### Put canonical orchestration in an optional formatter package

Create `@silk-lang/formatter` with public `Formatter` and package-owned error actors. It depends on
`@silk-lang/compiler` for Silk syntax and layout and on `@silk-lang/documentation` for CommonMark
documents and lossless documentation rewriting. The compiler's current `Formatter` actor becomes
`SyntaxFormatter`: a lower-level, Markdown-neutral operation that validates and lays out one Silk
syntax artifact while preserving comment atoms. `FormattedDocument` remains compiler-owned because
both layers return the same immutable bytes-plus-changed value.

`silk format` and LSP whole-document formatting import the new public formatter. Compiler syntax
and feature tests that need only grammar layout import `SyntaxFormatter`; user-facing adapters do
not bypass the documentation-aware layer. The old compiler `Formatter` export is deleted rather
than retained as an alias.

This layering keeps the dependency graph acyclic:

```text
@silk-lang/compiler
        ▲       ▲
        │       │
documentation  formatter ◀── compiler-cli / lsp
        ▲          │
        └──────────┘
```

Alternatives considered:

- Making the compiler depend on `@silk-lang/documentation` creates a cycle and violates the
  established optional Markdown boundary.
- Adding another Markdown parser or fence scanner to the compiler duplicates CommonMark semantics
  and will disagree on containers, fence forms, or info strings.
- Implementing the feature only in `FormatWorkflow` makes the CLI disagree with LSP formatting.
- Passing an optional documentation callback into the compiler formatter creates configurable
  canonical output and lets callers silently omit part of the policy.

### Keep documentation attachment inventory in the compiler

Add `DocBlock.all(syntax)` as the source-ordered compiler-owned inventory of leading module
documentation and attached declaration documentation. It owns the allowlist of documentable syntax
kinds and span deduplication, using the same attachment policy as `Analysis.documentationOfSyntax`.
The allowlist covers every currently documentable declaration-like surface, including top-level
declarations and conformances, fields, type parameters, value parameters, service operations, and implementation
operations; statement-local, trailing, unattached `///`, and nonleading `//!` trivia are excluded.

The formatter consumes this inventory without inspecting syntax node kinds. This prevents Markdown
tooling from becoming a second owner of which Silk constructs can carry documentation.

Alternative: recursively call `DocBlock.ofNode` for every node and deduplicate spans in the
formatter. Rejected because `ofNode` accepts any supplied node; deduplication removes wrapper
duplicates but cannot distinguish a documentable declaration from a statement with leading doc-like
trivia.

### Canonicalize outer layout before injecting formatted fence bodies

`Formatter.format(syntax)` performs a pure staged transformation:

1. Invoke `SyntaxFormatter.format` on the supplied artifact and retain its result. This rejects
   damaged outer syntax through the established typed boundary and produces canonical outer bytes,
   but it does not replace or mutate the original `SyntaxFile` used for discovery and diagnostics.
2. Use `DocBlock.all` and the documentation package's `CodeFence` actor against the original source
   bytes. Retain each original physical fence range, verify active fences have authored matching
   closers, and select fences whose CommonMark language word is exactly `silk`.
3. Parse every selected body as a complete synthetic Silk source artifact and recursively invoke the
   public formatter. Strip its one formatter-owned final newline when embedding the result before
   the closing fence. Semantic analysis is never run.
4. If the original source contains no active fence, return the retained `SyntaxFormatter` result
   directly. Otherwise parse its canonical outer bytes, inventory the corresponding documentation
   and fences in source order, and pair them with the original selected fences by owning-block and
   fence ordinal. Pairing compares normalization-stable structure: attachment, CommonMark language
   and metadata, delimiter character and length, and container ancestry. The syntax-canonical
   destination's opening and closing lines are authoritative after permitted outer terminal-whitespace
   normalization. A mismatch in invariant structure is an invalid reconstruction state rather than
   permission to guess.
5. Ask `CodeFence` to inject the already formatted bodies into those canonical outer documentation
   blocks, apply non-overlapping raw-block replacements in descending source order, and return the
   resulting bytes. The injection happens after outer layout so semantic trailing whitespace inside
   embedded multiline literals cannot be trimmed by the comment-opaque syntax renderer. A final
   outer parse verifies that reconstruction did not damage Silk syntax; no second syntax-layout pass
   is run. Construct `FormattedDocument.changed` by comparing the final bytes with the original
   caller-supplied bytes.

Discovery and every user-facing failure range stay anchored to the original snapshot. Canonical
outer bytes are used only as the reconstruction destination, where indentation, LF line endings,
ordinary comment trailing whitespace, and final newline are already settled. Since injection changes
only documentation-comment bodies and synthesizes their prefixes from canonical context, it cannot
affect outer layout decisions. On the next run, the same stages produce byte-identical output.

Alternative: discover fences only after outer syntax formatting. Rejected because outer layout and
CRLF normalization shift byte offsets away from the user's open or on-disk source and can erase
trailing spaces that are semantic inside an embedded multiline literal. Alternative: rewrite the
original source and run syntax layout afterward. Rejected because that final layout pass removes the
same semantic trailing spaces from physical comment lines.

### Let a documentation-owned CodeFence actor own lossless reconstruction

Add a `CodeFence` actor to `@silk-lang/documentation`. It parses one compiler-owned `DocBlock`
against its owning `SourceFile` through the same internal CommonMark seam as `Document`, and returns
source-ordered immutable fence values carrying the raw info string, CommonMark language and metadata,
whether an authored matching closer exists, body value, and outer source range. Keeping these fields
here avoids adding CommonMark meta fields to the formatter-neutral documentation JSON merely to
support source rewriting.

A sibling rewrite operation takes the raw block and replacements identified by `CodeFence` values.
It returns the bytes for that raw documentation block after replacing only selected fenced bodies.
The actor retains or recreates an internal normalized line map as needed; no third-party Markdown
node or private offset table becomes public, and `Document` and `CodeFence` share normalization and
parsing helpers rather than implementing two Markdown dialects.

The reconstructor records the CommonMark container stack and fence indentation in addition to the
physical line map. Opening, closing, and prose lines are copied from the syntax-canonical destination.
Every rewritten body line uses that destination's canonical outer indentation and comment delimiter,
followed by the container continuation prefix derived from the opening fence's container stack and
fence indentation. A formatted empty line remains a documentation-comment line with that prefix; it
never becomes an outer blank source line. Reparsing the rewritten documentation must retain the same
container ancestry, delimiter kind and length, language, and metadata. The canonical destination's
raw opener and closer spelling is copied, so permitted removal of trailing spaces on either line does
not become a false reconstruction failure.

Returning a complete raw-block replacement instead of one apparent source range is necessary
because a multiline body is physically discontinuous around comment markers. Keeping this actor in
the documentation package also gives documentation parsing and rewriting one CommonMark authority.

Alternative: render the whole normalized Markdown document back into canonical `/// ` lines.
Rejected because that would rewrite unrelated prose spelling and marker spacing contrary to the
source-formatting contract.

The external CommonMark call and reconstruction validation cross typed Effect boundaries.
`CodeFenceError` distinguishes a wrapped parser failure from invalid source or rewrite state; only a
wrapped external failure carries JavaScript causal ancestry. `Document.parse` continues converting
the same parser failure into its documented plain-text fallback, but formatter fence discovery must
not silently hide a possibly active block.

### The CommonMark language word separates active modules from intentional fragments

Only a case-sensitive CommonMark language word equal to `silk` selects embedded formatting. Metadata
does not change the language, so `silk ignore` remains active exactly as it does in doctest.
`silk,ignore` is a different language word and remains the explicit opaque-fragment spelling.
Other languages, `SILK`, and unlabelled fences remain opaque. Classification is owned by
`CodeFence`; its language-word rule matches the existing doctest classification and is guarded by a
cross-tool parity test.

An active body is a complete module because the reusable Silk parser and formatter have no
fragment mode and doctest already establishes whole-module examples. The formatter does not invent
imports, entry points, or wrappers. Although CommonMark accepts an opening fence without a closer,
source formatting rejects an active unclosed fence with a source-damage reason: inserting or
inventing a closer would be source repair, while rewriting the unterminated container would make the
boundary ambiguous. Documentation rendering remains total and diagnostic-free.

Alternative: try active formatting and silently leave a body unchanged on parse damage. Rejected
because exact `silk` asserts active Silk source; silently retaining damage would make `--check`
report canonical success for a noncanonical or malformed program.

### Failures distinguish source damage from documentation infrastructure

The public formatter error distinguishes outer syntax damage, embedded syntax damage, malformed
active fences, and documentation infrastructure failure. Embedded damage retains the inner
lexical/parser diagnostics and the original physical range of its top-level containing source fence.
A recursively nested failure additionally retains a path of source-relative inner fence ranges so
tooling can explain depth without pretending synthetic offsets are physical file offsets. No
intermediate or partially rewritten bytes escape the effect on failure.

Malformed active fences and outer or embedded Silk syntax damage are user-source rejection. The CLI
classifies them through its existing damaged outcome and exit class while continuing with other
files; LSP returns no edit. A wrapped CommonMark failure or invalid reconstruction state is an
infrastructure failure and the CLI uses its failed outcome and exit class. `FormatCommand` reports
the fence range and nested path for source rejection rather than assuming every formatter error has
only outer diagnostics. Recursion remains naturally bounded by strictly smaller fenced bodies in the
finite source input.

Alternative: translate diagnostics character-by-character into the discontinuous physical comment
body. Rejected for the first version because the outer fence range is stable and actionable, while exact
inner-to-outer point mapping across synthesized comment prefixes adds protocol complexity without
changing whether the artifact can be formatted.

### Verification is divided by ownership boundary

Compiler tests retain exhaustive grammar layout, comment-order, low-level idempotence, and the
documentable-node inventory against `SyntaxFormatter` and `DocBlock`. Documentation tests prove
language classification, closer detection, typed parser/rewrite failure, and byte-preserving
reconstruction for `///`, `//!`, backtick and tilde fences, changed line counts, empty lines, nested
CommonMark containers, Unicode, CRLF input, multiple fences, multiline-literal whitespace, and
unchanged prose. Formatter-package tests prove complete staging, ignored/non-Silk behavior, embedded
failure, recursive examples, original-range provenance, original-byte changed detection, semantic
preservation, and idempotence. CLI and LSP each get one integration case that must produce the same
expected complete source.

The standard-library source is then formatted through the public operation. Any changed shipped
source regenerates the compiler source inventory and generated standard-library documentation before
the release-candidate check.

## Risks / Trade-offs

- **[Documentation line reconstruction corrupts markers or containers]** → Share normalization
  between `Document` and `CodeFence`, keep reconstruction in the fence actor, and test exact byte
  fixtures for every supported marker, fence, and container form before integrating the public
  formatter.
- **[CommonMark parsing adds latency to ordinary formatting]** → Return the retained syntax-layout
  result when `DocBlock.all` finds no possible active fence, retain pure document-local work, and
  benchmark only if editor traces show material regression; correctness does not gain timing
  assertions.
- **[Renaming the compiler formatter causes broad repository churn]** → Apply the breaking rename and
  every caller/export update atomically; retain no alias under the green-field policy.
- **[Strict active-block failure surprises authors of fragments]** → Specify language word `silk` as
  a complete module and retain `silk,ignore` as the explicit opaque-fragment spelling shared with
  doctest.
- **[Recursive formatting changes nested documentation examples]** → Use the same public formatter
  recursively so one canonical policy holds at every embedded level and cover termination and
  idempotence with a finite nested fixture.

## Migration Plan

1. Rename the compiler's existing formatter actor and exports to `SyntaxFormatter`, add the
   compiler-owned complete documentation-block inventory, migrate compiler-owned tests, and delete
   the old formatter export.
2. Add and verify the documentation-owned `CodeFence` parsing and reconstruction actor.
3. Add `@silk-lang/formatter`, its public actors, package exports, error mapping, staged pipeline,
   and focused tests.
4. Move compiler CLI and LSP whole-document adapters to the public formatter and add parity tests.
5. Format shipped Silk sources, regenerate any changed source inventory, add release metadata, and
   run the required repository and release-candidate checks.

Rollback is a source revert of the package, export migration, and source-byte updates; there is no
persisted user data or compatibility path to preserve.
