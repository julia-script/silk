## Context

The documentation JSON is the only interface both consumers get. It carries, per module and per
declaration, a parsed document whose blocks include `CodeBlock` nodes with an optional `language`
string, the block's text, and a `source` range of module-relative byte offsets. That is the whole
input. The decisions below are the ones where the JSON's shape, rather than taste, forced the
answer.

## Decisions

### The opt-out marker is `silk,ignore`, not `silk ignore`

The prose documents under `apps/docs/content/language/` already spell a non-compiled example
```` ```silk ignore ````, and `DocumentationExamples.test.ts` reads that form straight out of the
Markdown. A doctest workflow cannot reuse it.

The documentation model parses a `///` comment with CommonMark, and CommonMark splits a fence's info
string into a language word and a trailing meta string. The model records the language word and
drops the meta string. So inside a documentation comment, ```` ```silk ignore ```` and
```` ```silk ```` are byte-identical by the time they reach the JSON: both say `"language": "silk"`.
Honoring the space-separated form would require adding the meta string to the JSON, which is a
schema change this change refuses to make.

The comma form survives, because a comma is not whitespace: ```` ```silk,ignore ```` reaches the
JSON as `"language": "silk,ignore"`. This is also the form Rust uses for the same job, so it is not
a local invention. The cost is a real trap — an author who writes the prose documents' form inside a
`///` comment gets an example that is compiled rather than skipped — and the mitigation is that the
failure report names the comma form explicitly, so the author is told what to write instead of being
left to guess.

An unrecognized attribute is a failure rather than a silently-ignored word. `silk,ignor` must not
quietly become a compiled example, because the whole point of the marker is that the author believed
the example was skipped.

### An example is a whole module

Rust's doctests wrap a fragment in `fn main`, which works because Rust has one obvious entry shape
and an implicit prelude. Silk has neither yet, and the `?` requirement row means there is no single
wrapper that is right for both a pure fragment and one that runs effects. Every wrapper that would
be general enough is a language design decision this change is not entitled to make.

So an example is compiled exactly as written, as a complete module. That is already what the prose
documents do — `DocumentationExamples.test.ts` compiles each block whole — so the convention is
consistent across both places examples live, rather than differing by where the example was written.

A fragment is therefore not a compilable example, and its author marks it `silk,ignore`. The one
fenced example in the standard library today is such a fragment: it shows how to order a scoped
temporary directory against `Effect.ensuring` using names that do not exist outside the illustration.
Marking it is the intended outcome, not a workaround.

### Provenance is a byte offset, so the workflow reads the module source too

The JSON carries `sourceId`, `start`, and `end` — module-relative bytes, deliberately not line
numbers and deliberately not absolute paths. A report that says "byte 21874" helps nobody, so the
workflow converts the offset into a one-based line by counting newlines in the module's source.

This does not weaken the JSON-only rule for the *renderer*, which never needs a line number. It is
also not re-parsing the documentation: the workflow reads the JSON for the example text, the
language token, and the offset, and reads the source only to count newlines before that offset. The
source lookup is a caller-supplied function, so the standard library resolves through the compiler's
shipped manifest and a user project resolves through its own source root, with neither policy baked
into the workflow.

### The renderer's boundary is its dependency list

"The renderer reads no compiler internal type" is easy to satisfy on the day it is written and easy
to lose later, because the compiler package is one import away in a workspace. A test that greps
imports catches the obvious violation and misses a re-export.

The stronger form is structural: the renderer package declares no workspace runtime dependency, so
there is no compiler package installed under it to import. Its own view of the JSON is a set of
locally declared interfaces plus a decoder that validates an `unknown` parsed from a file. The test
that keeps this true asserts the empty runtime dependency set, which a future import cannot pass
without a deliberate, visible edit to `package.json`.

The drift that this trades away — the local interfaces slowly falling behind the emitter — is caught
by a conformance test that generates real standard-library documentation JSON, round-trips it
through `JSON.parse`, and renders it. The compiler appears there as a development dependency of the
test, never of the shipped renderer.

### The renderer tolerates a schema it does not recognize

The JSON schema is chartered as experimental and free to change without migration. A renderer that
throws on an unfamiliar block tag would turn every schema experiment into a broken site. So an
unrecognized block or inline node renders as its recoverable text — or as nothing when it carries
none — and the page still builds. A missing top-level shape is different: that is not an unfamiliar
node, it is the wrong file, and the decoder reports it.

### The search index is data, not a search engine

The index is one JSON file listing each declaration's module, name, kind, signature, page URL, and a
short text summary drawn from its documentation. Ranking and matching live in the page's own script.
Shipping a prebuilt inverted index or a scoring model would freeze a retrieval decision into a build
artifact that the site cannot change without regenerating every page, and at roughly a thousand
declarations a linear scan over names in the browser is not the bottleneck.

## Risks / Trade-offs

- **The space-separated fence trap.** Mitigated by naming the comma form in the failure message, and
  by the fact that a wrongly-compiled example fails loudly rather than passing silently.
- **Whole-module examples are verbose.** An example that wants to show three lines has to show the
  imports too, or opt out. Accepted: the alternative is inventing a wrapper that encodes a language
  decision, and a verbose example that is verified beats a terse one that is not.
- **Two consumers of an experimental schema.** Both are in this repository and both are covered by
  tests that read real generated JSON, so a schema change breaks them in `pnpm check` rather than in
  a published artifact.
