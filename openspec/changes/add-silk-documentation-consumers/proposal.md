## Why

`silk doc` emits a formatter-neutral documentation JSON value today, and nothing reads it. Three
consumers are absent, and each absence has a different cause.

**Doctests were deferred, not rejected.** `add-silk-documentation` says it will "defer executable
documentation tests while retaining fenced Silk examples and exact source provenance needed by a
later doctest workflow". The retained material is in the JSON already — every fenced block reaches
it as a `CodeBlock` with a language token and a byte range into the declaring module. What is
missing is a charter saying what compiling one of those blocks means, which fence opts out, and what
a failure has to name. Without that charter an implementation would have to invent the semantics,
and an example that is wrong today still ships green.

**HTML rendering is external on purpose.** `silk-documentation-json` requires that the output "MUST
NOT serialize private compiler data structures, third-party Markdown AST types, absolute filesystem
paths, HTML, or a site-generator-specific format". That requirement only holds if some consumer
actually renders from the JSON alone; until one exists, the neutrality is untested and the first
renderer to arrive will be tempted to reach past the JSON into the compiler.

**The search index has no charter at all.** A documentation site with 38 standard-library modules
and roughly a thousand declarations is not usable by scrolling, and there is nothing saying where
the index comes from or what shape it ships in.

The bootstrap JSON schema is deliberately experimental, so a consumer that needs the schema changed
to work has been designed wrong. Both consumers chartered here read the schema exactly as it stands.

## What Changes

- Charter a doctest workflow that reads the documentation JSON, compiles every fenced Silk example
  it finds, and reports each failure with the example's file, its one-based line, and the
  declaration it documents.
- Fix the opt-out marker as a comma-delimited attribute inside the fence's language token —
  `silk,ignore` — because that token is the only part of the info string the JSON preserves. A
  space-separated attribute is dropped before the JSON is written, so it cannot be the marker.
- Fix what an example is: a complete Silk module, compiled exactly as written, with no implicit
  prelude and no synthesized wrapper. This is already the convention the prose documents follow.
- Charter a documentation site renderer that reads only the JSON, writes one HTML page per module
  plus an index, and tolerates a schema it does not fully recognize instead of failing.
- Charter a static search index built from declaration names and documentation text, shipped as one
  file the rendered site loads.
- Mark the one standard-library example that is a composition fragment rather than a program with
  the new opt-out marker, and regenerate the standard-library documentation page from it.

## Capabilities

### New Capabilities

- `silk-documentation-doctests`: what a fenced example in a documentation comment means, which fence
  opts out, how an example is compiled, and what a failure report must carry.
- `silk-documentation-site`: the JSON-only rendering contract, the pages the renderer writes, the
  static search index, and the tolerance the renderer owes an experimental schema.

## Impact

The change adds two packages that read the documentation JSON and adds no compiler capability. It
does not change the `silk-documentation-json` schema, does not add HTML output to the compiler or to
`silk doc`, and does not add a hosted site or a deployment pipeline.

It touches canonical standard-library source in exactly one place: the fence marker on the
`silk/filesystem` composition example, which the doctest workflow would otherwise report as a
failure forever. The generated standard-library documentation page is regenerated from that edit
rather than hand-corrected.

The doctest workflow needs the compiler to compile an example, so it depends on the compiler. The
renderer must not, and the boundary is structural: the renderer package declares no workspace
runtime dependency at all, which is a stronger guarantee than a convention about imports.
