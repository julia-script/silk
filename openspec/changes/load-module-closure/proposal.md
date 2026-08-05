## Why

The spike compiles exactly one source file; the pinned pipeline's first step is loading the
complete source-module closure reachable from one compilation request by following syntactic
imports. This is one of the two deliberate language-visible additions in the realignment: a
minimal import spelling must exist for closure loading to be buildable at all. Issue 08 owns the
final surface form.

## What Changes

- Introduce a compilation request that names a root module; load and parse modules while
  following their syntactic imports until the complete reachable closure is known, each as a
  `SyntaxFile`.
- Add a minimal import declaration to the bootstrap grammar — deliberately provisional spelling,
  to be revisited by issue 08.
- Assign canonical module identities; work order and diagnostics must not depend on filesystem
  traversal or insertion order.
- Report module-level irreducible dependency cycles as diagnostics per issue 04's rules.
- Add an inspector lab: the import graph of the loaded closure, with cycles marked.

## Capabilities

### New Capabilities

- `bootstrap-module-closure`: Compilation requests, syntactic import following, canonical module
  identities, deterministic closure ordering, and module-cycle diagnostics.

### Modified Capabilities

- `bootstrap-syntax`: Parse the minimal import declaration.
- `bootstrap-syntax-inspector`: Import-graph lab over the loaded closure.

## Impact

Grammar (one new declaration form), parser, the compiler's entry surface (single file → request +
closure), fixtures, and the inspector. Downstream phases keep operating per module until later
proposals consume the closure.

## Plan References

- [Roadmap — Track 2, proposal 3, and "Foundation vs. features"](../../../roadmaps/compiler-realignment.md)
- [Issue 06](../../../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md),
  frontend checking order, step 1: "Starting at the root module, load and parse modules while
  following their syntactic imports until the complete reachable closure is known." Also the
  determinism commitment: "filesystem traversal and insertion order do not affect program meaning
  or output."
- [Issue 04 — Modules, visibility, and name resolution](../../../wayfinder/bootstrap-language/issues/04-modules-visibility-and-name-resolution.md):
  cycle-reporting rules referenced by issue 06 step 2.
- [Issue 08 — Prototype the bootstrap language syntax](../../../wayfinder/bootstrap-language/issues/08-prototype-bootstrap-syntax.md):
  owns the final import spelling.
