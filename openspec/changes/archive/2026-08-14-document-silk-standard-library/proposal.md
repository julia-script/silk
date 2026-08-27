## Why

The standard library already has broad one-line declaration coverage, but it does not yet teach
users how to choose, compose, and safely use its APIs. Its generated reference also omits several
documentable child declarations and has no enforceable quality or doctest gate, so adding richer
comments alone would not reliably publish or preserve the intended documentation.

## What Changes

- Establish a Silk-native documentation style for standard-library modules and public APIs: a
  concise summary followed, when useful, by ordered CommonMark sections for **When to use**,
  **Details**, **Gotchas**, titled **Examples**, and **See also** relationships.
- Add leading `//!` documentation to every shipped standard-library module and complete the
  documentation of every intended public root declaration, public field, and service or interface
  operation.
- Place valuable parameter and type-parameter documentation immediately above the declaration it
  describes, while omitting comments that merely restate an obvious name and type. Return types,
  failure rows, and requirement rows remain compiler-derived and are not duplicated in prose.
- Ground documentation in implementation behavior, tests, call sites, related APIs, and the
  existing standard-library design prose. Add only examples that teach a meaningful contract,
  using complete Silk modules that the doctest tool can compile.
- Make generated standard-library documentation render the complete intended public hierarchy,
  including documented type parameters, fields, parameters, service or interface operations,
  implementations, and implementation operations, with correct public declaration counts and
  heading nesting.
- Publish a navigable standard-library index and module-oriented generated reference suitable for
  the main documentation site rather than allowing the enriched reference to become one monolithic
  page.
- Add documentation policy checks and wire standard-library doctests and generated-output freshness
  into the normal repository verification path.
- Audit undocumented public implementation-state types before writing permanent public guidance;
  report questionable exposure separately rather than silently teaching accidental API as a
  recommended contract.

## Capabilities

### New Capabilities

- `silk-standard-library-documentation`: Defines the authored documentation contract, generated
  public reference, example quality, coverage policy, and verification gates for the compiler-
  shipped Silk standard library.

### Modified Capabilities

None.

## Impact

- Affects all canonical sources under `packages/compiler/stdlib/silk/` and the generated language
  reference under `apps/docs/content/language/`.
- Affects the standard-library Markdown generator, documentation-policy checks, doctest wiring, and
  documentation-site navigation or content layout.
- Produces a large documentation-only source diff and regenerated documentation artifacts, but does
  not intentionally change compiler semantics, runtime behavior, or public types.
- May surface questionable public declarations during the audit; changing their visibility or API
  shape is outside this change and requires a separately approved change.
