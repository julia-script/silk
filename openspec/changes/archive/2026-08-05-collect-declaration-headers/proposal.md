## Why

The spike's `SemanticAnalysis` discovers declarations and resolves bodies in one pass over one
file. The pinned pipeline requires every top-level declaration header to receive a canonical
identity _before_ any body resolves — canonical identities are what determine work order,
diagnostic order, and every fact-table key downstream. This proposal extracts the header half of
the monolith; body elaboration follows separately.

## What Changes

- Collect every top-level declaration header across the loaded closure and assign canonical
  declaration identities before resolving any body.
- Resolve imports, declared types, public signatures, and explicit function contracts at the
  header level; report irreducible dependency cycles as specified by issue 04.
- Publish the declaration index as an immutable fact table keyed by canonical IDs.
- Add an inspector lab: the declaration index — every declaration, its module, identity, and
  resolved signature, with unresolved states explicit.

## Capabilities

### New Capabilities

- `bootstrap-declaration-index`: Canonical declaration identities, header collection and
  signature resolution across the closure, and the published declaration-index fact table.

### Modified Capabilities

- `bootstrap-semantic-facts`: Declaration discovery moves out of the analysis monolith; existing
  declaration facts re-key onto canonical IDs.
- `bootstrap-syntax-inspector`: Declaration-index lab.

## Impact

Begins dismantling `SemanticAnalysis.ts`: its declaration-collection half moves into the new
phase. Downstream consumers (body analysis, evaluator, inspector flow model) re-key from ordinal
IDs to canonical IDs. No grammar changes.

## Plan References

- [Roadmap — Track 2, proposal 4](../../../roadmaps/compiler-realignment.md)
- [Issue 06](../../../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md),
  frontend checking order, step 2: "Collect every top-level declaration header and assign
  canonical identities before resolving any body. Resolve imports, declared types, interfaces,
  conformances, public signatures, and explicit function contracts, reporting irreducible
  dependency cycles as specified by issue 04."
- Same ticket: "Canonical module, declaration, type, and instance identities determine work and
  diagnostic ordering."
- [Issue 04](../../../wayfinder/bootstrap-language/issues/04-modules-visibility-and-name-resolution.md):
  cycle rules.
