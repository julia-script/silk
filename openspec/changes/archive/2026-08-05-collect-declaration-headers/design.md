# Design — collect-declaration-headers

## Context

See proposal.md — Why. `SemanticAnalysis.ts` currently derives declaration headers (names,
parameters, declared types, duplicates) and resolves bodies in one per-module pass. Ticket 06's
step 2 requires headers with canonical identities across the whole closure before any body
resolves. The closure and its canonical module identities exist since `load-module-closure`;
this change extracts the header half of the monolith into a `DeclarationIndex` phase.

## Goals / Non-Goals

**Goals**

- One header-collection phase over the closure producing an immutable, canonically ordered index.
- Canonical declaration identity = canonical module identity + declaration name, owned by the
  first present occurrence; duplicates and unavailable names stay explicit with causes.
- Header-level signature resolution (visibility, parameters, duplicate parameter names, return
  type, parameter count) with existing codes (`SEM0001`, `SEM0003`, `SEM0005`) moving to this
  phase.
- Semantic analysis consumes collected headers instead of re-deriving them.
- A declaration-index inspector lab.

**Non-Goals**

- No body resolution here — `elaborate-bodies-to-hir` owns that.
- No cross-module name resolution or import bindings — imports resolve at closure level; binding
  names into module namespaces arrives with body elaboration and issue 04's binding rules.
- No irreducible-cycle reporting yet: with only function declarations and the built-in `I32`,
  header resolution has no inter-declaration dependencies, so issue 04's cycle rules are vacuous
  in this slice. The reporting obligation attaches when types, constants, or inferred contracts
  introduce header-level dependencies.

## Decisions

1. **The header fact types move to `DeclarationIndex.ts`; `SemanticAnalysis` re-exports them.**
   `DeclaredName`, `DeclaredTypeFact`, `ParameterFact`, `DeclarationFact` and the header analysis
   functions relocate; semantic analysis imports and consumes them, keeping its public fact
   shapes source-compatible (`SemanticAnalysis.DeclarationFact` remains a valid name). This is
   extraction, not duplication — the monolith's header half is deleted, not copied.

2. **Canonical identity is an added fact, not a replacement key.** `DeclarationFact` gains a
   `canonical` state: `Canonical {module, name}` for first present occurrences,
   `Duplicate {original, cause}` for later ones, `Unidentified` for unavailable names. The
   structural per-module `DeclarationId {sourceId, ordinal}` stays as the deterministic
   source-local identity that parameter/argument identities nest under — the ordinal is exactly
   the concrete-order fact the existing specs pin. The *index* is keyed canonically; downstream
   phases key new tables against canonical identities as they arrive.

3. **`collectModule(syntax)` and `collect(closure)`.** Per-module collection stays available so
   single-module semantic analysis keeps its current entry point; the closure-level `collect`
   maps it over canonically ordered modules and merges diagnostics with the unified ordering.
   Index order: canonical module identity, then concrete declaration order within the module —
   name-sorting header lists would shuffle source facts for no downstream gain; canonical *keys*
   are what later phases sort by.

4. **Header diagnostics move phase ownership, not codes.** `SEM0001` (on declared types),
   `SEM0003`, `SEM0005` are emitted during header collection and surface through the index (and
   through semantic analysis unchanged, which consumes the collected headers). Codes, messages,
   spans, and reasons stay identical — fixtures diff only if they asserted where a diagnostic was
   computed, which none do.

5. **Lookup keeps the spike's three-way outcome** (Resolved first-present / Ambiguous all /
   Missing) per module, now answered by the index.

## Risks / Trade-offs

- [Type moves ripple through imports] → Re-exports from `SemanticAnalysis` keep every existing
  consumer compiling; the inspector and evaluator touch fact *values*, not module paths.
- [Two entry points (module, closure) could drift] → `collect` is defined as the canonical-order
  fold of `collectModule`; there is one implementation of header analysis.
- [Canonical states add a field every fixture sees] → `deepEqual`-style assertions on whole
  declaration facts gain one field; the migration is mechanical and the determinism suites guard
  the rest.

## Migration Plan

1. Move header types + functions into `DeclarationIndex.ts`; add canonical states; re-export from
   `SemanticAnalysis`; keep all existing tests green.
2. Add `collect(closure)` with the canonical-order index and lookup; new tests.
3. Add the declaration-index lab; exports and release-candidate surface.
4. Rollback is git-revert; no persisted data.

## Open Questions

None — cross-module binding and body elaboration are owned by the next proposals.
