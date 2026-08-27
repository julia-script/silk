# Design — establish-analysis-facade

## Context

See proposal.md — Why. Every phase now publishes immutable facts (SyntaxFile, closure, index,
elaboration result with HIR), but the inspector wires phases together itself. Ticket 06 promises
a supported facade over the snapshot, and the realignment's definition of done makes the facade
the enforcement point for "every step is visualizable."

## Goals / Non-Goals

**Goals**

- `Analysis.ts`: one snapshot per compilation request (plus a single-source convenience), with
  queries for modules, syntax, imports/cycles, declarations and lookups, elaborated facts, HIR,
  merged driver-order diagnostics, and evaluation.
- Docs labs and flow model consume the facade exclusively; an automated import-boundary check.
- The rule documented in the compiler package.

**Non-Goals**

- No new analysis power — the facade wraps existing phase outputs.
- No editor-grade query set (find-references, rename); identities and recovery states must merely
  allow those to grow later without a second semantics implementation.
- Ownership queries join when that phase lands.

## Decisions

1. **The snapshot composes the existing artifacts**: closure, declaration index, and one
   elaboration result per loaded module, built in canonical order. The facade is deliberately
   thin — its value is the single entry point, the merged diagnostic stream (the first real
   consumer of the driver-side ordering authority), the evaluation query, and the boundary rule.

2. **Fact types are the facade's vocabulary, not internals.** Data-model modules (`SyntaxTree`,
   `SourceFile`, `SourceSpan`, `Token`, `Diagnostic`, `Hir`, and the fact type namespaces) remain
   importable — facade answers are made of them, and type-only imports of phase namespaces stay
   legal for annotating those answers. What tooling must not do is *run* phases: value imports of
   `Lexer`, `Parser`, `ModuleClosure`, `DeclarationIndex`, `Elaboration`, `BootstrapEvaluation`
   are the disallowed set.

3. **`rootAnalysis(snapshot)` is total.** The root module is always loaded (a missing root is
   rejected at request time), so the single-module inspector path needs no optional handling;
   per-module queries for other modules answer `undefined` for unknown identities.

4. **Evaluation is a facade query** (`evaluate(snapshot)` for the root), so the dynamic layer
   obeys the same boundary as the static facts.

5. **Enforcement is a docs-side test**, not tooling magic: one test scans the labs' sources for
   value imports of the disallowed phase modules from `@silk-lang/compiler`. Cheap, visible,
   and it fails with the offending file named.

## Risks / Trade-offs

- [Thin facade invites bypassing] → The import-boundary test makes bypasses fail CI, and the rule
  is documented where the package is consumed.
- [Snapshot recomputes everything per edit] → Same cost as the labs' current wiring; demand-driven
  laziness is a later concern the immutable shapes already permit.

## Migration Plan

1. Land `Analysis.ts` + tests; export from the package; release-candidate surface.
2. Migrate labs and flow model; convert remaining phase imports to type-only; add the boundary
   test; document the rule in the package README.
3. Rollback is git-revert.

## Open Questions

None.
