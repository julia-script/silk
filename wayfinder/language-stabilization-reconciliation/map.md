# Reconcile the stabilized Silk language with the repository

Status: active

## Destination

Make it possible to answer, for every confirmed programmer-visible Silk rule, whether the current
compiler implements it, partially implements it, contradicts it, or does not implement it yet. Each
classification must point to concrete compiler, test, diagnostic, and specification evidence and
must identify the smallest coherent implementation handoff when reconciliation is needed.

## Authority and method

- `docs/language/` records the confirmed intended programmer model during stabilization.
- Existing OpenSpec requirements, compiler behavior, tests, issues, and older decision maps are
  evidence of current state; they do not silently override a confirmed language rule.
- This map performs read-only reconciliation. It does not change compiler behavior or create an
  implementation plan while the relevant mismatch inventory is incomplete.
- A rule receives one implementation classification:
  - **Implemented** — the compiler, relevant engines, and focused tests enforce the confirmed rule.
  - **Partial** — a meaningful subset works, but a confirmed valid case, boundary, engine, or
    diagnostic is missing or contradictory.
  - **Contradicted** — current behavior deliberately or structurally enforces a different rule.
  - **Not implemented** — the confirmed surface or behavior has no usable implementation path.
  - **Unknown** — available evidence is insufficient; the ticket names the investigation needed.
- Diagnostic quality is tracked separately as **Aligned**, **Partial**, **Contradicted**,
  **Missing**, or **Unassigned**. A semantically implemented rule can still need diagnostic work.
- Every mismatch is grouped into the smallest coherent future SLP/OpenSpec implementation batch.
  No issue, test, or implementation artifact becomes language authority merely because it exists.

## Audit sequence

- [Audit Effects, failures, services, interfaces, and entry boundaries](issues/01-effects-interfaces-and-entry.md)
  — first because these rules caused the stabilization effort and form the execution contract used
  by most other features.
- [Audit ownership, borrowing, captures, and callable application](issues/02-ownership-borrowing-and-callables.md)
  — moves, loans, closure access, partial application, pipelines, and cleanup.
- [Audit values, generics, representations, and operators](issues/03-values-generics-representations-and-operators.md)
  — type identity, inference, specialization, opaque/exact representations, conversions, and
  custom operators.
- [Audit control flow, patterns, modules, names, and visibility](issues/04-control-patterns-modules-and-visibility.md)
  — statements, conditionals, loops, matching, destructuring, imports, collisions, and public
  boundaries.
- [Audit runtime, standard library, targets, termination, and tooling](issues/05-runtime-termination-targets-and-tooling.md)
  — runtime guarantees, providers, distribution contents, host boundaries, reports, LSP, and
  import completion.
- [Reconcile explicit Effect suspension](issues/06-effect-suspension.md)
  — execute the already audited SLP-0009/OpenSpec direction after the more foundational Effect
  contract mismatches are understood.

## Decision index

Resolved tickets are summarized here with links to their complete evidence and implementation
handoffs.

- [Effects, failures, services, interfaces, and entry audit](issues/01-effects-interfaces-and-entry.md)
  — The execution core is substantial, but seven shared old-model seams explain the mismatches:
  failure-row kinds, missing return diagnostics, duplicated providers and service-specific witness
  paths, access-bearing requirement selectors, legacy entry rules, construction-identity joins,
  and incomplete failure observability. Eight ordered implementation handoffs replace symptom-level
  fixes; issue 226 belongs to the first frontend-soundness handoff.

## Not yet specified

- Exact milestone or release label for the point at which every confirmed rule must be implemented.
- Whether diagnostic-only reconciliation should ship inside semantic batches or as a dedicated
  cross-language diagnostic pass.
- Which optional LSP warnings should be enabled by default after compiler conformance is restored.

## Completion condition

This map is complete when every confirmed language rule has an evidence-backed implementation and
diagnostic classification, every mismatch belongs to a named coherent handoff, no `Unknown`
classification remains, and the implementation frontier is ordered without reopening confirmed
language decisions implicitly.
