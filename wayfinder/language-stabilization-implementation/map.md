# Implement the stabilized Silk core

Status: resolved

## Destination

Bring the compiler, evaluator, native and Wasm backends, standard library, diagnostics, and language tooling into conformance with every confirmed rule in `docs/language/`. At completion, every valid reference example works on its applicable engines, every invalid boundary fails during its owning phase with an accurate diagnostic, and every deliberately future feature is named rather than confused with a bug.

## Authority and workflow

- `docs/language/` is the programmer-visible semantic authority.
- The completed reconciliation map supplies current-state evidence and implementation boundaries.
- Each queue item is a complete OpenSpec change containing proposal, delta spec, design, and tasks.
- Before implementation, audit the selected OpenSpec against the reference and reconciliation evidence.
- After implementation, audit code and tests against both authorities, then sync and archive the change.
- Delete superseded behavior in the same batch; no compatibility aliases or dual semantic paths.

## Queue

| # | Change | Depends on | Stabilization outcome |
| --- | --- | --- | --- |
| 01 | [Return-contract soundness](issues/01-return-contract-soundness.md) | — | Invalid bodies stop before HIR/MIR; issue 226 becomes a semantic regression. |
| 02 | [Ordinary Effect failure types](issues/02-ordinary-effect-failure-types.md) | 01 | Failure channels use ordinary types and unions; catch and error naming align. |
| 03 | [Unified interface/service conformance](issues/03-interface-service-conformance.md) | 01 | Services receive only dependency eligibility; all static contracts share one model. |
| 04 | [Requirement keys and provision](issues/04-requirement-provision.md) | 02, 03 | Roles, access, row algebra, flattening, and provision APIs become predictable. |
| 05 | [Entry, termination, and reporting](issues/05-entry-termination-reporting.md) | 02, 04 | Entrypoints, statuses, errors, traps, traces, embeddings, and adapters agree. |
| 06 | [Compatible Effect joins](issues/06-compatible-effect-joins.md) | 02, 04 | Finite compatible Effects join without construction-identity errors. |
| 07 | [Copy and executable ownership](issues/07-copy-executable-ownership.md) | 01 | One sealed Copy property controls all compound and executable storage behavior. |
| 08 | [Borrows and callable lifetimes](issues/08-borrows-callable-lifetimes.md) | 07 | Stable places, borrow locals, general sections, and last-use loans work uniformly. |
| 09 | [Ordinary structural unions](issues/09-ordinary-structural-unions.md) | 02, 07 | Every detached finite value type participates in one union representation. |
| 10 | [Struct construction and inference](issues/10-struct-construction-inference.md) | 01 | Construction is field-visible and omitted ordinary parameters infer completely. |
| 11 | [Text and scalar values](issues/11-text-scalar-values.md) | 08 | Text uses ordinary borrowing and checked Unicode scalar conversion. |
| 12 | [Operator and short-circuit semantics](issues/12-operator-short-circuit.md) | 03, 08 | Operators use explicit contracts and booleans use ordinary branch analysis. |
| 13 | [Shared pattern destructuring](issues/13-shared-pattern-destructuring.md) | 08, 09 | Match, let, and if-let share patterns, coverage, ownership, and union narrowing. |
| 14 | [Explicit modules, catalogs, and imports](issues/14-explicit-modules-catalogs-imports.md) | 02, 04 | No implicit prelude; catalog layers and auto-import tooling agree. |
| 15 | [Source unsafe callable contracts](issues/15-source-unsafe-callables.md) | 08 | Low-level source APIs transfer explicit caller obligations without bypassing checks. |
| 16 | [Matched-toolchain integrity](issues/16-matched-toolchain-integrity.md) | 05, 14, 15 | Compiler, catalog, intrinsics, providers, and runtime fail early when mismatched. |

## Parallel frontier

After 01, items 02, 03, 07, and 10 are independent. Later work may proceed concurrently only when it edits disjoint compiler seams and every declared dependency is resolved. The default single-thread frontier remains the lowest-numbered open unblocked item.

Current frontier: complete. Items 01 through 16 are implemented, synchronized, and archived.

Completed legacy OpenSpec changes are a separate hygiene lane: validate, sync, and archive them without letting their historical contracts override this queue.

## Reconciliation coverage

- Effects, failures, services, interfaces, and entry: 01–06; failure observability is owned by 05.
- Ownership, borrowing, captures, and callables: 07–08; return, failure, termination, and suspension lifecycle edges remain with 01, 02, 05, and the completed suspension change.
- Values, generics, representations, and operators: 09–12; ordinary failure and Copy foundations remain with 02 and 07.
- Control flow, patterns, modules, names, and visibility: 13–14; exact non-nominal patterns consume the union model from 09.
- Runtime, standard library, targets, termination, and tooling: 05, 14–16.

## Completion condition

- Every queue ticket is resolved and its OpenSpec change is audited, implemented, synced, and archived.
- No confirmed language rule remains classified Partial, Contradicted, Not implemented, or Unknown.
- Valid examples agree across analysis, evaluation, native, and Wasm wherever the rule applies.
- Invalid boundaries fail before later compiler phases and name the actual violated rule.
- Obsolete specs, diagnostics, tests, standard-library APIs, and implementation branches are removed.
- Remaining work is explicitly future scope, including async scheduling, JSX templates, static schema composition, re-exports, optional/default services, and package distribution.
