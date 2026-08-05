# Design — check-ownership-and-cleanup

## Context

See proposal.md — Why. Ticket 06's step 4 pins the ownership phase as a producer: ownership
facts plus the target-neutral cleanup plan that MIR lowering consumes. Issue 01's resolved
semantics (affine single ownership, lexical borrows, `Scope<S>` outlives, LIFO cleanup) are the
rules the phase enforces. The frozen grammar slice contains only copyable `I32` parameters and
temporaries, so this change lands the phase, fact table, and artifact — not interesting verdicts.

## Goals / Non-Goals

**Goals**

- `Ownership.ts`: one check per declaration over typed HIR producing binding facts (category,
  live range), a closed verdict, and the per-exit cleanup plan; deterministic textual encoder
  with goldens.
- Ownership facts queryable through the analysis facade.
- An ownership lab: binding timelines and cleanup plans.

**Non-Goals**

- No new language surface. **The roadmap's open question resolves: stay frozen.** A vacuous check
  still proves what this proposal needs proven — the phase boundary, the fact idiom, the artifact
  and its encoder, the facade query, and the lab all exist and are golden-tested; minimal
  bindings/moves would buy real verdicts at the cost of grammar/elaboration churn owned by issue
  08's syntax work. The decision is recorded in the roadmap.
- No ownership diagnostics yet: on the frozen slice no rule can fail, so no `OWN`-prefixed codes
  are introduced — inventing codes nothing can emit would be speculative. The `ownership` phase
  joins the diagnostic phase union when its first diagnostic can actually fire.
- No drops inserted anywhere — the plan is data for `discover-instances-and-lower-to-mir`.

## Decisions

1. **The phase consumes `Elaboration.Result` per module** (typed HIR + headers) and runs in the
   snapshot after elaboration, before instance discovery. `checkModule(result)` returns the
   module's ownership facts; `Analysis.make` stores them and `Analysis.ownershipOf` answers them.

2. **Binding facts reference parameter identities.** In the frozen slice bindings are exactly the
   declared parameters: category `Copyable`, live range from the parameter's declaration span to
   the function body's end span. The shape leaves room for owner categories and borrow facts
   without re-keying (category is a tagged union from day one).

3. **Verdict is `Satisfied | Unavailable {cause?}`.** A function whose HIR body is unavailable, or
   whose contract is unavailable, gets `Unavailable` carrying the body's or contract's cause —
   the checker must not claim it checked what elaboration could not type.

4. **The cleanup plan models exits explicitly.** One `Return` exit per function (the only
   structured exit in the slice) at the returned expression's span, with an ordered `releases`
   list — empty here, LIFO by construction when owners exist. The plan is part of the ownership
   fact but encoded as its own artifact section so lowering can consume it independently.

5. **Encoder mirrors the HIR encoder's conventions**: `ownership-module` header, `fn` lines with
   verdicts, `binding` lines with category and live range, `exit` lines with ordered releases
   (`releases none` when empty). Goldens: accepted fixture with parameters, damaged fixture with
   an unavailable verdict.

## Risks / Trade-offs

- [A vacuous phase invites bit-rot] → The determinism suite, goldens, facade query, and lab keep
  it exercised on every change; the first non-copyable type immediately gets real coverage
  because the shapes already distinguish categories.
- [Live ranges are approximate (whole body)] → Exact for the frozen slice (parameters live
  through the single return); finer ranges arrive with bindings and control flow.

## Migration Plan

1. Land `Ownership.ts` + tests + goldens; wire into `Analysis` (snapshot field + query).
2. Add the ownership lab; exports and release-candidate surface; mark the roadmap question
   resolved.
3. Rollback is git-revert.

## Open Questions

None — the recorded open question is resolved above (stay frozen).
