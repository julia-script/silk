# Design — retarget-evaluator-to-mir

## Context

See proposal.md — Why. The evaluator's old position — walking elaboration facts — dissolved with
the monolith; the pipeline now produces a lowered MIR program per snapshot. Moving the
interpreter onto MIR makes it the second consumer that forces MIR's meaning to live in MIR, and
keeps it the semantics oracle for the coming native differential checks.

## Goals / Non-Goals

**Goals**

- `BootstrapEvaluation.evaluate(discovery, program)`: a CFG interpreter over lowered MIR —
  locals, blocks, operations, terminators — entered from discovery's resolved entry.
- Trace events re-tied to MIR: entry, call, binding (with argument/parameter ordinals, values,
  and a from-nested-call marker), return — all carrying canonical function identities and lowered
  provenance. Parameter-read events retire: MIR has no read operation to replay.
- Blocked outcomes on MIR's vocabulary: discovery's entry reasons, executed traps (which is how
  every unavailable fact now reaches evaluation), missing lowered functions, recursive cycles.
- The shared corpus as a reusable harness scaffold (`test/support/corpus.ts`): programs with
  pinned expected outcomes, consumed by evaluation tests now and by the native acceptance
  differential later.
- Inspector Evaluated layer and evaluation panel rewired onto the MIR trace via the facade.

**Non-Goals**

- No native comparison yet — `accept-end-to-end-pipeline` completes the differential harness.
- No new execution semantics: expected results on the corpus are unchanged from the fact-based
  evaluator; only blockage _vocabulary_ changes where unavailable facts now surface as traps.

## Decisions

1. **The fact-based evaluator is replaced in place, and the corpus is the differential
   evidence.** The old evaluator's expected outcomes (exact results, cycle shapes, partial-trace
   prefixes) are pinned into the corpus before the rewrite; the MIR interpreter must reproduce
   them. Where the old evaluator blocked with fact-level reasons (`MissingCallTarget`,
   `UnavailableInteger`, …), lowering has already turned those facts into generated traps, so the
   corpus pins the trap equivalents — a deliberate, reviewed vocabulary change, not a semantics
   change.

2. **Binding events carry ordinals and a `fromCall` marker instead of fact references.** The flow
   model matched arguments by call-site span and ordinals already; the marker replaces its
   structural peek at the argument expression. `ParameterRead` events retire with the read-less
   MIR model, and the flow overlay's parameter-read evidence retires with them.

3. **Recursion stays bounded by the active canonical-identity path**, exactly as before but keyed
   by canonical ids — re-entering an active function blocks with the ordered cycle and closing
   call provenance.

4. **`Analysis.evaluate(snapshot)` keeps its signature** — tooling notices only the new outcome
   vocabulary. The facade passes the snapshot's discovery and lowered program to the interpreter.

## Risks / Trade-offs

- [Inspector overlay loses parameter-read evidence] → Accepted: the read was a fact-walker
  artifact; bindings and returns carry the values the overlay shows. The affected flow items
  simply have no dynamic badge.
- [Blocked-reason vocabulary changes user-visible text] → The panel renders the new reasons with
  the same structure; tests pin the new wording.

## Migration Plan

1. Pin the corpus with current expected outcomes; rewrite `BootstrapEvaluation.ts` as the MIR
   interpreter; rewrite its tests on the corpus.
2. Rewire `Analysis.evaluate`, the evaluation panel, and the flow model's Evaluated overlay;
   update inspector tests.
3. Rollback is git-revert.

## Open Questions

None.
