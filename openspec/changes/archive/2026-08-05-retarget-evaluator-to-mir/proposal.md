## Why

The tree-walking evaluator is not a stage of the pinned pipeline, but it is deliberately kept:
it is the semantics oracle for differential testing against native output, the second consumer
that forces MIR's meaning to live in MIR rather than in the LLVM lowering, and the only source
for the inspector's dynamic (Evaluated) layer. Its current position — executing semantic-analysis
facts — dissolves when the monolith does, so it moves down to execute MIR, where every later
guarantee wants it.

## What Changes

- **BREAKING**: Re-target evaluation from analysis facts to MIR programs: a CFG interpreter over
  the lowered instance set, entered from user entry.
- Trace events are re-tied to MIR operations and their provenance (entry, call, binding,
  parameter read, return — extended per MIR's vocabulary), preserving the replay contract the
  inspector's Evaluated layer consumes.
- Blocked outcomes map onto MIR's explicit vocabulary (traps, unavailable states) with the same
  honest provenance the current `BlockedReason` model carries.
- During migration, run old and new evaluators against the shared corpus as a differential check;
  delete the fact-based evaluator once outputs agree.
- Establish the differential-harness scaffold that later compares interpreter results against
  compiled native output (completed in `accept-end-to-end-pipeline`).
- Rewire the inspector's Evaluated flow layer onto MIR traces via the analysis facade.

## Capabilities

### Modified Capabilities

- `bootstrap-evaluation`: Executes MIR; trace events carry MIR op identity and provenance; the
  fact-based evaluator is removed.
- `bootstrap-syntax-inspector`: Evaluated layer replays MIR traces.

## Impact

Replaces `BootstrapEvaluation.ts`'s execution model; the inspector flow model's evaluation
projections re-key to MIR ops. The interpreter remains a severable leaf: nothing in the pipeline
depends on it, so its maintenance cost can be re-judged at any time without unwinding other work.

## Plan References

- [Roadmap — Track 4, proposal 10, including the keep-rationale](../../../roadmaps/compiler-realignment.md)
- [Issue 06](../../../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md):
  the pipeline the interpreter double-checks — MIR as the meaning-bearing artifact ("MIR makes
  moves, borrows, drops, cleanup paths, success/failure branches … explicit") and the future
  second backend that MIR-resident semantics protect ("A future direct WebAssembly implementation
  may provide the same `Backend` capability").
- [Issue 09 — Self-hosting build and acceptance](../../../wayfinder/bootstrap-language/issues/09-self-hosting-build-and-acceptance.md):
  owns the fixed corpus the differential harness runs against.
