## Why

With every phase and service in place, the spine needs to be provable, not just present: one
driver path from a compilation request to a running native executable, with the pinned
determinism and observability gates checked continuously rather than aspirationally. This
proposal closes the realignment — after it, the grammar freeze lifts and feature work rides the
foundation.

## What Changes

- Wire the compiler driver end to end: compilation request → closure → snapshot → instances →
  MIR → backend → linker → executable at the requested durable destination, with the driver
  orchestrating backend and linker services itself.
- Stand up the differential harness as a continuous check: interpreter outcome versus native
  execution across the fixed corpus; disagreement is a failing build, and the failure names the
  program and the diverging side.
- Enforce determinism gates in CI: identical compiler, source snapshot, target, profile, and
  pinned toolchain inputs produce byte-identical syntax, HIR, and MIR textual encodings and LLVM
  bitcode.
- Report per phase: elapsed time, input and output counts, diagnostic counts, and allocator-backed
  memory totals — the observability substrate issue 09's benchmark protocol builds on. Scaling
  and native-performance budgets themselves remain issue 09's milestone gates.
- Add the inspector's pipeline-overview lab: the full pipeline with per-phase status, timings,
  artifact links, and diagnostics counts for the last compilation — every step visible in one
  place.
- Declare the vertical slice complete: the roadmap's grammar freeze lifts; subsequent language
  features are ordinary proposals that flow through every phase and appear in every lab.

## Capabilities

### New Capabilities

- `bootstrap-compiler-driver`: The end-to-end orchestration path, differential harness,
  determinism checks, and per-phase reporting.

### Modified Capabilities

- `bootstrap-syntax-inspector`: Pipeline-overview lab.

## Impact

CI gains determinism and differential jobs; the roadmap's status flips to done and its freeze
note is lifted. No new language surface. Everything after this change is feature work on the
foundation.

## Plan References

- [Roadmap — Track 5, proposal 13](../../../roadmaps/compiler-realignment.md)
- [Issue 06](../../../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md),
  performance paragraph: "Performance and predictability are milestone gates rather than
  aspirations. Identical compiler, source snapshot, target, profile, and pinned toolchain inputs
  must produce byte-identical syntax, HIR, and MIR textual encodings and LLVM bitcode. Every
  phase reports elapsed time, input and output counts, diagnostic counts, and allocator-backed
  memory totals."
- Same ticket: "The compiler itself orchestrates backend and linker calls; a Node.js or
  TypeScript harness may test the compiler but may not perform a stage required for stage-2
  self-hosting."
- [Issue 09 — Self-hosting build and acceptance](../../../wayfinder/bootstrap-language/issues/09-self-hosting-build-and-acceptance.md):
  owns the fixed corpus, reference machines, run protocol, and stored measurements this change's
  reporting feeds.
