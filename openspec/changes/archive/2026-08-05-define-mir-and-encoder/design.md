# Design — define-mir-and-encoder

## Context

See proposal.md — Why. Ticket 06 pins MIR as a monomorphic, backend-neutral CFG over logical Silk
types with an explicit target-layout input consumed only at emission. Defining the model,
verifier, and encoder against hand-built samples stabilizes them before
`discover-instances-and-lower-to-mir` produces MIR for real.

## Goals / Non-Goals

**Goals**

- `Mir.ts`: typed locals, basic blocks (normal and cleanup), operations (literal, move, call,
  drop), terminators (return, jump, branch, trap), provenance with generated markers, canonical
  call targets; `TargetLayout` as a separate type; structural verifier returning violations as
  data; deterministic encoder with goldens over hand-built samples.
- A MIR CFG lab rendering the samples — resolving the roadmap's open question by landing the view
  here.

**Non-Goals**

- No lowering, no interpreter, no backend — nothing consumes MIR yet.
- No optimization machinery (ticket 06 permits only unreachable-block removal, constant-branch
  folding, cleanup-block merging, and verification — none of which exist until lowering creates
  their inputs).
- No service slots, witness calls, or matches yet: the slice cannot produce them and the verifier
  could state nothing real about them. The closed unions and block/terminator shape are where the
  full vocabulary will land without re-keying.

## Decisions

1. **Locals are ordinal-indexed virtual registers typed by a `localTypes` table**; parameters
   pre-bind to the first locals. Def-before-use is checked structurally (a use must reference a
   declared local; full dominance analysis waits for real lowering to need it).

2. **Cleanup paths are `kind: 'Cleanup'` blocks**, matching ticket 06's mergeable-cleanup-block
   rule and the ownership phase's plan vocabulary.

3. **`TargetLayout` lives beside MIR, not inside it** — triple, pointer width, endianness, and
   logical-type size/alignment. Nothing in the module or encoder reads it; the backend proposal
   consumes it.

4. **Samples are exported from the package** (`Mir.samples`) so the lab, tests, and goldens share
   one definition. They are dev fixtures by contract and retire when lowering lands real MIR.

5. **Verifier returns `ReadonlyArray<Violation>`** ordered by (function, block, position) — data,
   never a throw, matching the diagnostics discipline.

6. **Encoder mirrors the established conventions**: `mir-module` header, `fn` lines with
   canonical identity and contract, `bbN[ cleanup]:` block headers, `%N = op … : type [span)`
   lines with a trailing `generated` marker where set.

## Risks / Trade-offs

- [Hand-built samples drift from what lowering will emit] → The next proposal replaces samples
  with lowered output under the same verifier and encoder; goldens make every divergence a
  visible diff.
- [Subset vocabulary invites premature generalization] → Unions are closed; adding a variant is a
  reviewed spec change when a phase actually produces it.

## Migration Plan

1. Land `Mir.ts` (model, layout, verifier, encoder, samples) + tests + goldens.
2. Exports, release-candidate surface, CFG lab.
3. Rollback is git-revert.

## Open Questions

None — the roadmap's CFG-lab question is resolved here (the lab lands with the samples).
