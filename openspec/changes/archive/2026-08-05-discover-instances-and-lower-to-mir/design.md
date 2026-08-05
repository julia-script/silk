# Design — discover-instances-and-lower-to-mir

## Context

See proposal.md — Why. Ticket 06's steps 5 and 6 close the frontend: discover reachable concrete
instances from the entry with a recorded worklist, then lower them to MIR, inserting the drops
and cleanup edges the ownership plan directs. Keys are degenerate without generics; the worklist
discipline and lowering contract are the real structure.

## Goals / Non-Goals

**Goals**

- `Instances.ts`: entry resolution (root module's unique zero-parameter `I32` `main`),
  record-before-follow worklist over resolved HIR calls, canonical instance keys with empty
  type/contract-row arguments, explicit unavailable entry states.
- `Lower.ts`: one MIR program per snapshot — instances lowered in discovery order, evaluation-order
  linearization, cleanup insertion driven by the ownership plan, unavailable bodies lowered to
  generated traps, verifier-clean goldens.
- Facade queries (`instancesOf`, `loweredMir`); instance-discovery lab; CFG lab program mode with
  source-slice hover.

**Non-Goals**

- No typed host adapter yet — the user entry is the only instance root until the native pipeline
  proposals introduce the adapter; function values, witness entries, drop glue, and runtime
  helpers have no producers to follow.
- No MIR optimization; no interpreter changes (`retarget-evaluator-to-mir` is next).

## Decisions

1. **Discovery consumes elaboration results, not the facade** — phases compose forward:
   `discover(rootModule, results)` finds HIR functions by canonical identity across modules. Only
   canonical (first-present-occurrence) declarations are reachable, since calls resolve by name.

2. **Entry rules mirror the evaluator's** (unique, zero-parameter, resolved `I32` `main`), as an
   explicit tagged state (`Resolved` / `Unavailable {reason}`) so an invalid entry is data, not
   an error. Discovery with an unavailable entry records nothing.

3. **Instance keys carry empty argument lists by construction.** `{declaration, typeArguments:
   [], contractRow: []}` — the shape the generic language fills in; equality is structural.

4. **Lowering is per-instance and purely local.** Each HIR body linearizes into one entry block:
   post-order over arguments (evaluation order), one fresh local per intermediate result,
   parameters pre-bound to the first locals. The ownership plan's exits direct cleanup: an exit
   with releases would produce a marked cleanup block with drops in plan order and a jump edge —
   with the frozen slice's empty release lists, no cleanup block is fabricated. Unavailable
   bodies lower to one generated `trap` carrying the causative span, keeping the program total
   and verifier-clean.

5. **The lowered program is one MIR module named after the root module**, functions in discovery
   order — the deterministic unit the backend consumes.

6. **The CFG lab gains a program mode** (edit source → snapshot → lowered program) beside the
   samples; provenance hovers include the exact source slice, which is the op-to-source
   navigation the roadmap asked for in list form.

## Risks / Trade-offs

- [Entry duplication with the evaluator] → Two small rule sets today; `retarget-evaluator-to-mir`
  collapses the evaluator onto discovery's entry next, which is why the rules match exactly.
- [Linearization only handles the slice's expression shapes] → It consumes the closed HIR union;
  new HIR variants fail the exhaustive switch at compile time.

## Migration Plan

1. Land `Instances.ts` + tests; `Lower.ts` + goldens verified clean.
2. Facade fields/queries + tests; exports and release-candidate surface.
3. Instance lab; CFG lab program mode.
4. Rollback is git-revert.

## Open Questions

None.
