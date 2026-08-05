## Why

With HIR, the ownership proof, and the MIR definition in place, the pipeline's spine closes its
frontend: discover which concrete instances are reachable, then lower them to MIR — inserting the
concrete drops and cleanup edges the ownership phase promised. Instance keys are degenerate while
the slice has no generics, but the worklist, the key shape, and the record-before-follow
termination discipline are the real structure the full language will inherit.

## What Changes

- Analyze every declaration in the reachable source closure, but discover concrete runtime
  instances only from the typed host adapter and user entry, additionally following function
  values, service-witness entries, drop glue, and runtime helpers (degenerate cases now; the
  traversal contract is real).
- An instance key is the canonical declaration ID plus normalized concrete type and contract-row
  arguments; the deterministic worklist records an instance before following it so ordinary
  recursion terminates.
- Lower reachable instances to MIR: structured control flow becomes basic blocks; concrete drops
  and cleanup edges are inserted from the generic ownership proof; typed failures become explicit
  success/failure branches; requirements become canonical hidden service slots. Source and
  semantic provenance stay attached.
- Extend the inspector: instance-discovery lab (entry → worklist → discovered instances) and the
  CFG lab now renders lowered real programs, with drops/cleanup edges visually distinguished and
  op-hover navigating to source.

## Capabilities

### New Capabilities

- `bootstrap-instances`: Deterministic reachability discovery, instance keys, and the recorded
  worklist.

### Modified Capabilities

- `bootstrap-mir`: MIR programs are now constructed from HIR by lowering, not only hand-built.
- `bootstrap-syntax-inspector`: Instance-discovery lab; CFG lab over lowered programs.

## Impact

New phases 5 and 6 of the pinned order; consumes the cleanup plan from `check-ownership-and-cleanup`
and the MIR model from `define-mir-and-encoder`. Golden tests extend to lowered output. No
grammar changes.

## Plan References

- [Roadmap — Track 4, proposal 9](../../../roadmaps/compiler-realignment.md)
- [Issue 06](../../../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md),
  frontend checking order, step 5: "discover concrete runtime instances only from the typed host
  adapter and user entry. … An instance key is the canonical declaration ID plus normalized
  concrete type and contract-row arguments. The deterministic worklist records an instance before
  following it so ordinary recursion terminates."
- Same ticket, step 6: "Lower reachable instances to MIR, turning structured control flow into
  basic blocks and inserting concrete drops and cleanup edges from the generic ownership proof.
  Typed failures become explicit success/failure branches, requirements become canonical hidden
  service slots, and source and semantic provenance remain attached to lowered operations."
