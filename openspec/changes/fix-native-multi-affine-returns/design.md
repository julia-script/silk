## Context

See `proposal.md` for motivation. Layout already publishes one flattened calling shape per semantic
type, MIR carries the returned logical value, and the evaluator and direct WebAssembly backend
preserve the stack VM's original two-Vector result. LLVM declares multi-lane results as literal
structures, builds the aggregate at each return, then extracts lanes positionally at calls. The
first known failure appears only after a result containing two generic affine fields crosses that
native boundary: cleanup reads an invalid active tag from the second field.

## Goals / Non-Goals

**Goals:**

- Reduce the VM failure to a small checked-in matrix that identifies the first divergent phase or
  engine boundary.
- Make LLVM result construction and call extraction symmetric with the selected calling shape for
  every lane count and repeated scalar representation.
- Preserve whole-value ownership transfer and declaration-ordered exactly-once cleanup.

**Non-Goals:**

- Changing the compiler-owned flattened calling convention, publishing it as a stable external ABI,
  or adopting a platform C ABI for internal Silk calls.
- General structural-union `Slot.copy`, nested dynamic reference places, named constants, or shared
  Vector reads.
- Rewriting the completed stack VM back to separate vectors as part of the compiler repair.

## Decisions

### D1: Characterize the result shape in layers before repairing LLVM

The focused matrix starts with a Copy-only nested aggregate, then two empty generic affine fields,
then two independently allocated fields whose values are observed by the caller. Each case asserts
layout paths, MIR verification, evaluator behavior, direct Wasm behavior, emitted LLVM structure
shape, and native execution. The first failing layer determines whether the defect belongs to
planning, MIR, emission, optimization, or cleanup.

Using only the full VM was rejected because it mixes effects, unions, allocation growth, and a large
result. Testing only two scalar structs was rejected because that shape already passes and does not
carry the repeated generic cleanup layout that exposes the defect.

### D2: Treat selected lane identity, not LLVM scalar equality, as the source of truth

Callee return construction and caller extraction must enumerate the same ordered
`Layout.CallingLane` sequence and retain its canonical paths through the backend's declared-function
record. Equal LLVM lane types are not interchangeable evidence: the second owner's tag and payload
lanes may share representations with the first while remaining logically distinct.

Recomputing a result structure independently at each emission site was rejected because it permits
producer and consumer drift. Changing semantic layout was rejected unless the characterization
shows its paths are already wrong; evaluator and Wasm agreement currently says otherwise.

### D3: Repair the narrowest shared LLVM result boundary

The implementation first records enough declared result-shape data to validate and drive both
aggregate construction and extraction. If emitted unoptimized LLVM IR is already lane-correct, the
repair moves to the exact builder encoding or optimization boundary proven by the test instead of
adding compensating reordering. No code path may inspect `Vector`, a generic declaration name, or a
cleanup kind to choose the result representation.

### D4: Prove ownership through use and cleanup, not merely an exit code

The allocated case makes each returned field contain a different observable value, consumes those
values in the caller, and leaves both owners live for ordinary recursive cleanup. Evaluator traces
must contain balanced acquisitions/releases and Drop calls; native and Wasm must return the same
observation without trapping. Fresh-process artifacts cover the repaired aggregate boundary.

## Risks / Trade-offs

- **[Risk] The failure depends on LLVM optimization rather than IR construction.** → Run the reduced
  native case in debug and release and inspect both IR and executable outcomes before changing
  lowering.
- **[Risk] A passing empty-owner case hides payload corruption.** → Keep the independently allocated
  case as the acceptance gate and give the two fields different values and element types when useful.
- **[Risk] The repair accidentally changes every aggregate artifact.** → Compare unaffected scalar
  and Copy-aggregate fixtures, update deterministic goldens only when the selected lane sequence
  genuinely changes, and run the full suite.
- **[Risk] Native success cannot directly report deallocation counts.** → Pair it with exact
  evaluator cleanup traces and a native invalid-tag trap boundary; do not claim host allocator
  instrumentation that the runtime does not expose.

## Migration Plan

This is an internal correctness repair with no source migration. Land the focused regression and
backend fix together. Rollback restores the prior lowering and regression failure; no persisted
format or public API depends on the repair.
