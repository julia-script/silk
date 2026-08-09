## Why

The stack-VM pressure program exposed a native correctness defect: returning a composite value with
two generic affine fields corrupted the second field's union tag and trapped during cleanup, while
evaluation and direct WebAssembly preserved the same value. This blocks ordinary APIs from
returning multiple owned values and violates the compiler-owned aggregate calling-shape contract.

## What Changes

- Add a minimal reproduction that distinguishes plain nested aggregate returns from composites
  containing multiple generic affine owners.
- Preserve every compiler-planned result lane, field path, and union discriminant across LLVM
  function returns and their call sites.
- Prove that callers receive and clean up each returned affine field exactly once in declaration
  order, including empty and allocated Vector values.
- Keep evaluator, native LLVM, and direct WebAssembly execution in exact parity and retain
  deterministic artifacts.
- Remove the stack VM's compiler-defect disposition once the natural two-vector result shape is
  independently safe; changing the VM back to two vectors is not required by this repair.
- Keep a new return convention, public ABI promise, Vector-specific intrinsic, or runtime cleanup
  registry out of scope.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-target-layout`: Require every lane of a composite result containing multiple generic
  affine fields to retain its canonical path and representation across calls and returns.
- `bootstrap-backend`: Require native aggregate return lowering to preserve multiple generic
  affine fields and their exactly-once cleanup in evaluator and WebAssembly parity.

## Impact

- Affects focused compiler tests plus the LLVM backend's internal function signature, return, and
  call-result lowering where the characterization identifies the lane mismatch.
- Reuses existing layout, MIR, ownership, Drop, `Vector`, evaluator, native toolchain, and direct
  WebAssembly mechanisms.
- Does not change Silk syntax, public compiler APIs, standard-library source, dependencies, or the
  user-visible representation of aggregates and vectors.
