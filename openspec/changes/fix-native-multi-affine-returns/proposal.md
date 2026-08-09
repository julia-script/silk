## Why

The stack-VM pressure program exposed a native correctness defect: a mutable affine local borrowed
only on an untaken branch was later reloaded from uninitialized address storage, corrupting its
union tag and trapping during cleanup. Evaluation and direct WebAssembly preserved the same value;
LLVM's compile-time "materialized" set incorrectly crossed runtime control-flow paths.

## What Changes

- Add a minimal reproduction that distinguishes sound multi-affine calls and returns from an
  address-taken mutable affine root whose borrow occurs only on another branch.
- Make LLVM address-root storage valid on every runtime path before any post-call reload can read it,
  and synchronize a root's value when its defining operation executes.
- Prove that untaken exclusive-borrow branches preserve the original affine value and its recursive
  exactly-once cleanup, including the stack VM's original separate trace and diagnostic vectors.
- Keep evaluator, native LLVM, and direct WebAssembly execution in exact parity and retain
  deterministic artifacts.
- Correct the stack VM finding from a composite-return ABI defect to path-insensitive native
  address-root materialization; changing the VM back to two vectors is not required by this repair.
- Keep a new return convention, public ABI promise, Vector-specific intrinsic, runtime cleanup
  registry, or general control-flow rewrite out of scope.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-backend`: Require native address-taken mutable roots to remain path-correct when their
  borrow or mutation occurs on only some control-flow paths.

## Impact

- Affects focused compiler tests plus the LLVM backend's private address-root materialization and
  post-call reload bookkeeping.
- Reuses existing layout, MIR, ownership, Drop, `Vector`, evaluator, native toolchain, and direct
  WebAssembly mechanisms.
- Does not change Silk syntax, public compiler APIs, standard-library source, dependencies, or the
  user-visible representation of aggregates and vectors.
