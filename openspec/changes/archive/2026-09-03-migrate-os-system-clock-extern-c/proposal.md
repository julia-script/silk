## Why

The system-clock standard-library provider still impersonates libc through two sealed compiler
intrinsics and two compiler-generated C functions. Silk can now express the required C-layout
record and pointer call directly, so keeping this compiler privilege makes every clock boundary
change unnecessarily cross-cutting.

## What Changes

- Rewrite `OsSystemClock` over ordinary `clock_gettime` and `clock_getres` C declarations for the
  supported Darwin and Linux targets.
- Delete the system-clock intrinsic operations, evaluator-specific host provider, reserved runtime
  symbols, and generated C entry points instead of retaining fallbacks.
- Define evaluator absence through `ForeignHostUnavailable` and direct WebAssembly through explicit
  `silk:runtime/foreign@v1` imports.
- Preserve the public `SystemClock` service contract and native clock behavior.
- File every remaining compiler-owned OS provider subsystem as a focused follow-up issue.

## Capabilities

### Modified Capabilities

- `bootstrap-clock-services`: The native system-clock provider owns its libc boundary in ordinary
  Silk while retaining the portable service contract on Darwin and Linux.
- `bootstrap-intrinsic-boundary`: Only monotonic clock operations remain in the sealed intrinsic
  namespace.
- `bootstrap-evaluation`: A reachable system clock uses the ordinary foreign-host availability
  contract instead of a bespoke evaluator provider.
- `bootstrap-silk-stdlib`: The canonical system-clock provider declares its C record and functions
  without a compiler-recognized declaration.

## Impact

This changes compiler standard-library source, intrinsic/MIR inventories, generated runtime
selection, evaluator options and blocked reasons, public compiler exports, clock tests, and
reference documentation. The `SystemClock` language service is unchanged; the removed TypeScript
host API was private machinery for the deleted intrinsic route and has no compatibility standing in
this green-field repository.
