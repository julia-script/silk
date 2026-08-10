## Why

Silk can describe shared borrowed service provision as ordinary library code, but the same wrapper
cannot forward an exclusive provider or infer a conforming implementation for an abstract service
capability. The temporary intrinsic `Allocator.provide` hides that language gap and gives one
service privileged syntax that user-defined services cannot reproduce.

## What Changes

- Add ordinary, visible `Effect.provideMut` Silk source for borrowing an existing provider with
  exclusive access, alongside shared `Effect.provide` and owned `Effect.provideWith`.
- Permit the compiler-owned requirement-binding core to forward an exclusive scoped borrow received
  through a function parameter.
- Make generic call inference use service conformance when one type parameter connects an Effect's
  required capability to a concrete provider implementation.
- Require data-first and pipe forms to preserve identical contracts and execution for shared and
  exclusive provision.
- **BREAKING**: remove the compiler-registered `Allocator.provide` alias and migrate allocation code
  to `Effect.provideMut`.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-flow-functions`: Distinguish shared borrowed `provide`, exclusive borrowed
  `provideMut`, and owned per-run `provideWith`, while keeping both borrowed operations ordinary
  Silk declarations over the compiler-owned requirement-binding core.
- `bootstrap-owned-allocation`: Remove allocator-specific provision and require allocation to use
  the same exclusive borrowed service-provision API as every other service capability.

## Impact

- Compiler elaboration for exclusive provider forwarding and conformance-aware generic inference.
- The visible Effect standard-library source and generated embedded standard library.
- The intrinsic catalog, editor navigation/completion expectations, allocation examples, labs,
  pressure programs, and acceptance tests.
- No new runtime requirement representation, dynamic service lookup, backend intrinsic, or
  concurrency runtime cost.
