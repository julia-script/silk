## Why

The sealed core must become a safe, usable actor without granting a library declaration compiler
privilege. This slice supplies the ordinary-source `Shared<T>` wrapper and keeps allocation and
conflict policy visible in its API.

Source: [SLP-0002, revision 6](../../../proposals/0002-allocation-backed-local-shared-ownership/proposal.md),
SHA-256 `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`,
realization slice 4 of 6. Depends on `add-local-shared-control-block-allocation` and
`add-local-shared-lifecycle-operations`.

## What Changes

- Ship `Shared<T>` as canonical ordinary Silk source containing one opaque local-shared core.
- Implement effectful `make` over `Allocator.allocate`, allocation-free `clone`, and callback-shaped
  `withMut` over the sealed operations.
- Derive `with` in ordinary source by narrowing the exclusive callback borrow to `&T`.
- Select an ordinary-source trap policy for every reentrant access conflict while leaving the
  primitive's callback outcome reusable by future data-returning wrappers.
- Keep `Shared`, allocator actors, Deferred, Scheduler, and ready-inbox actors unknown to every compiler phase by spelling.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-silk-stdlib`: add the canonical local `Shared<T>` actor and its safe public contracts.

## Impact

This affects shipped Silk sources, the standard-library manifest and generated source table,
documentation, and source-level acceptance tests. It intentionally exposes no raw addresses,
identity comparison, Weak handles, thread-safe sharing, or hidden allocation.
