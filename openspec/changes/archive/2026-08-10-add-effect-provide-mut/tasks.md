## 1. Acceptance Coverage

- [x] 1.1 Add source-level tests for `Effect.provideMut` in data-first and piped forms.
- [x] 1.2 Add tests proving an exclusive provider parameter forwards mutations to the caller.
- [x] 1.3 Add tests for a custom service, a concrete implementation satisfying an abstract capability, and a non-conforming provider producing a source diagnostic.

## 2. Generic Exclusive Requirement Binding

- [x] 2.1 Permit `Effect.bindRequirement` to forward a parameter whose type carries exclusive access.
- [x] 2.2 Record a deferred provider-conformance obligation when the capability or provider type is generic.
- [x] 2.3 Validate deferred requirement bindings after concrete instance substitution and report invalid provision before lowering.

## 3. Standard-Library API

- [x] 3.1 Define `Effect.provideMut` in shipped Silk source with separate capability and provider type parameters.
- [x] 3.2 Regenerate the embedded standard library and verify source navigation and callable contracts include `provideMut`.

## 4. Remove Allocator-Specific Provision

- [x] 4.1 Remove the `Allocator.provide` intrinsic catalog entry and its allocator-specific tests.
- [x] 4.2 Migrate compiler tests, examples, labs, pressure programs, and documentation to `Effect.provideMut`.
- [x] 4.3 Retain direct coverage of `Effect.bindRequirement` as the compiler-owned core used by ordinary user-defined wrappers.

## 5. Validation and Delivery

- [x] 5.1 Run focused compiler and standard-library tests for requirement binding, call inference, execution, and diagnostics.
- [x] 5.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`.
- [x] 5.3 Run `pnpm release:candidate` and validate the OpenSpec change strictly.
- [x] 5.4 Sync the delta specs, archive the completed change, commit it, and merge it into local `main` while preserving unrelated user changes.
