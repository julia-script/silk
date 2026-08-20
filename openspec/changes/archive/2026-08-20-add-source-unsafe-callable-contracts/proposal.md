## Why

Silk has lexical `unsafe` acknowledgement for intrinsics but cannot declare that an ordinary source function transfers a caller-owned safety obligation. That prevents safe wrappers and low-level libraries from exposing precise unsafe APIs without compiler-name privilege.

## What Changes

- Parse and type `unsafe fn` and `unsafe effect fn` as ordinary declarations with an unsafe call qualifier.
- Require one lexical acknowledgement at each unsafe call while keeping all type, ownership, Effect, and requirement checks active.
- Preserve the qualifier through function values, generic substitution, partial application, storage, and interface operation contracts.
- Require an implementation to be no less safe than its declared interface operation; a safe implementation may satisfy an unsafe contract, not the reverse.
- Keep intrinsic unsafety in the same call rule without granting source functions intrinsic privilege.

## Capabilities

### Modified Capabilities

- `bootstrap-syntax`: admit unsafe ordinary and effectful callable declarations.
- `bootstrap-callable-values`: preserve unsafe qualification through values and sections.
- `bootstrap-flow-functions`: compose unsafe effectful calls without changing channels.
- `bootstrap-complete-interface-contracts`: enforce safety compatibility for operation implementations.
- `bootstrap-intrinsic-boundary`: share the lexical acknowledgement rule with source callables.
- `bootstrap-ownership`: retain all ordinary move, borrow, and cleanup checks inside unsafe code.

## Impact

Depends on `generalize-borrows-and-callable-lifetimes`. It changes parsing, formatting, callable types, conformance, HIR/MIR call facts, diagnostics, LSP, and tests. It does not add ambient unsafe modules, disable checks, define FFI, or permit undefined behavior without a documented caller contract.
