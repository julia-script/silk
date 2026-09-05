## 1. Exclusive storage and cleanup

- [x] 1.1 Admit exclusive stored values with invariant mutable payloads and retained child ancestry; verify semantic positive/negative cases for affine transfer, generic wrappers, shared-child copies and parent restoration.
- [x] 1.2 Integrate conservative dependent Drop into cleanup validity and access liveness; verify destructor-only dependencies, recursive generic cleanup, ordered exits and unchanged hook restrictions.
- [x] 1.3 Verify and repair replacement, swap, field extraction/reinitialization and partial-owner cleanup with stable destination types; cover short-source and displaced-storage rejection, complete Drop-child moves, ancestor rejection, incoming failure and MIR initializedness.

## 2. Owned dependent storage

- [x] 2.1 Validate RawBuffer/Slot type, lifetime, provenance and variance contracts, affine transfer and Copy reads; verify owner-backed views, extracted external payloads and cleanup before release without adding collection-specific compiler authority.
- [x] 2.2 Prove maintained Vector make/append/insert/set/remove/pop/growth/failure/destruction for shared and affine exclusive payloads; add semantic/MIR cases and a unique exactly-once native corpus witness, retaining dependent Effect and partial-suspension gates.

## 3. Reuse and handoff

- [x] 3.1 Extend structural summary invalidation and erased generic reuse witnesses and opt-in exclusive-chain/recursive-cleanup/replacement failure workloads; run and document attributable work without state-combination expansion.
- [x] 3.2 Reconcile prescriptive reference, affected main specs, diagnostics, generated artifacts and fixtures; verify strict OpenSpec validation and documentation checks.
- [x] 3.3 Run pnpm typecheck, pnpm format:check, pnpm lint, pnpm test, pnpm check and pnpm release:candidate; record exact results and distinguish pre-existing failures.
