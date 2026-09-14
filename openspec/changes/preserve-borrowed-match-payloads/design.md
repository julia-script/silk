## Context

See proposal.md for the observed TLS failure. MIR currently gives a borrowed pattern binding an
ordinary value local, and LLVM copies the selected lanes into that local. Nominal union storage also
uses unified calling lanes, so simply pointing at its first payload lane is invalid when another
variant widens those lanes. Structural unions already support canonical member storage through
conditional value locations.

## Goals / Non-Goals

**Goals:** Give borrowed bindings stable original-payload addresses across calls, captures, and
suspension; reuse existing typed storage and borrow machinery; retain precise ownership diagnostics.

**Non-Goals:** Change match syntax, allow escaping borrowed affine values, introduce a public union
ABI, or alter the owned TLS API to avoid the compiler defect.

## Decisions

1. Nominal union memory contains a tag and enough aligned space for its largest canonical variant
   aggregate. `Layout` retains ordinary variant field offsets. `ValueStorage` resolves a calling
   lane to the active variant's physical field through its existing conditional-location mechanism.
   Calling lanes remain a separate transport shape. This extends the existing structural-union
   distinction between memory and calls rather than adding another storage representation.
2. Borrowed match bindings use the existing `EnvironmentBorrow` representation. It carries the
   semantic payload type and access while storing a reference descriptor. Reads, reborrows, captured
   references, and suspended frames therefore retain the same referent. The scrutinee is lowered as
   its original place, including intermediate references, and is evaluated once. A shared
   `NativePlaceAddress` resolver serves ordinary loans and match selection, retaining one address
   calculation for constant fields, runtime array indices, and slice indices.
3. Remove obsolete nominal carrier materialization rather than retaining two memory paths. A
   copy-in/writeback design was considered but would require committing state on every arm exit and
   maintaining alias identity while an operation suspends. Canonical original storage avoids that
   additional lifecycle and lets existing cleanup address the active fields directly.
4. Prove field layout and binding representation through focused structural tests. Add mutation
   cases to existing native acceptance sources; use the already-required owned TLS native and Wasm
   programs for the publication failure. Temporary investigation binaries are removed.
5. Preserve nested variant discrimination with the canonical field path. Check each inner variant
   before binding its fields or evaluating the arm guard, and continue to later arms on mismatch.
   Coverage must account for the remaining nested alternatives rather than treating one inner
   variant as covering its entire outer variant. This repairs the existing recursive-pattern
   contract exposed by the TLS error patterns; it adds no pattern syntax.

## Risks / Trade-offs

- Changed nominal union size or padding can affect nested aggregates and suspension frames → derive
  every offset from the selected target plan and update structural golden expectations together.
- Calling-lane coercions can differ from field storage widths → exercise differently sized variants
  and preserve strict MIR and storage validation.
- Borrowed bindings can be captured or live across suspension → retain the existing indirect
  representation throughout lowering and frame storage rather than rebinding a backend local only.

## Migration Plan

Update the compiler representation, affected tests, and language reference in one change. All
artifacts are rebuilt from source; no compatibility representation or artifact migration is retained.
