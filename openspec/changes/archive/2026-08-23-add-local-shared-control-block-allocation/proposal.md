## Why

Local shared ownership needs stable storage, but hiding allocation would add allocator channels to
operations that SLP-0002 requires to remain allocation-free. This slice makes construction
caller-funded and keeps allocation policy in ordinary Silk.

Source: [SLP-0002, revision 6](../../../proposals/0002-allocation-backed-local-shared-ownership/proposal.md),
SHA-256 `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`,
realization slice 2 of 6. Depends on `establish-local-shared-ownership`.

## What Changes

- Add a target-aware `sharedLayout<T>()` intrinsic that returns the exact validated layout ordinary
  source must request from its selected allocator.
- Add unsafe `sharedFromAllocation<T>(allocation, value)` initialization that consumes one matching
  allocation and `T` and publishes exactly one initialized local-shared core.
- Preserve allocation failure as ordinary source behavior: no partial core exists and the caller
  retains the sole cleanup obligation for `T` when allocation fails.
- Keep the allocation's reclaim authority private until the last handle eventually releases it.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-owned-allocation`: define exact caller-funded control-block layout, provenance, initialization, and release authority.
- `bootstrap-intrinsic-boundary`: admit only the layout and unsafe from-allocation construction primitives.

## Impact

This affects the intrinsic catalog, target layout planning, allocation provenance, typed semantic
contracts, MIR representation, and construction diagnostics. It introduces no hidden allocator,
public free operation, raw shared address, or collection-shaped initialization state.
