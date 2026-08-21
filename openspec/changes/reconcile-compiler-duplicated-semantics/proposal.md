## Why

A deep review of `packages/compiler` found that the same semantic rule is implemented two to eight times in the type/compatibility/ownership layer, and the copies have already drifted apart. The most serious case is requirement/access satisfaction, encoded four different ways with contradictory semantics (`Type.ts:1682` uses exact-only matching while every other site lets a stronger access mode satisfy a weaker requirement). Two compiler paths currently disagree about which programs type-check, the exact class of silent divergence a determinism-sensitive compiler cannot tolerate.

## What Changes

- **One canonical requirement/access satisfaction rule** reconciled across `Type`, `TypeCompatibility`, and `InterfaceWitnessCompatibility`. The four divergent encodings collapse into one `Type.compareAccess` / `Type.requirementSatisfies` helper pair.
- **One canonical LIFO release-order computation** shared by `Ownership`, `SuspensionOwnership`, and `Lower` (today four independent copies differ in deduplication and in how they apply the reverse order).
- **One shared set-equality/union primitive** replacing `sameLive`/`intersection` (Ownership) and `equalSet`/`union` (SuspensionOwnership).
- **One boolean fold over `Type.fold`** replacing the five near-identical `contains*` predicates, three of which already disagree on whether they recurse into `requirements`.
- **Fix `Token.describe('Invalid')`** so it no longer reports "valid token".

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

- `bootstrap-complete-interface-contracts`: pin the single requirement/access satisfaction order and require it consistently across compatibility, representation-shape equality, and witness selection.

## Impact

Touches `Type.ts`, `TypeCompatibility.ts`, `InterfaceWitnessCompatibility.ts`, `Ownership.ts`, `SuspensionOwnership.ts`, `Lower.ts`, and `Token.ts`. No public subpath or import-renaming. The access-satisfaction fix can accept programs the exact-only path previously rejected (see the new scenario in `bootstrap-complete-interface-contracts`); every other change is behavior-preserving and is pinned by the existing determinism and golden suites.
