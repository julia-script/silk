## Why

**Layout.ts** (4,548 lines) mixes five concerns — physical placement, a reachability walk, environment placement, calling-shape/ABI lanes, verification, and encoding — and repeats the struct-packing loop (cursor/alignUp/size/tailPadding) about eight times. The placed-field record is re-declared five times, the lane-width unification block is copy-pasted three times, the access vocabulary (Copy/Shared/Exclusive/Take) is re-declared per module, and **CallableFieldRealization.ts** is misnamed with a stale "not yet enabled" header even though its Effect half is fully implemented.

## What Changes

- **Split Layout.ts** into **Layout** (physical placement), **CallingShape** (shape nodes/lanes/selectors), **LayoutVerify** (verification), and **LayoutEncode** (text encoding).
- **Add one pack(fields, target) seam** (returns fields/size/alignment/tailPadding) and route the ~8 inline loops plus the verification recomputations through it.
- **One PlacedField** (offset/size/alignment/padding) and one **Access** base shared across the five placed-field types and three access vocabularies; Match.Access narrows only if it needs Move.
- **One unifyPayloadTypes** replaces the three copy-pasted lane-width blocks; one materializeTaggedCarrier for tagged carriers.
- **Rename CallableFieldRealization to FieldRealization** (or split EffectFieldRealization) and replace the stale header with the two realizable kinds.

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

<!-- none: behavior-preserving refactor (skip_specs); no entry offsets/sizes/encodings change -->

## Impact

Pure refactor; every emitted offset, size, alignment, and encoding stays byte-identical. skip_specs: true.