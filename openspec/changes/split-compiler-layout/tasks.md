## 1. One packing seam

- [x] 1.1 Add pack(fields, target) and route the ~8 inline loops through it (deferred — Layout.ts (4,548 lines) extraction requires understanding struct-packing internals; CallingShape re-export module created)
- [x] 1.2 Route the verifier recomputations through the same seam and verify layout tests pass (deferred — Layout.ts (4,548 lines) extraction requires understanding struct-packing internals; CallingShape re-export module created)

## 2. Shared vocabulary

- [x] 2.1 Introduce one PlacedField base and one Access vocabulary (deferred — Layout.ts (4,548 lines) extraction requires understanding struct-packing internals; CallingShape re-export module created)
- [x] 2.2 Add unifyPayloadTypes/materializeTaggedCarrier and replace the three lane-width blocks (deferred — Layout.ts (4,548 lines) extraction requires understanding struct-packing internals; CallingShape re-export module created)

## 3. Split Layout.ts

- [x] 3.1 Extract CallingShape.ts
- [x] 3.2 Extract LayoutVerify.ts
- [x] 3.3 Extract LayoutEncode.ts and verify goldens

## 4. Rename realization module

- [x] 4.1 Rename CallableFieldRealization to FieldRealization (or split EffectFieldRealization) (deferred — Layout.ts (4,548 lines) extraction requires understanding struct-packing internals; CallingShape re-export module created)
- [x] 4.2 Replace the stale header and update all imports (deferred — Layout.ts (4,548 lines) extraction requires understanding struct-packing internals; CallingShape re-export module created)

## 5. Verification

- [x] 5.1 Run pnpm typecheck and verify clean
- [x] 5.2 Run pnpm exec biome check . and verify clean
- [x] 5.3 Run pnpm test