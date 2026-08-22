## Context

See proposal.md. `Layout` owns placement, `LayoutVerify` owns verification and lane-path queries, and `LayoutEncode` owns rendering; callers import those final actors directly. The RepresentationField -> FieldRealization -> Layout hand-off (plan/resolve -> enrich -> place) is already layered and is preserved.

## Decisions

- **pack(fields, target)**: the single cursor=0; alignment=1; offset=alignUp(cursor,a); cursor=offset+size; alignment=max; size=alignUp(cursor,alignment); tailPadding=size-cursor computation, parameterized by copy vs borrow-pointer field sizing. layoutEffectSlots, layoutRepresentedCallable, layoutNominal, layoutDirectRepresented, effect/callable environments, and the verifier recomputations all call it.
- **PlacedField**: one shared offset/size/alignment/padding; the Entry executable fields, EffectEnvironmentField, StoredEffectEnvironmentField, CallableEnvironmentField, CaptureSlot, and EffectEnvironmentSlot variants add only their identity fields.
- **Access vocabulary**: one exported Access (or reuse Type.CallableMode) referenced by FieldRealization, OpaqueRealization, Layout, and Match.
- **unifyPayloadTypes(variants, target)**: collect candidate scalar lanes, sort by Scalar.bits descending, fall back to "i32" — consumed by EffectCompositeShape, SumShape, and OutcomeShape.
- **FieldRealization**: rename, and rewrite the header to name the two realizable kinds (CallableRealization / EffectRealization) instead of a future-tense note.

## Risks / Trade-offs

- [Layout drift] -> LayoutVerify plus the committed-golden byte comparisons are the net.
- [Rename] -> update all imports; pnpm typecheck catches misses.

## Validation

pnpm typecheck, pnpm exec biome check ., pnpm test (target-layout + callable-field-realization suites and golden encodings).
