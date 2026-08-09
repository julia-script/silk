## Context

The compiler has closed, repeated cases for `I32`, `Usize`, and `Bool` across type analysis, intrinsic signatures, HIR, MIR, layout, evaluation, LLVM, Wasm, encoders, and editor tooling. Integer literals and evaluation primarily use JavaScript `number`; array/slice indices still use `I32`. The accepted bootstrap model already calls for the full fixed-width family, target-width integers, unit, bottom, contextual literals, explicit conversions, and explicit arithmetic modes.

## Goals / Non-Goals

**Goals:** one authoritative integer vocabulary, lossless magnitudes, atomic lowercase migration, `usize` indexing, and complete evaluator/backend parity.

**Non-Goals:** floats, text, output, arbitrary precision as a Silk type, SIMD, or compatibility aliases.

## Decisions

### A declarative scalar catalog owns shared facts

Introduce one immutable catalog for spelling, logical category, fixed/target width, signedness, operations, layout, and backend lanes. Actor modules still own phase behavior; the catalog prevents vocabulary drift.

*Alternative considered:* extend every switch independently. Rejected because omissions would appear only late in backend/editor parity.

### Exact integers use bigint internally

Literal magnitude and evaluator values use `bigint` until checked target/serialization boundaries. Every operation applies explicit width and signedness; deterministic encoders use canonical decimal text.

*Alternative considered:* keep `number` below 64-bit boundaries. Rejected because native `usize`, `u64`, and `i64` would be observably lossy.

### Recoverable checked operations use Option

Ordinary operations trap. `checked*` returns `Some<T> | None`; wrapping and saturating return `T`. Canonical Option declarations are ordinary shipped Silk source, while MIR carries backend-neutral arithmetic outcome data.

*Alternative considered:* return value-plus-flag tuples. Rejected because anonymous tuples are outside bootstrap.

### Usize migration is atomic

Array/slice runtime indices, lengths, allocation sizes, capacities, and related bounds logic move together. Fixed array lengths remain compile-time naturals. No release accepts a mixed signed-index convention.

## Risks / Trade-offs

- [Large casing diff obscures semantics] → land catalog/support first, then one mechanical source/golden migration.
- [Subword Wasm values share i32 lanes] → keep logical widths in MIR and test masking/sign extension at every boundary.
- [Option couples intrinsics to stdlib identity] → use canonical nominal identities and keep arithmetic outcome representation independent of physical layout.
- [Change is still substantial] → gate frontend, MIR/evaluator, and each backend with a complete width/operation matrix.

## Migration Plan

1. Add the catalog and lowercase identities; migrate old built-ins with no aliases.
2. Add unit/bottom and exact literal typing.
3. Add integer operations, conversions, and Option.
4. Migrate lengths/indices atomically.
5. Complete MIR, evaluator, LLVM, Wasm, editor tooling, fixtures, and differential gates.

Rollback is revision-level by completed parity phase; no public compatibility mode is preserved.

## Open Questions

None.

