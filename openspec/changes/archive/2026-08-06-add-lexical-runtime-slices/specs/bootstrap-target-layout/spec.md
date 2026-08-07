## ADDED Requirements

### Requirement: The compiler plans one target-aware slice representation

For every reachable concrete slice element type, target layout SHALL publish one logical slice
entry containing an internal correctly aligned address lane followed by one `I32` length lane,
including exact offsets, padding, total size, alignment, and element stride. Shared and exclusive
slices of the same element type SHALL reuse the same physical representation. The address lane MUST
remain an internal layout scalar and MUST NOT resolve as a safe Silk type.

#### Scenario: Plan native and Wasm slice layouts

- **WHEN** the same `&[I32]` program is planned for a 64-bit native target and a 32-bit Wasm target
- **THEN** both plans retain the same logical slice type while selecting their target address widths and exact resulting layouts before backend emission

#### Scenario: Plan a zero-sized element slice

- **WHEN** a slice element has zero byte size and positive logical length
- **THEN** the plan retains its canonical element alignment, stride, address provenance lane, and independent logical length

### Requirement: Slice calling shapes carry heterogeneous typed lanes

The compiler-owned calling shape for a slice SHALL contain one typed address lane and one typed
`I32` lane in deterministic order. Callers, callees, evaluators, and backends MUST consume that
shape rather than flattening the source array or reconstructing a backend-private slice ABI.

#### Scenario: Preserve one multi-length calling shape

- **WHEN** arrays of different fixed lengths are borrowed for the same slice parameter
- **THEN** both calls use the same two-lane slice calling shape and neither array length expands the callee signature

#### Scenario: Keep native addresses pointer-typed

- **WHEN** a native target uses a pointer width different from `I32`
- **THEN** its slice address lane remains pointer-width and is not narrowed to the source-visible length type
