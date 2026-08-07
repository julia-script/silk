## ADDED Requirements

### Requirement: Slice types and borrows retain canonical semantic facts

Semantic analysis SHALL publish slice type facts containing canonical element type and shared or
exclusive access without a fixed length. Each explicit borrow or reborrow SHALL retain its access,
stable source root, source type, resulting slice type, call destination, exact syntax provenance,
and an explicit unavailable state when any prerequisite is missing.

#### Scenario: Resolve different arrays to one slice type

- **WHEN** `&short` and `&long` borrow `Array<I32, 3>` and `Array<I32, 6>` for `&[I32]`
- **THEN** both borrow facts retain their distinct source types and the same canonical resulting slice type

#### Scenario: Preserve an invalid exclusive borrow

- **WHEN** `&mut values` targets an immutable array binding
- **THEN** the fact retains exclusive intent, the resolved source root and type, and the diagnostic cause without claiming an available exclusive slice

### Requirement: Slice operations publish runtime-place facts

The `length` projection of an available slice SHALL have type `I32`. Indexing SHALL publish one
borrowed place fact whose element type and access derive from the slice, whose index fact is `I32`,
and whose bounds are identified as runtime slice bounds rather than a fabricated fixed length.

#### Scenario: Analyze a shared slice index

- **WHEN** `values[index].kind` indexes `values: &[Token]`
- **THEN** semantic facts identify the runtime-bounded shared element place and the projected Copy field without materializing a `Token` value

#### Scenario: Keep an unavailable slice index explicit

- **WHEN** a slice index has an unavailable or non-`I32` expression
- **THEN** the index and source facts remain inspectable while no valid borrowed element place is fabricated
