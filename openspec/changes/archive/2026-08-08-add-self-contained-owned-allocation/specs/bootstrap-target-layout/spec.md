## ADDED Requirements

### Requirement: Target planning owns allocation and typed-storage shapes

After concrete instance discovery, target layout SHALL plan validated `Layout` values,
repeated-element stride and total bytes, affine allocation handles, private reclaim tickets,
`RawBuffer<T>`, lexical Slot addresses, Drop calling shapes, and typed allocation outcomes using the
selected target's address and `Usize` width. Zero-sized allocations SHALL retain distinct logical
ownership without requiring nonzero physical bytes. Evaluator and backends SHALL consume these
facts unchanged and MUST NOT choose stride, alignment, ticket shape, failure transport, or cleanup
representation independently.

#### Scenario: Plan padded elements on two targets

- **WHEN** the same repeated aggregate type is reachable for a 64-bit native target and `wasm32`
- **THEN** each plan uses its selected address width and compiler-derived padded stride while retaining identical logical ownership and Drop order

#### Scenario: Plan zero-sized ownership

- **WHEN** a valid runtime count of a zero-sized element type is allocated
- **THEN** layout records zero physical bytes with the exact logical count and a distinct affine allocation identity
