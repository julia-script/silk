## ADDED Requirements

### Requirement: Vector provides canonical lexical slice accessors

Canonical Silk source SHALL define `Vector.asSlice(&self) -> &[T]` and
`Vector.asMutSlice(&mut self) -> &mut [T]` as ordinary wrappers over the minimal raw-buffer view
intrinsics. The accessors MUST cover only initialized elements, MUST NOT allocate or copy, and MUST
remain subject to returned lexical borrow checking.

#### Scenario: Borrow initialized vector elements for reading

- **WHEN** source calls `Vector.asSlice` on a live vector containing initialized elements
- **THEN** it receives one shared lexical view whose length equals the vector length

#### Scenario: Borrow initialized vector elements for mutation

- **WHEN** source calls `Vector.asMutSlice` through an exclusive vector borrow
- **THEN** writes through the returned view affect the vector and competing vector access remains suspended

#### Scenario: Keep Vector ordinary source

- **WHEN** tooling navigates either slice accessor or the compiler lowers its body
- **THEN** the accessor resolves to canonical Silk source and only its raw-buffer operation resolves to `Intrinsic`
