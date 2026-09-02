## ADDED Requirements

### Requirement: MIR carries raw pointer types and primitives

MIR SHALL carry `*const T` and `*mut T` as a logical pointer type with the canonical pointee and
mutability, realized by the layout plan as one address-width scalar. Pointer null, null test,
formation from a reference or slice place, offset, read, and write SHALL be explicit verified
operations; verification SHALL reject a read or write whose pointee is not Copy and an operation
whose operand types disagree with the pointer type. Pointer MIR SHALL encode deterministically.

#### Scenario: Lower a slice pointer formation

- **WHEN** a function calls `Pointer.fromMutSlice(&mut bytes)`
- **THEN** its MIR contains one pointer-formation operation from the slice's address lane to a `*mut u8` destination

#### Scenario: Verify a move-only write as data

- **WHEN** a constructed pointer-write operation targets a `*mut Vector<i32>`
- **THEN** verification reports one structural violation and no artifact is emitted
