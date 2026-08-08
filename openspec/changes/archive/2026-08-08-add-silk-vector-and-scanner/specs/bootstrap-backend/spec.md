## ADDED Requirements

### Requirement: Runtime layout operations lower natively

`Layout.make` validation and `Layout.repeat` checked repetition SHALL lower in the LLVM and
direct WebAssembly backends with the evaluator's exact semantics: power-of-two alignment
validation, aligned stride rounding, and overflow classification against the selected target's
`Usize` range, producing the same tagged union members on every engine.

#### Scenario: Repeat a layout at a runtime count

- **WHEN** a program repeats an element layout by a runtime count within range and allocates the result
- **THEN** the evaluator, native, and WebAssembly runs agree on the allocation size and result

#### Scenario: Classify overflow identically

- **WHEN** the repeated size exceeds the target's `Usize` range
- **THEN** every engine produces the overflow member and no allocation occurs

### Requirement: Owning union fields release conditionally

Cleanup of a structural-union value whose members carry reclaim obligations SHALL release
exactly the live member's obligations, selected by the union tag at runtime, in both native
backends' cleanup paths.

#### Scenario: Release only the live member

- **WHEN** a dropped union currently holds its allocation-owning member
- **THEN** exactly that allocation releases once, and dropping the same union holding its empty member releases nothing
