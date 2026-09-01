## ADDED Requirements

### Requirement: Backends realize the compiler-owned union plan

Native LLVM and direct WebAssembly emission SHALL consume the union's compiler-owned discriminant,
member tags, payload placement, padding, calling shape, and member-slot mappings without choosing a
different representation. Injection, calls, returns, struct/array storage, reads, moves, and writes
SHALL preserve the same active member and complete payload as evaluation.

#### Scenario: Emit one union through both backends

- **WHEN** a program injects a move-only nominal value, transports it through an aggregate, and widens it
- **THEN** native and WebAssembly execution agree with evaluation on the final result and cleanup

### Requirement: Backend union dispatch remains private and deterministic

A backend MAY introduce private branches or structured target constructs to remap a widening or
clean an active payload, but it SHALL derive them only from the verified logical conversion and
cleanup mappings. Such control MUST NOT alter MIR regions or leak labels, branch depths, blocks, or
numeric target tags back into compiler-owned relationships. Equivalent emissions SHALL preserve
stable symbols, text, binary bytes, and provenance.

#### Scenario: Lower active-member cleanup

- **WHEN** a union with two differently shaped move-only members reaches cleanup
- **THEN** each backend dispatches through its private target form and releases exactly the active member

#### Scenario: Repeat backend emission

- **WHEN** one union program is emitted repeatedly for a supported target
- **THEN** LLVM IR, bitcode, WAT, WebAssembly bytes, and union provenance are identical
