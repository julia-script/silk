## ADDED Requirements

### Requirement: Structural unions admit every detached ordinary member type

A structural union SHALL accept finite detached ordinary value types, including scalars, arrays, callable or Effect values with finite representation, and nominal types. Normalization SHALL flatten nested unions, remove `never`, deduplicate identical members, and assign deterministic member identity independently of source ordering.

#### Scenario: Store a scalar and nominal in one union

- **WHEN** a value is typed `i32 | NotFoundError`
- **THEN** both members have deterministic injection, layout, tag, ownership, and narrowing behavior

#### Scenario: Reject a lexical borrow member

- **WHEN** a structural union attempts to include a non-detached local borrow
- **THEN** analysis reports the ordinary non-escape violation rather than silently materializing storage

### Requirement: Union behavior is derived from normalized members

Compatibility, layout, Copy, ownership, cleanup, matching evidence, HIR/MIR tags, evaluation, LLVM, and Wasm SHALL consume one normalized member plan. Generic specialization SHALL NOT change whether two declared member types are distinct unless the generic declaration proved that distinction.

#### Scenario: Clean the active ordinary member

- **WHEN** a union holding a droppable non-nominal member leaves scope
- **THEN** every engine cleans exactly that active payload once

#### Scenario: Encode equivalent unions deterministically

- **WHEN** equivalent member sets are written in different orders
- **THEN** their normalized semantic and emitted representations are identical
