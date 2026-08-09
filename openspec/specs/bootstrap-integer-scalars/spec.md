# bootstrap-integer-scalars Specification

## Purpose

Define Silk's complete bootstrap integer vocabulary, ergonomic unit and bottom forms, exact literals, conversions, operation modes, and cross-engine behavior for real width-conscious programs.

## Requirements

### Requirement: Integer primitive spellings are lowercase and closed

Silk SHALL recognize exactly `bool`, `u8`, `u16`, `u32`, `u64`, `usize`, `i8`, `i16`, `i32`, `i64`, and `isize` as the integer and Boolean primitive spellings. Uppercase spellings such as `Bool`, `I32`, and `Usize` MUST NOT remain aliases.

#### Scenario: Resolve every integer primitive

- **WHEN** declarations use every canonical lowercase integer spelling
- **THEN** each resolves to one distinct compiler-known type

#### Scenario: Reject a removed spelling

- **WHEN** source uses `I32` without a user declaration of that name
- **THEN** it is unresolved rather than mapped to `i32`

### Requirement: Unit and bottom use ergonomic source forms

`()` SHALL be the unit type and sole value. A function without a written result SHALL return `()`, and bare `return` SHALL return it. `never` SHALL be uninhabited bottom and SHALL contribute no runtime payload.

#### Scenario: Return unit

- **WHEN** a function omits its result and reaches its end
- **THEN** it completes with `()` and no value lane

#### Scenario: Join non-returning control flow

- **WHEN** a branch of type `never` appears where `i32` is required
- **THEN** the join accepts it without constructing a bottom value

### Requirement: Integer literals retain exact contextual magnitude

Integer literals SHALL retain exact magnitude until typed. An immediate integer context, including a concrete ordinary-call parameter, pipeline-inserted parameter, or known homogeneous operator operand, SHALL select a type only when representable; an unconstrained integer SHALL default to `i32`. An enclosing operator-result context MUST NOT suppress the operand context. Range rejection MUST occur before MIR lowering without JavaScript-number rounding. Contextual literal selection MUST NOT convert an already-typed integer expression or otherwise introduce implicit numeric conversion.

#### Scenario: Type a byte magnitude

- **WHEN** `255` appears where `u8` is required
- **THEN** it receives `u8` with exact value 255

#### Scenario: Contextualize an ordinary call literal

- **WHEN** an exact integer literal is passed directly to a concrete integer parameter of an ordinary function
- **THEN** the literal receives the parameter type when its value is representable

#### Scenario: Contextualize a pipeline literal

- **WHEN** an exact integer literal is inserted as a concrete integer parameter by the pipeline operator
- **THEN** the literal receives the parameter type when its value is representable

#### Scenario: Contextualize a comparison operand literal

- **WHEN** `return byte == 13` compares a known `u8` operand with an exact literal inside a `bool` result context
- **THEN** the literal receives `u8` from the homogeneous comparison operands rather than `bool` or the unconstrained `i32` default

#### Scenario: Reject a wide out-of-range magnitude

- **WHEN** a literal exceeds its selected integer range
- **THEN** analysis reports the range before MIR lowering

#### Scenario: Reject an already-typed mismatch

- **WHEN** an integer expression with an established type is passed to a different integer parameter type without an explicit conversion
- **THEN** analysis rejects the call rather than converting the expression

### Requirement: Integer operations are homogeneous and explicit

Ordinary arithmetic SHALL accept one identical integer type and trap on overflow, invalid division/remainder, or invalid shift counts. Comparisons SHALL return `bool`. Every integer SHALL expose bitwise operations, shifts, and rotates. Named recoverable checked operations SHALL return `Option<T>`; wrapping and saturating variants SHALL return `T`. No numeric conversion SHALL be implicit.

#### Scenario: Trap ordinary byte overflow

- **WHEN** `u8.add(255, 1)` executes
- **THEN** evaluation, native, and WebAssembly trap at the same operation

#### Scenario: Recover checked overflow

- **WHEN** `u8.checkedAdd(255, 1)` executes
- **THEN** it returns `None`, while representable addition returns `Some<u8>`

#### Scenario: Reject mixed arithmetic

- **WHEN** an expression combines `i32` and `i64` without conversion
- **THEN** analysis rejects it without choosing a wider type

### Requirement: Every admitted integer operation has engine parity

HIR, MIR, layout, evaluator, native LLVM, and direct WebAssembly SHALL support every integer type and operation accepted by analysis. Unsupported target behavior MUST be rejected before artifact commitment.

#### Scenario: Compare integer engines

- **WHEN** an accepted integer fixture completes, returns `Option`, or traps
- **THEN** evaluator, native, and WebAssembly agree on its outcome and provenance
