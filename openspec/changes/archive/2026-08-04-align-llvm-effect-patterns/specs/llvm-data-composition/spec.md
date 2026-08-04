## Purpose

Defines composable immutable LLVM configuration values that support both direct data-first calls and idiomatic pipe-based transformation without changing their semantics.

## ADDED Requirements

### Requirement: Immutable transformations support both call forms
Immutable update operations for `FastMath`, `IntegerMath`, and `MemoryAccess` SHALL support equivalent data-first and pipeable forms wherever an operation takes the actor value plus configuration arguments.

#### Scenario: Integer flags are updated data-first
- **WHEN** a caller invokes `withExact(flags, true)`
- **THEN** the operation returns a new value with exactness enabled
- **AND** the original value is unchanged

#### Scenario: Integer flags are updated in a pipe
- **WHEN** a caller pipes `flags` through `withExact(true)`
- **THEN** the result equals the data-first result for the same arguments

#### Scenario: Atomic memory access is updated in a pipe
- **WHEN** a caller pipes a `MemoryAccess` value through `withAtomic(ordering, scope)`
- **THEN** the result equals `withAtomic(value, ordering, scope)`
- **AND** the original value is unchanged

### Requirement: Existing data-first behavior remains compatible
Adding pipeable overloads SHALL preserve existing data-first parameter order, defaults, return values, and immutability for all affected operations.

#### Scenario: Existing call omits an optional argument
- **WHEN** an existing data-first call relies on an operation's default option
- **THEN** it produces the same value after pipeable overloads are introduced

