## Purpose

Defines an LLVM-specific typed failure contract so callers can recover from invalid IR construction and encoding inputs without JavaScript exceptions or Effect defects.

## ADDED Requirements

### Requirement: LLVM-specific public error identity

The package SHALL expose expected LLVM operation failures as a yieldable `LlvmError` tagged with `LlvmError`, and SHALL export that contract from `@silklang/llvm/LlvmError` and the package root. The superseded `SilkError` class, tag, actor, and package subpath SHALL be removed as a breaking API migration.

#### Scenario: Caller handles an LLVM failure

- **WHEN** a public LLVM operation rejects caller input
- **THEN** its Effect fails with a `LlvmError` that identifies the rejecting operation
- **AND** the caller can recover with `catchTag('LlvmError', ...)`

#### Scenario: Caller imports the error contract

- **WHEN** a consumer imports the package root or `@silklang/llvm/LlvmError`
- **THEN** the exported error class is named `LlvmError`
- **AND** no `SilkError` public export or subpath remains

### Requirement: Expected invalid inputs stay in the typed channel

Every public operation that accepts values requiring runtime validation SHALL represent rejected values as `LlvmError` failures. It MUST NOT throw, die, or expose an untyped failure for expected invalid input, including fractional, non-finite, unsafe, negative, or out-of-range numeric values.

#### Scenario: Fractional number passed to an integer API

- **WHEN** a caller passes `1.5` to an API that accepts `number | bigint` but requires an integer
- **THEN** the returned Effect fails with `LlvmError`
- **AND** its failure cause contains a typed failure rather than a defect

#### Scenario: Non-finite number passed to an integer API

- **WHEN** a caller passes `NaN`, positive infinity, or negative infinity to an integer-valued API
- **THEN** the returned Effect fails with `LlvmError`
- **AND** no native `RangeError` escapes into the Effect defect channel

### Requirement: Public fallible helpers are effectful

A public helper whose valid input domain cannot be expressed by its TypeScript signature SHALL either be total for that signature or return an Effect with `LlvmError`; it SHALL NOT synchronously throw a yieldable typed error.

#### Scenario: Alignment cannot be encoded

- **WHEN** an explicit alignment exceeds the supported LLVM bitcode field
- **THEN** alignment encoding fails through an Effect with `LlvmError`
- **AND** invoking the public helper does not synchronously throw

### Requirement: Error diagnostics distinguish semantics from ancestry

`LlvmError` SHALL retain the operation and human-readable message. Rejected input and invalid library state SHALL be represented as semantic error details, while JavaScript `cause` SHALL be reserved for an underlying caught failure that genuinely caused the LLVM error.

#### Scenario: Validation failure records input

- **WHEN** a caller supplies an invalid address space, alignment, type, value, or instruction option
- **THEN** `LlvmError` records that value as semantic validation detail
- **AND** it does not claim the rejected value is a JavaScript causal error

#### Scenario: Encoder wraps an implementation failure

- **WHEN** a private encoder or renderer throws unexpectedly inside its Effect boundary
- **THEN** the resulting `LlvmError` retains that thrown value as its diagnostic cause
