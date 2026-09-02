## MODIFIED Requirements

### Requirement: Impl declarations accept a contextual type-parameter list

`impl` declarations SHALL accept an optional type-parameter list between the keyword and the
first type, using the same contextual angle-bracket discipline as function and struct type
parameters. The list SHALL parse losslessly, format canonically, and recover locally: a malformed
parameter list confines its errors to the impl declaration and the following top-level declaration
still parses. The list SHALL be accepted on both the conformance form and the inherent form.

#### Scenario: Parse a parametric conformance losslessly

- **WHEN** source declares `impl<T> Drop for Vector<T> { ... }`
- **THEN** the syntax tree retains the parameter list, capability, and target with full-fidelity reproduction of the original text

#### Scenario: Parse a parametric inherent impl losslessly

- **WHEN** source declares `impl<T> Option<T> { ... }`
- **THEN** the syntax tree retains the parameter list and owner type with full-fidelity reproduction of the original text

#### Scenario: Recover from a malformed parameter list

- **WHEN** an impl type-parameter list is unclosed or contains an unexpected token
- **THEN** the parser reports deterministic diagnostics inside the impl declaration and the next top-level declaration parses normally

## ADDED Requirements

### Requirement: Inherent impl declarations parse losslessly

The declaration grammar SHALL accept `impl [<Binders>] Owner { members }` where no `for` keyword
follows the first type. The syntax tree SHALL use the same declaration node kind as a conformance
and SHALL let the absence of the `for` token distinguish the inherent form; it MUST NOT introduce a
second impl node kind. Members SHALL be ordinary function declarations, including `pub`, `unsafe`,
and `effect` forms. Mapped operations `name: path` SHALL parse inside an inherent impl and SHALL be rejected by
semantic analysis with a diagnostic confined to that member. A malformed inherent body SHALL recover at the next member or
at the closing brace, and the following top-level declaration SHALL parse normally.

#### Scenario: Parse an inherent impl with mixed members

- **WHEN** source declares `impl Counter { pub fn zero() -> Self { ... } fn value(self: &Self) -> i32 { ... } }`
- **THEN** the syntax tree retains both functions, their visibility, and every token with full-fidelity reproduction, and formatting is idempotent

#### Scenario: Distinguish inherent and conformance forms

- **WHEN** one file declares `impl<T> Option<T> { ... }` and `impl<T> Display for Option<T> { ... }`
- **THEN** both parse as impl declarations and only the second carries a `for` token and a second type

#### Scenario: Reject a mapped member in an inherent impl

- **WHEN** an inherent impl body contains `zero: make`
- **THEN** the member parses, semantic analysis reports a diagnostic at that member, and the remaining members still parse

#### Scenario: Recover from a malformed inherent body

- **WHEN** an inherent impl member is missing its closing brace
- **THEN** diagnostics stay inside the impl declaration and the next top-level declaration parses normally
