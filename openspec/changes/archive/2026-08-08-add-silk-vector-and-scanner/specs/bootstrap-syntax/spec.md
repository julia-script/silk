## ADDED Requirements

### Requirement: Impl declarations accept a contextual type-parameter list

`impl` declarations SHALL accept an optional type-parameter list between the keyword and the
capability path, using the same contextual angle-bracket discipline as function and struct type
parameters. The list SHALL parse losslessly, format canonically, and recover locally: a malformed
parameter list confines its errors to the impl declaration and the following top-level declaration
still parses.

#### Scenario: Parse a parametric conformance losslessly

- **WHEN** source declares `impl<T> Drop for Vector<T> { ... }`
- **THEN** the syntax tree retains the parameter list, capability, and target with full-fidelity reproduction of the original text

#### Scenario: Recover from a malformed parameter list

- **WHEN** an impl type-parameter list is unclosed or contains an unexpected token
- **THEN** the parser reports deterministic diagnostics inside the impl declaration and the next top-level declaration parses normally

### Requirement: Whole-member binding patterns parse losslessly

Match patterns SHALL accept `Member name` alongside field destructuring, parsing losslessly with
canonical formatting and local recovery inside the containing arm.

#### Scenario: Parse and format a binding pattern

- **WHEN** source matches with arms `Empty nothing => 0` and `Full full => 1`
- **THEN** the syntax tree retains both binding patterns with full-fidelity reproduction and the formatter prints them canonically
