## ADDED Requirements

### Requirement: Reflection and template failures retain static source context

Static diagnostics produced while reflecting an aggregate, iterating static values, parsing a
template, matching a placeholder, projecting a field, or selecting `Display` evidence SHALL retain
the primary authored operation and ordered static trace. When a failure originates from template
text, its location data SHALL retain the authored template expression and applicable transformed
UTF-8 byte start and end. Static-text slicing SHALL compose offsets through static bindings and
helper calls rather than resetting provenance to the whole literal or current parameter. When a
failure involves an authorized field, structured details MAY include its public label or position
and concrete type; inaccessible field names MUST NOT be disclosed.

Ordinary source validation ending in `compileError` SHALL retain the existing compile-error identity.
Phase violations, evaluation budgets, interface failures, and ownership failures SHALL retain their
own existing diagnostic identities rather than being relabeled as template errors.

#### Scenario: Diagnose an unmatched brace

- **WHEN** an evaluated template contains an unmatched opening brace
- **THEN** the compile-error diagnostic points to that template byte range and retains the formatting static-call trace

#### Scenario: Preserve a nested static slice range

- **WHEN** a template passes through a static binding and helper that slices a malformed multibyte segment
- **THEN** `compileError` retains the original template expression and the composed UTF-8 byte start and end without formatting-specific compiler recognition

#### Scenario: Preserve an interface diagnostic

- **WHEN** a selected field lacks `Display` evidence
- **THEN** the ordinary interface-selection diagnostic identifies the concrete field type and placeholder provenance rather than becoming `compileError`

#### Scenario: Hide a private field spelling

- **WHEN** a placeholder attempts to select a field that is not visible at the reflection site
- **THEN** the diagnostic identifies an unavailable placeholder without listing the private declaration's spelling as an available candidate
