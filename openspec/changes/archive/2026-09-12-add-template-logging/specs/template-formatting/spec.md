## ADDED Requirements

### Requirement: Logger providers reuse template formatting semantics

The ordinary Logger service and all Effect logging helpers SHALL accept the same static template and
borrowed tuple or record argument-pack model as `Format.format`. Logging template parsing, aggregate
matching, field projection, and Display selection SHALL have the same compile-time outcomes as direct
Writer-backed formatting. The residual logging operation MUST NOT contain a second parser, runtime
reflection, variadic convention, argument-pack copy, or mandatory intermediate String.

#### Scenario: Format a borrowed record through Logger

- **WHEN** source runs `Effect.log("Hello, {name}", &.{ name: "Julia" })`
- **THEN** specialization selects the same field and Display evidence as `Format.format` and the provider observes `Hello, Julia`

#### Scenario: Reject an invalid logging template

- **WHEN** a logging template has malformed braces, mixed placeholder modes, a missing field, or missing Display evidence
- **THEN** specialization reports the same template validation failure as direct formatting before a Logger invocation is published

#### Scenario: Reuse a logging argument pack

- **WHEN** one local tuple or record is borrowed by several logging calls
- **THEN** every call reads the live owner without consuming or copying the argument pack

#### Scenario: Preserve provider-specific runtime failure

- **WHEN** compile-time template validation succeeds but the selected Logger cannot render or retain the complete invocation
- **THEN** execution fails with that provider's LogError rather than exposing WriterError or changing the template-formatting grammar
