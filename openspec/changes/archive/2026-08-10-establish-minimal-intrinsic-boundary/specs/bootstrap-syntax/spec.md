## ADDED Requirements

### Requirement: Service declarations are lossless and recoverable

The syntax SHALL preserve `service` declarations, visibility, nominal names, generic parameters,
operation names, complete function contracts, documentation, delimiters, and source spans. Service
operations SHALL reuse ordinary function-contract syntax rather than introduce method bodies or
stored fields. Missing names, contracts, or delimiters MUST recover inside the declaration without
consuming a following top-level member.

#### Scenario: Parse a service contract

- **WHEN** source declares a public service with effectful operations and explicit failure and requirement rows
- **THEN** the syntax tree retains every token and distinguishes the service from a struct, interface implementation, and source module

#### Scenario: Recover a damaged service operation

- **WHEN** one service operation omits a parameter delimiter before a valid following operation
- **THEN** the parser records local missing syntax and preserves the following operation and declaration
