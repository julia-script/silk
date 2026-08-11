## ADDED Requirements

### Requirement: Hover distinguishes Intrinsic from source APIs

Hover over a qualified `Intrinsic` member SHALL show its authoritative concrete signature, safety
classification, and source-less intrinsic identity. Hover over a standard-library wrapper,
interface, service, or provider operation SHALL show its authored source declaration and
documentation without presenting it as a compiler intrinsic.

#### Scenario: Hover concrete and generic addition

- **WHEN** a document contains both a generic integer addition call and `Intrinsic.i32Add`
- **THEN** hover presents the navigable generic source contract for the first and the concrete intrinsic contract for the second
