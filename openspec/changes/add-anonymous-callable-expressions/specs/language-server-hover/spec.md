## ADDED Requirements

### Requirement: Anonymous callable hover presents contract and captures

Hover on an available anonymous callable expression or its `fn` token SHALL present its source-like
ordinary or effectful callable contract, derived invocation mode, and deterministic captures with
their selected lexical names and shared, exclusive, Copy, or moved access. The presentation MUST NOT
invent a declaration name, import path, independent generic signature, or erased closure type.
Capture-free callables SHALL be identified as having no captures. Hover inside the body SHALL retain
the existing token-specific facts for parameters, outer lexical references, types, and operations.

#### Scenario: Hover a captured anonymous callable

- **WHEN** hover selects an anonymous callable that reads `offset` and mutates `counter`
- **THEN** hover shows its authored callable contract, derived `mut fn` mode, and the shared and exclusive capture facts without a synthetic symbol name

#### Scenario: Hover a capture-free effectful callable

- **WHEN** hover selects `effect fn(error: Failure) -> i32 { return 42 }`
- **THEN** hover preserves the effectful source contract, reports no captures, and does not reduce it to an importable named function

