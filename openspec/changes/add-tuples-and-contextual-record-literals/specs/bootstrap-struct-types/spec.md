## ADDED Requirements

### Requirement: Positional and anonymous aggregates have canonical nominal declarations

Each named tuple declaration SHALL enter the nominal declaration catalog with ordered synthesized
position identities and explicit element types. Each uncontextualized tuple or record literal SHALL
enter the semantic catalog as one compiler-synthesized nominal struct declaration keyed by its
canonical module and source occurrence. Synthesized identities SHALL be stable across fresh
processes and MUST NOT depend on inferred member shape, source traversal order, cache state, target,
or backend layout.

Synthesized declarations SHALL remain semantic facts rather than source declarations: they MUST NOT
introduce a spelling into lexical lookup, imports, exports, hover text pretending that the user
declared a name, or compiler-recognized standard-library names. Their ordered positions or fields
SHALL otherwise participate in the same finite-layout validation as source structs.

#### Scenario: Distinguish two generated declarations

- **WHEN** one module contains two separate same-shaped anonymous record literals
- **THEN** the declaration catalog records two deterministic nominal identities tied to their separate source occurrences

#### Scenario: Keep a generated declaration unnameable

- **WHEN** tooling and name lookup inspect an anonymous aggregate
- **THEN** they expose its source occurrence and members without adding any identifier that source can import or write
