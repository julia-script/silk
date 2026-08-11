## ADDED Requirements

### Requirement: The facade exposes the intrinsic boundary and its source wrappers

The analysis facade SHALL expose the sealed `Intrinsic` namespace, catalog signatures, safety
classification, and source-less identities separately from navigable source declarations for
standard-library wrappers and services. All answers SHALL come from one coherent analysis snapshot
and MUST NOT reconstruct intrinsic or wrapper identity from spelling.

#### Scenario: Query an intrinsic and its wrapper

- **WHEN** a consumer queries a public numeric wrapper and the concrete intrinsic selected after specialization
- **THEN** the facade returns one source declaration identity and one distinct intrinsic identity with their relationship preserved
