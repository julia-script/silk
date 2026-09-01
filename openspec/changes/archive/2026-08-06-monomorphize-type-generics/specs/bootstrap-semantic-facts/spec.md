## ADDED Requirements

### Requirement: Semantic facts expose generic binding and specialization

Semantic analysis SHALL publish canonical facts for type-parameter declarations and references,
applied nominal types, inferred and explicit call arguments, substitutions, and unavailable
specializations. Every fact SHALL retain source provenance and causal diagnostic identity.

#### Scenario: Inspect an inferred substitution

- **WHEN** a generic call infers `T` as `Token` from its argument
- **THEN** semantic facts expose the parameter, concrete argument, inference source, and specialized result type

#### Scenario: Preserve a conflicting inference

- **WHEN** two arguments require incompatible types for one parameter
- **THEN** facts retain both constraints and one deterministic specialization diagnostic
