## ADDED Requirements

### Requirement: The analysis facade exposes generic provenance

The immutable analysis facade SHALL query generic declarations, parameter bindings, applications,
call substitutions, discovered concrete instances, layouts, ownership facts, MIR functions, and
diagnostics by canonical identity without reconstructing specialization from rendered text.

#### Scenario: Trace a specialization across phases
- **WHEN** a consumer selects one concrete generic call
- **THEN** the facade returns its source application, substitution, instance key, layout, ownership proof, and MIR provenance

