## ADDED Requirements

### Requirement: Semantic facts distinguish primitives, source calls, and service calls

Analysis SHALL publish distinct canonical facts for a qualified `Intrinsic` operation, an ordinary
source function, a service operation, a service declaration, and an implementation mapping. Facts
for an intrinsic SHALL retain its catalog identity and safety contract; facts for source and service
declarations SHALL retain their source identity and spans. No fact MAY infer compiler privilege
from an ordinary declaration spelling.

#### Scenario: Inspect a standard-library numeric call

- **WHEN** a generic source wrapper specializes to one concrete scalar intrinsic
- **THEN** semantic facts preserve both the navigable wrapper declaration and the selected intrinsic identity

#### Scenario: Inspect a service witness call

- **WHEN** a provided source-defined service operation is analyzed
- **THEN** facts identify the service, provider, conformance witness, role, access, and mapped actor function
