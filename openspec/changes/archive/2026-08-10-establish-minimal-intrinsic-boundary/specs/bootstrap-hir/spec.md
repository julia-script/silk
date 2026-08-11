## ADDED Requirements

### Requirement: HIR separates intrinsic, source, and service operations

HIR SHALL represent an explicit intrinsic call with its catalog identity and concrete contract, an
ordinary source call with its canonical declaration identity, and a service operation with its
service and witness obligations. HIR MUST NOT encode standard-library actor names, provider kinds,
or wrapper-specific operation tags as primitive behavior.

#### Scenario: Elaborate a generic numeric wrapper

- **WHEN** a source numeric wrapper calls an interface operation mapped to a concrete intrinsic
- **THEN** HIR retains the source call and conformance before specialization and the explicit intrinsic at its primitive boundary

#### Scenario: Elaborate a source service call

- **WHEN** source calls an operation on a declared service
- **THEN** HIR records general service dispatch without a service-specific operation tag
