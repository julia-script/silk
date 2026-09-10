## ADDED Requirements

### Requirement: Slice field indexing retains descriptor bounds and element layout

Indexing a slice projected from a borrowed aggregate SHALL retain that slice's backing address, runtime length and element layout through lowering. Direct field indexing and indexing a copied local view SHALL have equivalent consumed values and bounds behavior. A slice descriptor SHALL NOT be addressed as inline repeated element storage.

#### Scenario: Index a slice field through a borrowed receiver

- **WHEN** a borrowed holder reads self.slice[i] directly
- **THEN** lowering emits the same runtime-bounded element access as reading through a local copy of self.slice without a backend defect
- **AND** an index at or beyond the runtime length traps under the ordinary bounds contract

### Requirement: Hidden initializer owners use ordinary storage lifecycles

Materialized array owners retained by binding initializers SHALL have ordinary local storage, initialized-state tracking, ordered cleanup and suspension-frame liveness. Lowering SHALL preserve the original producer evaluation point and SHALL NOT duplicate producers or retain iteration owners beyond their lexical exit.

#### Scenario: Retain a hidden backing array across suspension

- **WHEN** a dependent holder is live in a suspended computation
- **THEN** its hidden backing owner is stored for the suspension and restored for resumed use
- **AND** completion or interruption cleans the initialized owner exactly once after its dependents
