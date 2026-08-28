## ADDED Requirements

### Requirement: Evaluation carries nominal union values by active variant

Evaluation SHALL construct, move, copy when explicitly admitted, borrow, pass, return, store, match,
and clean nominal unions according to their canonical applied parent, active variant, complete field
payload, and MIR layout plan. It SHALL NOT expose or independently select numeric tags, flatten
nominal variants into structural members, or evaluate inactive payload storage.

#### Scenario: Evaluate construction and direct nested matching

- **WHEN** a program injects `HttpError.Dns { ... }` into `HttpError | OutOfMemoryError` and matches the variant directly
- **THEN** evaluation selects the `Dns` arm, binds its exact fields, and preserves both nominal and structural identities in the trace

#### Scenario: Evaluate active cleanup

- **WHEN** a droppable generic payload is stored in one variant and the union leaves scope through success or typed failure
- **THEN** evaluation releases exactly that active payload once under the verified cleanup plan
