## ADDED Requirements

### Requirement: MIR represents Effect and owned allocation in the structured DAG

MIR SHALL represent effect entry, capture access, retry, typed outcomes, provider acquisition,
validated allocation, raw-buffer slot operations, initialization commit or rollback, explicit drop,
and automatic Drop as ordered regions and operations in the existing acyclic structured control DAG.
It MUST NOT encode source named scopes, dynamic cleanup registries, or backend control structures.

MIR SHALL carry the compiler-selected hidden Effect instance and capture-environment plan separately
from its success/failure outcome. Running a statically known instance SHALL call its generated runner
without universal runtime Effect dispatch.

#### Scenario: Encode a failed append attempt

- **WHEN** Vector growth fails with OutOfMemory inside a retried Effect
- **THEN** MIR orders failed acquisition, rollback of attempt-local owners, failure propagation, and retry without a leaked allocation or cyclic MIR edge
