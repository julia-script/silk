## ADDED Requirements

### Requirement: Pressure programs use shared sequence observation

The lexer and stack VM SHALL use the public shared `Vector.get` surface for ordinary non-union Copy
observations needed after construction. They MUST NOT regain read access by destructuring an owned
vector, taking an exclusive borrow solely to copy an element, or adding program-specific compiler
behavior. Their findings SHALL record the repeated shared-observation wall as repaired and keep the
structural-union copy defect as a separate deferred finding.

#### Scenario: Inspect lexer results without consuming them

- **WHEN** the differential lexer harness fingerprints owned Copy token and diagnostic records
- **THEN** it reads both vectors through shared borrows and the vectors remain live for later observation and cleanup

#### Scenario: Inspect VM observations without union copying

- **WHEN** the stack-VM harness fingerprints ordinary Copy step and diagnostic vectors
- **THEN** it reads each vector through shared borrows without requiring structural-union element reads

#### Scenario: Preserve the separate union defect

- **WHEN** the updated findings classify the observation changes
- **THEN** they mark ordinary shared Vector reads repaired and retain structural-union copy provenance as its own focused follow-up

