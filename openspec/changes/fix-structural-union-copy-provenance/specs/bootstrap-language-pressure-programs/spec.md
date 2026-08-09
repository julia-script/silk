## MODIFIED Requirements

### Requirement: A bounded stack VM exercises execution and owned observations

The repository SHALL contain a readable bounded stack bytecode VM written in ordinary Silk source.
It SHALL consume a runtime-sized borrowed bytecode slice, execute general arithmetic and
control-flow instructions against a fixed-capacity operand stack, and return one owned growable
ordered `Step | VmDiagnostic` observation vector. Both members SHALL remain ordinary Copy records,
and the stream SHALL be inspected through shared sequence reads after execution. Compiler phases
and backends MUST NOT gain VM-specific, opcode-specific, operand-stack-specific, or event-union-
specific operations, layouts, or branches.

#### Scenario: Branching bytecode produces an ordered owned stream

- **WHEN** the Silk VM executes valid or malformed bytecode whose branch target and instruction count are known only at runtime
- **THEN** it returns the expected result and one ordered step-and-diagnostic stream that remains valid independently of the input borrow and can be read through a shared vector borrow

#### Scenario: Pressure VM remains ordinary Silk

- **WHEN** its published source, MIR, evaluation trace, or backend artifact is inspected
- **THEN** only general language, allocation, collection, control-flow, failure, union-copy, and cleanup mechanisms are present

### Requirement: Pressure programs use shared sequence observation

The lexer and stack VM SHALL use the public shared `Vector.get` surface for recursively Copy
observations needed after construction. The VM's structural-union event elements SHALL preserve
their active-member and payload provenance. Neither program may regain read access by destructuring
an owned vector, taking an exclusive borrow solely to copy an element, or adding program-specific
compiler behavior. Their findings SHALL record both ordinary shared observation and structural-
union copy provenance as repaired.

#### Scenario: Inspect lexer results without consuming them

- **WHEN** the differential lexer harness fingerprints owned Copy token and diagnostic records
- **THEN** it reads both vectors through shared borrows and the vectors remain live for later observation and cleanup

#### Scenario: Inspect ordered VM union observations

- **WHEN** the stack-VM harness fingerprints its ordered `Step | VmDiagnostic` vector after execution
- **THEN** it reads each event through a shared vector borrow and observes the exact active member and payload in source order

#### Scenario: Retire the separate union defect

- **WHEN** the updated pressure findings classify the completed observation changes
- **THEN** they mark structural-union copy provenance repaired without implying another self-hosting step
