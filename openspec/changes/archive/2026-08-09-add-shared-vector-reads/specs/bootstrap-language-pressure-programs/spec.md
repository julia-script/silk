## MODIFIED Requirements

### Requirement: A bounded stack VM exercises execution and owned observations

The repository SHALL contain a readable bounded stack bytecode VM written in ordinary Silk source.
It SHALL consume a runtime-sized borrowed bytecode slice, execute general arithmetic and
control-flow instructions against a fixed-capacity operand stack, and return owned growable step
and diagnostic observation vectors. The vectors SHALL use ordinary non-union Copy records so they
can be inspected independently through shared sequence reads while the structural-union copy defect
remains isolated. Compiler phases and backends MUST NOT gain VM-specific, opcode-specific, or
operand-stack-specific operations, layouts, or branches.

#### Scenario: Branching bytecode produces owned observations

- **WHEN** the Silk VM executes valid bytecode whose branch target and instruction count are known only at runtime
- **THEN** it returns the expected result and owned step and diagnostic observations that remain valid independently of the input borrow and can be read through shared vector borrows

#### Scenario: Pressure VM remains ordinary Silk

- **WHEN** its published source, MIR, evaluation trace, or backend artifact is inspected
- **THEN** only general language, allocation, collection, control-flow, failure, and cleanup mechanisms are present

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
