## ADDED Requirements

### Requirement: Inspect instance discovery

The docs site SHALL expose a direct-link instance-discovery lab presenting, for an edited
source: the entry state (resolved or explicitly unavailable with its reason), and the recorded
instances in discovery order with their canonical keys. The lab SHALL keep its state in browser
memory only.

#### Scenario: Inspect a discovery worklist

- **WHEN** a developer edits a program whose `main` calls other functions
- **THEN** the lab shows the resolved entry and every discovered instance in discovery order with its canonical key

#### Scenario: Inspect an unavailable entry

- **WHEN** the edited program has no valid `main`
- **THEN** the lab shows the unavailable entry state with its reason and an empty instance list

### Requirement: The CFG lab renders lowered programs

The MIR CFG lab SHALL additionally render the lowered MIR of an edited source program — blocks,
edges, cleanup blocks visually distinguished, and per-operation provenance including the exact
source slice revealed on hover — alongside the hand-built samples.

#### Scenario: Render a lowered program

- **WHEN** a developer edits a program with nested calls in the CFG lab's program mode
- **THEN** the lab renders the lowered functions' blocks and edges from the same source

#### Scenario: Reveal the source slice on hover

- **WHEN** a developer hovers a lowered operation
- **THEN** the entry reveals its span and the exact source slice it lowered from
