## ADDED Requirements

### Requirement: Inspect MIR control-flow graphs

The docs site SHALL expose a direct-link MIR CFG lab rendering the hand-built MIR samples: every
block with its kind, ordered operations, and terminator, the control-flow edges between blocks,
per-operation provenance (span and generated marker) revealed on hover or focus, and the
sample's deterministic textual encoding. The lab SHALL keep its state in browser memory only.

#### Scenario: Inspect blocks and edges

- **WHEN** a developer selects a sample with a branch and a cleanup block
- **THEN** the lab lists every block with its operations and terminator and names each outgoing edge's target block

#### Scenario: Reveal provenance on hover

- **WHEN** a developer hovers a generated drop operation
- **THEN** the entry reveals its causative span and its generated marker

#### Scenario: Show the encoded text

- **WHEN** a developer selects any sample
- **THEN** the lab shows the sample's deterministic textual encoding
