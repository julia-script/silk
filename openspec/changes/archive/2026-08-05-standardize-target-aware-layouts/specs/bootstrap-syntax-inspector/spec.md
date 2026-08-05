## MODIFIED Requirements

### Requirement: Inspect MIR control-flow graphs

The docs site SHALL expose a direct-link MIR CFG lab rendering the hand-built MIR samples: the
canonical selected target, ordered layout table, every block with its kind, ordered operations and
terminator, the control-flow edges between blocks, per-operation provenance (span and generated
marker) revealed on hover or focus, and the sample's deterministic textual encoding. The lab SHALL
keep its state in browser memory only.

#### Scenario: Inspect blocks and edges

- **WHEN** a developer selects a sample with a branch and a cleanup block
- **THEN** the lab lists its target and scalar layouts and every block with its operations, terminator, and named outgoing edges

#### Scenario: Reveal provenance on hover

- **WHEN** a developer hovers a generated drop operation
- **THEN** the entry reveals its causative span and its generated marker

#### Scenario: Show the encoded text

- **WHEN** a developer selects any sample
- **THEN** the lab shows the deterministic textual encoding containing the same target and ordered layout entries displayed by the visual view

### Requirement: Inspect emitted LLVM IR beside MIR

The docs site SHALL expose a direct-link LLVM IR lab presenting, for an edited program: the
compiler-selected target and layout plan, emitted IR text, symbol table, and lowered MIR blocks with
per-operation provenance — so emitted representations are readable beside the compiler facts they
realize. The target and layout shown beside LLVM SHALL come from the same analysis snapshot and MIR
program passed to emission. The lab SHALL keep its state in browser memory only.

#### Scenario: Inspect emitted IR

- **WHEN** a developer edits a program with nested calls
- **THEN** the lab shows one target and layout plan beside IR containing `silk_main`, the symbol table, and lowered MIR blocks from the same snapshot

#### Scenario: Compare planned Bool with emitted Bool

- **WHEN** a developer edits a branching program
- **THEN** the lab shows the compiler's four-byte `Bool` plan beside the LLVM instructions that realize it

#### Scenario: Toggle debug metadata

- **WHEN** a developer switches the lab to a debug emission
- **THEN** the IR text shows the compile unit and located instructions without changing the displayed target or layout plan

## ADDED Requirements

### Requirement: Inspect emitted WebAssembly beside its MIR layout

The existing direct-link WebAssembly lab SHALL build its analysis snapshot for
`wasm32-unknown-unknown` and SHALL display that canonical target and ordered compiler layout plan
beside the emitted WAT and binary facts. The lab MUST NOT reuse a native-target snapshot or present
WebAssembly's `i32` choice as an independent backend layout decision.

#### Scenario: Inspect the WebAssembly scalar plan

- **WHEN** a developer opens a branching program in the WebAssembly lab
- **THEN** the lab shows the `wasm32-unknown-unknown` target and planned `I32` and `Bool` entries beside the emitted WAT that realizes them
