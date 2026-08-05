## ADDED Requirements

### Requirement: Inspect emitted LLVM IR beside MIR

The docs site SHALL expose a direct-link LLVM IR lab presenting, for an edited program: the
emitted IR text, the symbol table, and the lowered MIR blocks with per-operation provenance —
so emitted code is readable beside the MIR it lowered from. The lab SHALL keep its state in
browser memory only.

#### Scenario: Inspect emitted IR

- **WHEN** a developer edits a program with nested calls
- **THEN** the lab shows the IR text containing `silk_main`, the symbol table, and the lowered MIR blocks from the same snapshot

#### Scenario: Toggle debug metadata

- **WHEN** a developer switches the lab to a debug emission
- **THEN** the IR text shows the compile unit and located instructions
