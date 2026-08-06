## ADDED Requirements

### Requirement: Backends privately realize verified match dispatch

Native LLVM and direct WebAssembly emission SHALL consume the verified logical match region and the
compiler-owned union layout to select the active member, project its complete payload, evaluate
guards, realize pattern bindings, and join one result. A backend MAY introduce target-private
blocks, switches, comparisons, nesting, or branches, but MUST NOT change canonical case meaning,
source decision order, ownership, cleanup, or the compiler-owned DAG.

#### Scenario: Emit one exhaustive match through both backends

- **WHEN** a program consumes and destructures a two-member union with one guarded arm
- **THEN** native and WebAssembly execution select the same arm, payload, cleanup, and result as evaluation

#### Scenario: Keep backend control private

- **WHEN** LLVM uses cyclic or block control and WebAssembly uses structured nesting for the same match
- **THEN** neither representation leaks labels, block identities, branch depths, or reconstructed edges into MIR or facade relationships

### Requirement: Backend match artifacts are deterministic

Equivalent verified matches SHALL preserve canonical symbols, source provenance, LLVM IR and
bitcode, WAT, and WebAssembly bytes across fresh processes. Invalid match or cleanup metadata SHALL
be rejected before partial artifact construction.

#### Scenario: Repeat match emission

- **WHEN** one nested exhaustive match is emitted repeatedly for each supported target
- **THEN** its target-specific text, binary bytes, symbols, and match provenance are identical
