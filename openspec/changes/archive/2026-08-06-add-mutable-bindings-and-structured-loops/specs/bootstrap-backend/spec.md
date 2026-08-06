## ADDED Requirements

### Requirement: Backends lower the compiler-owned control DAG

Every backend SHALL consume the verified structured control DAG as its sole control-flow input and
convert it to the target's required form. A backend MUST NOT infer loops, conditionals, lexical exits,
or cleanup regions from a flattened cyclic graph, and MUST preserve the DAG's operation order,
outcomes, provenance, and cleanup behavior.

#### Scenario: Lower one DAG through two backends

- **WHEN** native and WebAssembly emission receive the same nested-loop MIR structure for their selected targets
- **THEN** both consume the same canonical regions and produce behavior matching evaluation

### Requirement: Native emission linearizes structured loops

Native LLVM emission SHALL deterministically linearize loop and conditional regions into backend-local
basic blocks, branches, and loop back-edges. The resulting LLVM CFG MAY be cyclic, but no derived block
or edge SHALL leak back into MIR or alter compiler-owned region identities.

#### Scenario: Emit a native while loop

- **WHEN** a loop repeats an indexed update until its condition becomes false
- **THEN** LLVM IR contains the required header, body, exit, and back-edge blocks and the executable matches evaluation

### Requirement: WebAssembly emission preserves structured loops

Direct WebAssembly emission SHALL map DAG loop, conditional, repeat, exit, and cleanup regions into
deterministic nested `block`, `loop`, `if`, and branch forms. It MUST NOT run a CFG-restructuring pass
or synthesize a dispatch loop for control already represented structurally.

#### Scenario: Emit nested WebAssembly loops

- **WHEN** MIR contains nested loops with inner `continue` and outer `break`
- **THEN** WAT retains deterministic nested structured control and execution matches evaluation and native code

### Requirement: Backends realize mutable place writes consistently

Native and WebAssembly emission SHALL realize checked root, field, and array-element replacement from
the compiler-planned layout and selector paths. Bounds failure SHALL trap before right-hand evaluation
or commit, and valid writes SHALL preserve complete logical values and exact replacement cleanup.

#### Scenario: Compare checked array mutation

- **WHEN** a valid program updates several array elements in a loop
- **THEN** native, WebAssembly, and evaluation return the same final projected value
