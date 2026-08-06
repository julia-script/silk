## REMOVED Requirements

### Requirement: MIR is a backend-neutral CFG over logical types

**Reason**: A general cyclic basic-block graph discards the structured loop information required by
structured backends and forces them to reconstruct compiler-known control regions.

**Migration**: Construct and consume the canonical structured control DAG defined below. LLVM-style
back-edges and WebAssembly nesting are backend-local derivations and are no longer valid MIR input.

## ADDED Requirements

### Requirement: MIR is a backend-neutral structured control DAG over logical types

A MIR module SHALL represent each function as a structured control DAG over logical Silk types and
typed locals. Ordered operation blocks, conditional regions, loop regions, cleanup regions, and
terminal outcomes SHALL retain canonical identities and provenance. Child and continuation edges
MUST be acyclic and deterministically ordered. Repetition SHALL exist only as the semantics of an
explicit loop region whose condition and body are themselves DAG regions; arbitrary block back-edges
MUST fail verification.

MIR SHALL carry the compiler-selected target and layout plan but MUST NOT contain LLVM or WebAssembly
types, instructions, labels, nesting depths, or backend-owned physical representations. A backend
SHALL receive the preserved DAG and convert it into its own control form without recovering source
structure from flattened control flow.

#### Scenario: Model a straight-line function

- **WHEN** a hand-built function returns a called constant
- **THEN** its entry region contains ordered literal and call operations ending in a return outcome over logical `I32`

#### Scenario: Model structured repetition without a cycle

- **WHEN** MIR represents a `while` loop with a conditional `continue` and `break`
- **THEN** one loop region owns acyclic condition and body regions whose terminal outcomes name repeat or exit ports without a graph back-edge

#### Scenario: Reject an arbitrary cycle

- **WHEN** a hand-built MIR region directly or indirectly lists itself as a child or continuation
- **THEN** verification reports the cycle deterministically before evaluation or emission

### Requirement: MIR writes replace typed places explicitly

MIR SHALL represent assignment as one checked `WritePlace` carrying the root local, ordered field and
index selectors, dynamic index locals and canonical lengths, exact destination and source types,
replacement cleanup, and provenance. Place checks and right-hand evaluation SHALL precede the commit,
and the verifier SHALL reject inconsistent mutability, selectors, types, layouts, calling shapes, or
cleanup modes.

#### Scenario: Lower an array element replacement

- **WHEN** HIR assigns a complete value to `values[index]`
- **THEN** MIR checks the index and evaluates the source before one verified write commits

### Requirement: MIR loop outcomes preserve lexical cleanup

Each loop region SHALL expose canonical repeat and exit outcomes. Lowering SHALL map body fallthrough
and `continue` to repeat, `break` to exit, and `return` to the function outcome through the exact
cleanup regions selected by ownership. Cleanup sharing MAY make the representation a DAG rather than
a tree, but every owner SHALL be released at most once on any execution path.

#### Scenario: Lower continue through cleanup

- **WHEN** an iteration-local owner is live at `continue`
- **THEN** the transfer traverses its cleanup region before reaching the loop repeat outcome

### Requirement: Control DAG verification and encoding are deterministic

Verification SHALL reject missing or duplicate region identities, cyclic child/continuation edges,
invalid lexical transfer targets, incompatible loop-header locals, unreachable required outcomes,
and operation/type/layout disagreements as ordered data. Text encoding SHALL traverse regions in one
canonical topological order and encode structured children, outcomes, selectors, cleanup, and
provenance identically across fresh processes.

#### Scenario: Repeat loop encoding

- **WHEN** one nested mutable-loop program is lowered repeatedly in fresh processes
- **THEN** its region identities, topological order, operations, outcomes, and textual bytes are identical
