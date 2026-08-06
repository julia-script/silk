## ADDED Requirements

### Requirement: The driver compiles fixed-array programs end to end

The driver SHALL carry array syntax, types, ownership, reachability, layout, calling shapes, MIR,
evaluation, and emission through the existing phase report while retaining the scalar host entry.
Array failures SHALL remain phase-owned closed outcomes rather than thrown host errors.

#### Scenario: Compile an array-of-structs program

- **WHEN** a valid program constructs an array of structs and returns one indexed scalar field
- **THEN** the driver produces a native executable whose result agrees with MIR evaluation

### Requirement: Array differential and determinism gates remain continuous

The driver corpus SHALL cover inferred, contextual, empty, nested, struct-element, moved, indexed,
out-of-bounds, mismatched, and unavailable-layout arrays. Repeated compilation SHALL preserve
diagnostics, HIR, layouts, MIR, symbols, IR, WAT, and binary output exactly.

#### Scenario: Run the fixed-array corpus

- **WHEN** continuous checks execute the corpus on supported targets
- **THEN** valid programs agree across available engines and invalid programs retain their expected phase-owned outcomes
