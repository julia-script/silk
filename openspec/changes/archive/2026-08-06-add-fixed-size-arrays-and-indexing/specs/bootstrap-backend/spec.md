## ADDED Requirements

### Requirement: Native emission realizes compiler-planned arrays

Native emission SHALL realize logical array locals, parameters, results, construction, indexing,
projection, moves, and drops from the selected repeated-element layout and lane paths. Dynamic index
checks SHALL trap before selection. Emission MUST NOT recalculate stride, total size, field order,
element order, or calling shape from backend types.

#### Scenario: Execute a native dynamic index

- **WHEN** a native program indexes an array in bounds
- **THEN** its executable returns the selected value using the compiler-selected element path

### Requirement: WebAssembly emission realizes the same array plan

Direct WebAssembly emission SHALL use the same canonical lane sequence for array parameters, results,
locals, construction, and projection, including multi-value and zero-lane internal contracts. Bounds
checks SHALL have the same success and trap behavior as evaluation and native execution.

#### Scenario: Emit a zero-length internal result

- **WHEN** a WebAssembly function returns `Array<I32, 0>` internally
- **THEN** emission preserves the logical call with zero runtime results and no engine-owned array layout

### Requirement: Array backend parity is exact

For every supported target, valid indexed-array programs SHALL agree with evaluation and invalid or
incompatible plans SHALL be rejected before artifact construction. Equivalent emissions SHALL retain
stable symbols and deterministic text and binary output.

#### Scenario: Compare three engines

- **WHEN** one program constructs, passes, indexes, and projects an array of structs
- **THEN** native execution, WebAssembly execution, and MIR evaluation produce the same scalar result
