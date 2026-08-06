## ADDED Requirements

### Requirement: Native emission realizes compiler-planned aggregates

The native backend SHALL lower canonical nominal MIR values, construction, projection, whole moves,
drops, parameters, and results according to the snapshot's physical layout entries and aggregate
calling shapes. Internal function signatures and calls SHALL use the compiler-selected flattened
scalar lanes in canonical field-path order. Emission MUST NOT recalculate field order, offsets,
padding, or aggregate ABI from LLVM types.

#### Scenario: Emit a native factory and projection

- **WHEN** an internal factory returns a nested struct and its caller projects a scalar field
- **THEN** emitted LLVM IR realizes the selected lane order and field identity and the executable returns the projected value

#### Scenario: Emit an empty marker

- **WHEN** an internal call passes or returns an empty struct
- **THEN** native emission preserves the logical nominal contract while emitting zero runtime lanes for that value

### Requirement: WebAssembly emission realizes compiler-planned aggregates

The direct WebAssembly backend SHALL lower the same nominal MIR operations and selected aggregate
calling shapes to WebAssembly scalar parameters, results, and locals without using engine-owned GC
struct layout. It SHALL flatten lanes by the compiler-provided canonical field paths and MUST NOT
derive a second layout or ABI from WebAssembly value types.

#### Scenario: Emit a WebAssembly aggregate call

- **WHEN** a WebAssembly snapshot contains an internal function passing and returning a nested struct
- **THEN** WAT and binary emission use the selected ordered scalar lanes and execution returns the same projected scalar as evaluation

#### Scenario: Avoid an engine-owned struct layout

- **WHEN** WebAssembly emits a nominal Silk value
- **THEN** no WebAssembly GC struct definition is treated as the authority for Silk field offsets or calling shape

### Requirement: Aggregate backend parity is exact

For every supported target, valid aggregate programs SHALL agree with MIR evaluation on the final
result and invalid or incompatible aggregate plans SHALL be rejected before artifact construction.
Equivalent emissions SHALL preserve canonical symbols, declaration order, debug provenance, and
deterministic text and binary output.

#### Scenario: Compare native, WebAssembly, and evaluation

- **WHEN** one program constructs, moves, returns, and projects nested structs
- **THEN** native execution, WebAssembly execution, and evaluation produce the same scalar result

#### Scenario: Reject a mismatched aggregate plan

- **WHEN** a backend receives a nominal MIR function whose plan lacks or contradicts its calling shape
- **THEN** emission returns a typed incompatibility outcome without creating a partial artifact
