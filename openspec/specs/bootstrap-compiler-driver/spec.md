# bootstrap-compiler-driver Specification

## Purpose
The end-to-end compiler driver: one orchestration path from a compilation request to a running
native executable at the requested destination, with the differential harness and byte-identical
determinism gates enforced continuously and per-phase reporting as the observability substrate
issue 09 builds on.
## Requirements
### Requirement: The driver compiles a request to a native executable

The driver SHALL orchestrate the complete path itself — closure loading, header collection,
elaboration, ownership, instance discovery, canonical target layout, MIR lowering, backend
emission, object emission, shim compilation, and linking — writing the executable to the requested
durable destination under a fixed optimization profile. Because this operation produces a native
executable, the driver SHALL select an explicitly requested native bootstrap target or the
supported current host before layout and SHALL pass the resulting target-aware MIR program through
later phases without a second layout value. The driver, not any
external harness, SHALL invoke the backend and linker services. An unavailable entry, unsupported
target, inconsistent layout, or toolchain failure SHALL surface as a closed outcome naming the
failing stage with its provenance, never as a thrown error.

#### Scenario: Compile and run the nested program

- **WHEN** the driver compiles the nested-call corpus program to a destination with the release profile on a supported host
- **THEN** an executable exists there and running it exits with the interpreter's result

#### Scenario: Surface an entry failure as a closed outcome

- **WHEN** the request's root module has no valid entry
- **THEN** the driver returns a no-entry outcome carrying the discovery reason and the phase report, without invoking the toolchain

#### Scenario: Name the failing stage

- **WHEN** the pinned toolchain path is invalid
- **THEN** the driver returns a failed outcome naming the object stage with the full command provenance

#### Scenario: Stop on an unsupported target

- **WHEN** the request selects a target outside the bootstrap matrix
- **THEN** the driver returns a target-stage failure before MIR lowering, backend emission, or toolchain invocation

#### Scenario: Keep WebAssembly out of native linking

- **WHEN** the native-executable driver receives `wasm32-unknown-unknown`
- **THEN** it returns a target-kind failure before object emission or linking while leaving WebAssembly emission available through its compatible backend path

### Requirement: The differential harness is a continuous check

The fixed corpus SHALL run through both the interpreter and native compilation-and-execution as
part of the test suite CI enforces. For completing programs the native exit status SHALL equal
the interpreter's `I32` result; trap-blocked programs SHALL terminate abnormally natively;
recursion-blocked programs SHALL still compile. A disagreement SHALL fail the build naming the
program and both sides' outcomes.

#### Scenario: Agree on completing programs

- **WHEN** every completing corpus program is interpreted and natively executed
- **THEN** each native exit status equals the interpreted result

#### Scenario: Fail on divergence

- **WHEN** a corpus program's native outcome differs from the interpreter's
- **THEN** the harness fails naming the program and the diverging outcomes

### Requirement: Determinism gates are enforced continuously

The test suite CI runs SHALL enforce the pinned gates: identical compiler, source snapshot,
target, profile, and toolchain inputs produce byte-identical syntax, HIR, and MIR textual
encodings and LLVM bitcode.

#### Scenario: Gate the four encodings

- **WHEN** the determinism suite runs
- **THEN** syntax, HIR, and MIR encodings and the bitcode digest are all byte-compared against committed goldens and repeated fresh runs

### Requirement: Every phase reports its work

Each driver run SHALL produce one report with an entry per executed phase, including the canonical
target-layout phase between instance discovery and MIR lowering: elapsed time, input and output
counts, diagnostic counts, and the engine-heap memory total observed after the phase — the
bootstrap approximation of allocator-backed totals until self-hosting owns real allocators.
Reports are observability data, not artifacts, and are exempt from byte-identity.

#### Scenario: Report the full pipeline

- **WHEN** the driver compiles any valid request for a supported target
- **THEN** the report lists every executed phase in order, including target layout between instances and MIR, with elapsed time, input and output counts, diagnostic counts, and memory totals


### Requirement: The driver compiles internal aggregate call chains

The compiler driver SHALL carry nominal struct values through declaration analysis, HIR, ownership,
runtime discovery, target layout and calling-shape selection, MIR lowering, backend emission, and
native linking while preserving the fixed scalar host entry boundary. Struct construction and
projection failures SHALL remain ordinary deterministic diagnostics and SHALL prevent only invalid
downstream work.

#### Scenario: Compile through a public factory

- **WHEN** a root module calls another module's public factory, passes the returned struct through an internal function, and returns a projected `I32`
- **THEN** the driver produces a native executable whose exit result matches MIR evaluation

#### Scenario: Refuse external raw construction

- **WHEN** a root module attempts a raw literal for another module's struct
- **THEN** the driver reports the defining-module diagnostic and performs no MIR or backend work for the invalid program

### Requirement: Aggregate differential and determinism gates remain continuous

The driver corpus SHALL include valid, invalid, nested, empty, reordered, cross-module, moved,
projected, and cleanup-bearing aggregate programs. Native execution, WebAssembly execution, and MIR
evaluation SHALL agree where applicable, and repeated fresh-process compilation SHALL preserve
diagnostics, HIR, layouts, MIR, symbols, IR, WAT, and bitcode exactly.

#### Scenario: Run the aggregate parity corpus

- **WHEN** continuous checks execute the aggregate corpus on supported targets
- **THEN** every valid program agrees across evaluation and available backends and every invalid program preserves its expected phase-owned diagnostics

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

### Requirement: Mutable-loop programs retain three-engine parity

The driver corpus SHALL cover immutable-write rejection, scalar and place assignment, Copy and
move-only replacement, zero and multiple iterations, nested loops, `break`, `continue`, `return`,
checked index traps, loop-header ownership failure, and cleanup. Supported programs SHALL agree
across MIR evaluation, native execution, and WebAssembly execution; invalid programs SHALL retain
their phase-owned outcomes before artifact construction.

#### Scenario: Run an array algorithm

- **WHEN** the corpus mutates and scans a fixed-size array through a structured loop
- **THEN** every available engine produces the same result and traceable control decisions

### Requirement: Control DAG artifacts are deterministic

Repeated compilation in fresh processes SHALL preserve semantic loop facts, HIR regions, ownership
fixed points, cleanup plans, MIR DAG nodes and topological encoding, evaluation traces, symbols, LLVM
IR and bitcode, WAT, and WebAssembly bytes exactly for equivalent inputs.

#### Scenario: Repeat nested-loop compilation

- **WHEN** one nested-loop program is compiled repeatedly for supported targets
- **THEN** every compiler-owned artifact is identical and backend-local control conversion is deterministic
