## MODIFIED Requirements

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

### Requirement: Every phase reports its work

Each driver run SHALL produce one report with an entry per executed phase, including the canonical
target-layout phase between instance discovery and MIR lowering: elapsed time, input and output
counts, diagnostic counts, and the engine-heap memory total observed after the phase — the
bootstrap approximation of allocator-backed totals until self-hosting owns real allocators.
Reports are observability data, not artifacts, and are exempt from byte-identity.

#### Scenario: Report the full pipeline

- **WHEN** the driver compiles any valid request for a supported target
- **THEN** the report lists every executed phase in order, including target layout between instances and MIR, with elapsed time, input and output counts, diagnostic counts, and memory totals
