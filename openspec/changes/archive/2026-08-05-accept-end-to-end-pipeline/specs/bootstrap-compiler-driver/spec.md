## Purpose

The end-to-end compiler driver: one orchestration path from a compilation request to a running
native executable at the requested destination, with the differential harness and byte-identical
determinism gates enforced continuously and per-phase reporting as the observability substrate
issue 09 builds on.

## ADDED Requirements

### Requirement: The driver compiles a request to a native executable

The driver SHALL orchestrate the complete path itself — closure loading, header collection,
elaboration, ownership, instance discovery, MIR lowering, backend emission, object emission,
shim compilation, and linking — writing the executable to the requested durable destination
under a fixed optimization profile and the host-derived target layout. The driver, not any
external harness, SHALL invoke the backend and linker services. An unavailable entry or a
toolchain failure SHALL surface as a closed outcome naming the failing stage with its
provenance, never as a thrown error.

#### Scenario: Compile and run the nested program

- **WHEN** the driver compiles the nested-call corpus program to a destination with the release profile
- **THEN** an executable exists there and running it exits with the interpreter's result

#### Scenario: Surface an entry failure as a closed outcome

- **WHEN** the request's root module has no valid entry
- **THEN** the driver returns a no-entry outcome carrying the discovery reason and the phase report, without invoking the toolchain

#### Scenario: Name the failing stage

- **WHEN** the pinned toolchain path is invalid
- **THEN** the driver returns a failed outcome naming the object stage with the full command provenance

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

Each driver run SHALL produce one report with an entry per phase: elapsed time, input and output
counts, diagnostic counts, and the engine-heap memory total observed after the phase — the
bootstrap approximation of allocator-backed totals until self-hosting owns real allocators.
Reports are observability data, not artifacts, and are exempt from byte-identity.

#### Scenario: Report the full pipeline

- **WHEN** the driver compiles any request
- **THEN** the report lists every executed phase in order with elapsed time, input and output counts, diagnostic counts, and memory totals
