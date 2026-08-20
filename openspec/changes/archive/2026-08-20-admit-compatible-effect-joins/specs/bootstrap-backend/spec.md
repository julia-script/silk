## ADDED Requirements

### Requirement: Backends realize finite Effect composites without allocation

Native LLVM and direct WebAssembly SHALL realize a finite Effect composite as a statically planned
tag plus storage sufficient for its largest alternative. Construction SHALL initialize only the
selected member, execution SHALL dispatch only to its runner, and cleanup SHALL release only that
member. The representation SHALL require no source or private heap allocation, and equivalent
inputs SHALL emit deterministic artifacts.

#### Scenario: Execute the same selected member across engines

- **WHEN** a closed program constructs and runs one member of a finite compatible Effect join
- **THEN** native and WebAssembly agree with evaluation on its result, failure identity, and cleanup

#### Scenario: Inspect allocation-free lowering

- **WHEN** a finite Effect composite is emitted for either backend
- **THEN** its tag, maximum static storage, dispatch, and cleanup are present without an allocation request or universal Effect interpreter

#### Scenario: Emit joined Effects deterministically

- **WHEN** equivalent joined Effect programs are compiled repeatedly
- **THEN** native and WebAssembly artifacts preserve identical alternative ordering and bytes
