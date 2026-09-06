## MODIFIED Requirements

### Requirement: Compilation selects one canonical bootstrap target

Each compilation SHALL select exactly one canonical target within one normalized compilation profile before runtime instance discovery. Compiler-core selection SHALL NOT inspect ambient host state. The
required native-host targets SHALL be `aarch64-apple-darwin`, `x86_64-unknown-linux-gnu`, and
`aarch64-unknown-linux-gnu`; the supported non-host emission target SHALL be
`wasm32-unknown-unknown`. An explicit request SHALL resolve only to one of these four machine descriptions. A
request without an explicit target SHALL resolve a host explicitly supplied at the application edge only when it matches one
of the three required hosts. Unsupported or inconsistent requests SHALL produce a closed typed
compiler outcome before MIR lowering or backend emission.

#### Scenario: Select an explicit Linux target

- **WHEN** a compilation requests `x86_64-unknown-linux-gnu`
- **THEN** the compiler selects that exact canonical profile independently of the host running the compiler

#### Scenario: Default to a supported host

- **WHEN** no target is requested and the compiler runs on `aarch64-apple-darwin`
- **THEN** the compiler selects the canonical `aarch64-apple-darwin` profile

#### Scenario: Select WebAssembly explicitly

- **WHEN** a compilation requests `wasm32-unknown-unknown`
- **THEN** the compiler selects the non-host WebAssembly profile and never treats it as the native host default

#### Scenario: Reject an unsupported target

- **WHEN** a compilation requests a target outside the four supported profiles
- **THEN** compilation returns a typed unsupported-target outcome before layout, MIR, or backend work begins
