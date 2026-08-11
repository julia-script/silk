## ADDED Requirements

### Requirement: Backends preserve portable FileSystem service boundaries

Backends SHALL lower FileSystem requirements and provided implementations through the ordinary
service model without adding FileSystem-shaped MIR operations or selecting one platform provider.
Native execution MAY reach a private platform adapter through the selected native provider. Direct
WebAssembly MAY expose versioned host imports for a hosted provider, but those imports MUST use
complete operation requests/results and MUST NOT become the public Silk FileSystem contract.

#### Scenario: Compile an in-memory filesystem without host imports

- **WHEN** a closed program supplies an ordinary Silk in-memory FileSystem implementation
- **THEN** direct WebAssembly lowers it without adding native-filesystem host imports

#### Scenario: Compile a hosted WebAssembly provider

- **WHEN** a direct-Wasm entry intentionally leaves the hosted FileSystem adapter at its application boundary
- **THEN** the artifact records the versioned imports needed by that provider without exposing Unix handles or paths in Silk source

### Requirement: Filesystem provider observations remain deterministic

Equivalent MIR, target, provider contract, and codegen request SHALL produce deterministic native
and WebAssembly artifacts. Provider enumeration order and native error codes MUST NOT affect
portable result ordering, semantic FileError reasons, or symbol identities.

#### Scenario: Emit repeated directory-listing programs

- **WHEN** the same portable filesystem program is emitted in fresh processes
- **THEN** backend artifacts are byte-identical and runtime entry ordering remains provider-independent
