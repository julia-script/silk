## MODIFIED Requirements

### Requirement: The native provider reads through unsafe OS primitives

Canonical standard-library source SHALL define `OsHostInput` as an ordinary provider reading the
process command line, environment block, and working directory through unsafe `Intrinsic` operations
returning `bool` and writing complete length, low-level reason, and native code to explicit initialized
scalar outputs. Success SHALL report the value's complete byte length and copy the prefix that fits,
so an undersized buffer is completed by one exactly sized second pass. A `false` result with the
not-found reason SHALL become ordinary absence through the nominal `Option` declaration; any other
`false` result SHALL become `HostInputError`. No compiler phase MAY construct Option or recognize the
`HostInput`, `OsHostInput`, or operation spellings to select special behavior.

#### Scenario: Complete a value longer than the provider buffer

- **WHEN** a value is longer than the buffer the provider first offered
- **THEN** the provider learns its complete length and returns the complete value

#### Scenario: Reject the native lookups on direct WebAssembly

- **WHEN** a reachable native host-input lookup is compiled for a direct WebAssembly target
- **THEN** target availability rejects it rather than inventing a process-input import

#### Scenario: Link only the reachable runtime symbols

- **WHEN** a native program reads host input and touches no filesystem
- **THEN** the artifact links the host-input runtime symbols and no filesystem runtime symbol
