## MODIFIED Requirements

### Requirement: The native provider reads through one unsafe OS primitive

Canonical standard-library source SHALL define `OsStandardInput` as an ordinary provider that reads
the process standard-input descriptor through one unsafe `Intrinsic` operation returning `bool` and
writing transferred count, low-level reason, and native code to explicit initialized scalar outputs.
A successful zero-length transfer SHALL become `EndOfInput`; a successful positive count SHALL become
`Filled`; and `false` SHALL become `StreamReadError`. No compiler phase MAY construct Option or
recognize the `StandardInput`, `OsStandardInput`, `ReadOutcome`, or `read` spellings to select special
behavior.

#### Scenario: Read through the native implementation

- **WHEN** a provided native implementation receives one exclusive buffer
- **THEN** its source operation invokes one primitive read boundary and preserves the service's outcome and typed failure

#### Scenario: Reject the native read on direct WebAssembly

- **WHEN** a reachable native read is compiled for a direct WebAssembly target
- **THEN** target availability rejects it rather than inventing an input host import

#### Scenario: Link only the reachable runtime symbol

- **WHEN** a native program reads standard input and touches no filesystem
- **THEN** the artifact links the byte-input runtime symbol and no filesystem runtime symbol
