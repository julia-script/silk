## ADDED Requirements

### Requirement: The three random capabilities are canonical ordinary source

The standard library SHALL ship canonical documented ordinary Silk source for secure `Random`,
deterministic `InsecureRandom`, immutable `InsecureSeed`, and the native `OsRandom` provider. The
portable actors, service contracts, provider implementations, derived operations, seed policy, and
xoshiro algorithm SHALL receive no compiler privilege from module or declaration spelling. Only
the explicit OS provider SHALL invoke the sealed native random intrinsic.

#### Scenario: Navigate each public random actor

- **WHEN** tooling resolves a random service, provider, seed value, or derived operation
- **THEN** go-to-definition opens its canonical `.silk` declaration rather than generated or compiler-known behavior

#### Scenario: Copy a service implementation

- **WHEN** user source defines an equivalent provider under another legal name
- **THEN** it receives ordinary service conformance and lexical provision without intrinsic registration

### Requirement: Random module identities make security explicit

The manifest SHALL assign `silk/random` exclusively to secure `Random` and
`silk/insecure_random` exclusively to non-cryptographic `InsecureRandom` and
`Xoshiro256StarStar`. The former deterministic `silk/random` surface SHALL have no compatibility
alias, forwarding module, deprecated declaration, or dual path. Generated embeddings,
documentation, navigation, resolution indexes, examples, and tests SHALL agree with the canonical
module identities.

#### Scenario: Resolve the secure module

- **WHEN** source imports `silk/random`
- **THEN** resolution exposes the secure service and derived secure operations without the seeded xoshiro provider

#### Scenario: Resolve the insecure module

- **WHEN** source imports `silk/insecure_random`
- **THEN** resolution exposes `InsecureRandom`, `Xoshiro256StarStar`, and the stable seeded operations under explicitly non-cryptographic documentation

### Requirement: Portable random actors do not depend on an OS provider

The portable `random`, `insecure_random`, and `insecure_seed` modules MUST NOT import an OS
provider, target selector, native runtime type, WASI interface, or browser API. Applications SHALL
select and provide implementations at their outer boundary. Importing portable modules on direct
WebAssembly SHALL remain valid when no reachable function calls the native intrinsic.

#### Scenario: Supply an ordinary secure provider

- **WHEN** an application provides a source-defined `Random` implementation
- **THEN** portable secure derived operations use it without loading `silk/os_random`

#### Scenario: Load portable random source on direct Wasm

- **WHEN** a direct-WebAssembly program imports only portable random modules
- **THEN** module closure contains no operating-system random import or native symbol

## REMOVED Requirements

### Requirement: Canonical source exports portable seeded random generation

**Reason**: The canonical `silk/random` identity now denotes secure randomness, and the seeded implementation moves to the explicitly non-cryptographic `silk/insecure_random` module.

**Migration**: Use the new secure `silk/random` service where unpredictable bytes are required, or import `silk/insecure_random` for the former deterministic xoshiro API.
