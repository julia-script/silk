## ADDED Requirements

### Requirement: Canonical source exports separate portable clock actors

The deterministic standard-library manifest SHALL export `silk/system_clock` with `Instant` and
`SystemClock`, and `silk/monotonic_clock` with `MonotonicClock` using the shared `Instant` type.
Both modules SHALL contain their public service contracts, ordinary module-level service wrappers,
complete public documentation, and no target-provider dependency or compiler-recognized
declaration. Their service requirements SHALL be exclusive so ordinary scripted providers can
advance a timeline or record calls while preserving lexical replacement and deterministic behavior.

#### Scenario: Navigate portable clocks

- **WHEN** tooling resolves `Instant`, either service, or any public clock wrapper
- **THEN** go-to-definition opens canonical shipped Silk source rather than a generated signature
  or compiler catalog entry

#### Scenario: Use a pure clock provider on direct Wasm

- **WHEN** a direct-Wasm application implements and provides both clock services in ordinary source
- **THEN** it uses the complete portable API without importing either OS-provider module or a host
  clock ABI

### Requirement: Canonical source exports separate native clock providers

The manifest SHALL export `silk/os_system_clock` and `silk/os_monotonic_clock` as separate native
provider actors. Each module SHALL define one stateless provider, an infallible constructor, the
ordinary source operations needed for its matching service conformance, and documented fatal and
target limitations. Portable service signatures MUST NOT mention either provider, a platform clock
identifier, runtime symbol, target selector, or native status protocol.

#### Scenario: Construct providers without reading time

- **WHEN** an application constructs either OS clock provider and does not invoke a clock operation
- **THEN** construction completes without consulting the host and contributes no reachable clock
  runtime symbol

#### Scenario: Keep provider modules independent

- **WHEN** an application imports and provides only `OsSystemClock`
- **THEN** its source closure does not require `OsMonotonicClock` and its executable closure gains
  no monotonic wait support

### Requirement: Clock documentation is generated and verified from source

All four clock modules SHALL participate in generated standard-library embedding, reference
generation, documentation policy checks, and doctest verification. Documentation SHALL teach the
units, epoch or unspecified origin, canonical negative-time representation, non-decreasing rather
than strictly increasing behavior, same-provider mark limitation, blocking wait semantics, fatal
host boundary, explicit provision, and direct-Wasm exclusion where each applies.

#### Scenario: Generate clock reference pages

- **WHEN** standard-library source and documentation are regenerated
- **THEN** the index lists all four modules and their pages retain the documented service
  operations, conceptual Instant components, public constructor and accessors, provider
  constructors, examples, and portability limitations

#### Scenario: Reject stale clock documentation

- **WHEN** a clock signature or documented example changes without regenerating its reference
- **THEN** normal repository verification fails with the authored or generated source location
