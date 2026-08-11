## ADDED Requirements

### Requirement: Public abstractions wrap the minimum intrinsic surface

The standard library SHALL ship navigable Silk source for every reusable abstraction removed from
the compiler-known catalog, including numeric interfaces and actor functions, service contracts
and implementations, layout validation, Effect wrappers, and safe storage operations. A public
standard-library declaration MUST NOT receive special semantics from its name; any required
primitive call SHALL be explicit in its source body through `Intrinsic`.

#### Scenario: Navigate from a numeric wrapper

- **WHEN** tooling selects the public generic integer addition function
- **THEN** it navigates to canonical Silk source whose implementation selects a concrete intrinsic through conformance

#### Scenario: Copy a standard-library implementation

- **WHEN** equivalent source declarations are copied under different valid names
- **THEN** they retain equivalent behavior without compiler registration
