## ADDED Requirements

### Requirement: Navigation stops only at the intrinsic boundary

Definition navigation SHALL follow public numeric, service, Effect, layout, and storage wrappers to
their canonical Silk source. A direct `Intrinsic` operation MAY have no source location, but its
identity and presentation MUST remain queryable. Navigation MUST NOT stop at a wrapper merely
because its body calls an intrinsic.

#### Scenario: Navigate through a public wrapper

- **WHEN** definition is requested on a generic integer addition call
- **THEN** navigation opens the standard-library declaration rather than reporting a source-less scalar intrinsic

#### Scenario: Query a direct intrinsic

- **WHEN** definition is requested on `Intrinsic.i32Add`
- **THEN** no source location is invented and the intrinsic semantic identity remains available
