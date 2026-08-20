## ADDED Requirements

### Requirement: Compilation validates one matched toolchain set

Before analysis, emission, or execution consumes distribution artifacts, the compiler driver SHALL validate compatible identities and content digests for the compiler, standard-library catalog and sources, sealed intrinsic inventory, selected target providers, and required runtime support.

#### Scenario: Reject a mismatched catalog

- **WHEN** the supplied catalog identity or source digest does not match the compiler's declared toolchain contract
- **THEN** the driver reports an incompatible distribution before resolving program imports

#### Scenario: Admit a pay-for-use runtime subset

- **WHEN** a program reaches only a subset of target intrinsics
- **THEN** compatibility validation requires only the matched runtime support for that reachable inventory

### Requirement: Integrity failures belong to their owning boundary

The driver SHALL distinguish missing source, malformed or mismatched distribution, unsupported target inventory, unresolved entry contract, and operational execution failure as deterministic structured outcomes. It SHALL NOT reclassify these failures as source type errors or backend defects.

#### Scenario: Distinguish unsupported target from bad installation

- **WHEN** a valid matched toolchain lacks target support for one reachable intrinsic
- **THEN** the driver reports target unavailability, while a missing artifact promised by the same toolchain reports distribution corruption
