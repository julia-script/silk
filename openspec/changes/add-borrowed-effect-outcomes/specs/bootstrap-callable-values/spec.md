## ADDED Requirements

### Requirement: Quantified Effect operations use fresh invocation lifetimes

Expected-type-directed callable and interface compatibility SHALL compare a known signature with at most one outer finite lifetime binder using fresh rigid placeholders. Surrounding bound lifetimes SHALL remain distinct. An offered implementation MUST NOT strengthen required source validity or access, and placeholders MUST NOT escape. Lifetime arguments SHALL remain proof information rather than dispatch or specialization selectors. Nested universal callable types and unconstrained higher-ranked inference SHALL remain rejected.

#### Scenario: Invoke a fixed-item operation twice

- **WHEN** two invocations use an operation quantified over its receiver lifetime with a fixed externally borrowed item
- **THEN** each receiver lifetime is fresh and both results retain the fixed item's external lifetime

#### Scenario: Reject stronger offered validity

- **WHEN** an offered callback requires static validity where the expected callback accepts every invocation lifetime
- **THEN** compatibility rejects the selected callback without trying another implementation

### Requirement: Effect composition retains environment ownership

Effect construction, callable results, generic composition and provision SHALL preserve captured lifetime bounds and shared, exclusive or consuming access. Reusable Effects SHALL retain their capture loans between runs. Consuming Effects SHALL transfer or clean captures once. Provision SHALL retain the selected provider's loan after removing its service requirement.

#### Scenario: Retain an exclusive computation

- **WHEN** a stored reusable Effect retains an exclusive receiver borrow between runs
- **THEN** another receiver access is rejected until that Effect releases its capture

#### Scenario: Reject provider escape after provision

- **WHEN** a provided Effect outlives its borrowed provider despite having no remaining service requirement
- **THEN** the compiler rejects the environment escape
