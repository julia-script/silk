## ADDED Requirements

### Requirement: The declaration index stores one canonical contract fact

Interface and service headers SHALL lower to one canonical contract fact containing implicit `Self`, ordered operation contracts, explicit generic parameters, visibility, and dependency eligibility. Any interface-only or service-only collection exposed by inspectors SHALL be a projection of those facts rather than an independent semantic identity.

#### Scenario: Index an interface and a service

- **WHEN** one module declares an interface and a service with equivalent operation shapes
- **THEN** both declarations have the same contract fact shape and differ only in declaration identity and dependency eligibility

#### Scenario: Preserve ordered operations

- **WHEN** a contract declares multiple operations and a conformance mixes inline bodies with mapped functions
- **THEN** the index resolves one witness table in contract operation order

### Requirement: The declaration index stores one canonical conformance identity

A conformance identity SHALL consist of one contract application, one provider type, normalized conditional requirements, and one resolved witness table. Provider matching, proof search, static calls, and service provision SHALL reuse that identity rather than synthesizing service-specific or duplicated-provider witnesses.

#### Scenario: Reuse one service conformance for provision and bounds

- **WHEN** one provider conforms to a service used both as a generic bound and as an Effect dependency
- **THEN** bound proof and provider selection reference the same indexed conformance identity

#### Scenario: Withhold an invalid witness

- **WHEN** completeness, signature, locality, overlap, termination, or visibility validation rejects a conformance
- **THEN** the index publishes no witness for static calls, proof search, or provider selection
