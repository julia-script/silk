## ADDED Requirements

### Requirement: Interfaces and services share one static conformance model

Interface and service declarations SHALL use the same implicit `Self`, operation contract, implementation mapping, witness identity, static call resolution, bound specialization, visibility, completeness, overlap, and termination rules. A service SHALL receive no different operation behavior after passing dependency-eligibility validation.

#### Scenario: Implement a service operation inline

- **WHEN** a provider supplies an inline operation body satisfying the service contract
- **THEN** conformance validation and static operation selection use the same path as an interface implementation

#### Scenario: Mix inline and mapped operations

- **WHEN** one conformance implements one operation inline and maps another to a provider-local function
- **THEN** the conformance is complete when both resolved contracts are satisfied

### Requirement: Service privilege is limited to dependency eligibility

Only declarations satisfying the service eligibility rule MAY appear as Effect dependencies. After that check, the compiler SHALL NOT create a duplicate provider identity, service-specific witness, or name-selected dispatch rule.

#### Scenario: Use an ordinary interface outside a requirement row

- **WHEN** an interface is not dependency-eligible
- **THEN** it remains fully usable for static bounds and conformances but is rejected only when source attempts to place it in an Effect requirement
