## ADDED Requirements

### Requirement: Interfaces and services share one static conformance model

Interface and service declarations SHALL use the same implicit `Self`, operation contract, implementation mapping, witness identity, static call resolution, bound specialization, visibility, completeness, overlap, and termination rules. A service SHALL receive no different operation behavior after passing dependency-eligibility validation.

#### Scenario: Implement a service operation inline

- **WHEN** a provider supplies an inline operation body satisfying the service contract
- **THEN** conformance validation and static operation selection use the same path as an interface implementation

#### Scenario: Mix inline and mapped operations

- **WHEN** one conformance implements one operation inline and maps another to a provider-local function
- **THEN** the conformance is complete when both resolved contracts are satisfied

#### Scenario: Bind implicit Self once

- **WHEN** a contract operation mentions `Self` and an `impl Contract for Provider` is indexed
- **THEN** the operation contract substitutes `Self = Provider` without adding Provider to the contract's written type arguments

#### Scenario: Reject an incompatible inline operation

- **WHEN** an inline operation changes an operand mode, function kind, result, failure type, or requirement beyond the substituted contract
- **THEN** conformance validation rejects it before publishing a witness

### Requirement: Service privilege is limited to dependency eligibility

Only declarations satisfying the service eligibility rule MAY appear as Effect dependencies. After that check, the compiler SHALL NOT create a duplicate provider identity, service-specific witness, or name-selected dispatch rule.

#### Scenario: Use an ordinary interface outside a requirement row

- **WHEN** an interface is not dependency-eligible
- **THEN** it remains fully usable for static bounds and conformances but is rejected only when source attempts to place it in an Effect requirement

### Requirement: Conformances are coherent provider-local facts

A source conformance SHALL be declared in the module defining its provider's outer nominal type. Its visibility SHALL be determined by the visibility of its contract and provider endpoints. Potentially overlapping heads, non-terminating conditional requirements, incomplete witnesses, and statically unprovable concrete uses SHALL be rejected before lowering.

#### Scenario: Reject a foreign provider conformance

- **WHEN** a module declares an implementation for a provider nominal defined by another module
- **THEN** the compiler rejects the implementation as non-local even when the contract is locally defined

#### Scenario: Reuse one endpoint-visible conformance

- **WHEN** a caller can name both a public provider and its public contract
- **THEN** its provider-local conformance is available without importing or activating the implementation separately

#### Scenario: Reject potentially overlapping generic heads

- **WHEN** two conformance heads can unify under some substitution
- **THEN** the later declaration is rejected without using its bounds to choose a winner

#### Scenario: Reject a non-descending conditional proof

- **WHEN** a conditional conformance requires evidence for an equal, unrelated, growing, or occurrence-multiplying provider
- **THEN** declaration indexing rejects it before concrete proof search
