## ADDED Requirements

### Requirement: Generic parameters retain bound conjunctions

A type parameter MAY declare an unordered conjunction of interface or service applications with `+`. Every conjunct SHALL be preserved as an independent static proof obligation. The conjunction SHALL NOT create a runtime value, witness bundle, Effect requirement, or general intersection type.

#### Scenario: Call operations from two bounds

- **WHEN** a generic parameter is bounded by `First + Second` and its body calls one qualified operation from each contract
- **THEN** both calls resolve through the parameter's declared static evidence

#### Scenario: Specialize a conjunction

- **WHEN** a concrete generic call supplies a provider with one coherent conformance for every bound conjunct
- **THEN** specialization substitutes all selected witnesses into finite monomorphic code

#### Scenario: Reject a missing conjunct

- **WHEN** a concrete provider satisfies only some conjuncts
- **THEN** the generic application reports the complete missing provider-contract goal before lowering

#### Scenario: Reject a duplicate conjunct

- **WHEN** one bound repeats the same normalized contract application
- **THEN** the compiler reports the later duplicate regardless of source ordering

### Requirement: Static operations share one specialization path

Qualified operations selected from interface bounds, service bounds, and concrete conformances SHALL use the same implicit-`Self` substitution and canonical witness selection. Services SHALL NOT introduce a separate generic call or specialization identity.

#### Scenario: Specialize a service bound

- **WHEN** a generic function calls a qualified operation under a service bound and receives a concrete conforming provider
- **THEN** the call specializes to the provider's ordinary static witness with no runtime dependency lookup
