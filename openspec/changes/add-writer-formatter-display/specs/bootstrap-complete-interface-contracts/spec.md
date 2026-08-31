## MODIFIED Requirements

### Requirement: Conformances are coherent provider-local facts

A source conformance whose provider has an outer nominal type SHALL be declared in the module
defining that nominal type. A source conformance whose provider is a scalar SHALL instead be
declared in the module defining the source interface or service it implements. No other module
SHALL declare either conformance, and other structural non-nominal providers SHALL remain
ineligible for source conformances. Conformance visibility SHALL be determined by the visibility of
its contract and provider endpoints. Potentially overlapping heads, non-terminating conditional
requirements, incomplete witnesses, and statically unprovable concrete uses SHALL be rejected
before lowering.

#### Scenario: Reject a foreign provider conformance

- **WHEN** a module declares an implementation for a provider nominal defined by another module
- **THEN** the compiler rejects the implementation as non-local even when the contract is locally defined

#### Scenario: Admit an interface-owned scalar conformance

- **WHEN** the module defining a source interface declares that interface for a concrete scalar
- **THEN** the compiler publishes one coherent conformance visible with its interface endpoint

#### Scenario: Reject a foreign scalar conformance

- **WHEN** a module other than the source contract's defining module declares that contract for a scalar
- **THEN** the compiler rejects the implementation as non-local rather than activating it through imports

#### Scenario: Reject a structural provider conformance

- **WHEN** source declares an interface implementation for a non-scalar type with no outer nominal owner
- **THEN** the compiler rejects the provider before witness validation

#### Scenario: Reuse one endpoint-visible conformance

- **WHEN** a caller can name both a public provider and its public contract
- **THEN** its coherently owned conformance is available without importing or activating the implementation separately

#### Scenario: Reject potentially overlapping generic heads

- **WHEN** two conformance heads can unify under some substitution
- **THEN** the later declaration is rejected without using its bounds to choose a winner

#### Scenario: Reject a non-descending conditional proof

- **WHEN** a conditional conformance requires evidence for an equal, unrelated, growing, or occurrence-multiplying provider
- **THEN** declaration indexing rejects it before concrete proof search

## ADDED Requirements

### Requirement: Scalar conformances admit source-authored inline witnesses

An interface-owned scalar conformance SHALL admit an inline ordinary or effect operation when its
complete substituted operands, success type, failure row, and requirement row satisfy the interface
contract. The inline declaration SHALL retain a canonical source identity through conformance
validation, reachability, instance discovery, specialization, and lowering. A scalar conformance
MAY continue to map an operation to a sealed `Intrinsic` target, but it MUST NOT map to an ordinary
source actor function because a scalar has no source-owned nominal actor. Witness admissibility MUST
NOT depend on the spelling or standard-library origin of the interface.

#### Scenario: Admit an effectful scalar witness

- **WHEN** an interface operation and its scalar inline implementation both return unit, fail with `WriterError`, and require exclusive `Writer` access
- **THEN** conformance validation publishes the inline declaration as a compatible source witness

#### Scenario: Explain a real scalar signature mismatch

- **WHEN** a scalar inline implementation strengthens an operand, failure, or requirement beyond its substituted interface contract
- **THEN** `SEM0083` identifies the first incompatible contract component instead of reporting a generic incompatibility

#### Scenario: Reject an ordinary scalar source mapping

- **WHEN** a scalar conformance maps an operation to a non-intrinsic source function instead of defining it inline
- **THEN** conformance validation reports that scalar source witnesses must be inline

#### Scenario: Copy the scalar interface pattern into user source

- **WHEN** a user module defines an interface and equivalent inline scalar conformances without standard-library names
- **THEN** the compiler applies the same ownership, compatibility, and static witness rules
