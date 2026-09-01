# bootstrap-complete-interface-contracts Specification

## Purpose

Define ordinary user interfaces whose operations retain literal operand ownership and complete
success, failure, requirement, access, and generic witness contracts.

## Requirements

### Requirement: Copy is a sealed zero-operation conformance

`Copy` SHALL remain a compiler-sealed interface with no user-definable operations. A source
implementation MAY be declared only in empty form for an eligible nominal aggregate in its defining
module and SHALL publish evidence only after the compiler proves every stored field Copy, proves the
complete type cleanup-free, and finds no `Drop` implementation, cycle, or conflicting evidence.

The compiler SHALL prove every shared reference `&T` Copy independently of whether `T` is Copy,
because duplication copies the shared reference rather than its referent. Every exclusive reference
`&mut T` SHALL remain affine. Source MUST NOT declare `Copy` for either reference kind; such a
declaration neither reads the referent nor overrides the sealed reference rule.

#### Scenario: Reject a Copy operation body

- **WHEN** an implementation of `Copy` declares or maps an operation
- **THEN** conformance validation rejects it rather than treating duplication as user code

#### Scenario: Reject Copy and Drop together

- **WHEN** one provider attempts to implement both `Copy` and `Drop`
- **THEN** the Copy implementation is invalid and no Copy witness is published

#### Scenario: Prove a shared reference Copy

- **WHEN** ownership asks whether `&T` is Copy for either a Copy or affine `T`
- **THEN** the compiler proves the shared reference Copy without source conformance evidence

#### Scenario: Keep an exclusive reference affine

- **WHEN** ownership asks whether `&mut T` is Copy
- **THEN** the compiler rejects duplication so two usable exclusive aliases cannot be created

#### Scenario: Reject a source reference Copy implementation

- **WHEN** source declares `impl Copy for &u32 {}` or a Copy implementation for any other reference
- **THEN** conformance validation rejects the structural provider rather than publishing redundant or alias-unsafe evidence

### Requirement: Interface operations use complete contracts

Interface declarations and applications SHALL bind ordinary type, failure-row, and requirement-row
arguments and SHALL describe each operation's flow kind, literal operand types and ownership,
success type, failure row, requirement row, and receiver access. Interfaces SHALL remain compile-time
contracts and MUST NOT create service slots or runtime dispatch.

#### Scenario: Declare an effectful decoder
- **WHEN** `Decoder<S, Arguments, A, !E, ?R>` declares an effectful operation
- **THEN** its complete operation contract retains provider, consumed arguments, success, exact rows, and access

### Requirement: Interface operands retain literal ownership

Ordinary source interface operands SHALL use their declared ownership literally. A value operand
transfers ownership, `&T` observes through a shared borrow, and `&mut T` grants exclusive access.
General witness mapping MUST NOT blanket-adapt value operands to shared borrows.

#### Scenario: Consume decoder input
- **WHEN** an interface declares `decode(self: &S, encoded: Arguments)`
- **THEN** a mapped witness receives `&S` and owns `Arguments`, not `&&S` and `&Arguments`

#### Scenario: Reject a stronger receiver demand
- **WHEN** a witness requires `&mut S` for an interface operation declaring `&S`
- **THEN** mapping fails because a generic caller promises only shared access

### Requirement: Witness rows use explicit subsumption

At a generic call site, instantiated interface failure and requirement rows SHALL remain exact and
source-observable. A witness with smaller failure or requirement rows MAY satisfy the interface by
subsumption; its local rows SHALL be widened to the interface contract for type checking, while
specialization MAY optimize unobservable dead machinery.

#### Scenario: Admit a pure decoder under a fallible contract
- **WHEN** a pure witness satisfies `Decoder<S, Arguments, A, !DecodeError, ?R>`
- **THEN** the generic call retains the interface rows while lowering may omit unreachable failure work

### Requirement: Generic mapped witnesses infer complete binders

Conformance mapping SHALL infer a target function's type, row, and representation arguments from the
specialized interface operation, provider, and conformance binders. Unresolved, conflicting, or
incompatible access arguments MUST produce deterministic diagnostics. Concrete witness instance keys
SHALL include every inferred argument.

#### Scenario: Specialize one generic mapped decoder twice
- **WHEN** two mapped schemas select the same generic witness declaration with distinct source and
  transform representations
- **THEN** instance discovery records two concrete static witness targets

### Requirement: Existing interfaces migrate to literal operands

Ordinary `Order` and `HashKey` source interfaces SHALL declare the borrows they intend explicitly.
Any transitional operand adapter MUST remain confined to sealed intrinsic witness lowering and MUST
NOT affect general user interfaces.

#### Scenario: Inspect a migrated order interface
- **WHEN** ordinary source maps an `Order` operation after migration
- **THEN** its declared borrow shapes match the witness directly without blanket adaptation

### Requirement: Conformance bodies satisfy specialized operation returns

An inline or mapped conformance operation body SHALL satisfy the interface operation's resolved
return contract after applying `Self`, interface, conformance, and operation generic substitutions.
An invalid body MUST NOT be published as an available witness.

#### Scenario: Reject an invalid specialized witness

- **WHEN** a mapped effect operation declared to succeed with `i32` returns `Effect<i32>` after all substitutions
- **THEN** semantic analysis reports the return mismatch and does not publish that mapping as an available witness

### Requirement: Interfaces and services share one static conformance model

Interface and service declarations SHALL use the same implicit `Self`, operation contract,
implementation mapping, witness identity, static call resolution, bound specialization, visibility,
completeness, overlap, and termination rules. A service SHALL receive no different operation
behavior after passing dependency-eligibility validation.

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

Only declarations satisfying the service eligibility rule MAY appear as Effect dependencies. After
that check, the compiler SHALL NOT create a duplicate provider identity, service-specific witness,
or name-selected dispatch rule.

#### Scenario: Use an ordinary interface outside a requirement row

- **WHEN** an interface is not dependency-eligible
- **THEN** it remains fully usable for static bounds and conformances but is rejected only when source attempts to place it in an Effect requirement

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

### Requirement: Operator markers are closed interface contract metadata

An interface operation MAY begin with `operator <token>` before its function contract. The marker
SHALL name exactly one supported eager prefix or infix token, SHALL agree with the operation's arity,
and SHALL be retained as semantic contract data. Services, free functions, control operators, and
operations with incompatible arity MUST NOT acquire operator eligibility.

#### Scenario: Retain one binary marker

- **WHEN** an interface declares `operator * fn multiply(left: Self, right: Scalar) -> Self`
- **THEN** its completed operation fact retains `*` as binary multiplication eligibility

#### Scenario: Reject control syntax as a marker

- **WHEN** an operation attempts to declare `operator &&`
- **THEN** declaration analysis reports the stable invalid-operator-contract diagnostic at the marker

#### Scenario: Reject a marker on a service

- **WHEN** a dependency-eligible service operation carries an operator marker
- **THEN** declaration analysis rejects it without changing any other service behavior

### Requirement: Requirement access satisfaction follows one partial order

Requirement/access satisfaction SHALL be decided by a single partial order applied identically by
type compatibility, representation-shape equality, and interface-witness selection. Given access
levels Shared, Exclusive, and Take, a supplied access SHALL satisfy a required access when the
supplied access is at least as strong as the required access: Take satisfies every access,
Exclusive satisfies Exclusive and Shared, and Shared satisfies only Shared.

#### Scenario: A stronger access satisfies a weaker requirement

- **WHEN** a requirement demands Shared access
- **THEN** a provider supplying Exclusive or Take access satisfies it

#### Scenario: A weaker access does not satisfy a stronger requirement

- **WHEN** a requirement demands Exclusive access
- **THEN** a provider supplying Shared access does not satisfy it

#### Scenario: Shape equality and compatibility agree

- **WHEN** two requirement rows are compared for representation-shape equality and for compatibility
- **THEN** both paths report the same satisfaction result for every Shared/Exclusive/Take pair

#### Scenario: Witness selection obeys the same order

- **WHEN** an interface witness is selected for a provider whose access differs from the requirement
- **THEN** satisfaction is judged by the same partial order used by compatibility, never by an exact-equality-only rule

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
