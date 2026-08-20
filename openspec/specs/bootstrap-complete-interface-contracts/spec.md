# bootstrap-complete-interface-contracts Specification

## Purpose

Define ordinary user interfaces whose operations retain literal operand ownership and complete
success, failure, requirement, access, and generic witness contracts.

## Requirements

### Requirement: Copy is a sealed zero-operation conformance

`Copy` SHALL be a compiler-sealed interface with no user-definable operations. An implementation
MAY be declared only in the empty form and SHALL publish evidence only after the compiler proves
every stored field Copy, proves the complete type cleanup-free, and finds no `Drop` implementation,
cycle, or conflicting evidence.

#### Scenario: Reject a Copy operation body

- **WHEN** an implementation of `Copy` declares or maps an operation
- **THEN** conformance validation rejects it rather than treating duplication as user code

#### Scenario: Reject Copy and Drop together

- **WHEN** one provider attempts to implement both `Copy` and `Drop`
- **THEN** the Copy implementation is invalid and no Copy witness is published

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

A source conformance SHALL be declared in the module defining its provider's outer nominal type.
Its visibility SHALL be determined by the visibility of its contract and provider endpoints.
Potentially overlapping heads, non-terminating conditional requirements, incomplete witnesses, and
statically unprovable concrete uses SHALL be rejected before lowering.

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
