# bootstrap-complete-interface-contracts Specification

## Purpose

Define ordinary user interfaces whose operations retain literal operand ownership and complete
success, failure, requirement, access, and generic witness contracts.

## Requirements

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
