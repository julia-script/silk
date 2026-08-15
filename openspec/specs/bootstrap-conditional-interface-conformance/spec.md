# bootstrap-conditional-interface-conformance Specification

## Purpose

Define bounded generic interface conformances with coherent, terminating compile-time proof search
and statically specialized witnesses.

## Requirements

### Requirement: Conformance declarations bind requirements inline

`impl<...>` SHALL bind ordinary, row, and representation parameters plus interface requirements in
its parameter list. Every interface application SHALL explicitly include a provider equal to the
`for` type; this capability SHALL NOT add implicit `Self` or `where` syntax.

#### Scenario: Declare a mapped conformance

- **WHEN** `MappedSchema<A, B, S, F>` declares a decoder conformance requiring a decoder for `S`
- **THEN** the index records the generic head and required provider relationship

### Requirement: Conditional heads are coherent at declaration time

The declaration index SHALL reject any two provider/interface heads that may overlap after
alpha-normalization and kind-aware conservative unification, without consulting their bounds.
Ordinary types SHALL use first-order unification; normalized rows and representation bounds SHALL
conservatively overlap whenever a common admissible argument cannot be disproved.

#### Scenario: Reject bound-distinguished overlap

- **WHEN** two wrapper conformances have unifying heads but different `Left` and `Right` requirements
- **THEN** declaration analysis rejects the overlap even if no current type satisfies both bounds

#### Scenario: Reject open-row overlap

- **WHEN** one head contains row variable `!E` and another contains a compatible closed failure row
- **THEN** kind-aware overlap treats the heads as potentially ambiguous

### Requirement: Conditional proof search terminates structurally

Every required provider SHALL be a strict structural subterm of the current `for` provider, generic
variable occurrences across the complete goal SHALL not increase, and ground non-provider interface
arguments SHALL remain unchanged. Requirements that construct an equal provider, peer, or superterm
MUST be rejected at declaration time.

#### Scenario: Accept nested optional schemas

- **WHEN** proving `Decoder` for nested `OptionalSchema<S>` repeatedly requires the immediate inner provider
- **THEN** proof search descends to the base witness and terminates

#### Scenario: Reject a growing provider

- **WHEN** a requirement replaces provider `S` with `Wrap<S>`
- **THEN** declaration analysis reports the non-decreasing provider sizes

### Requirement: Concrete specialization proves every requirement

At each reachable concrete interface goal, analysis SHALL prove all conditional requirements before
admitting one witness. Missing, unavailable, non-terminating, or cyclic proofs MUST retain a finite
requirement trace and MUST NOT create runtime dictionaries or provisional witnesses.

#### Scenario: Report a missing base witness

- **WHEN** one mapped provider's source type lacks the required decoder conformance
- **THEN** the call is rejected with the conditional requirement chain

### Requirement: Conditional witnesses remain static and deterministic

Each admitted concrete provider/interface pair SHALL select one canonical witness instance and
deterministic proof. HIR and instance discovery SHALL retain the unresolved goal until concrete
specialization, then MIR SHALL contain only the selected static target.

#### Scenario: Specialize one conditional conformance twice

- **WHEN** two concrete mapped schemas satisfy the same generic conformance declaration
- **THEN** discovery records two deterministic concrete witness keys and no runtime interface lookup
