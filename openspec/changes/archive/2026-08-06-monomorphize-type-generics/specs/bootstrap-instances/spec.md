## ADDED Requirements

### Requirement: Generic instance keys carry normalized concrete arguments

Every discovered generic runtime instance SHALL be identified by its canonical declaration plus an
ordered normalized concrete type-argument list. Worklist discovery SHALL record an instance before
following calls, values, cleanup, and runtime helpers reachable through its substitution, and its
ordering and encoding SHALL remain deterministic.

#### Scenario: Distinguish specializations

- **WHEN** the entry reaches `identity<I32>` and `identity<Bool>`
- **THEN** discovery records two keys differing only in their concrete argument lists

#### Scenario: Exclude an unused specialization

- **WHEN** a generic declaration can accept `Token` but no reachable call uses that argument
- **THEN** no `Token` instance is discovered merely from the declaration
