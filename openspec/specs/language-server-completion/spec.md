# language-server-completion Specification

## Purpose

Defines deterministic, recovery-aware Silk completions selected from compiler scope, type, member,
intrinsic, and syntax context rather than protocol-side spelling heuristics.
## Requirements
### Requirement: Type completion offers canonical integer types

Type completion SHALL derive the ordered lowercase integer vocabulary plus `bool`, `()`, and `never` from semantic identities and MUST NOT offer removed uppercase aliases.

#### Scenario: Complete an integer type

- **WHEN** completion is requested in a type position
- **THEN** all fixed- and target-width integer spellings are offered deterministically

### Requirement: The server advertises context-aware completion

The language server SHALL advertise completion support, including qualified-member triggering after
`.` where the client supports trigger characters, and SHALL interpret request positions using the
negotiated position encoding.

#### Scenario: Client initializes completion support

- **WHEN** a compatible client initializes a Silk language-server session
- **THEN** the returned capabilities advertise completion support and `.` as a member trigger

### Requirement: Expression completion follows visible semantic scope

At an expression position, completion SHALL include visible local bindings, pattern bindings,
parameters, callable declarations, value constructors, imported values, and applicable language
keywords. Candidates SHALL follow compiler visibility and lexical shadowing; the LSP MUST NOT
reconstruct scope or merge same-spelled declarations independently.

#### Scenario: Complete a shadowed local

- **WHEN** completion is requested inside a nested block where a local shadows an outer binding
- **THEN** the candidate for that spelling identifies the nearest compiler-selected binding

#### Scenario: Complete at the start of an expression

- **WHEN** a user begins an expression in a valid function body
- **THEN** completion includes the visible values, callable declarations, and expression keywords valid at that position

### Requirement: Type completion follows type context

At a declared-type or type-argument position, completion SHALL include visible nominal types, type
parameters, built-in types, and imported types that are accessible in that context. Value-only
bindings and expression-only keywords MUST NOT be offered as type candidates.

#### Scenario: Complete a function parameter type

- **WHEN** completion is requested after `parameter:` in a function declaration
- **THEN** completion includes accessible types and type parameters but excludes local values

#### Scenario: Complete a generic type argument

- **WHEN** completion is requested inside an incomplete type argument list
- **THEN** completion remains available from the recovered type context

### Requirement: Qualified completion uses resolved actor and value semantics

After a qualifier or typed subject followed by `.`, completion SHALL return accessible namespace or
actor operations, imported module members, or fields appropriate to the resolved subject. Intrinsic
operations SHALL come from the same authoritative catalog used by analysis and hover.

#### Scenario: Complete an Effect operation

- **WHEN** completion is requested after `Effect.`
- **THEN** the result includes the supported Effect operations with source-like signature detail

#### Scenario: Complete an allocator operation

- **WHEN** completion is requested after `SystemAllocator.`
- **THEN** the result includes `make` with the same signature presented by hover

#### Scenario: Complete a struct field

- **WHEN** completion is requested after a value whose available type has accessible fields
- **THEN** the result includes those fields and excludes fields unavailable by visibility or subject type

### Requirement: Completion remains deterministic under recovery

Completion SHALL use available facts from the exact synchronized snapshot even when the token at
the cursor or unrelated source is incomplete. Missing, inaccessible, ambiguous, or conflicting
facts MUST NOT be guessed from spelling. Candidate ordering, labels, kinds, insertion text, and
signature detail SHALL be deterministic for identical source and position.

#### Scenario: Complete after a partial identifier

- **WHEN** the user requests completion after typing a partial identifier in otherwise analyzable source
- **THEN** available candidates are returned without requiring the partial spelling to resolve as a complete reference

#### Scenario: Ambiguous recovered qualifier

- **WHEN** a recovered qualifier remains semantically ambiguous
- **THEN** completion does not invent one candidate member set from source order

#### Scenario: Repeat completion

- **WHEN** identical snapshots are queried repeatedly at the same position
- **THEN** the ordered completion items and their semantic details are identical

### Requirement: Type completion offers float types

Type completion SHALL offer canonical `f32` and `f64` items derived from semantic type identities.

#### Scenario: Complete a float type

- **WHEN** completion is requested in a type position
- **THEN** `f32` and `f64` appear deterministically

### Requirement: Completion keeps intrinsics explicit

Qualified completion on `Intrinsic` SHALL offer the deterministic catalog of compiler primitives
with concrete signatures and safety markers. Ordinary expression and actor completion SHALL offer
visible source APIs and MUST NOT leak unqualified intrinsic operations or former compiler-known
actor members. Standard-library APIs SHALL remain the preferred completion path outside the sealed
namespace.

#### Scenario: Complete integer operations

- **WHEN** completion is requested after `Intrinsic.` and after an ordinary integer API qualifier
- **THEN** the first result set contains concrete primitives and the second contains source-defined numeric operations

### Requirement: Pattern-local completion follows visible semantic scope

Expression completion SHALL include shared pattern bindings only where their semantic scope is
active. Irrefutable let bindings SHALL appear after their declaration; match-arm and if-let
bindings SHALL appear only in the selected body and SHALL NOT appear in a mismatch body or after
their lexical scope.

#### Scenario: Complete inside if-let

- **WHEN** completion is requested inside the taken and mismatch bodies of one if-let
- **THEN** the pattern binding appears only in the taken-body result
