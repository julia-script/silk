# bootstrap-hir Specification

## Purpose
The resolved, typed semantic representation of elaborated function bodies: one integrated
elaboration phase that resolves names, types expressions, validates contracts, and constructs HIR
with canonical identities and exact source provenance, published as immutable fact tables with a
deterministic textual encoder.
## Requirements
### Requirement: One integrated elaboration phase constructs HIR

Elaboration SHALL consume collected declaration headers and resolve every function body in one
integrated phase: local and referenced-name resolution, expression typing, and positional
contract validation together with HIR construction. Elaboration SHALL preserve the existing body
diagnostics (`SEM0002`, `SEM0004`, `SEM0006`, `SEM0007`) with their codes, spans, and reasons,
and SHALL return complete ordered facts and diagnostics rather than throw for source mistakes.

#### Scenario: Elaborate the accepted fixture

- **WHEN** `pub fn main() -> I32 { return 42 }` is elaborated
- **THEN** the result contains one HIR function whose body is a typed `I32` integer-literal return with exact source provenance and no diagnostics

#### Scenario: Preserve body diagnostics

- **WHEN** a body contains an out-of-range literal, an unknown call target, an unknown parameter reference, and a wrong-arity call across functions
- **THEN** elaboration reports the same stable codes at the same spans as the superseded analysis

### Requirement: HIR is resolved, typed, and canonically identified

Every HIR function body SHALL be an ordered statement sequence: zero or more binding statements
followed by one return statement. Every HIR expression SHALL be a core semantic operation —
integer literal, parameter reference, binding reference, move, or call — carrying its resolved
type and exact source provenance. Calls SHALL reference their target's canonical declaration
identity, parameter references their function-local parameter identity, and binding references
and moves their function-local binding identity. Normalized function contracts (ordered
parameter types and result type) SHALL be published per declaration. An unknown fact SHALL
remain an explicit unavailable state carrying the originating diagnostic's identity where one
exists, and MUST NOT masquerade as a valid empty contract, resolved reference, or concrete type.

#### Scenario: Reference a call target canonically

- **WHEN** `main` returns `answer()` and `answer` is a present unique declaration
- **THEN** the HIR call references `answer`'s canonical identity and carries the resolved `I32` type

#### Scenario: Keep unknown facts explicit

- **WHEN** a body references an unknown function or an unknown parameter
- **THEN** the corresponding HIR expression is an explicit unavailable state carrying the originating diagnostic's identity, and the enclosing contract or type is not defaulted

#### Scenario: Normalize function contracts

- **WHEN** a declaration has two resolved `I32` parameters and a resolved `I32` return
- **THEN** its published contract lists both parameter types in order and the result type, while any unresolved header type keeps the whole contract explicitly unavailable

#### Scenario: Elaborate a binding sequence

- **WHEN** a body spells `let value = identity(42) return value`
- **THEN** the HIR body is one binding statement whose initializer is a typed call followed by one return whose expression is a typed binding reference to that binding

#### Scenario: Keep a damaged statement explicit

- **WHEN** one binding statement's initializer contains an unresolved reference
- **THEN** that initializer is an explicit unavailable expression carrying the originating diagnostic's identity while the statement sequence and the other statements' facts remain intact

### Requirement: Elaboration output is deterministic and encodable

Elaboration over the same input SHALL produce identical facts, HIR, and diagnostics across fresh
processes. The HIR SHALL expose a deterministic textual encoder observing the completed artifact;
identical input SHALL produce byte-identical encodings, gated by committed golden files.

#### Scenario: Repeat elaboration

- **WHEN** equivalent modules are elaborated repeatedly in fresh processes
- **THEN** the functions, HIR bodies, contracts, and diagnostics are identical

#### Scenario: Match the HIR golden encoding

- **WHEN** a committed fixture is elaborated and encoded
- **THEN** the encoding equals the committed golden text byte-for-byte, naming every function, contract, typed expression, and unavailable state

