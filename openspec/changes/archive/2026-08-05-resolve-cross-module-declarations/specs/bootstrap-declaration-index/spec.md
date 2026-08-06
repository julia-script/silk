## MODIFIED Requirements

### Requirement: Canonical identities precede body resolution

Header collection SHALL visit every public and private top-level function declaration of every
module in the loaded closure and SHALL assign canonical declaration identities — the canonical
module identity plus the declaration name — before any import member or body reference resolves.
The first present occurrence of a name within a module SHALL own the canonical identity; a later
duplicate SHALL remain an explicit duplicate state carrying the original's identity and the
duplicate diagnostic's identity as its cause; a declaration whose name is unavailable after
recovery SHALL remain an explicit unidentified state without a fabricated name. Visibility SHALL
be retained independently from canonical identity.

#### Scenario: Assign identities across modules

- **WHEN** two loaded modules each declare a present function named `answer`
- **THEN** the index contains two headers with distinct canonical identities qualified by their own module identities

#### Scenario: Index public and private declarations

- **WHEN** one module declares `pub fn parse` and `fn helper`
- **THEN** both headers receive canonical identities in concrete order and retain public and private visibility respectively

#### Scenario: Keep duplicates explicit with causes

- **WHEN** one module declares the same present name twice
- **THEN** the first header owns the canonical identity and the second is a duplicate state carrying the first's identity and the `SEM0003` diagnostic's identity as its cause

#### Scenario: Keep unavailable names unidentified

- **WHEN** a declaration's name is a missing token after parser recovery
- **THEN** its header remains in the index as an unidentified state and no module or semantic diagnostic repeats the parser's mistake

### Requirement: Header signatures resolve at the header level

Every header SHALL resolve its explicit public or default-private visibility and complete signature
without touching imports or any body: ordered parameter names and declared types, duplicate
parameter names, return type, and the exact parameter-count contract. Unknown declared types SHALL
produce their stable diagnostics at exact spans, and unresolved or unavailable states SHALL remain
explicit rather than defaulting. Header collection SHALL complete for the entire reachable closure
before cross-module member binding or body elaboration begins.

#### Scenario: Resolve a complete public signature

- **WHEN** a module declares `pub fn choose(left: I32, right: I32) -> I32`
- **THEN** its header is public and resolves two ordered `I32` parameters, an `I32` return type, and a parameter count of two

#### Scenario: Resolve a complete private signature

- **WHEN** a module declares `fn helper(value: I32) -> I32`
- **THEN** its header is private and resolves its parameter and result without consulting a function body

#### Scenario: Diagnose header-level unknown types

- **WHEN** a parameter or return type names an unknown type
- **THEN** the header retains the unresolved type state and the index carries the `SEM0001` diagnostic at the type's exact span

## ADDED Requirements

### Requirement: Downstream references reuse indexed declarations

The published closure-wide index SHALL be the declaration-header authority for name resolution and
body elaboration. A resolved import binding, call-reference fact, HIR call, instance key, and lowered
call SHALL identify the same indexed canonical declaration rather than a phase-local recollection of
its source module. Damaged declarations SHALL retain their indexed recovery state for every
downstream query.

#### Scenario: Reuse one imported declaration identity

- **WHEN** a public function is selected into another module and called from its body
- **THEN** the import binding, reference fact, HIR call, discovered instance, and lowered call all carry the canonical identity assigned by the closure-wide index

#### Scenario: Preserve an indexed unavailable declaration

- **WHEN** a selected member names a declaration whose header cannot form a canonical identity
- **THEN** downstream resolution remains explicitly unavailable and does not synthesize a replacement declaration
