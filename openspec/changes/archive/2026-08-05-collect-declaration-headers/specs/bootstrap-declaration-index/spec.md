## Purpose

The closure-wide declaration index: every top-level declaration header collected and given a
canonical identity before any body resolves, header-level signatures resolved with explicit
unresolved states, and the result published as an immutable, canonically ordered fact table that
downstream phases key against.

## ADDED Requirements

### Requirement: Canonical identities precede body resolution

Header collection SHALL visit every top-level function declaration of every module in the loaded
closure and SHALL assign canonical declaration identities — the canonical module identity plus
the declaration name — before any body resolves. The first present occurrence of a name within a
module SHALL own the canonical identity; a later duplicate SHALL remain an explicit duplicate
state carrying the original's identity and the duplicate diagnostic's identity as its cause; a
declaration whose name is unavailable after recovery SHALL remain an explicit unidentified state
without a fabricated name.

#### Scenario: Assign identities across modules

- **WHEN** two loaded modules each declare a present function named `answer`
- **THEN** the index contains two headers with distinct canonical identities qualified by their own module identities

#### Scenario: Keep duplicates explicit with causes

- **WHEN** one module declares the same present name twice
- **THEN** the first header owns the canonical identity and the second is a duplicate state carrying the first's identity and the `SEM0003` diagnostic's identity as its cause

#### Scenario: Keep unavailable names unidentified

- **WHEN** a declaration's name is a missing token after parser recovery
- **THEN** its header remains in the index as an unidentified state and no module or semantic diagnostic repeats the parser's mistake

### Requirement: Header signatures resolve at the header level

Every header SHALL resolve its public signature without touching any body: visibility, ordered
parameter names and declared types, duplicate parameter names, return type, and the exact
parameter-count contract. Unknown declared types SHALL produce their stable diagnostics at exact
spans, and unresolved or unavailable states SHALL remain explicit rather than defaulting.

#### Scenario: Resolve a complete signature

- **WHEN** a module declares `pub fn choose(left: I32, right: I32) -> I32`
- **THEN** its header resolves two ordered `I32` parameters, an `I32` return type, and a parameter count of two

#### Scenario: Diagnose header-level unknown types

- **WHEN** a parameter or return type names an unknown type
- **THEN** the header retains the unresolved type state and the index carries the `SEM0001` diagnostic at the type's exact span

### Requirement: The declaration index is an immutable canonical fact table

The published index SHALL be immutable, SHALL order headers by canonical module identity and then
concrete declaration order, and SHALL answer name lookups per module distinguishing exactly one
match, no match, and multiple matches without discarding any collected header. Identical closures
SHALL produce identical indexes across fresh processes.

#### Scenario: Order headers canonically

- **WHEN** a closure loads modules whose identities sort differently from their traversal order
- **THEN** the index lists headers grouped by canonical module identity in identity order, each module's headers in concrete source order

#### Scenario: Repeat index collection

- **WHEN** the same closure is collected repeatedly in fresh processes
- **THEN** the resulting headers, identities, signature states, lookup outcomes, and diagnostics are identical

#### Scenario: Look up declarations per module

- **WHEN** a module declares one present unique `main`
- **THEN** looking up `main` in that module resolves its header while an undeclared name reports no match
