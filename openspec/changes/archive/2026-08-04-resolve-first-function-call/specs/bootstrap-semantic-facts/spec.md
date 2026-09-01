## ADDED Requirements

### Requirement: First top-level call reference fact

Semantic analysis SHALL resolve every present zero-argument call callee against all collected
top-level declarations without depending on declaration order. A call-reference fact SHALL be
`Resolved` with the exact target declaration identity and callee syntax when exactly one declaration
matches, `Missing` when none matches, `Ambiguous` when multiple declarations match, or unavailable
when parser recovery did not supply a usable callee. Forward and self references SHALL resolve by
the same rules because this phase records relationships and does not execute functions.

#### Scenario: Resolve a call to an earlier declaration

- **WHEN** `answer` is declared before `main` and `main` returns `answer()`
- **THEN** the call reference resolves to `answer`'s exact declaration identity and preserves the call-site identifier span

#### Scenario: Resolve a forward call

- **WHEN** `main` returns `answer()` and `answer` is declared later in the same source
- **THEN** the call resolves to the later declaration independently of source ordering

#### Scenario: Resolve a self reference as data

- **WHEN** a function returns a call to its own unique name
- **THEN** the reference resolves to that declaration without evaluating the call or deciding recursion policy

#### Scenario: Preserve an ambiguous target

- **WHEN** a call name matches multiple duplicate declarations
- **THEN** the call reference is ambiguous, exposes all matching declaration identities in source order, and does not select one target

### Requirement: Unknown call target diagnostic

A present call name with no matching declaration SHALL produce one `SEM0004` semantic diagnostic at
the callee identifier span and retain a `Missing` reference fact. An ambiguous call SHALL rely on the
existing `SEM0003` duplicate-declaration diagnostics and MUST NOT add a second ambiguity diagnostic
at the call site. Missing or damaged callee syntax SHALL not duplicate parser diagnostics.

#### Scenario: Diagnose an unknown function

- **WHEN** `main` returns `missing()` and no declaration is named `missing`
- **THEN** the call reference is missing and one `SEM0004` diagnostic identifies the exact `missing` span

#### Scenario: Avoid duplicate ambiguity diagnostics

- **WHEN** a call targets a name already diagnosed as duplicated
- **THEN** the call remains ambiguous and the semantic collection contains the declaration-owned `SEM0003` diagnostics without an additional call-site ambiguity diagnostic

#### Scenario: Preserve parser ownership for a missing callee

- **WHEN** parser recovery inserts the call's identifier
- **THEN** the call reference is unavailable and no `SEM0004` diagnostic is emitted

## MODIFIED Requirements

### Requirement: First return compatibility fact

Every function fact SHALL publish return compatibility as `Compatible` only when that function's
declared return type and returned expression type are both available and equal. An integer expression
uses its existing `I32` type. A uniquely resolved call expression SHALL use its target declaration's
resolved return type even for forward or self references. Compatibility SHALL be `Unavailable` when
the caller type, expression type, or call target is unresolved, missing, ambiguous, or invalid, and
one function's compatibility MUST NOT overwrite another function's facts.

#### Scenario: Check an integer return

- **WHEN** a function declares `I32` and returns an available `I32` integer
- **THEN** that function reports `Compatible`

#### Scenario: Check a resolved call return

- **WHEN** `answer` declares `I32` and `main` declares `I32` and returns a uniquely resolved call to `answer`
- **THEN** the call expression has type `I32` and `main` reports `Compatible`

#### Scenario: Withhold compatibility for an unknown call

- **WHEN** a function returns a call whose reference is missing
- **THEN** the call expression type and caller return compatibility are `Unavailable`

#### Scenario: Withhold compatibility for an ambiguous call

- **WHEN** a function returns a call whose reference is ambiguous
- **THEN** the call expression type and caller return compatibility are `Unavailable` without selecting a declaration

#### Scenario: Withhold compatibility for an unresolved callee type

- **WHEN** a call resolves uniquely to a declaration whose return type is unresolved or unavailable
- **THEN** the call reference remains resolved but its expression type and caller return compatibility are `Unavailable`
