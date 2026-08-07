## Purpose

Defines semantic target lookup and go-to-definition behavior for Silk source positions, including
recovered programs and references whose declarations reside in other project modules.

## ADDED Requirements

### Requirement: The server advertises semantic definition support

The language server SHALL advertise go-to-definition support to compatible clients and SHALL
interpret request positions using the negotiated position encoding. Definition responses SHALL
identify both the source reference range and the declaration-name range when the protocol response
form supports them.

#### Scenario: Client initializes the server

- **WHEN** a compatible client initializes a Silk language-server session
- **THEN** the returned capabilities advertise definition support and the selected position encoding

#### Scenario: Unicode before a reference

- **WHEN** a definition request uses a UTF-16 position after non-ASCII source text
- **THEN** the server resolves the reference at the corresponding Silk source byte span

### Requirement: Definition follows resolved semantic identity

The language server SHALL resolve definition requests from the compiler's semantic reference
identity rather than matching source spelling. Supported targets SHALL include local bindings,
parameters, top-level declarations, imported declarations, qualified declarations, struct fields,
and callable references represented by available compiler facts.

#### Scenario: Shadowed local binding

- **WHEN** a reference spelling is shadowed by a nearer local binding
- **THEN** definition navigates to the binding selected by semantic analysis rather than another declaration with the same spelling

#### Scenario: Function parameter reference

- **WHEN** the cursor is on a resolved parameter reference
- **THEN** definition navigates to that parameter's declared name

#### Scenario: Qualified imported call

- **WHEN** the cursor is on a resolved call through a module namespace alias
- **THEN** definition navigates to the selected declaration in the imported module

#### Scenario: Struct field projection

- **WHEN** the cursor is on a resolved struct field projection
- **THEN** definition navigates to the field declaration identified by semantic analysis

#### Scenario: First-class callable reference

- **WHEN** the cursor is on a resolved function value or other callable reference
- **THEN** definition navigates to the callable declaration carried by that reference fact

### Requirement: Cross-file definitions use exact analyzed sources

For a target in another module, the language server SHALL return that module's document URI and
the declaration-name range calculated from the exact source bytes in the snapshot that resolved the
reference. An open target document SHALL use its synchronized URI and contents; a closed target
module SHALL use its project file URI.

#### Scenario: Definition in an open unsaved module

- **WHEN** a reference resolves to a declaration in an open imported module with unsaved changes
- **THEN** definition returns the open document URI and declaration range from the synchronized contents

#### Scenario: Definition in a closed module

- **WHEN** a reference resolves to a declaration loaded from a closed project file
- **THEN** definition returns the file URI and declaration range from the analyzed on-disk source

### Requirement: Recovery and ambiguity do not invent navigation

The language server SHALL return no definition when the selected position has no semantic target or
when its target is missing, inaccessible, ambiguous, conflicting, or unavailable because of
recovered syntax. Damage outside the selected target MUST NOT prevent navigation for otherwise
available facts.

#### Scenario: Unresolved reference

- **WHEN** the cursor is on a reference whose semantic result is missing or inaccessible
- **THEN** definition returns no location

#### Scenario: Ambiguous reference

- **WHEN** the compiler retains multiple conflicting candidates without selecting one
- **THEN** definition returns no location rather than choosing a candidate by spelling or order

#### Scenario: Unrelated syntax damage

- **WHEN** a module contains recovered syntax outside an available resolved reference
- **THEN** definition still navigates from that reference to its declaration

### Requirement: Target selection is deterministic

When nested semantic facts contain a requested position, the selected target SHALL be the smallest
available reference-bearing source range, with deterministic source-order tie breaking. Trivia,
keywords, punctuation without a semantic reference, and positions at a half-open range end SHALL
produce no target unless another containing semantic fact applies.

#### Scenario: Nested callable expression

- **WHEN** a position lies inside nested expressions with more than one containing semantic fact
- **THEN** the server selects the smallest reference-bearing range containing that position

#### Scenario: Cursor on trivia

- **WHEN** a definition request points to whitespace or a comment
- **THEN** the server returns no location

#### Scenario: Cursor at token end

- **WHEN** a request position equals the exclusive end of a reference token
- **THEN** that token is not selected as the semantic target
