# language-server-navigation Specification

## Purpose

Defines semantic target lookup and go-to-definition behavior for Silk source positions, including
recovered programs and references whose declarations reside in other project modules.

## Requirements

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

The language server SHALL resolve definition requests from the compiler's token-level semantic
occurrence identity rather than matching source spelling. Supported source-backed occurrences SHALL
include declaration-site names, local and pattern bindings, parameters, function and callable
references, nominal type references, type arguments, type parameters, imports, qualified
declarations, struct fields, and actor or namespace references represented by available compiler
facts.

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
- **THEN** definition navigates to the callable declaration carried by that occurrence

#### Scenario: Nominal type reference

- **WHEN** the cursor is on a resolved type in a parameter, return type, field, or generic argument
- **THEN** definition navigates to that type's declaration name

#### Scenario: Declaration-site name

- **WHEN** the cursor is on a source declaration's own available name
- **THEN** definition returns that declaration as the selected location

### Requirement: Cross-file definitions use exact analyzed sources

For a target in another module, the language server SHALL return that module's actual document URI
and declaration-name range calculated from the exact analyzed bytes. An open target SHALL use its
synchronized URI; a closed project module SHALL use its project file URI; a file-backed
standard-library target SHALL use the URI of its canonical shipped `.silk` file.

#### Scenario: Definition in an open unsaved module

- **WHEN** a reference resolves to a declaration in an open imported module with unsaved changes
- **THEN** definition returns the open document URI and declaration range from the synchronized contents

#### Scenario: Definition in a closed project module

- **WHEN** a reference resolves to a declaration loaded from a closed project file
- **THEN** definition returns the analyzed project file URI and declaration range

#### Scenario: Definition in the standard library

- **WHEN** a reference resolves to a declaration in a shipped standard-library module
- **THEN** definition returns the canonical toolchain file URI and exact declaration range

### Requirement: Recovery and ambiguity do not invent navigation

The language server SHALL return no definition when the selected position has no semantic
occurrence, when its target is missing, inaccessible, ambiguous, conflicting, or unavailable because
of recovered syntax, or when an intrinsic semantic entity intentionally has no source declaration.
Damage outside the selected occurrence MUST NOT prevent navigation for otherwise available facts.

#### Scenario: Unresolved reference

- **WHEN** the cursor is on a reference whose semantic result is missing or inaccessible
- **THEN** definition returns no location

#### Scenario: Ambiguous reference

- **WHEN** the compiler retains multiple conflicting candidates without selecting one
- **THEN** definition returns no location rather than choosing a candidate by spelling or order

#### Scenario: Source-less intrinsic operation

- **WHEN** the cursor is on a recognized intrinsic actor or operation with no Silk declaration
- **THEN** definition returns no location rather than fabricating a virtual source file

#### Scenario: Unrelated syntax damage

- **WHEN** a module contains recovered syntax outside an available resolved occurrence
- **THEN** definition still navigates from that occurrence to its declaration

### Requirement: Target selection is deterministic

When nested semantic facts contain a requested position, the selected occurrence SHALL be the
smallest token-level semantic source range, with deterministic source-order tie breaking. Trivia,
keywords, punctuation without a semantic occurrence, and positions at a half-open range end SHALL
produce no target unless another containing semantic occurrence applies.

#### Scenario: Qualified operation tokens

- **WHEN** a position selects either side of `Effect.catch`
- **THEN** the server selects the distinct actor or operation occurrence for that exact token

#### Scenario: Cursor on trivia

- **WHEN** a definition request points to whitespace or a comment
- **THEN** the server returns no location

#### Scenario: Cursor at token end

- **WHEN** a request position equals the exclusive end of a semantic token
- **THEN** that token is not selected as the semantic occurrence
