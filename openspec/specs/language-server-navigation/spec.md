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

### Requirement: Navigation stops only at the intrinsic boundary

Definition navigation SHALL follow public numeric, service, Effect, layout, and storage wrappers to
their canonical Silk source. A direct `Intrinsic` operation MAY have no source location, but its
identity and presentation MUST remain queryable. Navigation MUST NOT stop at a wrapper merely
because its body calls an intrinsic.

#### Scenario: Navigate through a public wrapper

- **WHEN** definition is requested on a generic integer addition call
- **THEN** navigation opens the standard-library declaration rather than reporting a source-less scalar intrinsic

#### Scenario: Query a direct intrinsic

- **WHEN** definition is requested on `Intrinsic.i32Add`
- **THEN** no source location is invented and the intrinsic semantic identity remains available

### Requirement: References enumerate one semantic identity across the project

The language server SHALL advertise references support and SHALL answer a references request with
every occurrence in the accepted project revision whose semantic identity equals the identity
selected at the request position, rather than every token that shares its spelling. Occurrences
SHALL be returned in canonical module order and, within a module, in source order. The
declaration-site occurrence SHALL be included when the client sets `includeDeclaration` and
excluded otherwise. A position with no available semantic occurrence SHALL produce no references.

#### Scenario: Client initializes the server

- **WHEN** a compatible client initializes a Silk language-server session
- **THEN** the returned capabilities advertise references support

#### Scenario: Uses in another module

- **WHEN** references are requested on a declaration used from a second module
- **THEN** the response contains that module's uses at their exact ranges together with the uses in the declaring module

#### Scenario: Selected member and its local alias

- **WHEN** references are requested through an aliased selected-member import
- **THEN** the response contains the declaration, the imported source name, the local alias, and every use of that alias, because all of them carry one semantic identity

#### Scenario: Declaration excluded on request

- **WHEN** a references request clears `includeDeclaration`
- **THEN** the declaration-site occurrence is the only occurrence removed from the response

### Requirement: Rename rewrites one semantic identity in one workspace edit

The language server SHALL advertise rename support with `prepareProvider` enabled. A prepare-rename
request SHALL return the range of the selected name token, and SHALL fail for a token with no
source-backed declaration, including keywords, trivia, and intrinsics that have no Silk
declaration. A rename SHALL return one `WorkspaceEdit` covering every module of the accepted
project revision, and SHALL edit only the occurrences whose analyzed spelling equals the selected
name so that an aliased import keeps the local name its own module chose. A rename SHALL be refused
rather than partially applied when an occurrence cannot be placed in a document.

The accepted project revision is the rename's outer bound: the open documents that serve as roots
plus every module they transitively import, closed project files included, resolved from disk. A
closed module that imports the renamed declaration but is reachable from no open root is therefore
outside that revision and SHALL NOT be edited. Rooting analysis at open documents is what the
`language-server-synchronization` capability specifies, so widening the root set is a change to
synchronization rather than to navigation. The resulting failure is visible — the unedited module
stops compiling — rather than silent.

Within that revision, an `as` clause binds a name in one module only, so a rename selected through
such a binding SHALL be confined to the module that wrote the clause. Two modules that alias one
declaration to the same spelling agree on both identity and spelling, and neither owns the other's
choice of name. The declaration site and the source half of an import clause name the declaration
itself and SHALL stay project-wide. This confinement applies to rename alone; references are
read-only and SHALL continue to report every occurrence of the identity.

Because Silk has one flat non-shadowing module namespace, a rename of a name that occupies that
namespace SHALL be refused when the new spelling is already bound in any module whose flat namespace
the rename would extend. The refusal SHALL carry the existing `SEM0016` binding-collision code and
message rather than a rename-specific diagnostic.

#### Scenario: Client initializes the server

- **WHEN** a compatible client initializes a Silk language-server session
- **THEN** the returned capabilities advertise rename support with prepare support enabled

#### Scenario: Prepare a rename on a keyword

- **WHEN** a prepare-rename request selects a keyword token
- **THEN** the request fails instead of returning a range

#### Scenario: Rename a declaration used in another module

- **WHEN** a top-level declaration used from a second module is renamed
- **THEN** one workspace edit changes the declaration and every use in both modules

#### Scenario: Rename an imported source name

- **WHEN** a declaration reached through an aliased selected-member import is renamed
- **THEN** the imported source name changes and the local alias and its uses keep their own spelling

#### Scenario: Two modules alias one declaration to the same spelling

- **WHEN** an alias is renamed in a module while a second module aliases the same declaration to the same spelling
- **THEN** only the selecting module's clause and uses change, and the second module keeps the name it chose

#### Scenario: Rename a project alias of a standard-library member

- **WHEN** a project module's alias of a standard-library member is renamed and the installation aliases that member to the same spelling
- **THEN** the edit covers the project module alone and reaches no file inside the installed toolchain

#### Scenario: Rename onto an existing top-level name

- **WHEN** a rename would give a declaration a spelling already bound in a module's flat namespace
- **THEN** the rename is refused with the `SEM0016` collision reason and no edit is returned

### Requirement: Pattern-local navigation follows resolved semantic identity

Definition, references, and rename SHALL treat each shared pattern binding as one compiler-owned
semantic identity. A use in a match arm, after an irrefutable let, or inside a taken if-let body
SHALL navigate to the exact binding token; unavailable and out-of-scope uses SHALL invent no target.

#### Scenario: Navigate a local destructuring binding

- **WHEN** definition is requested on a later use of a field-shorthand binding
- **THEN** the target selection range is the shorthand token that declared the local
