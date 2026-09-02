# silk-documentation-model Specification

## Purpose

Defines Silk's Markdown-native documentation source model, lazy total parsing, semantic links, and
renderer-neutral documentation values shared by editor and generation tooling.

## Requirements

### Requirement: Documentation comments have explicit ownership

One or more consecutive `///` comments with no intervening blank line SHALL document the
immediately following declaration at every supported declaration level, including functions,
structs, fields, parameters, and implementation operations. Leading `//!` comments SHALL document
their containing source module. An ordinary line comment or blank line SHALL prevent declaration
attachment. The compiler SHALL expose the attached comments as exact source-owned raw blocks and
SHALL NOT parse their Markdown while constructing syntax or semantic facts.

#### Scenario: Attach documentation to a referenced function

- **WHEN** consecutive `///` lines immediately precede a function declaration
- **THEN** the function's declaration identity exposes one raw documentation block containing those exact comment tokens and spans

#### Scenario: Attach parameter documentation

- **WHEN** a `///` block immediately precedes a parameter in a broken parameter list
- **THEN** the parameter declaration owns that documentation independently of the containing function

#### Scenario: Keep separated documentation unattached

- **WHEN** a blank line or ordinary `//` comment separates `///` lines from a declaration
- **THEN** those lines remain syntax trivia and are not returned as that declaration's documentation

#### Scenario: Attach module documentation

- **WHEN** leading `//!` lines occur before the module's declarations
- **THEN** the source module exposes those lines as its module documentation block

### Requirement: Documentation parsing is lazy, total, and lossless

Documentation consumers SHALL parse raw blocks as CommonMark only when they request parsed
documentation. Parsing SHALL return a document for every raw block without a typed failure or
documentation diagnostic. Unsupported, malformed, or otherwise unrecognized content SHALL remain
readable text, and every parsed node SHALL retain enough source provenance to identify its authored
comment bytes.

#### Scenario: Ordinary compilation ignores Markdown

- **WHEN** a project is parsed, analyzed, checked, built, or run without requesting documentation
- **THEN** no documentation Markdown parser executes and documentation content cannot change the compilation outcome

#### Scenario: Recover malformed Markdown

- **WHEN** a raw documentation block contains an unclosed fence or another malformed construct
- **THEN** parsing returns a document preserving readable authored content without a diagnostic or failure

### Requirement: Documentation uses Markdown conventions instead of contract directives

The documentation language SHALL contain no `@param`, `@returns`, `@fails`, `@requires`, or
`@examples` directives. Parameters SHALL own their documentation directly, examples SHALL use a
Markdown `Examples` heading and fenced blocks, and tooling SHALL derive signatures, return types,
failure rows, and requirement rows from compiler semantic facts rather than documentation text.

#### Scenario: Infer an effect contract

- **WHEN** documentation is requested for an effect function with a return type, failure row, and requirement row
- **THEN** its documentation model carries the compiler-derived source-like signature without requiring those facts in the comment body

#### Scenario: Preserve examples

- **WHEN** an `Examples` section contains multiple fenced `silk` blocks
- **THEN** the parsed document preserves each block, its language, content, and source provenance without compiling it

### Requirement: Intra-document symbol links resolve best-effort

The Markdown form ``[`Symbol`]`` SHALL request a link to a declaration resolved in the documented
declaration's source scope. A resolved reference SHALL retain its canonical target identity.
An unresolved or ambiguous reference SHALL render as inline code and SHALL NOT produce a diagnostic
or make documentation unavailable.

#### Scenario: Resolve a local type link

- **WHEN** a function comment contains ``[`Problem`]`` and `Problem` resolves uniquely in that module scope
- **THEN** the parsed link identifies the canonical `Problem` declaration

#### Scenario: Fall back from an unresolved link

- **WHEN** a comment contains ``[`MissingThing`]`` and no declaration resolves
- **THEN** documentation remains available and represents `MissingThing` as inline code without a link or diagnostic

### Requirement: One parsed model serves every documentation consumer

The documentation package SHALL expose immutable, renderer-neutral document and project values that
preserve paragraphs, headings, lists, emphasis, links, code, examples, semantic declaration
identity, source-like signatures, child declarations, and source provenance. Hover, highlighting,
and generation consumers MUST use this shared model rather than implement independent documentation
parsers.

#### Scenario: Reuse one declaration document

- **WHEN** hover and JSON generation request the same documented declaration from an unchanged analysis snapshot
- **THEN** both observe equivalent normalized content, examples, links, signature facts, and source provenance

### Requirement: Generated module references present canonical import forms

The standard-library reference renderer SHALL present a selected import of the module's public
owner type for each nonprimitive module and an unaliased namespace import for each primitive
module. The rendered instruction SHALL be valid source for the same generated module revision.

#### Scenario: Render a nonprimitive module import

- **WHEN** the renderer emits the reference page for `silk/raw_buffer`
- **THEN** the page presents `import silk.raw_buffer { RawBuffer }`

#### Scenario: Render a primitive module import

- **WHEN** the renderer emits the reference page for `silk/u32`
- **THEN** the page presents `import silk.u32` without an alias or selected list

#### Scenario: Compile rendered examples

- **WHEN** documentation validation collects examples containing canonical imports
- **THEN** every non-ignored example resolves its preserved qualifiers without missing-member diagnostics

### Requirement: Inherent members document under their owner

Documentation ownership SHALL treat a `///` block above a member of an inherent impl as that
member's documentation and a block above the impl head as attached to the head. Generated module
references SHALL present inherent members grouped under their owner type, labeled as methods or
associated functions, with the canonical `Owner.member` qualifier rather than a module-qualified
spelling, and intra-document links `[`member`]` inside the owner's module SHALL resolve to the
member.

#### Scenario: Present Option members under Option

- **WHEN** documentation is generated for a module declaring `pub union Option<T>` and `impl<T> Option<T> { pub fn map ... }`
- **THEN** the reference lists `map` under `Option` as a method spelled `Option.map`

#### Scenario: Resolve a member link

- **WHEN** the module documentation for `Option` links `[`map`]`
- **THEN** the link resolves to the inherent member rather than reporting an unresolved symbol
