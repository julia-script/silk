# Silk Source Formatting Specification

## Purpose

Defines Silk's single canonical source representation and the strict formatting workflows shared by
the command line today and in-memory language tooling in the future.

## Requirements

### Requirement: Formatting accepts only complete syntax

The system SHALL format one lossless syntax artifact without requiring name resolution, type
checking, import resolution, or another semantic phase. It SHALL reject an artifact containing a
lexical diagnostic, parser diagnostic, missing token, or unexpected-token error region, and SHALL
NOT insert a missing required token or otherwise repair damaged syntax. A semantic diagnostic on
otherwise complete syntax SHALL NOT prevent formatting.

#### Scenario: Format complete syntax

- **WHEN** a syntax artifact has no lexical or parser damage
- **THEN** formatting produces canonical source bytes without running semantic analysis

#### Scenario: Reject damaged syntax

- **WHEN** a syntax artifact contains an invalid token, missing token, or parser error region
- **THEN** formatting reports a typed source-formatting failure and produces no replacement bytes for that artifact

#### Scenario: Format syntax with semantic errors

- **WHEN** a syntactically complete source contains an unknown type or unresolved name
- **THEN** formatting succeeds because the source grammar is complete

#### Scenario: Do not autofix parser damage

- **WHEN** a required closing delimiter is missing at a known insertion point
- **THEN** formatting rejects the artifact rather than materializing the missing delimiter

### Requirement: Fixed-array source types have one canonical bracketed layout

The formatter SHALL print every complete fixed-array source type with no space immediately inside
either bracket, no space before the semicolon, and one space after the semicolon. Nested fixed-array
types SHALL apply the same layout recursively. Missing required fixed-array punctuation SHALL remain
damaged syntax and SHALL NOT be repaired by formatting.

#### Scenario: Format a fixed-array type

- **WHEN** complete source spells a fixed-array type with noncanonical trivia as `[ i32 ;4 ]`
- **THEN** formatting emits `[i32; 4]`

#### Scenario: Format a nested fixed-array type

- **WHEN** complete source contains a nested fixed-array type equivalent to `[[i32; 4]; 3]`
- **THEN** formatting emits `[[i32; 4]; 3]` with the canonical layout at both levels

#### Scenario: Refuse to repair a missing separator

- **WHEN** a fixed-array type is missing its required semicolon
- **THEN** formatting rejects the damaged syntax instead of inserting the semicolon

### Requirement: Formatting has one public canonical policy

The system SHALL expose one public formatting policy with a 100-column target, two-space
indentation, spaces instead of tabs, LF line endings, no trailing whitespace, and exactly one final
newline. The public formatter SHALL NOT accept options that select another width, indentation,
line-ending, or trailing-whitespace style. The width SHALL be a breaking target for layout groups,
not a requirement to rewrite or split an indivisible token or preserved comment.

#### Scenario: Normalize physical whitespace

- **WHEN** complete source contains tabs, CRLF line endings, trailing spaces, or multiple final newlines
- **THEN** its formatted bytes use two-space indentation, LF line endings, no trailing whitespace, and one final newline

#### Scenario: Ignore editor presentation preferences

- **WHEN** an editor requests a tab width or tab-based indentation that differs from canonical Silk style
- **THEN** the resulting Silk source still uses the canonical public policy

#### Scenario: Preserve an indivisible over-width spelling

- **WHEN** one identifier, literal, or preserved comment is longer than the 100-column target
- **THEN** formatting preserves its spelling even though the resulting line exceeds the target

### Requirement: Width-aware groups break deterministically

The system SHALL keep a delimited list or other eligible layout group compact when its complete
flat representation fits the remaining width. When the flat representation does not fit, the
system SHALL break the group at its grammatical boundaries with two-space nested indentation.
Broken parameter, argument, import-member, array-element, and struct-initializer lists SHALL place
one item on each line and SHALL contain a trailing comma; their flat forms SHALL omit the trailing
comma. Identical syntax and starting column SHALL make the same break decision in every run.

#### Scenario: Keep a short call compact

- **WHEN** a complete call and its arguments fit within the remaining target width
- **THEN** the formatted call remains on one line with comma-space separators and no trailing comma

#### Scenario: Break a long call

- **WHEN** a complete call's flat representation exceeds the remaining target width
- **THEN** each argument is placed on its own indented line and the final argument has a trailing comma

#### Scenario: Repeat a width boundary

- **WHEN** equivalent syntax is formatted repeatedly at the same grammatical position
- **THEN** every run chooses identical compact and broken groups

### Requirement: Comments retain content and stable attachment

Formatting SHALL preserve every line-comment, declaration-documentation, and module-documentation
token in source order and SHALL preserve the token's spelling byte-for-byte except for terminal
spaces or tabs, which SHALL be removed to satisfy the canonical no-trailing-whitespace policy. A
trailing line comment SHALL remain attached to the preceding grammatical element and force the
following element onto a new line. A standalone comment SHALL be indented to its surrounding
grammatical context. One or more consecutive `///` comments with no intervening blank line
immediately before a function, struct, field, parameter, or implementation operation SHALL form
that element's documentation block and SHALL remain immediately before it after formatting. Leading
`//!` comments SHALL remain the module documentation block before module declarations.

#### Scenario: Preserve a trailing comment

- **WHEN** a line comment follows a complete statement on the same source line
- **THEN** its spelling remains after that statement, terminal horizontal whitespace is removed, and the next statement begins on a new line

#### Scenario: Indent a standalone comment

- **WHEN** a standalone line comment appears within a nested block
- **THEN** its spelling except terminal horizontal whitespace is emitted at that block's canonical indentation

#### Scenario: Retain a documentation block

- **WHEN** consecutive `///` comments immediately precede a function, struct, field, parameter, or implementation operation
- **THEN** they remain consecutive and immediately precede that same element in the formatted source

#### Scenario: Retain module documentation

- **WHEN** leading `//!` comments document a module
- **THEN** they remain consecutive at the module boundary before declarations

#### Scenario: Keep an unattached comment unattached

- **WHEN** a blank line separates a `///` comment from the following declaration or field
- **THEN** formatting preserves one separating blank line and does not attach the comment as documentation

### Requirement: Blank lines are bounded but meaningful

Formatted source SHALL contain exactly one blank line between top-level declarations. Within a
block, a run of one or more author-supplied blank lines between statements or standalone comment
groups SHALL become exactly one blank line; formatting SHALL NOT introduce an interior blank line
where the source had none. Runs of blank lines in every other grammatical context SHALL collapse to
the single canonical separation required by that context.

#### Scenario: Separate top-level declarations

- **WHEN** two top-level declarations are adjacent or separated by several blank lines
- **THEN** the formatted source contains exactly one blank line between them

#### Scenario: Preserve one block grouping line

- **WHEN** statements inside a block are separated by one or more blank lines
- **THEN** the formatted block retains exactly one blank line at that boundary

#### Scenario: Keep adjacent statements adjacent

- **WHEN** two statements inside a block have no blank line between them
- **THEN** formatting places them on consecutive lines without introducing a blank line

### Requirement: Canonical formatting is deterministic and idempotent

Formatting SHALL preserve declaration and expression structure, identifier and literal spellings,
operator grouping, and comment order while changing only canonical layout and optional list
punctuation. Reparsing formatted bytes SHALL produce complete syntax with the same grammatical
program. Formatting an already formatted artifact SHALL return byte-identical output and SHALL
report that the source did not change.

#### Scenario: Reparse formatted output

- **WHEN** complete source is formatted and the resulting bytes are reparsed
- **THEN** reparsing has no lexical or parser diagnostics and retains the same grammatical program and comment sequence

#### Scenario: Format twice

- **WHEN** formatted bytes are parsed and formatted again
- **THEN** the second output is byte-identical to the first and is reported as unchanged

#### Scenario: Repeat in fresh processes

- **WHEN** equivalent complete syntax is formatted in fresh processes
- **THEN** every process produces byte-identical formatted output

### Requirement: Reusable formatting is document-local

The reusable formatter SHALL accept an in-memory syntax artifact and SHALL return immutable
formatted bytes together with whether they differ from the artifact's source. It SHALL NOT read or
write files, discover projects, accept a cursor position, or return protocol-specific text edits.
Filesystem mutation, project selection, cursor preservation, position conversion, and minimal edit
generation SHALL remain responsibilities of adapters.

#### Scenario: Format an in-memory editor document

- **WHEN** language tooling supplies a complete in-memory syntax artifact
- **THEN** formatting succeeds without a filesystem, project, cursor, or language-server protocol dependency

#### Scenario: Detect unchanged source

- **WHEN** the syntax artifact's source bytes already equal the canonical bytes
- **THEN** the result contains those bytes and reports that no change is required

### Requirement: The format command selects project source deterministically

`silk format` SHALL discover or explicitly select a Silk project and, when no positional path is
given, SHALL select every exact `.silk` file beneath that project's source root whether or not the
file is reachable from the project entry. Positional file and directory paths SHALL restrict the
selection, directory selection SHALL recurse for exact `.silk` files, and selected paths outside
the source root SHALL be rejected. Every selected file SHALL be processed and reported in canonical
path order.

#### Scenario: Format the whole source root

- **WHEN** `silk format` runs without positional paths
- **THEN** every `.silk` file below the selected project's source root is selected, including unreachable files

#### Scenario: Format one explicit file

- **WHEN** `silk format src/Draft.silk` names a file below the source root
- **THEN** only that selected source file is considered for formatting

#### Scenario: Format one explicit directory

- **WHEN** `silk format src/model` names a directory below the source root
- **THEN** every `.silk` file below that directory is selected in canonical path order

#### Scenario: Reject an external selection

- **WHEN** a positional path resolves outside the selected project's source root
- **THEN** the command reports a selection failure without formatting that path

### Requirement: Write and check modes classify every selected file

In its default mode, `silk format` SHALL write canonical bytes for each selected complete file whose
source changes. A damaged selected file SHALL be reported and skipped without preventing other
complete selected files from being formatted. With `--check`, the command SHALL perform no writes
and SHALL report every selected file that is non-canonical or damaged. Reports SHALL use
deterministic path order.

#### Scenario: Write changed files

- **WHEN** default mode selects complete non-canonical and already canonical files
- **THEN** only the non-canonical files are rewritten and the command reports success for the selection

#### Scenario: Continue after one damaged file

- **WHEN** default mode selects one damaged file and one complete non-canonical file
- **THEN** the damaged file remains unchanged, the complete file is formatted, and the command reports source rejection

#### Scenario: Check without writing

- **WHEN** `silk format --check` selects a non-canonical complete file
- **THEN** no file is written and the file is reported as requiring formatting

#### Scenario: Check canonical files

- **WHEN** `silk format --check` selects only complete canonical files
- **THEN** no file is written and the command reports success

### Requirement: The format command uses stable exit classes

The format command SHALL exit zero when every selected file satisfies the requested write or check
mode, one when any selected source is damaged or `--check` finds non-canonical source, and two when
project discovery, path selection, source storage, or write operations fail. A semantic diagnostic
SHALL NOT change the format command's exit status.

#### Scenario: Report check drift

- **WHEN** `silk format --check` finds at least one complete non-canonical file
- **THEN** the command exits one

#### Scenario: Report damaged syntax

- **WHEN** any selected file has lexical or parser damage
- **THEN** the command exits one after classifying every safely readable selected file

#### Scenario: Report storage failure

- **WHEN** a selected source cannot be read or canonical bytes cannot be committed
- **THEN** the command exits two and identifies the affected path

#### Scenario: Ignore semantic rejection

- **WHEN** every selected file is syntactically complete and canonical but semantic analysis would reject the project
- **THEN** `silk format --check` exits zero because formatting does not run semantic analysis

### Requirement: Match expressions format as canonical arm blocks

Complete match syntax SHALL format with one space around its access mode and fat arrows, one arm per
line, two-space indentation inside the match body, canonical nested pattern spacing, and stable guard
placement before `=>`. Compact nominal patterns MAY remain on one line when they fit the existing
width policy; broken field-pattern lists SHALL use one item per line and a trailing comma. Formatting
MUST preserve arm order, bindings, guards, `..`, comments, and expression grouping.

#### Scenario: Format a multi-arm match

- **WHEN** complete source contains an irregularly spaced consuming match with two arms
- **THEN** formatting emits canonical mode spacing and one ordered indented arm per line

#### Scenario: Format twice

- **WHEN** canonical nested guarded match syntax is parsed and formatted again
- **THEN** the second output is byte-identical to the first

### Requirement: Formatting preserves generic context

Formatting SHALL render type parameter lists and generic applications canonically without changing
comparison grouping or reserved-template interpretation, and repeated formatting SHALL be
idempotent for valid and recovered generic syntax.

#### Scenario: Format nested applications idempotently
- **WHEN** a source contains nested generic applications and is formatted twice
- **THEN** the second output equals the first byte-for-byte and reparses to the same generic syntax

#### Scenario: Preserve damaged generic syntax
- **WHEN** a generic list is missing a closing bracket
- **THEN** formatting retains the explicit recovery boundary without consuming the following declaration
