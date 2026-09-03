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
indentation, spaces instead of tabs, LF physical line endings, no trailing whitespace outside
literal content, and exactly one final newline. The public formatter SHALL NOT accept options that
select another width, indentation, line-ending, or trailing-whitespace style. The width SHALL be a
breaking target for layout groups, not a requirement to rewrite or split an indivisible token,
documentation prose outside an active embedded Silk body, another preserved comment, or a
multiline literal body. An active embedded Silk body SHALL use the same canonical policy as ordinary
Silk source. Horizontal whitespace inside multiline literal content SHALL remain semantic content
rather than formatter layout.

#### Scenario: Normalize physical whitespace

- **WHEN** complete source contains tabs, CRLF line endings, trailing spaces outside literals, or multiple final newlines
- **THEN** its formatted bytes use two-space indentation, LF physical line endings, no non-literal trailing whitespace, and one final newline

#### Scenario: Ignore editor presentation preferences

- **WHEN** an editor requests a tab width or tab-based indentation that differs from canonical Silk style
- **THEN** the resulting Silk source still uses the canonical public policy

#### Scenario: Preserve an indivisible over-width spelling

- **WHEN** one identifier, literal, documentation-prose line, or other preserved comment is longer than the 100-column target
- **THEN** formatting preserves its spelling except for permitted physical CRLF normalization even though the resulting line exceeds the target

#### Scenario: Preserve trailing spaces inside literal content

- **WHEN** a complete multiline literal contains spaces immediately before a physical line ending
- **THEN** formatting retains those spaces as literal content while removing trailing whitespace from ordinary source layout

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
spaces or tabs and the body lines of active embedded Silk fences. Terminal spaces and tabs SHALL be
removed to satisfy the canonical no-trailing-whitespace policy unless they belong to multiline
literal content in an active embedded Silk body, in which case they SHALL remain semantic literal
content. Active embedded Silk body lines SHALL otherwise be replaced only with their canonical Silk
layout. A trailing line comment SHALL remain attached to the preceding grammatical element and
force the following element onto a new line. A standalone comment SHALL be indented to its
surrounding grammatical context. One or more consecutive `///` comments with no intervening blank
line immediately before a documentable declaration or conformance, field, type parameter, value
parameter, service operation, or implementation operation SHALL form that element's documentation
block and SHALL remain immediately before it after formatting. Leading `//!` comments SHALL remain
the module documentation block before module declarations. Formatting an embedded body SHALL
preserve its owning documentation block's marker kind, attachment, prose, fence delimiters and info
string, Markdown container prefixes, and surrounding blank lines.

#### Scenario: Preserve a trailing comment

- **WHEN** a line comment follows a complete statement on the same source line
- **THEN** its spelling remains after that statement, terminal horizontal whitespace is removed, and the next statement begins on a new line

#### Scenario: Indent a standalone comment

- **WHEN** a standalone line comment appears within a nested block
- **THEN** its spelling except terminal horizontal whitespace is emitted at that block's canonical indentation

#### Scenario: Retain a documentation block

- **WHEN** consecutive `///` comments immediately precede a documentable declaration or conformance, field, type parameter, value parameter, service operation, or implementation operation
- **THEN** they remain consecutive and immediately precede that same element in the formatted source

#### Scenario: Retain module documentation

- **WHEN** leading `//!` comments document a module
- **THEN** they remain consecutive at the module boundary before declarations

#### Scenario: Keep an unattached comment unattached

- **WHEN** a blank line separates a `///` comment from the following declaration or field
- **THEN** formatting preserves one separating blank line and does not attach the comment as documentation

#### Scenario: Preserve the documentation around an embedded body

- **WHEN** an active Silk fence appears inside an attached or module documentation block and its body requires canonical layout changes
- **THEN** formatting changes only the fenced body and ordinary outer layout while preserving the block's prose, marker kind, fence spelling and info string, Markdown container prefixes, and attachment

#### Scenario: Preserve embedded multiline-literal whitespace

- **WHEN** a physical documentation-comment line ends with spaces or tabs that belong to multiline literal content in an active Silk body
- **THEN** formatting preserves that horizontal whitespace as embedded literal content while continuing to remove terminal horizontal whitespace from ordinary comment content

### Requirement: Active Silk documentation fences use canonical source layout

The formatter SHALL interpret fenced CommonMark code blocks in attached declaration documentation
and leading module documentation. A fence SHALL be active Silk when CommonMark interprets its
case-sensitive language word as exactly `silk`; trailing CommonMark metadata SHALL NOT change that
classification. A language word such as `silk,ignore`, another language, or no language SHALL remain
opaque comment content. Every active Silk fence MUST have an authored matching CommonMark closing
fence. Its body SHALL be treated as one complete Silk source artifact, formatted with the same
canonical policy as its containing source, and written back into that containing `.silk` source
file. Reparsing the formatted body SHALL produce complete syntax with the same grammatical program
and decoded literal values as the authored body. If an active Silk body has lexical or parser damage
or lacks an authored matching closing fence, the formatter SHALL report a typed failure located in
the original containing source and SHALL produce no replacement bytes for that source artifact.

#### Scenario: Format a declaration example in source

- **WHEN** an attached `///` documentation block contains a syntactically complete closed fence whose CommonMark language word is exactly `silk` and whose body is not canonical
- **THEN** formatting rewrites that fenced body in the containing `.silk` source with canonical Silk bytes expressed through `///` lines

#### Scenario: Format a module example in source

- **WHEN** leading `//!` module documentation contains a syntactically complete active Silk fence
- **THEN** formatting rewrites that fenced body in the containing `.silk` source with canonical Silk bytes expressed through `//!` lines

#### Scenario: Format a nested documentation example

- **WHEN** an active Silk fence belongs to documentation attached to a field, type parameter, value parameter, service operation, or implementation operation, including a fence nested in a CommonMark container
- **THEN** its body is formatted and its documentation attachment and container prefixes remain intact

#### Scenario: Treat Silk metadata consistently with doctest

- **WHEN** a fence uses language word `silk` followed by CommonMark metadata such as the authored info string `silk ignore`
- **THEN** formatting treats the fence as active Silk because the metadata does not change its language word

#### Scenario: Preserve an ignored Silk fragment

- **WHEN** a documentation fence has the info string `silk,ignore`
- **THEN** formatting leaves the fence body byte-for-byte unchanged except for the existing terminal-whitespace and physical-line-ending policy

#### Scenario: Preserve another fenced language

- **WHEN** a documentation fence is unlabelled or CommonMark assigns it a case-sensitive language word other than exactly `silk`, including `SILK` and `silk,ignore`
- **THEN** formatting does not parse or rewrite its body as Silk

#### Scenario: Reject an unclosed active fence

- **WHEN** CommonMark recognizes an opening fence with language word `silk` but the documentation block contains no authored matching closing fence
- **THEN** formatting reports a typed malformed-active-fence failure at the original opening fence and produces no replacement bytes while ordinary documentation parsing remains total

#### Scenario: Reject damaged active Silk

- **WHEN** an active Silk documentation fence contains a lexical diagnostic, parser diagnostic, missing token, or unexpected-token error region
- **THEN** formatting reports a typed source-formatting failure associated with the outer fence and produces no replacement bytes for the containing source artifact

#### Scenario: Ignore semantic errors in embedded Silk

- **WHEN** an active Silk documentation fence is syntactically complete but contains unresolved names or other semantic errors
- **THEN** its body is formatted without running semantic analysis

#### Scenario: Preserve the embedded grammatical program

- **WHEN** a syntactically complete active Silk body is formatted and reparsed
- **THEN** it retains the same grammatical program and decoded literal values, including multiline literal leading and trailing whitespace

#### Scenario: Keep formatted empty lines inside documentation

- **WHEN** canonical embedded Silk contains an empty physical body line
- **THEN** the containing source represents it as a documentation-comment line with the owning marker and CommonMark continuation prefix rather than as an outer blank source line

#### Scenario: Keep excluded documentation trivia opaque

- **WHEN** a Silk-language fence occurs in an unattached `///` group, a nonleading `//!` group, or another comment outside the compiler's documentation attachment policy
- **THEN** its body remains opaque comment content and cannot cause embedded-source formatting failure

#### Scenario: Report one canonical result to every adapter

- **WHEN** the format command and whole-document language-server formatting receive the same complete source containing active Silk fences
- **THEN** both adapters use the reusable document-local formatter and produce byte-identical canonical source

#### Scenario: Check embedded drift without writing

- **WHEN** `silk format --check` selects a complete source whose outer layout is canonical but whose active Silk body is not
- **THEN** the command reports that source as needing formatting and performs no write

#### Scenario: Continue after embedded source damage

- **WHEN** `silk format` selects one file with a damaged active Silk body and another complete noncanonical file
- **THEN** the damaged file remains byte-identical and is reported with the source-rejection exit class while the other file is formatted

#### Scenario: Return no editor edit for embedded source damage

- **WHEN** whole-document language-server formatting receives a source with a damaged active Silk body
- **THEN** it returns no formatting edit for that document

#### Scenario: Reject all replacements when one active body is damaged

- **WHEN** one containing source has an earlier formatable active fence and a later damaged active fence
- **THEN** formatting produces no replacement bytes for the containing source artifact

#### Scenario: Locate nested embedded damage in original source

- **WHEN** a recursively embedded active Silk fence is damaged
- **THEN** the failure reports the original physical range of the top-level source fence that contains it and retains a source-relative path of nested fence ranges for the embedded levels

#### Scenario: Format embedded source idempotently

- **WHEN** source containing an active Silk fence is formatted, reparsed, and formatted again
- **THEN** the second result is byte-identical to the first and reports no change

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

Formatting SHALL preserve declaration and expression structure, identifier spellings, decoded
literal values, operator grouping, and comment order while changing only canonical layout,
optional list punctuation, and physical CRLF pairs that normalize to LF without changing literal
semantics. Reparsing formatted bytes SHALL produce complete syntax with the same grammatical
program and decoded literal values. Formatting an already formatted artifact SHALL return
byte-identical output and SHALL report that the source did not change.

#### Scenario: Reparse formatted output

- **WHEN** complete source is formatted and the resulting bytes are reparsed
- **THEN** reparsing has no lexical or parser diagnostics and retains the same grammatical program, decoded literal values, and comment sequence

#### Scenario: Format twice

- **WHEN** formatted bytes are parsed and formatted again
- **THEN** the second output is byte-identical to the first and is reported as unchanged

#### Scenario: Repeat in fresh processes

- **WHEN** equivalent complete syntax is formatted in fresh processes
- **THEN** every process produces byte-identical formatted output

### Requirement: Multiline literal bodies are protected formatter content

The formatter SHALL emit every complete multiline literal as one content-aware document region.
It SHALL preserve the modifier, delimiters, escapes, embedded line structure, indentation, blank
lines, and horizontal whitespace without dedenting, trimming, or reindenting the body. It SHALL
normalize physical CRLF pairs to LF and SHALL account for embedded line endings when deciding the
column and layout of following syntax. A damaged or unterminated literal SHALL continue to make the
syntax artifact ineligible for formatting.

#### Scenario: Format around an exact multiline body

- **WHEN** a complete binding contains a multiline literal whose content lines use deliberate unequal indentation
- **THEN** formatting canonicalizes the binding around the token while preserving the body indentation and decoded value exactly

#### Scenario: Track the closing delimiter column

- **WHEN** syntax follows a multiline literal's closing delimiter on the same physical line
- **THEN** width decisions use the closing delimiter's actual ending column rather than the literal token's total byte length

#### Scenario: Reject an unterminated multiline literal

- **WHEN** a syntax artifact contains the lexical diagnostic for a missing triple-quote delimiter
- **THEN** formatting returns its typed damaged-syntax failure and produces no replacement bytes

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

### Requirement: Constant declarations have one canonical layout

The formatter SHALL render a complete constant declaration as `[pub] const name: type = literal`,
preserving comments through the existing attachment policy and separating adjacent top-level
declarations with the canonical module spacing.

#### Scenario: Format a constant idempotently

- **WHEN** a complete constant declaration contains irregular spaces and attached comments
- **THEN** two formatting passes produce identical canonical source without changing tokens or comment content

### Requirement: Contract-row algebra has one canonical source layout

Formatting complete syntax SHALL print `Without<R, S>` with one space after its comma and preserve
ordinary union precedence inside each operand. A short `where` constraint list SHALL remain inline;
a list that exceeds the configured width SHALL break after `where` with one constraint per
continuation line and stable commas. Provider access tokens and explicit row-generic arguments SHALL
retain their source meaning. Formatting SHALL be deterministic, comment-preserving, and idempotent.

#### Scenario: Format a short constrained signature inline

- **WHEN** a complete declaration with `where S in R, &mut P provides S from R` fits the width
- **THEN** both constraints remain on one canonical line with normalized spacing

#### Scenario: Break a long constrained signature deterministically

- **WHEN** the signature and constraint list exceed the width
- **THEN** the formatter breaks after `where`, preserves constraint order and comments, and produces identical output on a second pass

#### Scenario: Preserve nested difference precedence

- **WHEN** a row contains `Without<R | Q, S | T>`
- **THEN** formatting does not introduce grouping that changes either operand or union membership

### Requirement: Scalar enum declarations have one canonical layout

The formatter SHALL preserve comments and source meaning while rendering optional visibility,
`enum`, an optional representation without interior padding, the enum name, braces, and ordered
members canonically. A multiline enum SHALL place one member per line, retain an explicit
` = <signed-decimal-literal>` when present, use canonical trailing commas, and remain idempotent.
Formatting SHALL NOT infer, insert, remove, or renumber discriminants or insert an omitted `u8`
representation.

#### Scenario: Format a represented enum

- **WHEN** a valid `enum(u8)` declaration contains explicit and implicit members with irregular whitespace
- **THEN** formatting produces canonical spacing and indentation while preserving the representation and every explicit discriminant

#### Scenario: Keep default representation omitted

- **WHEN** a default enum omits its representation clause
- **THEN** formatting remains idempotent without inserting `(u8)`

### Requirement: Nominal union declarations have one canonical layout

The formatter SHALL preserve comments and source meaning while rendering optional visibility,
`union`, the union name and type parameters, braces, source-ordered unit and named-field variants,
field visibility and types, separators, constructors, and patterns canonically. Multiline variants
and fields SHALL use deterministic indentation and trailing separators, and formatting SHALL remain
idempotent without changing variant or field identity.

#### Scenario: Format a generic mixed union

- **WHEN** a complete union contains unit and named-field variants with irregular whitespace
- **THEN** formatting emits one canonical generic declaration with stable variant and field indentation and preserves all comments

#### Scenario: Format an applied variant path

- **WHEN** construction or a pattern spells `Result<A, E>.Success { value }`
- **THEN** formatting preserves the applied parent before the dot and formats the field body under the ordinary struct-like policy

### Requirement: Type alias declarations have one canonical layout

The formatter SHALL render a complete type alias declaration as `[pub] type Name = <type>`, laying
out the target with the existing canonical type and union policies, preserving comments through the
existing attachment policy, and separating adjacent top-level declarations with the canonical module
spacing.

#### Scenario: Format a union alias idempotently

- **WHEN** a complete alias declaration whose target is a multi-member union contains irregular spaces and attached comments
- **THEN** two formatting passes produce identical canonical source without changing tokens or comment content

### Requirement: Foreign function declarations have one canonical layout

The formatter SHALL print a foreign function declaration as `[pub] unsafe extern "C" fn
<name>(<parameters>) [-> <type>] [as "<symbol>"]` with single spaces between modifiers, the existing
width-aware parameter-list layout, and no trailing body. An omitted result type SHALL stay omitted.
Formatting SHALL be idempotent and SHALL retain attached comments.

#### Scenario: Format a foreign declaration idempotently

- **WHEN** a source with irregular spacing declares `pub   unsafe extern "C"  fn cAbs( value : i32 )->i32 as "abs"` under a line comment
- **THEN** one pass yields the canonical single-line form with the comment retained and a second pass is byte-identical

### Requirement: Exported function declarations have one canonical layout

The formatter SHALL print an exported function as `[pub] export "C" fn <name>(<parameters>) ->
<type> [as "<symbol>"] {` followed by the ordinary block layout, with single spaces between
modifiers and the existing width-aware parameter layout. Formatting SHALL be idempotent and SHALL
retain attached comments.

#### Scenario: Format an export idempotently

- **WHEN** a source with irregular spacing declares `pub  export "C"fn double( value:i32 )->i32 as "silk_test_double_v1"{ return value * 2 }`
- **THEN** one pass yields the canonical form and a second pass is byte-identical

### Requirement: Pointer types have one canonical layout

The formatter SHALL print pointer types as `*const <type>` and `*mut <type>` with no space after
the star and one space before the pointee, idempotently.

#### Scenario: Format a pointer type

- **WHEN** source spells `* mut   u8`
- **THEN** formatting yields `*mut u8` and a second pass is byte-identical

### Requirement: Inherent impl declarations have one canonical layout

Formatting SHALL render an inherent impl as `impl[<Binders>] Owner[<Binders>] {` followed by its
members using the ordinary function-declaration layout with the same member separation the
conformance layout uses, and a closing brace on its own line. Doc comments and ordinary comments attached to the head or to a
member SHALL keep their attachment. Formatting SHALL be idempotent and MUST NOT rewrite an inherent
impl into a conformance or vice versa.

#### Scenario: Format a generic inherent impl

- **WHEN** the formatter processes `impl<T>   Option<T>{pub fn none()->Self{return Option<T>.None}}`
- **THEN** the output is the canonical multi-line layout and formatting the output again is unchanged

#### Scenario: Preserve member documentation

- **WHEN** a member inside an inherent impl carries a `///` doc block
- **THEN** the formatted output keeps the block immediately above that member

### Requirement: C-layout records format canonically

The formatter SHALL render a C-layout record header as `[pub ]extern "C" struct Name` with one space between modifiers and preserve the existing canonical struct body layout. Formatting SHALL be idempotent and SHALL retain malformed marker tokens losslessly during recovery.

#### Scenario: Format a C-layout record

- **WHEN** valid source contains irregular spacing around `pub extern "C" struct Timespec`
- **THEN** formatting emits the canonical header and existing canonical field indentation

#### Scenario: Reformat twice

- **WHEN** a C-layout record source is formatted twice
- **THEN** the second output is byte-identical to the first
