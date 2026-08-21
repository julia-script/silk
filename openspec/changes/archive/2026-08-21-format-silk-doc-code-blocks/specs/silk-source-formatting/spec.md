## MODIFIED Requirements

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

### Requirement: Comments retain content and stable attachment

Formatting SHALL preserve every line-comment, declaration-documentation, and module-documentation
token in source order and SHALL preserve the token's spelling byte-for-byte except for terminal
spaces or tabs and the body lines of active embedded Silk fences. Terminal spaces and tabs SHALL be
removed to satisfy the canonical no-trailing-whitespace policy unless they belong to multiline
literal content in an active embedded Silk body, in which case they SHALL remain semantic literal
content. Active embedded Silk body lines SHALL otherwise be replaced only with their canonical Silk
layout. A trailing line comment SHALL remain
attached to the preceding grammatical element and force the following element onto a new line. A
standalone comment SHALL be indented to its surrounding grammatical context. One or more
consecutive `///` comments with no intervening blank line immediately before a documentable
declaration or conformance, field, type parameter, value parameter, service operation, or
implementation operation SHALL form that element's documentation block and SHALL remain immediately
before it after formatting. Leading `//!` comments SHALL remain the module
documentation block before module declarations. Formatting an embedded body SHALL preserve its
owning documentation block's marker kind, attachment, prose, fence delimiters and info string,
Markdown container prefixes, and surrounding blank lines.

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

## ADDED Requirements

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
