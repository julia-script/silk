## MODIFIED Requirements

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
