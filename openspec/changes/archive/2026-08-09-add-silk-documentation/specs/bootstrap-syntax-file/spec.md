## ADDED Requirements

### Requirement: Syntax files expose raw documentation blocks

A syntax file SHALL expose immutable source-owned raw documentation blocks formed from its original
documentation tokens and intervening line-ending trivia. A block SHALL retain its declaration or
module kind, exact token objects, and complete source span. Building or querying a raw block SHALL
NOT normalize or parse Markdown content.

#### Scenario: Query an attached declaration block

- **WHEN** a syntax node has consecutive attached `///` tokens
- **THEN** the syntax file returns one declaration documentation block containing the original token identities and source span

#### Scenario: Preserve raw module bytes

- **WHEN** a module begins with `//!` documentation containing non-ASCII UTF-8 bytes
- **THEN** its raw block retains exact source-owned byte spans without decoding or normalizing them
