## ADDED Requirements

### Requirement: TextMate scopes documentation forms and nested structure

The TextMate grammar SHALL assign distinct documentation scopes to `///` declaration comments and
`//!` module comments while keeping `//` ordinary comments distinct. Within documentation comments,
the grammar SHALL expose useful nested scopes for Markdown headings, emphasis, inline code,
intra-document links, fences, and Silk fenced content without changing compiler semantics.

#### Scenario: Scope declaration and module documentation

- **WHEN** a TextMate consumer tokenizes adjacent `//! module`, `/// declaration`, and `// ordinary` lines
- **THEN** all three forms receive distinct stable comment scopes

#### Scenario: Scope nested documentation markup

- **WHEN** a documentation block contains an `Examples` heading, ``[`Problem`]``, and a fenced `silk` block
- **THEN** the heading, link, fence, and nested Silk content receive theme-friendly scopes within the documentation comment
