## ADDED Requirements

### Requirement: Inspect the syntax artifact

The syntax lab SHALL read its data from one `SyntaxFile` artifact: it SHALL present the complete
token stream including trivia and the surface tree, and SHALL highlight missing elements and
error regions distinctly from ordinary tokens and nodes.

#### Scenario: Present the token stream with trivia

- **WHEN** the inspected source contains whitespace, comments, and supported tokens
- **THEN** the lab lists every token of the artifact in source order with its kind and span, including trivia tokens

#### Scenario: Highlight recovered structure from the artifact

- **WHEN** the inspected source produces missing tokens and error regions
- **THEN** the lab's tree highlights each missing element and error region, sourced from the same artifact as the token stream
