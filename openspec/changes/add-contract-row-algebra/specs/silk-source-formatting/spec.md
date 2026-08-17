## ADDED Requirements

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
