# Delta: module tooling

## ADDED Requirements

### Requirement: Safe unused-import removal plan

For each unused valid binding, tooling SHALL offer a snapshot-bound SourceAction only when selector/delimiter, hybrid-clause, or whole-declaration ownership is deterministic. Whole-declaration spans SHALL begin at the import token or its indentation and MUST NOT consume parser-owned leading trivia or an adjacent declaration. The plan SHALL preserve the source line-ending convention. Comments whose attachment could change SHALL produce warning-only behavior.

#### Scenario: Mixed member list

- **WHEN** the last unused selector follows a used selector
- **THEN** its action removes only the delimiter and unused selector

#### Scenario: Hybrid import

- **WHEN** one namespace or selected-member binding is unused while another binding in the same declaration is used
- **THEN** the action removes only the unused alias, member-list clause, or selector

#### Scenario: Non-first whole declaration

- **WHEN** a whole unused import follows another declaration
- **THEN** its action removes only the import's own bytes and optional owned line ending

#### Scenario: Ambiguous comment ownership

- **WHEN** a comment occurs in adjacent selector, clause, or trailing-line trivia whose attachment could change
- **THEN** the unused binding has no removal plan
