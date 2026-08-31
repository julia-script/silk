## ADDED Requirements

### Requirement: Referent projections preserve lossless syntax correspondence

Syntax formatting and correspondence SHALL preserve the distinct referent-projection node, its dot
and star tokens, surrounding trivia, and its position inside a repeated projection chain. Editing a
referent projection SHALL reuse unaffected surrounding syntax identities.

#### Scenario: Round-trip a referent projection

- **WHEN** `self.*` is parsed, formatted, and parsed again
- **THEN** the resulting tree retains one referent-projection node with the same token order

#### Scenario: Retain surrounding projection correspondence

- **WHEN** one component of `items[index].*.field` is edited
- **THEN** unaffected index and field projection syntax remains corresponded
- **AND** the referent projection remains distinct from multiplication
