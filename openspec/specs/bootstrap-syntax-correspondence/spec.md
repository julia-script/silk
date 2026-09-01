# bootstrap-syntax-correspondence Specification

## Purpose

Relates unchanged concrete syntax across adjacent immutable source revisions without replacing each
revision's deterministic source-qualified syntax identities.

## Requirements

### Requirement: Correspondence is source-owned and bidirectional

A syntax correspondence SHALL relate elements only between two syntax artifacts with the same
logical source identity. Every accepted pair SHALL be queryable from the previous element to the
current element and from the current element to the previous element. Foreign elements and syntax
artifacts with different logical source identities MUST remain unmatched.

#### Scenario: Relate one shifted declaration

- **WHEN** a declaration is unchanged but an insertion before it shifts its spans and preorder identities
- **THEN** the previous declaration and current declaration resolve to one another through the correspondence

#### Scenario: Reject different logical sources

- **WHEN** equal syntax is supplied under two different logical source identities
- **THEN** no correspondence artifact is produced

### Requirement: Correspondence preserves canonical snapshot identity

Each syntax artifact SHALL continue to assign its own deterministic source-qualified preorder
identity. Correspondence SHALL expose the previous and current canonical identities of each pair
without replacing either identity with process-local object identity or a mutable persistent ID.

#### Scenario: Observe shifted canonical identities

- **WHEN** an insertion changes the current preorder ordinal of an unchanged declaration
- **THEN** the correspondence reports the declaration's distinct previous and current canonical identities as one adjacent-revision pair

#### Scenario: Repeat correspondence construction

- **WHEN** the same previous and current source bytes are parsed and compared in fresh processes
- **THEN** the ordered canonical identity pairs and correspondence counts are identical

### Requirement: Structural matching is exact and conservative

An unchanged pair SHALL have the same concrete element family, syntax or token kind, missing-token
expectation where applicable, source bytes, and recursively ordered child structure. Matching MUST
remain conservative when equal candidates are ambiguous among siblings: it SHALL omit the pair
rather than guess. Changed subtrees SHALL remain unmatched even when they occupy the same span or
ordinal.

#### Scenario: Retain untouched sibling declarations

- **WHEN** one function is edited among structurally distinct sibling declarations
- **THEN** the exact unchanged sibling declarations and all elements below them correspond while the edited function subtree does not

#### Scenario: Leave duplicate insertion ambiguous

- **WHEN** an identical declaration is inserted beside existing structurally identical declarations and sibling evidence cannot distinguish them
- **THEN** those duplicate declaration candidates remain unmatched

#### Scenario: Compare recovered syntax

- **WHEN** adjacent malformed revisions contain unchanged exact recovery subtrees and changed damaged regions
- **THEN** exact unchanged recovery subtrees may correspond and changed damaged regions remain unmatched

### Requirement: Correspondence reports reuse evidence

A correspondence SHALL report immutable counts for previous elements, current elements, and exact
corresponding elements. Counts SHALL include every recursively corresponding node, token, and
missing token exactly once and SHALL NOT claim that unmatched or merely contextual elements were
reused.

#### Scenario: Measure an insertion

- **WHEN** correspondence is constructed after inserting one new declaration
- **THEN** its counts distinguish exact corresponding elements from newly parsed unmatched elements

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
