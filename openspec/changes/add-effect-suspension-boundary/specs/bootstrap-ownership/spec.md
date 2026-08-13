## ADDED Requirements

### Requirement: Suspension transfers one ownership obligation per live value

At a suspension boundary, ownership SHALL derive the exact live set needed after the deferred child
completes. Copy values MAY be copied; affine values SHALL move into one continuation slot; and
shared or exclusive borrows and provider references SHALL retain their existing lexical
dependencies until resumption or exit. A value MUST NOT remain independently owned by both the
suspended activation and its continuation frame.

#### Scenario: Hold one owner per recursive level

- **WHEN** every level of a suspended recursive Effect creates one affine owner used after its child completes
- **THEN** ownership moves each owner into exactly one continuation slot and rejects any duplicate use from the suspended activation

#### Scenario: Retain an exclusive provider dependency

- **WHEN** a suspended Effect holds an exclusive provider reference across its deferred child
- **THEN** ownership keeps the provider immovable and exclusively borrowed until the continuation resumes and ends the loan

### Requirement: Continuation cleanup preserves structured-exit semantics

On successful resumption, ordinary return, fallthrough, explicit structured exit, or typed failure,
each continuation SHALL move or clean every live source value exactly once in the existing lexical
order, then consume its private storage obligation exactly once without replacing the original
typed outcome. If continuation allocation fails before transfer, the current activation SHALL
retain and clean its values normally. A source trap or target defect that cannot return to the
runner SHALL retain the existing no-unwind guarantee: it MUST NOT report that source Drop ran or
duplicate an obligation. Any compiler-private continuation storage reached by an orderly runner
teardown after an internal defect SHALL be consumed exactly once without being reported as source
cleanup.

#### Scenario: Clean deep success in order

- **WHEN** suspended recursion succeeds while every level retains one owner
- **THEN** owners release exactly once in the same inner-to-outer order as the equivalent unsuspended execution

#### Scenario: Clean deep typed failure in order

- **WHEN** an inner suspended level fails with a typed payload while outer levels retain owners
- **THEN** every exited level releases its owner exactly once before the unchanged failure reaches its handler

#### Scenario: Preserve trap semantics

- **WHEN** a resumed suspended computation reaches a source trap
- **THEN** the runner exposes no typed failure or successful Drop trace and makes no claim that normal source cleanup ran

