## ADDED Requirements

### Requirement: Dependent replacement preserves both cleanup obligations

Replacement SHALL check the unchanged destination type before a non-suspending cleanup/install commit. Incoming evaluation can commit moves or writes before failure; exits clean the actual initialized remainder without rollback. Missing destinations skip displaced cleanup and maybe-initialized destinations clean conditionally. A complete Drop-bearing field can move from a plain outer owner, but no move or consuming destructuring can cross a whole-value user Drop ancestor.

#### Scenario: Displaced storage cannot supply its replacement

- **WHEN** an incoming reference points into storage displaced by replacement
- **THEN** the replacement is rejected even through generic replacement or a shortened outer exclusive borrow

#### Scenario: Cleanup follows a failing incoming expression

- **WHEN** an incoming expression moves a disjoint field and then propagates a typed failure before installation
- **THEN** the moved value cleans at its new owner and the destination's still-initialized remainder cleans once without rollback

#### Scenario: Extract a complete dependent Drop child

- **WHEN** a complete initialized Drop-bearing child moves out of a plain outer owner
- **THEN** its new owner retains the child's dependencies and cleanup; moving a subfield across the child's own Drop boundary remains rejected

## MODIFIED Requirements

### Requirement: Borrowed-view loans remain lexical and non-escaping

Borrow requirements SHALL follow actual uses, transfer, copies, capture, and cleanup within a finite local control-flow domain. A returned view SHALL retain its source loans beyond its originating call whenever needed. Shared and exclusive references and slices SHALL be admitted in ordinary structs, unions, fixed arrays, generic wrappers, named tuples, and synthesized aggregates while preserving every nested semantic lifetime. Moving a holder SHALL transfer obligations and Copy SHALL duplicate dependents without detachment. Exclusive stored references SHALL remain affine, and dependent user Drop SHALL retain all observable payload lifetimes through cleanup. Borrowed Effect outcomes and suspension with partial owners SHALL remain rejected until their outcome and frame proofs are admitted. Lexically valid callable and Effect captures SHALL retain environment bounds immediately. No borrow SHALL outlive its referent or lose reborrow ancestry through abstraction.

#### Scenario: End a temporary loan after an ordinary call

- **WHEN** an exclusive whole-array borrow is passed to an ordinary function which returns without retaining a child dependent
- **THEN** the call loan ends and subsequent caller access to the mutable owner is permitted

#### Scenario: Preserve recursive storage of a shared slice

- **WHEN** a shared slice type appears directly or transitively inside an owned struct, union, array, or generic application
- **THEN** ownership retains the nested lifetime and accepts uses fitting source validity; an escape beyond that validity is rejected

#### Scenario: Reject a captured slice

- **WHEN** a lazy computation or callback would retain a borrowed view beyond its source root
- **THEN** ownership rejects the escape rather than ending the source loan prematurely

#### Scenario: Store a lexical borrow locally

- **WHEN** a local binding stores `&values` and is used only within the owner's lifetime
- **THEN** ownership ends the loan at the local view's last use and restores compatible owner access

#### Scenario: Reject recursive storage of a slice

- **WHEN** a shared slice is stored in a nested aggregate which escapes beyond its backing source validity
- **THEN** ownership rejects that escape and reports the nested retaining path
