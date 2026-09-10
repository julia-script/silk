## MODIFIED Requirements

### Requirement: Returned views preserve validity through ordinary storage

A returned view SHALL be usable in local bindings, compatible reborrows, shared borrowed aggregate storage, generic payloads, and valid callable or Effect captures. Every nested lifetime SHALL remain visible to compatibility, ownership, and escape checking. Retained uses MUST NOT exceed referent validity. Exclusive stored borrows SHALL preserve affine authority and dependent user Drop SHALL retain every observable payload lifetime through cleanup. Borrowed Effect success or failure values SHALL remain explicitly gated until their outcome proofs are implemented.

#### Scenario: Use and release a returned local view

- **WHEN** a caller binds a returned view, reads it, and makes no later use of the view
- **THEN** the view's live range ends at its last use and the source owner becomes available under the ordinary borrow rules

#### Scenario: Reject escape from the owner

- **WHEN** control could preserve a returned view after its source owner's lexical scope ends
- **THEN** ownership rejects the escape at the boundary that would outlive the owner

#### Scenario: Store a shared returned view

- **WHEN** source attempts to place a shared returned view in a struct field or array element
- **THEN** analysis accepts the lifetime-bearing payload and retains the view's loans through its containing value's uses

#### Scenario: Reject storing a returned view

- **WHEN** storing a view would permit a later use after its source storage becomes invalid
- **THEN** analysis rejects the escape and identifies the retaining aggregate path
