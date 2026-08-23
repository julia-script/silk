## ADDED Requirements

### Requirement: Local shared control blocks use exact caller-funded allocation

The language SHALL provide a target-selected validated `Layout` for one local shared control block
over concrete `T`. The layout SHALL cover every private header field, padding, reclaim state, and
the initialized value, and SHALL remain nonzero for zero-sized `T`. Ordinary source SHALL request
that layout through its selected allocator before initialization; construction MUST NOT acquire an
allocator implicitly or retain the provider borrow in the result.

If the concrete header, padding, reclaim state, and `T` cannot be represented by the selected
target, the `sharedLayout<T>` specialization SHALL be unavailable before MIR or execution and SHALL
retain a stable diagnostic whose primary span is the intrinsic call. This compile-time target-layout
rejection MUST NOT become runtime `LayoutOverflow` data, a trap, or `OutOfMemoryError`.

An unsafe from-allocation transition SHALL consume exactly one active `Allocation` proven to match
the layout and exactly one value of `T`, initialize count one and available access, retain the
allocation's private reclaim authority, and publish exactly one affine local-shared core. A valid
transition SHALL have no typed failure and MUST NOT expose partial state. The reclaim authority SHALL
remain unnameable and usable only by eventual last-handle cleanup.

#### Scenario: Initialize from the exact requested layout

- **WHEN** ordinary source allocates the result of `sharedLayout<Token>()` and supplies that allocation and one `Token` to the unsafe initializer
- **THEN** one initialized core owns count one, the token, and the private reclaim authority while the allocation and source token bindings are consumed

#### Scenario: Reject mismatched layout provenance

- **WHEN** unsafe source supplies an allocation planned for another concrete type, target, size, or alignment
- **THEN** semantic or MIR verification rejects the initializer with a stable diagnostic at the initializer call and related provenance at the mismatched allocation, and publishes no usable local-shared core

#### Scenario: Preserve ordinary exhaustion cleanup

- **WHEN** the selected allocator rejects the control-block layout with `OutOfMemoryError`
- **THEN** no initializer runs, no shared core or reclaim obligation exists, and ordinary typed-failure cleanup cleans the still-owned value exactly once

#### Scenario: Allocate a zero-sized element control block

- **WHEN** `T` has zero size
- **THEN** the requested layout still contains distinct stable control-block storage and private lifetime state

#### Scenario: End allocator access before shared lifetime

- **WHEN** construction returns after the allocator provider loan has ended
- **THEN** the core remains valid and its eventual last cleanup can release storage through the retained self-contained authority
