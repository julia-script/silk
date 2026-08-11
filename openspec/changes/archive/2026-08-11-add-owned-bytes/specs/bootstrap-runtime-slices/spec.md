## ADDED Requirements

### Requirement: Value borrows preserve stable field projections

An ordinary call-scoped value borrow MAY project through resolved nominal fields rooted in a stable
local, pattern binding, or borrowed parameter. The borrow SHALL retain that field path rather than
copying the projected value, and evaluator, native, and direct-Wasm execution MUST address the same
projected storage. Exclusive projection through a parameter SHALL require an exclusive reference.

#### Scenario: Forward a shared field view

- **WHEN** an ordinary wrapper borrows one field through `&T` and forwards a returned shared view
- **THEN** the view remains tied to the wrapper owner and reads the field's underlying storage without copying

#### Scenario: Mutate through an exclusive field view

- **WHEN** an ordinary wrapper borrows one field through `&mut T` and forwards an exclusive view
- **THEN** mutations update only the projected field storage and preserve adjacent narrow scalar elements

#### Scenario: Reject exclusive projection through shared access

- **WHEN** source attempts an exclusive field borrow rooted in `&T`
- **THEN** semantic analysis rejects the borrow without synthesizing stronger access
