## ADDED Requirements

### Requirement: Strong-handle transitions preserve one dynamic cleanup authority

Each successful clone SHALL add exactly one affine strong-handle obligation without copying, moving,
or cleaning `T`. A non-last explicit or structured drop SHALL discharge one obligation and perform
no payload cleanup. The drop that changes the count from one to zero SHALL exclusively clean `T`
exactly once and then release the retained allocation exactly once. Strong-count state SHALL remain
independent of access state, so clone and non-last drop MAY occur during active access without
creating another reference or changing the active access owner. A strong cycle SHALL remain live and
leak. Every acyclic graph whose handles are all discharged through structured execution SHALL reach
exact last cleanup. A fatal trap SHALL retain the language's no-unwind rule and MUST NOT claim that
live handles, payloads, or allocations were cleaned.

#### Scenario: Drop two handles in order

- **WHEN** one handle is cloned and the original is dropped before the clone
- **THEN** the first drop changes count two to one without cleaning `T`, and the second cleans `T` once before one allocation release

#### Scenario: Clean across typed-failure frames

- **WHEN** a deeper typed-failure frame drops its clone and the propagating caller later drops the original
- **THEN** the first cleanup only decrements and the caller's final cleanup destroys `T` and releases storage without replacing the failure payload

#### Scenario: Clone during access

- **WHEN** an active access callback clones its borrowed receiver through another live alias
- **THEN** the count increments while access remains active and no additional reference to `T` is created

#### Scenario: Leak a strong cycle

- **WHEN** external handles to an otherwise unreachable cycle of local shared cores are dropped
- **THEN** no count reaches zero and the cycle receives no payload cleanup or allocation release

### Requirement: Local shared access borrows are callback-scoped and non-escaping

Successful local shared access SHALL create one exclusive position-restricted borrow rooted in the
control block for exactly the ordinary callback invocation. Every competing reentrant access,
including shared-over-shared public wrappers derived from that exclusive operation, SHALL select the
conflict path before another reference is formed. The borrow SHALL end before access is restored and
before the result returns. It MUST NOT escape directly or through a generic result, aggregate,
failure value, Effect capture, callable capture, or suspended computation. Diagnostics SHALL retain
the access boundary and the attempted escape or suspension provenance.
Every direct, narrowed, generic, aggregate, failure, Effect, callable, or suspension rejection SHALL
use one stable local-shared-access diagnostic identity and retain the access-boundary span plus the
specific escape or suspension span.

#### Scenario: Return an ordinary value

- **WHEN** the access callback reads or mutates `T` and returns an owned result containing no restricted borrow
- **THEN** ownership ends the borrow, restores access, and permits later access through any live alias

#### Scenario: Reject a direct returned borrow

- **WHEN** the callback returns its `&mut T` parameter or a narrowed borrow derived from it
- **THEN** ownership rejects the result and relates the escape to the local shared access boundary

#### Scenario: Reject generic and executable escape

- **WHEN** the callback hides the borrow in a generic result, Effect, stored callable, aggregate, or failure payload
- **THEN** recursive ownership checking rejects the capture before executable lowering

#### Scenario: Reject suspension with active access

- **WHEN** a path attempts to suspend while the callback-scoped borrow remains live
- **THEN** ownership rejects the suspension and no suspended state owns the access loan

#### Scenario: Conflict every nested access combination

- **WHEN** public shared or exclusive access is nested under public shared or exclusive access through any alias
- **THEN** the nested call selects conflict and ownership never admits overlapping references
