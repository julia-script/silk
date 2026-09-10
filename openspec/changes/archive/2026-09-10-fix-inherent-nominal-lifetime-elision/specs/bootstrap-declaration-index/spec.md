## ADDED Requirements

### Requirement: Inherent owner lifetimes elaborate before member publication

An inherent impl that omits its nominal owner's lifetime arguments SHALL introduce independent declaration-relative owner lifetime binders using the nominal declaration's completed lifetime arity. Whole-family validation SHALL include those binders while preserving ordinary generic arity and kind requirements. Associated members SHALL be available through owner-qualified lookup after this elaboration. A member's omitted nominal output lifetimes SHALL follow ordinary function elision; naming `Self` SHALL retain the fully applied impl owner and SHALL NOT replace its stored-data lifetimes with a fresh input's lifetime. Used explicit owner lifetime binders and implicit owner binders referenced through `Self` SHALL remain in the member's generic contract.

#### Scenario: Call an elided holder constructor

- **WHEN** `struct SliceStream<A> { slice: &[A] }` declares `impl<A> SliceStream<A>` with `make(slice: &[A]) -> SliceStream<A>`
- **THEN** `SliceStream.make(&values)` resolves and its result retains the input's lifetime
- **AND** declaration acceptance and callable publication agree without inspecting the constructor body to infer its public relationship

#### Scenario: Preserve fixed owner semantics of Self

- **WHEN** the constructor returns `Self` while its borrowed input has an independent elided lifetime
- **THEN** `Self` is closed to the fully applied impl owner
- **AND** returning a holder of that input is rejected unless the input satisfies the owner's declared lifetime

#### Scenario: Equivalent explicit owner lifetime

- **WHEN** the owner and impl explicitly declare a data lifetime and the constructor accepts that lifetime and returns `Self`
- **THEN** owner-qualified constructor inference succeeds and preserves the same input/result relationship as the omitted nominal result form
