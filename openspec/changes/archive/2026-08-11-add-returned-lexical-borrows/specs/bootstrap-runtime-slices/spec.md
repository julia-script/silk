## ADDED Requirements

### Requirement: Ordinary functions may return one-source lexical views

An ordinary function SHALL be permitted to return a shared or exclusive slice view only when the
result is proven to originate from exactly one borrowed parameter. A shared returned view MAY
originate from `&T` or `&mut T`; an exclusive returned view MUST originate from `&mut T`. Effect,
service, lazy, and capturing functions MUST NOT return borrowed views in this slice.

#### Scenario: Return a shared subview

- **WHEN** an ordinary function takes one shared slice parameter and returns a shared subview of it
- **THEN** the caller receives a lexical view whose origin and maximum lifetime are that parameter's source owner

#### Scenario: Reborrow an exclusive parameter as shared

- **WHEN** an ordinary function takes one exclusive slice parameter and returns a shared view of it
- **THEN** the returned shared view is accepted without granting exclusive access

#### Scenario: Reject exclusive strengthening

- **WHEN** an ordinary function takes only a shared parameter and attempts to return an exclusive view
- **THEN** analysis rejects the result because no exclusive origin exists

#### Scenario: Reject multiple possible origins

- **WHEN** a returned view may originate from either of two borrowed parameters
- **THEN** analysis rejects the function without inventing lifetime parameters or a merged origin

### Requirement: Returned views remain lexical and non-storable

A returned view SHALL be usable as a local lexical binding and as a compatible call-scoped reborrow.
Its lifetime MUST NOT exceed the lexical lifetime of its source owner. Lifetime-bearing references
and slices MUST remain forbidden in structs, arrays, unions, Effect success or failure values,
captures, and other owned storage.

#### Scenario: Use and release a returned local view

- **WHEN** a caller binds a returned view, reads it, and makes no later use of the view
- **THEN** the view's live range ends at its last use and the source owner becomes available under the ordinary borrow rules

#### Scenario: Reject escape from the owner

- **WHEN** control could preserve a returned view after its source owner's lexical scope ends
- **THEN** ownership rejects the escape at the boundary that would outlive the owner

#### Scenario: Reject storing a returned view

- **WHEN** source attempts to place a returned slice in a struct field or array element
- **THEN** analysis retains the stored-borrow prohibition and reports that the lifetime-bearing value is not an owned field value
