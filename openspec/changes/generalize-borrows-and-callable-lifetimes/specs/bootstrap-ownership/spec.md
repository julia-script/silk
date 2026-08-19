## ADDED Requirements

### Requirement: Lexical borrows may name stable temporary and subplace roots

The compiler SHALL assign stable logical identities to materialized temporaries and addressable subplaces, allow shared or exclusive borrows to be stored in local bindings, and preserve provenance through projections and calls. A borrow SHALL remain lexical and SHALL NOT escape its owner's valid lifetime.

#### Scenario: Borrow an array temporary for one call

- **WHEN** `read(&[1, 2])` uses the borrow only during the call
- **THEN** the compiler materializes a stable temporary root and accepts the call

#### Scenario: Reject a returned local view

- **WHEN** a function returns a view borrowed from a local array
- **THEN** ownership reports that the view would outlive its owner

### Requirement: Callable sections admit every non-empty leading prefix

For an `N`-parameter callable, supplying `K` leading arguments where `0 < K < N` SHALL produce a callable awaiting the remaining ordered suffix. Sections MAY be applied in stages and SHALL move or borrow supplied arguments exactly once according to their parameter contracts.

#### Scenario: Partially apply a binary function

- **WHEN** `add` has two parameters and source evaluates `add(2)`
- **THEN** the result is a callable accepting the remaining parameter and eventually computing `add(2, value)`

#### Scenario: End a reusable capture loan at last invocation

- **WHEN** a reusable callable's last statically known invocation occurs before its lexical binding ends
- **THEN** a non-escaping capture loan may end after that invocation while escaping, stored, or later-used callables retain the loan
