## ADDED Requirements

### Requirement: Lexical borrows may name stable temporary and subplace roots

The compiler SHALL assign stable logical identities to materialized temporaries and addressable subplaces, allow shared or exclusive borrows to be stored in local bindings, and preserve provenance through projections and calls. A borrow SHALL remain lexical and SHALL NOT escape its owner's valid lifetime.

#### Scenario: Borrow an array temporary for one call

- **WHEN** `read(&[1, 2])` uses the borrow only during the call
- **THEN** the compiler materializes a stable temporary root and accepts the call

#### Scenario: Mutate an indexed subplace through its original storage

- **WHEN** `edit(&mut matrix[index])` mutates the selected inner array
- **THEN** the loan retains the root and checked selector path and the caller observes the mutation in `matrix`

#### Scenario: Extend a hidden owner through a returned local view

- **WHEN** `identity(&[1, 2])` returns its one-source view into a local binding
- **THEN** the hidden owner remains live through that binding's last use and is cleaned after the loan ends

#### Scenario: Reject a returned local view

- **WHEN** a function returns a view borrowed from a local array
- **THEN** ownership reports that the view would outlive its owner

### Requirement: Callable sections admit every non-empty trailing suffix

For an `N`-parameter callable, supplying `K` arguments where `0 < K < N` SHALL bind those arguments to the callable's ordered trailing suffix and produce a callable awaiting the remaining ordered leading parameters. Sections MAY be applied in stages and SHALL move or borrow supplied arguments exactly once according to their parameter contracts. A section SHALL NOT bind holes or reorder parameters.

#### Scenario: Partially apply a binary function

- **WHEN** `add` has two parameters and source evaluates `add(2)`
- **THEN** the result is a callable accepting the remaining parameter and eventually computing `add(value, 2)`

#### Scenario: Stage a multi-parameter section

- **WHEN** `combine(a, b, c)` is applied as `combine(3)(2)(1)`
- **THEN** each application binds the next trailing parameter exactly once and the final invocation computes `combine(1, 2, 3)`

#### Scenario: End a reusable capture loan at last invocation

- **WHEN** a reusable callable's last statically known invocation occurs before its lexical binding ends
- **THEN** a non-escaping capture loan may end after that invocation while escaping, stored, or later-used callables retain the loan

## MODIFIED Requirements

### Requirement: Slice loans remain lexical and non-escaping

Direct shared and exclusive borrow bindings, materialized temporary owners, and stable projected
places SHALL use the same lexical loan and non-escape rules as call-scoped borrows.

#### Scenario: Store a lexical borrow locally

- **WHEN** a local binding stores `&values` and is used only within the owner's lifetime
- **THEN** ownership ends the loan at the local view's last use and restores compatible owner access

### Requirement: Callable capture loans follow the last safe use

Reusable callable capture loans SHALL end after the last statically known invocation or explicit
drop only when the callable is not subsequently copied, stored, returned, captured, or otherwise
escaped. Effect runs and callable invocations SHALL use the same conservative last-use policy.

#### Scenario: Retain a stored callable loan

- **WHEN** a callable is invoked and then copied or stored for later use
- **THEN** its capture loan remains active rather than ending at the earlier invocation
