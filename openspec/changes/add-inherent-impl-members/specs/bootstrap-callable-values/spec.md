## ADDED Requirements

### Requirement: Associated members are first-class callable values

A resolved associated member SHALL be usable as a callable value under the same rules as a named
function: `Owner.member` is a function item whose contract lists the receiver as parameter zero
when present, `Owner.member(trailing)` with a non-empty trailing suffix forms an ordinary section
awaiting the leading prefix, and `value |> Owner.member(trailing)` applies that section. Sections
of associated members SHALL capture ownership modes, preserve evaluation order, and monomorphize
exactly as sections of root functions do. No method-specific currying or partial-application form
SHALL exist.

#### Scenario: Reference a receiver method as a function item

- **WHEN** `Option.map` is passed where `fn(Option<i32>, once fn(i32) -> i32) -> Option<i32>` is expected
- **THEN** the function item satisfies that contract with the receiver as its first parameter

#### Scenario: Pipe through an associated-member section

- **WHEN** source evaluates `Option.some(2) |> Option.map(addOne)`
- **THEN** `Option.map(addOne)` captures `transform` and the pipeline supplies the receiver, resolving to the same member as `Option.map(Option.some(2), addOne)`

#### Scenario: Section an associated function without a receiver

- **WHEN** `Pair.make(a, b)` is referenced as `Pair.make(2)`
- **THEN** it produces a unary section over `a` with `b` captured once
