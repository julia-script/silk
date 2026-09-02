## ADDED Requirements

### Requirement: A called member hovers with its receiver-bound contract

Hovering the member name in `receiver.member(args)` SHALL present the member's contract with
parameter zero bound to the receiver and owner binders substituted from the receiver type, while
hovering `Owner.member` SHALL present the complete explicit-receiver contract. Both hovers SHALL
identify the same declaration and include its authored documentation.

#### Scenario: Hover a called method

- **WHEN** the cursor hovers `map` in `option.map(addOne)` with `option: Option<i32>`
- **THEN** hover shows `fn<U>(transform: once fn(i32) -> U) -> Option<U>` and the member's documentation

#### Scenario: Hover the explicit form

- **WHEN** the cursor hovers `map` in `Option.map(move option, addOne)`
- **THEN** hover shows the complete declaration, `pub fn map<T, U>(self: Option<T>, transform: once fn(T) -> U) -> Option<U>`, with the return type spelled as the declaration's module spells it
