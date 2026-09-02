## ADDED Requirements

### Requirement: Receiver-syntax occurrences share the member identity

Definition, references, and rename SHALL treat the member name in `receiver.member(args)` as an
occurrence of the same identity as the member's declaration and its `Owner.member` forms. For an
interface-backed member call, definition SHALL offer the interface operation and the selected
conformance implementation as the same targets the explicit `Interface.op(...)` form offers.

#### Scenario: Rename across receiver and explicit forms

- **WHEN** `Option.map` is renamed to `transform`
- **THEN** `option.map(addOne)` becomes `option.transform(addOne)` in the same workspace edit as the explicit and section forms

#### Scenario: Navigate a bound method call

- **WHEN** the cursor is on `print` in `value.print()` inside `fn show<T: Printable>(value: &T)`
- **THEN** definition navigates to the `print` operation of `Printable`
