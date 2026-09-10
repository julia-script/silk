## ADDED Requirements

### Requirement: A concrete receiver call records the operation it selected

Resolving a receiver call through a conformance SHALL record the selected capability, concrete
provider, and operation on the call's semantic facts. The operation occurrence SHALL carry the
identity of the interface operation the call selected, so it agrees with the same operation named
through a bounded generic receiver and resolves to its declaration.

#### Scenario: Identity matches the bounded spelling

- **WHEN** `report.print()` and a bounded `value.print()` select the same operation of `Printed<i32>`
- **THEN** both occurrences of `print` carry the identity of the operation declared in `Printed`

#### Scenario: Navigation reaches the interface declaration

- **WHEN** navigation targets `print` in `report.print()`
- **THEN** it resolves to the operation declared in `Printed`, not to the conformance's mapped implementation
