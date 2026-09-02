## ADDED Requirements

### Requirement: Value-qualified completion lists receiver methods

After a typed value followed by `.`, completion SHALL list accessible fields and, for a nominal or
bounded generic subject, the receiver methods available to that subject: inherent receiver
methods, or for a generic subject the receiver operations of its bounds, labeled as methods and presented
with their receiver-bound signature. Associated functions without a receiver SHALL NOT be listed
after a value. Completion after a chained receiver such as `a.b.` or `f().` is outside this
requirement.

#### Scenario: Complete methods and fields on a value

- **WHEN** completion is requested after `option.` with `option: Option<i32>`
- **THEN** the result lists `map`, `flatMap`, and `unwrapOr` as methods with `T` shown as `i32`, and excludes `none` and `some`

#### Scenario: Complete a bound's operation on a generic value

- **WHEN** completion is requested after `value.` inside `fn show<T: Printable>(value: &T)`
- **THEN** the result lists `print` from the `Printable` bound and nothing from any concrete conformance
