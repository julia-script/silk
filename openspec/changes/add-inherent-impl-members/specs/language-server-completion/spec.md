## ADDED Requirements

### Requirement: Type-qualified completion lists associated members

After a nominal qualifier followed by `.`, completion SHALL list that declaration's associated
items: variants, enum members, generated operations, declared contract operations, and accessible
inherent members, each labeled by kind so a receiver method, an associated function, and a variant
are distinguishable. Inherent members SHALL present the same signature hover presents, including the
receiver as the first parameter. Private members SHALL be listed only inside their declaring module.

#### Scenario: Complete Option members

- **WHEN** completion is requested after `Option.` with `impl<T> Option<T>` declaring `none`, `some`, and `map`
- **THEN** the result lists `None`, `Some`, `none`, `some`, and `map` with `map` labeled as a method and `some` as an associated function

#### Scenario: Exclude a private member outside its module

- **WHEN** completion is requested after `Counter.` from another module and `Counter` has a private inherent member
- **THEN** the private member is absent and public members are present
