## ADDED Requirements

### Requirement: Nominal union syntax is lossless and recoverable

The parser SHALL retain optional declaration visibility, the `union` keyword, name, optional type
parameters, ordered unit and named-field variants, field visibility and types, separators, braces,
comments, and unavailable recovery elements in one lossless union CST. Expression and pattern
syntax SHALL parse a nominal union path with an optional contiguous explicit generic prefix followed
by a dot and variant name. A constructor MAY then have a named-field body; a pattern SHALL use a
complete applied parent and MAY have the selected variant's named-field pattern body.

#### Scenario: Parse a generic mixed union

- **WHEN** source declares `union Option<T> { None, Some { pub value: T } }`
- **THEN** the CST retains the parent type parameter and distinct unit variant, field variant, and field nodes with exact spans

#### Scenario: Parse an applied variant constructor

- **WHEN** an expression spells `Result<i32, Problem>.Success { value: 42 }`
- **THEN** the CST treats `Result<i32, Problem>` as the applied parent qualifier and `Success` as its variant rather than attaching the arguments to a detached member

#### Scenario: Parse a constructor with an omitted parent suffix

- **WHEN** an expression spells `Option.Some { value: 42 }`
- **THEN** the CST retains `Option` as the unapplied parent declaration path and leaves generic completion to semantic field inference

#### Scenario: Recover within one damaged variant

- **WHEN** a named-field variant has a missing field type or closing brace beside valid sibling variants
- **THEN** recovery remains within that declaration and preserves the valid siblings as available syntax
