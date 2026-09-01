## ADDED Requirements

### Requirement: Type parameter and application syntax is contextual and recoverable

The parser SHALL represent angle-bracket type parameter lists after struct and function declaration
names, generic applications in type positions, and explicit specialization after a recognized
callee. Generic brackets MUST NOT consume comparison operators, and reserved JSX-like template
starts SHALL remain reserved only at primary-expression boundaries. Missing names, commas, closing
brackets, and type arguments SHALL remain explicit local syntax nodes and diagnostics.

#### Scenario: Parse a generic declaration and call

- **WHEN** source contains `pub fn identity<T>(value: T) -> T` and `identity<I32>(1)`
- **THEN** syntax records the declaration parameter and call specialization losslessly

#### Scenario: Preserve a comparison

- **WHEN** source contains `left < right`
- **THEN** the expression remains a comparison rather than a damaged generic application

#### Scenario: Keep a reserved template start distinct

- **WHEN** `<Panel />` appears where a primary expression begins
- **THEN** the parser preserves the reserved template start rather than treating `Panel` as a type argument
