## ADDED Requirements

### Requirement: Anonymous callable contracts use enclosing substitutions without back-inference

An anonymous callable contract MAY refer to type and row parameters declared by its enclosing
executable and SHALL specialize finitely with that executable. An anonymous callable MUST NOT
declare independent generic parameters. Its explicit callable type MAY contribute supplied-argument
evidence to enclosing generic call inference, but an expected result or later use MUST NOT infer or
alter omitted parameter, result, failure, or requirement annotations.

#### Scenario: Infer a higher-order call from an explicit anonymous argument

- **WHEN** a generic function receives an anonymous callable whose written parameter and result types provide supplied-argument evidence
- **THEN** ordinary call inference may use that explicit callable type and each resulting anonymous target remains monomorphic

#### Scenario: Do not repair an anonymous contract from expected context

- **WHEN** an expected callable result conflicts with the anonymous callable's written result contract
- **THEN** analysis reports the incompatibility instead of rewriting the contract or back-inferring a different body type
