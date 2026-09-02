## ADDED Requirements

### Requirement: Anonymous callable expressions have one canonical layout

The formatter SHALL render complete ordinary anonymous callables with `fn` and complete effectful
anonymous callables with `effect fn`, followed by the authored parameter list, arrow result,
optional failure and requirement rows, and canonical statement block. It SHALL apply the existing
width-aware parameter, type, row, and block policies without adding a name, capture list, inferred
invocation modifier, or declaration-only syntax. Formatting and reparsing SHALL preserve the same
dedicated anonymous syntax and SHALL be idempotent.

#### Scenario: Format an ordinary anonymous callable

- **WHEN** complete source contains an ordinary anonymous callable inside a call argument
- **THEN** formatting produces canonical `fn(parameters) -> Result { ... }` layout and a second pass is unchanged

#### Scenario: Format an effectful anonymous callable

- **WHEN** complete source contains an over-width effectful anonymous callable with failure and requirement rows
- **THEN** formatting breaks its signature and body by the existing canonical policies while preserving `effect fn` and every authored contract

