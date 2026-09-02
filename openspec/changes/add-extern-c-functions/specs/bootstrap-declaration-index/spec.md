## ADDED Requirements

### Requirement: Foreign function headers join the canonical index

A foreign function declaration SHALL be indexed as a function header with a canonical identity,
ordinary visibility, an unsafe callable contract, the retained ABI and native symbol, an admitted
foreign signature, and no block body. Header resolution SHALL report foreign-ABI admission,
restriction, and symbol diagnostics at the header level, independent of the selected target, and a
rejected foreign header SHALL publish no callable. Body analysis SHALL skip a foreign header rather
than treat its missing body as an error.

#### Scenario: Index a foreign header

- **WHEN** a module declares `unsafe extern "C" fn abs(value: i32) -> i32`
- **THEN** the index records one function header with unsafe contract `(i32) -> i32`, ABI `C`, symbol `abs`, a foreign fact, and no block body, and body analysis reports nothing for it

#### Scenario: Collide with an ordinary declaration

- **WHEN** a module declares both `fn abs(value: i32) -> i32 { ... }` and `unsafe extern "C" fn abs(value: i32) -> i32`
- **THEN** the index reports the existing same-module collision diagnostic
