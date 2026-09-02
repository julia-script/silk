## ADDED Requirements

### Requirement: Foreign function declarations parse losslessly and recover locally

The parser SHALL recognize `[pub] [static] [unsafe] extern <text-literal> [effect] fn
<name>(<parameters>) [-> <type>] [as <text-literal>]` as one foreign function declaration node
retaining every token in source order; the `extern` marker occupies the slot after `unsafe` and
before `effect`. The ABI literal, the `as` literal, and the absence of a body SHALL be explicit in
the tree. For recovery the parser SHALL retain a `static` or `effect` keyword, a type parameter
list, failure row, requirement row, `where` clause, or block body that follows and SHALL leave
their rejection to semantic analysis. A malformed foreign declaration SHALL recover at the next
top-level declaration start without consuming it.

#### Scenario: Parse a renamed foreign declaration

- **WHEN** the source spells `pub unsafe extern "C" fn cAbs(value: i32) -> i32 as "abs"`
- **THEN** the result contains one complete foreign function declaration with the public modifier, the unsafe modifier, the ABI literal `"C"`, one typed parameter, the return type, the symbol literal `"abs"`, and no body

#### Scenario: Retain a body for semantic rejection

- **WHEN** the source spells `unsafe extern "C" fn f() -> i32 { return 1 }`
- **THEN** the parser produces one foreign function declaration containing the block body and no parser diagnostic

#### Scenario: Recover from a missing ABI literal

- **WHEN** the source spells `unsafe extern fn f() -> i32` followed by a complete function
- **THEN** the parser reports one missing-ABI diagnostic inside the foreign declaration and the following function parses completely
