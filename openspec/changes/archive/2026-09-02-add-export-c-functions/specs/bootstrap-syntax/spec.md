## ADDED Requirements

### Requirement: Exported function declarations parse losslessly and recover locally

The parser SHALL recognize `[pub] [static] [unsafe] export <text-literal> [effect] fn
<name>(<parameters>) [-> <type>] [as <text-literal>] { <statements> }` as one function declaration
node carrying an explicit export marker with its ABI literal and optional symbol literal, retaining
every token in source order; the `export` marker occupies the same slot as `extern`, after `unsafe`
and before `effect`. For recovery the parser SHALL retain a type parameter list, rows, `where`
clause, `effect`, `static`, or `unsafe` modifier and leave their rejection to semantic analysis. A malformed exported
declaration SHALL recover at the next top-level declaration start.

#### Scenario: Parse a renamed export

- **WHEN** the source spells `pub export "C" fn double(value: i32) -> i32 as "silk_test_double_v1" { return value * 2 }`
- **THEN** the result contains one complete function declaration with the public modifier, the export marker with ABI `"C"`, the symbol literal, one parameter, the return type, and the block body

#### Scenario: Recover from a missing body

- **WHEN** the source spells `export "C" fn f() -> i32` followed by a complete function
- **THEN** the parser reports one missing-body diagnostic inside the declaration and the following function parses completely
