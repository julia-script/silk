## ADDED Requirements

### Requirement: Exported function headers carry native export facts

A function declaration with an export marker SHALL be indexed as an ordinary function header with
the retained ABI, native symbol, and a classified C signature for the selected target. Header
resolution SHALL report foreign-ABI admission, restriction, and symbol diagnostics at the header
level, and a rejected exported header SHALL publish no callable and no export.

#### Scenario: Index an exported header

- **WHEN** a module declares `export "C" fn double(value: i32) -> i32 as "silk_test_double_v1" { ... }`
- **THEN** the index records one ordinary function header with export ABI `C`, symbol `silk_test_double_v1`, and signature `(i32) -> i32`
