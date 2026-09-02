## ADDED Requirements

### Requirement: Exported function declarations have one canonical layout

The formatter SHALL print an exported function as `[pub] export "C" fn <name>(<parameters>) ->
<type> [as "<symbol>"] {` followed by the ordinary block layout, with single spaces between
modifiers and the existing width-aware parameter layout. Formatting SHALL be idempotent and SHALL
retain attached comments.

#### Scenario: Format an export idempotently

- **WHEN** a source with irregular spacing declares `pub  export "C"fn double( value:i32 )->i32 as "silk_test_double_v1"{ return value * 2 }`
- **THEN** one pass yields the canonical form and a second pass is byte-identical
