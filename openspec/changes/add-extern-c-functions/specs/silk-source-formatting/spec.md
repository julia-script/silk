## ADDED Requirements

### Requirement: Foreign function declarations have one canonical layout

The formatter SHALL print a foreign function declaration as `[pub] unsafe extern "C" fn
<name>(<parameters>) [-> <type>] [as "<symbol>"]` with single spaces between modifiers, the existing
width-aware parameter-list layout, and no trailing body. An omitted result type SHALL stay omitted.
Formatting SHALL be idempotent and SHALL retain attached comments.

#### Scenario: Format a foreign declaration idempotently

- **WHEN** a source with irregular spacing declares `pub   unsafe extern "C"  fn cAbs( value : i32 )->i32 as "abs"` under a line comment
- **THEN** one pass yields the canonical single-line form with the comment retained and a second pass is byte-identical
