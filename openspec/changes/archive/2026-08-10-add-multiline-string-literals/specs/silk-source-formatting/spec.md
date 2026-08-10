## MODIFIED Requirements

### Requirement: Formatting has one public canonical policy

The system SHALL expose one public formatting policy with a 100-column target, two-space
indentation, spaces instead of tabs, LF physical line endings, no trailing whitespace outside
literal content, and exactly one final newline. The public formatter SHALL NOT accept options that
select another width, indentation, line-ending, or trailing-whitespace style. The width SHALL be a
breaking target for layout groups, not a requirement to rewrite or split an indivisible token,
preserved comment, or multiline literal body. Horizontal whitespace inside multiline literal
content SHALL remain semantic content rather than formatter layout.

#### Scenario: Normalize physical whitespace

- **WHEN** complete source contains tabs, CRLF line endings, trailing spaces outside literals, or multiple final newlines
- **THEN** its formatted bytes use two-space indentation, LF physical line endings, no non-literal trailing whitespace, and one final newline

#### Scenario: Ignore editor presentation preferences

- **WHEN** an editor requests a tab width or tab-based indentation that differs from canonical Silk style
- **THEN** the resulting Silk source still uses the canonical public policy

#### Scenario: Preserve an indivisible over-width spelling

- **WHEN** one identifier, literal, or preserved comment is longer than the 100-column target
- **THEN** formatting preserves its spelling except for permitted physical CRLF normalization even though the resulting line exceeds the target

#### Scenario: Preserve trailing spaces inside literal content

- **WHEN** a complete multiline literal contains spaces immediately before a physical line ending
- **THEN** formatting retains those spaces as literal content while removing trailing whitespace from ordinary source layout

### Requirement: Canonical formatting is deterministic and idempotent

Formatting SHALL preserve declaration and expression structure, identifier spellings, decoded
literal values, operator grouping, and comment order while changing only canonical layout,
optional list punctuation, and physical CRLF pairs that normalize to LF without changing literal
semantics. Reparsing formatted bytes SHALL produce complete syntax with the same grammatical
program and decoded literal values. Formatting an already formatted artifact SHALL return
byte-identical output and SHALL report that the source did not change.

#### Scenario: Reparse formatted output

- **WHEN** complete source is formatted and the resulting bytes are reparsed
- **THEN** reparsing has no lexical or parser diagnostics and retains the same grammatical program, decoded literal values, and comment sequence

#### Scenario: Format twice

- **WHEN** formatted bytes are parsed and formatted again
- **THEN** the second output is byte-identical to the first and is reported as unchanged

#### Scenario: Repeat in fresh processes

- **WHEN** equivalent complete syntax is formatted in fresh processes
- **THEN** every process produces byte-identical formatted output

## ADDED Requirements

### Requirement: Multiline literal bodies are protected formatter content

The formatter SHALL emit every complete multiline literal as one content-aware document region.
It SHALL preserve the modifier, delimiters, escapes, embedded line structure, indentation, blank
lines, and horizontal whitespace without dedenting, trimming, or reindenting the body. It SHALL
normalize physical CRLF pairs to LF and SHALL account for embedded line endings when deciding the
column and layout of following syntax. A damaged or unterminated literal SHALL continue to make the
syntax artifact ineligible for formatting.

#### Scenario: Format around an exact multiline body

- **WHEN** a complete binding contains a multiline literal whose content lines use deliberate unequal indentation
- **THEN** formatting canonicalizes the binding around the token while preserving the body indentation and decoded value exactly

#### Scenario: Track the closing delimiter column

- **WHEN** syntax follows a multiline literal's closing delimiter on the same physical line
- **THEN** width decisions use the closing delimiter's actual ending column rather than the literal token's total byte length

#### Scenario: Reject an unterminated multiline literal

- **WHEN** a syntax artifact contains the lexical diagnostic for a missing triple-quote delimiter
- **THEN** formatting returns its typed damaged-syntax failure and produces no replacement bytes
