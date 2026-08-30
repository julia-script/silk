## ADDED Requirements

### Requirement: Duration-looking numeric text commits to recoverable literal recognition

When numeric source text is immediately followed by ASCII letters, the lexer SHALL commit the
complete contiguous duration-looking extent to duration-literal recognition. A valid extent SHALL
produce one duration-literal token. An extent with an unknown unit, non-whole or non-decimal
component, invalid digit separator, reordered or repeated unit, or out-of-range subordinate field
SHALL remain one invalid-duration token and SHALL produce one focused lexical diagnostic for the
first determinable violation. The lexer SHALL resume at the first byte that cannot continue that
extent, preserving lossless source coverage. Existing numeric spellings with no trailing duration
unit, including floating exponents such as `1e5`, SHALL retain their existing tokenization.

#### Scenario: Recognize a complete compound token

- **WHEN** source bytes spell `waitFor(1h30m30s)`
- **THEN** `1h30m30s` is one duration-literal token with its exact source span

#### Scenario: Commit an unknown unit

- **WHEN** source bytes spell `3sec`, `1H`, or `1h30x`
- **THEN** each complete duration-looking spelling is one invalid-duration token with one diagnostic naming its first unknown unit

#### Scenario: Commit an invalid numeric component

- **WHEN** source bytes spell `1.5s` or `0x10s`
- **THEN** each complete spelling is one invalid-duration token with one diagnostic stating that duration components must be whole decimal amounts

#### Scenario: Commit an invalid canonical compound

- **WHEN** source bytes spell `1h60m`, `30s1m`, or `1h2h`
- **THEN** each complete spelling is one invalid-duration token with one diagnostic identifying the first bound, ordering, or repetition violation

#### Scenario: Stop at an expression boundary

- **WHEN** source bytes spell `1h+30m`, `1h + 30m`, or `1h.member`
- **THEN** each duration token ends before the operator, trivia, or projection punctuation and following bytes tokenize independently

#### Scenario: Preserve an ordinary exponent literal

- **WHEN** source bytes spell `1e5` without a trailing duration unit
- **THEN** the source remains one floating-point literal rather than becoming an invalid duration

#### Scenario: Recover after an invalid duration

- **WHEN** an invalid duration-looking token is followed by punctuation and another valid expression
- **THEN** the invalid token and its diagnostic preserve their exact span and lexing continues with the following punctuation and expression
