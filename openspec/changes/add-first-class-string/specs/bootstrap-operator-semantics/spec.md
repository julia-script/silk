## ADDED Requirements

### Requirement: String equality compares exact text sequences

The equality and inequality operators SHALL accept two `string` operands and compare their exact
valid UTF-8 sequences without allocation, normalization, case folding, locale behavior, or storage
identity. Physically distinct views of identical bytes SHALL compare equal; canonically equivalent
but scalar-distinct text SHALL compare unequal.

#### Scenario: Compare distinct backing storage

- **WHEN** a static literal and an owned-string view contain the same Unicode scalar sequence
- **THEN** string equality reports true independently of their backing storage

#### Scenario: Compare unnormalized text

- **WHEN** two valid strings differ only by precomposed versus combining scalar spellings
- **THEN** equality reports false and inequality reports true
