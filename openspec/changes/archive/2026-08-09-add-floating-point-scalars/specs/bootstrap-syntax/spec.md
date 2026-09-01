## ADDED Requirements

### Requirement: Floating literal syntax is lossless

The parser SHALL preserve decimal point, exponent marker/sign, leading sign, digits, trivia, recovery elements, and exact spans without rounding during syntax construction.

#### Scenario: Parse exponent notation

- **WHEN** source contains `-1.25e-3`
- **THEN** syntax retains every component as one recoverable expression
