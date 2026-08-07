## ADDED Requirements

### Requirement: Semantic facts retain exact pointer-sized values

Semantic analysis SHALL publish canonical `Usize` types and exact non-negative integer magnitudes
without narrowing them to `I32` or an imprecise host number. Target-independent facts SHALL remain
available before selection; target range validation SHALL publish an explicit available or
unavailable fact with originating diagnostics rather than substituting a truncated value.

#### Scenario: Preserve a 64-bit magnitude

- **WHEN** a contextual `Usize` literal exceeds JavaScript's safe integer range
- **THEN** semantic facts retain its exact digits and canonical value for later target validation
