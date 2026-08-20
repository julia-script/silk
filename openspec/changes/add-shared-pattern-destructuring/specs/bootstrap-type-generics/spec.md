## ADDED Requirements

### Requirement: Generic pattern selectors renormalize after substitution

A generic body SHALL check pattern selectors against its symbolic normalized member set. Every
complete application SHALL substitute and renormalize selectors and coverage before MIR lowering.
When source-distinct selectors collapse to one concrete member, the first source-ordered reachable
selector SHALL win and later equivalent selectors SHALL emit no duplicate runtime test or new
source diagnostic.

#### Scenario: Collapse two selectors

- **WHEN** source-ordered `A` and `B` patterns over `A | B` specialize with both parameters equal to `i32`
- **THEN** MIR tests one `i32` member and selects the first source arm

#### Scenario: Preserve distinct selectors

- **WHEN** the same patterns specialize with `A = i32` and `B = bool`
- **THEN** both canonical members remain covered by their source-ordered selections
