## MODIFIED Requirements

### Requirement: Runtime reachability follows canonical unions

Instance discovery SHALL include each concrete normalized union appearing in a reachable contract,
local, aggregate, array, conversion, or cleanup plan and SHALL follow every normalized member
required to compute storage, calling shape, and cleanup. Equivalent spelling orders and nested
forms SHALL produce one instance-key type, one worklist entry, and one deterministic member
dependency order.

#### Scenario: Discover an aggregate-contained union

- **WHEN** a reachable struct field has type `Token | i32 | Array<i32, 2>`
- **THEN** discovery records the canonical union and follows all represented member layouts exactly once

#### Scenario: Discover a represented executable member

- **WHEN** a reachable union contains an exact callable or opaque Effect value with a finite capture environment
- **THEN** discovery follows that executable representation and every captured member layout required by its storage plan

#### Scenario: Deduplicate equivalent union spellings

- **WHEN** reachable contracts use both `Token | i32` and `i32 | (Token | i32)`
- **THEN** their normalized instance keys identify the same runtime type
