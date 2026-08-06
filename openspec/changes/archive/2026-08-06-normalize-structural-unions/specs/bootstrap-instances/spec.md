## ADDED Requirements

### Requirement: Runtime reachability follows canonical unions

Instance discovery SHALL include each concrete normalized union appearing in a reachable contract,
local, aggregate, array, conversion, or cleanup plan and SHALL follow every nominal member required
to compute storage, calling shape, and cleanup. Equivalent spelling orders and nested forms SHALL
produce one instance-key type, one worklist entry, and one deterministic member dependency order.

#### Scenario: Discover an aggregate-contained union

- **WHEN** a reachable struct field has type `Token | End`
- **THEN** discovery records the canonical union and follows both nominal member layouts exactly once

#### Scenario: Deduplicate equivalent union spellings

- **WHEN** reachable contracts use both `Token | End` and `End | (Token | End)`
- **THEN** their normalized instance keys identify the same runtime type

