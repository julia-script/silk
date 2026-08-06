## ADDED Requirements

### Requirement: Runtime reachability follows match patterns and results

Instance discovery SHALL follow the scrutinee type, every canonical nominal member named or covered
by an executable arm, recursively bound field types, guard and result expressions, joined result
type, and branch cleanup requirements. Unreachable arms SHALL contribute no runtime instance, while
equivalent match spelling SHALL preserve canonical worklist and dependency order.

#### Scenario: Discover a nested payload pattern

- **WHEN** a reachable match destructures `Token | End` and a `Token` field contains `Span`
- **THEN** discovery records the union, `Token`, `End`, `Span`, and required result and cleanup types exactly once

#### Scenario: Omit an unreachable arm

- **WHEN** a universal arm precedes a diagnosed unreachable nominal arm
- **THEN** the unreachable arm's otherwise-unused result and pattern types do not enter runtime reachability
