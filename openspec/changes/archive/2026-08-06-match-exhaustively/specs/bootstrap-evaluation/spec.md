## ADDED Requirements

### Requirement: Evaluation dispatches matches by logical active member

Evaluation SHALL execute the match scrutinee exactly once, select arms in source order from the
logical active nominal member, evaluate matching guards in order, bind fields under the verified
access mode, and evaluate exactly one selected result. It MUST NOT inspect physical storage or
derive a different member mapping from backend tags.

#### Scenario: Fall through a rejected guard

- **WHEN** the active member matches a guarded arm whose guard is false and a later unguarded arm for that member exists
- **THEN** evaluation records the failed guard and evaluates only the later arm result

#### Scenario: Select a universal fallback

- **WHEN** no preceding nominal arm accepts the active member and `_` remains
- **THEN** evaluation selects the universal arm without changing the logical payload identity

### Requirement: Match traces preserve bindings and cleanup deterministically

Evaluation traces SHALL identify match entry, active canonical member, each attempted arm, guard
outcome, selected arm, pattern bindings, result, borrow end or ownership transfer, and active-field
cleanup with exact source provenance. Inactive members and unreachable arms SHALL produce no binding
or cleanup events.

#### Scenario: Trace consuming omitted-field cleanup

- **WHEN** a consuming arm moves one bound field and omits another
- **THEN** the trace records one payload transfer, the selected binding, exact omitted-field cleanup, and joined result in execution order

#### Scenario: Repeat a borrowed match trace

- **WHEN** the same shared match is evaluated repeatedly
- **THEN** its arm attempts, guard results, bindings, borrow end, result, and provenance are identical
