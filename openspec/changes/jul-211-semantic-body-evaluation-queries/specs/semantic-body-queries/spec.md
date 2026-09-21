# semantic-body-queries Specification

## ADDED Requirements

### Requirement: Body checking is a session request

The compiler SHALL execute `CheckBody` through the prepared semantic session and SHALL memoize an
immutable checked unit only within that session.

#### Scenario: Repeated body demand

- **WHEN** the same declaration body is demanded twice in one session
- **THEN** the provider executes once and both demands return the same checked unit

#### Scenario: Fresh correctness oracle

- **WHEN** the body is requested through the forced-fresh operation
- **THEN** the compiler recomputes without reading or publishing the session or cross-revision cache

### Requirement: Editor reuse remains dependency-sensitive

The compiler SHALL retain the existing cross-revision body artifact store and SHALL record nested
semantic reads, including negative lookups and evaluated static applications, as query observations.

#### Scenario: Presentation-only revision

- **WHEN** semantic inputs are unchanged but source presentation moves
- **THEN** the checked artifact is reused and current spans are presented

#### Scenario: Repaired missing dependency

- **WHEN** a previously missing observed member becomes available
- **THEN** the earlier checked answer is not reused
