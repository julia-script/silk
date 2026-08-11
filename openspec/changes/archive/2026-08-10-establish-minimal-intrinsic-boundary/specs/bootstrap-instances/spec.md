## ADDED Requirements

### Requirement: Instance discovery closes arbitrary service and primitive conformances

Instance discovery SHALL realize source-defined service witnesses and ordinary interface
conformances from canonical declarations rather than a compiler-known capability list. Generic
numeric wrappers SHALL specialize per concrete scalar conformance, and provided service operations
SHALL specialize per provider type and role without runtime nominal lookup.

#### Scenario: Discover a generic integer conformance

- **WHEN** a reachable function instantiates generic addition for two integer types
- **THEN** discovery records two concrete interface instances that select their respective intrinsic operations

#### Scenario: Discover a user service implementation

- **WHEN** a reachable provision binds a user-declared service to a conforming provider
- **THEN** discovery includes the mapped provider functions and no service-name-specific root
