# runtime-layout-plans Specification

## ADDED Requirements

### Requirement: Runtime plans consume instances explicitly

`Layout.computeRuntime` SHALL consume a type catalog and concrete instance graph and SHALL produce
only the reached layout entries plus runtime storage, environment, calling, execution, literal, and
diagnostic plans.

#### Scenario: Unreachable executable type

- **WHEN** a declared type is absent from every reachable instance
- **THEN** it is absent from the runtime plan even when present in the type catalog

#### Scenario: Concrete generic specialization

- **WHEN** reachability supplies a concrete generic type
- **THEN** the runtime operation completes its target layout and includes it in the plan
