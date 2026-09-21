# Spec Delta

## Purpose

Defines coherent dependency-based revision reuse for complete checked units, static applications, residual programs, and ownership results.

## ADDED Requirements

### Requirement: Complete checked units are reconstructible queries

The compiler SHALL represent each complete checked unit as one reconstructible query addressed by stable semantic identity. The result SHALL include every hidden body and diagnostic required by downstream phases, while source positions and current-revision presentation data MUST NOT participate in the stable address or stored semantic answer.

#### Scenario: Unchanged body moves in the source

- **WHEN** a body retains the same semantic inputs across adjacent revisions but its source position changes
- **THEN** the compiler reuses the checked unit and presents its facts and diagnostics against the current syntax revision

#### Scenario: Hidden generated body changes

- **WHEN** an input used by a hidden or generated body changes
- **THEN** validation rejects the complete checked-unit result and recomputes the affected query

### Requirement: Validation uses actual ordered dependencies

Checked-unit, evaluation, residual-program, and ownership queries SHALL record the semantic inputs and nested query results they actually read. Revision admission SHALL validate those dependencies in recorded order, stop at the first changed dependency, treat missing current input as changed, and admit a prior result only when its result fingerprint remains equal.

#### Scenario: Equal child result cuts off invalidation

- **WHEN** a nested dependency recomputes because one of its inputs changed but produces the same semantic result fingerprint
- **THEN** a dependent query remains eligible for reuse without validating later dependencies unnecessarily

#### Scenario: Dependency disappears

- **WHEN** a recorded input or nested query is absent from the current revision
- **THEN** the prior result is rejected rather than treated as unchanged

### Requirement: Evaluation and residual construction are distinct query families

Compile-time execution and residual-program construction SHALL use distinct reconstructible query families with stable application identities. Each family SHALL expose its own dependencies, result fingerprint, diagnostics, and recorded work cost; neither family may infer validity from the other family's cache presence.

#### Scenario: Static value remains stable while residual input changes

- **WHEN** a static application produces the same value but an input read only during residual construction changes
- **THEN** evaluation may reuse while residual construction is independently validated and recomputed

### Requirement: Evaluation reuse obeys budget and policy

Revision validation itself SHALL consume no evaluation budget. A reused evaluation or residual result SHALL charge its recorded deterministic work cost exactly once to the current request. Evaluation policy, normalized profile, limits, and every static application input that can change admission or diagnostics SHALL participate in query validity.

#### Scenario: Reused result exceeds the current budget

- **WHEN** a valid prior evaluation result has a recorded cost greater than the current request's remaining budget
- **THEN** the compiler reports the ordinary evaluation-limit outcome without executing the application and without partially charging the result more than once

#### Scenario: Policy changes

- **WHEN** evaluation limits or another normalized evaluation policy input changes
- **THEN** a prior evaluation result is not admitted solely because its source and arguments are unchanged

### Requirement: Ownership is derived through the shared validator

Ownership results SHALL be reconstructible queries over complete checked units and the exact semantic facts they inspect. Ownership reuse SHALL be admitted only by the shared revision validator and SHALL preserve current-revision diagnostic presentation.

#### Scenario: Unrelated body changes

- **WHEN** a revision changes a body that is not read by an ownership query
- **THEN** the unaffected ownership result remains reusable

#### Scenario: Ownership-relevant fact changes

- **WHEN** a checked-unit fact read by ownership changes
- **THEN** the ownership query is rejected and recomputed with current facts

### Requirement: Abort, cycles, recovery, and fresh mode remain coherent

An interrupted or failed query execution MUST publish no reusable result. Cyclic queries SHALL produce the existing deterministic cycle outcome without admitting partial work. Parser-recovery states SHALL remain explicit dependencies. Forced-fresh execution SHALL bypass current-session and prior-revision reuse for the root query and every nested query it executes.

#### Scenario: Interrupted body check

- **WHEN** checked-unit construction is interrupted after reserving its query address
- **THEN** no partial checked unit or dependency record becomes visible to a later request

#### Scenario: Nested fresh evaluation

- **WHEN** a complete checked-unit query is requested in forced-fresh mode and invokes evaluation and ownership queries
- **THEN** every nested query executes without a current-session or prior-revision hit
