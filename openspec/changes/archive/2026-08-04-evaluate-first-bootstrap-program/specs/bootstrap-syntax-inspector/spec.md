## ADDED Requirements

### Requirement: Evaluate the current bootstrap program
The Syntax Inspector SHALL provide an explicit browser-local evaluation action for the current
analyzed source. It SHALL display either the completed exact `I32` result or the closed blocked
reason and SHALL render the ordered evaluation trace with links to existing function, call,
argument, parameter, reference, and source provenance. Evaluation MUST NOT make a network request,
write files, persist results, or imply native compilation.

#### Scenario: Evaluate the canonical identity program
- **WHEN** a developer activates evaluation for `main` returning `identity(42)`
- **THEN** the inspector displays result `42` and an ordered trace matching the visible semantic data-flow path

#### Scenario: Inspect a blocked evaluation
- **WHEN** a developer evaluates a preset with a missing entry, wrong call arity, unavailable fact, or recursive cycle
- **THEN** the inspector shows the exact blocked reason, partial trace, and relevant source relationships without becoming unresponsive

#### Scenario: Edit after evaluation
- **WHEN** source text changes after an outcome is displayed
- **THEN** the stale outcome is cleared and the edited source must be analyzed before a new explicit evaluation

#### Scenario: Reload after evaluation
- **WHEN** the page reloads after a completed or blocked evaluation
- **THEN** the canonical source returns with no persisted result or trace
