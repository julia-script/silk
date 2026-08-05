## ADDED Requirements

### Requirement: Inspect ownership facts and cleanup plans

The docs site SHALL expose a direct-link ownership lab presenting, for each checked function of
the current snapshot: every binding with its ownership category and live range over source spans
as a per-binding timeline, the closed verdict with unavailable states explicit, and the cleanup
plan as an ordered release list per structured exit. The lab SHALL keep its state in browser
memory only.

#### Scenario: Inspect a binding timeline

- **WHEN** a developer inspects a function with typed parameters
- **THEN** the lab lists each binding with its ownership category and its live range's exact half-open spans

#### Scenario: Inspect the cleanup plan

- **WHEN** a developer inspects a frozen-slice function
- **THEN** the lab shows one return exit with an explicitly empty release list rather than omitting the plan

#### Scenario: Keep unavailable verdicts explicit

- **WHEN** the inspected source damages a function body
- **THEN** the lab marks that function's ownership verdict unavailable instead of showing a satisfied check
