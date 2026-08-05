## MODIFIED Requirements

### Requirement: Inspect ownership facts and cleanup plans

The docs site SHALL expose a direct-link ownership lab presenting, for each checked function of
the current snapshot: every binding — parameter or `let` — with its ownership category and live
range over source spans as a per-binding timeline, consuming moves marked where liveness ends
early, the closed verdict with unavailable and violation states explicit, and the cleanup plan
as an ordered release list per structured exit. The lab SHALL keep its state in browser memory
only.

#### Scenario: Inspect a binding timeline

- **WHEN** a developer inspects a function with typed parameters
- **THEN** the lab lists each binding with its ownership category and its live range's exact half-open spans

#### Scenario: Inspect the cleanup plan

- **WHEN** a developer inspects a frozen-slice function
- **THEN** the lab shows one return exit with an explicitly empty release list rather than omitting the plan

#### Scenario: Keep unavailable verdicts explicit

- **WHEN** the inspected source damages a function body
- **THEN** the lab marks that function's ownership verdict unavailable instead of showing a satisfied check

#### Scenario: Inspect a let binding's shortened liveness

- **WHEN** a developer inspects a function that binds a value and moves it before returning
- **THEN** the timeline shows the binding's live range ending at the move and the exit's release list omitting it

#### Scenario: Inspect a use-after-move violation

- **WHEN** the inspected source moves a binding and reads it again
- **THEN** the lab marks the verdict as a violation, shows the `OWN0001` diagnostic's span in the timeline, and still lists every binding's range
