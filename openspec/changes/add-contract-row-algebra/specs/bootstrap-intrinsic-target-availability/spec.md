## ADDED Requirements

### Requirement: Analysis-only intrinsic availability is reachability-gated and target-independent

Intrinsic availability SHALL be represented as `Executable(nonEmptyTargets)` or
`AnalysisOnly(diagnosticIdentity)`. After reachable-instance discovery and before layout, MIR, or
lowering, a target-independent gate SHALL report every reachable `AnalysisOnly` dependency. An
unreachable dependency SHALL produce no availability diagnostic.

Reachability SHALL carry an ordered set keyed by static source edge, intrinsic operation, and
diagnostic identity, independent of owner specialization. Wrapper expansion SHALL retain the
outermost incoming user application as origin; a direct intrinsic call SHALL use its own edge.
Invalid syntax, kind, inference, or constraint diagnostics SHALL suppress dependent availability.

#### Scenario: Report a reachable wrapper locally

- **WHEN** a reachable ordinary wrapper expands to `AnalysisOnly(SEM0098)`
- **THEN** availability reports `SEM0098` at each distinct originating user application in canonical source order

#### Scenario: Ignore unreachable direct and wrapped dependencies

- **WHEN** direct and wrapped analysis-only calls occur only in unreachable declarations
- **THEN** reachable discovery admits neither call to the availability gate

#### Scenario: Preserve origins across instance deduplication

- **WHEN** two source applications reach one deduplicated instance containing an analysis-only dependency
- **THEN** one diagnostic is emitted for each distinct origin edge and repeated dependency paths from one edge are deduplicated
