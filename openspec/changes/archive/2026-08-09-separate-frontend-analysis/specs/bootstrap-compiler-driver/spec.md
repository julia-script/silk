## MODIFIED Requirements

### Requirement: Every phase reports its work

Each driver run SHALL produce one report with an entry per executed canonical compiler or
artifact-production phase, including the target-layout phase between instance discovery and MIR
lowering: elapsed time, input and output counts, diagnostic counts, and the engine-heap memory total
observed after the phase when the runtime can supply it. The driver's frontend and realization phase
names and ordering SHALL agree with the analysis facade for equivalent work. Reports are
observability data, not artifacts, and are exempt from byte identity.

#### Scenario: Report the full pipeline

- **WHEN** the driver compiles any valid request for a supported target
- **THEN** the report lists every executed phase in order, including target layout between instances and MIR, with elapsed time, input and output counts, diagnostic counts, and available memory totals

#### Scenario: Report a frontend rejection

- **WHEN** recoverable frontend analysis produces an error diagnostic
- **THEN** the report uses the canonical frontend phase order and contains no instance-discovery, target-layout, MIR, backend, or finalization entry

#### Scenario: Compare analysis and driver observations

- **WHEN** the analysis facade and driver process the same valid compilation request through runtime realization
- **THEN** their compiler-owned frontend and realization phase names occur in the same order before the driver continues to backend and artifact-production phases
