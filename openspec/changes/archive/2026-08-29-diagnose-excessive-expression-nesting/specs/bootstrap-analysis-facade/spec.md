## ADDED Requirements

### Requirement: Excessive expression nesting remains queryable through the analysis facade

The executed analysis facade SHALL return a coherent frontend snapshot when a source module
exceeds the expression-nesting limit. The snapshot SHALL expose the recovered syntax and parser
diagnostics for that module, retain independently parseable following statements and declarations,
and answer supported queries outside the recovered region. Source-controlled excessive expression
nesting SHALL NOT escape from the facade as a host `RangeError` or other defect. Repeated analysis
of identical input SHALL return identical diagnostic and recovery data.

#### Scenario: Analyze an over-budget module

- **WHEN** `Analysis.ofSource` is executed for a module containing an expression substantially deeper than 256
- **THEN** the Effect succeeds with a frontend snapshot containing the recovered syntax and excessive-nesting diagnostic

#### Scenario: Query after recovered nesting

- **WHEN** a valid declaration follows an over-budget expression in the same analyzed module
- **THEN** facade queries for that declaration remain available independently of the recovered expression

#### Scenario: Repeat over-budget analysis

- **WHEN** identical over-budget source is analyzed repeatedly in fresh processes
- **THEN** the snapshot's syntax recovery and diagnostic sequence are identical across runs
