## ADDED Requirements

### Requirement: Evaluation consumes the MIR layout plan

Evaluation SHALL accept the target-aware MIR program produced by the snapshot and SHALL treat its
canonical target and verified layout table as program facts. The interpreter MUST NOT derive,
default, or accept a second representation plan. Structured scalar values remain logical interpreter
values; the layout table does not require simulation of raw bytes when no operation observes them.

#### Scenario: Evaluate with the shared scalar plan

- **WHEN** a branching program is evaluated from a snapshot using the canonical `I32` and `Bool` entries
- **THEN** evaluation uses that MIR program and completes with its logical result without creating an interpreter-specific layout

#### Scenario: Block malformed target-aware MIR before execution

- **WHEN** a MIR program omits the layout of a runtime type used by an operation
- **THEN** MIR verification reports the inconsistency and the interpreter does not execute the malformed program
