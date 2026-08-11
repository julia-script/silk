## ADDED Requirements

### Requirement: Evaluation receives OS behavior through an injected adapter

The evaluator SHALL access operating-system file and directory operations only through an explicit
host adapter supplied with the evaluation request. The adapter SHALL implement the normalized
handle protocol and stable reason mapping. Compiler core initialization MUST NOT import or construct
a process filesystem implementation by default.

#### Scenario: Block a missing OS adapter

- **WHEN** evaluation reaches a supported OS intrinsic without an injected host adapter
- **THEN** evaluation returns deterministic blocked data identifying the unavailable host capability rather than touching ambient process APIs

#### Scenario: Preserve normalized adapter outcomes

- **WHEN** an injected host operation reports a low-level reason and native code
- **THEN** evaluation exposes those exact protocol outputs to ordinary `OsFileSystem` source

#### Scenario: Load the evaluator in a browser-capable bundle

- **WHEN** a browser consumer imports compiler and evaluator core modules without configuring OS support
- **THEN** module loading requires no Node filesystem module or equivalent ambient host API

