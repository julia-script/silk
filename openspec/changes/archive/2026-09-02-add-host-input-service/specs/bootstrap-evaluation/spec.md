## ADDED Requirements

### Requirement: Host input is an injected evaluator host

Evaluation SHALL accept an explicit host-input provider, separate from the OS filesystem and
standard-input hosts, exposing an argument count, an argument lookup by index, an environment lookup
by raw byte name, and a working-directory lookup, each returning a value, absence, or a host failure.
The evaluator MUST NOT import an ambient process command line, environment, or working directory into
browser-capable compiler cores, and MUST NOT commit more bytes than the caller's buffer holds.

#### Scenario: Evaluate against a scripted command line

- **WHEN** a program reads host input with an injected provider holding a scripted command line, environment, and working directory
- **THEN** each lookup answers from the script, commits only the prefix that fits, and reports the complete byte length

#### Scenario: Block a reachable lookup without a host

- **WHEN** evaluation reaches a host-input lookup and no provider was injected
- **THEN** it reports a blocked outcome naming the missing host rather than inventing an empty command line
