# bootstrap-syntax-inspector Specification

## Purpose

A static compiler-inspection surface for source, syntax, HIR, MIR, ownership, targets, roots,
requirements, and diagnostics.

## Requirements

### Requirement: Inspection is static

Inspection SHALL render immutable compiler facts and SHALL NOT execute user code or expose runtime
values, outcomes, traces, terminals, blocked reasons, host interactions, or execution requests.

#### Scenario: Inspect a valid program

- **WHEN** a client requests every inspection projection
- **THEN** it receives static analysis and lowering facts only

### Requirement: Inspection uses serializable compiler facts

Every inspection response SHALL have a deterministic, byte-oriented serialization derived from the
analysis facade. Missing downstream facts SHALL be represented explicitly when diagnostics prevent
their construction.

#### Scenario: Inspect invalid source

- **WHEN** invalid source prevents MIR construction
- **THEN** syntax, available semantic facts, and diagnostics remain serializable without execution
