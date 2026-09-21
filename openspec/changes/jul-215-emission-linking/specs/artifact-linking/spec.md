# artifact-linking Specification

## ADDED Requirements

### Requirement: Linking consumes a complete physical plan

`Linker.link` SHALL accept a validated-plan candidate, caller-owned build scope, native artifact
kind, durable destination, and explicit cache policy. It SHALL NOT discover or compile helper,
runtime, source, or translation-unit inputs.

#### Scenario: Fresh link

- **WHEN** cache reuse is disabled or misses
- **THEN** the linker validates and executes the plan, commits the output, and returns metadata with `reused: false`

#### Scenario: Cached link

- **WHEN** read/write cache contains bytes matching the requested kind and target
- **THEN** the linker commits those bytes without executing the link command and reports `reused: true`

### Requirement: Artifact lifetime and identity are visible

The result SHALL carry the durable artifact plus build-scope name, link-plan identity, optional cache
key, and reuse status.

#### Scenario: Build-scope cleanup after success

- **WHEN** linking succeeds and the caller closes its build scope
- **THEN** the durable destination remains while temporary plan inputs are removed
