## MODIFIED Requirements

### Requirement: MIR lowers composed Effect recipes completely

MIR lowering SHALL recursively realize every semantically valid Effect recipe nested beneath
`run`, including transformations whose protected recipe is provisioned, recovered, retried, or
acquired. Direct and stored forms MUST select complete deterministic runner identities, callable
environments, provider arguments, failure mappings, loan boundaries, and cleanup regions. Runner
discovery SHALL retain the specialized callback and provided service executions reachable through
ordinary generic aggregate projection and nominal-union pattern selection.

Generated-runner construction MUST be total for every reachable canonical runner specification,
including repeated concrete callers that select the same generic provided operation through
different callback environments. If an internal lowering invariant prevents one such runner from
being constructed, the compiler MUST preserve the canonical runner, base, owner, and first
causative statement or expression provenance instead of silently dropping the runner while
retaining references to it. That condition is a compiler failure rather than a source diagnostic,
and no backend SHALL receive the incomplete module. Valid source MUST NOT lower to an
unavailable-transform trap, an unpublished region, a missing referenced runner, or a compiler
implementation exception.

#### Scenario: Lower map around provision

- **WHEN** a run subject is an `Effect.map` whose protected Effect is a service-provision recipe
- **THEN** MIR contains a complete execution path for the protected runner, provider, and mapper with no unavailable region

#### Scenario: Lower provision around transformation

- **WHEN** a run subject provides a requirement after one or more transformations preserve it
- **THEN** MIR passes the provider through the transformed execution and closes its loan at the composed run boundary

#### Scenario: Lower a stored composed recipe

- **WHEN** the same recipe tree is stored in a binding before `run`
- **THEN** MIR preserves its eager construction facts and emits behavior equivalent to the direct recipe tree

#### Scenario: Retain a pattern-selected generic bracket runner

- **WHEN** a directly run ordinary bracket invokes a specialized local callback whose generic service provider is selected from an owned nominal-union member
- **THEN** MIR contains the exact callback runner, substituted provider witness, call shape, loan boundary, and cleanup path required by that reachable execution

#### Scenario: Lower a second provided caller specialization

- **WHEN** two concrete callers with distinct callback environments reach the same generic provided Effect operation
- **THEN** MIR publishes every referenced canonical provided runner exactly once with the exact captures, provider witness, rows, loans, cleanup, and result contract selected for each call

#### Scenario: Preserve a generated-runner lowering failure

- **WHEN** an internal invariant deliberately makes a reachable generated runner unavailable during a compiler regression test
- **THEN** the lowering result identifies the canonical generated runner, its base and owner, and the first causative source provenance before any backend is entered

#### Scenario: Reject an invalid composition before MIR

- **WHEN** types, failures, requirements, callable access, or ownership make a pipeline invalid
- **THEN** semantic analysis reports the relevant source diagnostic and MIR emission remains unavailable without a fallback trap
