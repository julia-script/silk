## MODIFIED Requirements

### Requirement: Explicit selected storage

Artifacts SHALL declare their required storage component, allocator, capacity configuration, state initialization and lifetime. Selection SHALL use generic roots and typed contracts without privileged library names, implicit environment reads or process-global accounting. Synchronous artifacts without storage demand SHALL acquire no storage component. Executable preparation SHALL close demanded component sources to a monotone fixed point inside the preparation request, sharing one resolver snapshot across every pass, and SHALL seal the bundle before any downstream operation; downstream realization, lowering and emission MUST NOT resolve, load or reopen source. Unused catalog entries SHALL remain unresolved and absent from the sealed closure. Unresolvable demand SHALL end preparation with a structured configuration failure rather than repeated passes.

#### Scenario: Independent library instances

- **WHEN** two library instances execute recursively or reentrantly
- **THEN** their capacity/accounting ownership remains explicit and independent; unsupported concurrent sharing is diagnosed

#### Scenario: Seal demanded storage once

- **WHEN** an executable's lowered program demands private frame storage whose provider is not yet loaded
- **THEN** preparation resolves the provider module exactly once, admits it, re-lowers, and seals a bundle whose downstream realization needs no resolver

#### Scenario: Leave an unused catalog unloaded

- **WHEN** the composition lists a storage component but the lowered program demands no storage
- **THEN** the component module is never resolved and is absent from the sealed closure
