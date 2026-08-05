## ADDED Requirements

### Requirement: Inspect the declaration index

The docs site SHALL expose a direct-link declaration-index lab presenting the collected headers
of a loaded closure: every declaration with its module, canonical identity state, and resolved
signature, in canonical index order, with duplicate and unavailable states explicit and the
header-level diagnostics listed in driver order. The lab SHALL keep its state in browser memory
only.

#### Scenario: Inspect headers across modules

- **WHEN** a developer selects a preset whose modules declare functions with resolved signatures
- **THEN** the lab lists every header in canonical order with its module, canonical identity, parameters, and return type

#### Scenario: Inspect duplicate and unavailable states

- **WHEN** a preset contains a duplicate declaration name and a declaration with a missing name
- **THEN** the duplicate header is marked as a caused duplicate of the original and the unnamed header is marked unidentified, while both remain listed

#### Scenario: Surface header diagnostics

- **WHEN** a preset contains an unknown parameter or return type
- **THEN** the lab lists the `SEM0001` diagnostic with its exact span in the unified panel
