## ADDED Requirements

### Requirement: Differential gates enforce static Effect representation normalization

The continuous compiler corpus SHALL compare normalized and explicitly unnormalized synchronous
Effect programs through evaluation, optimized native entry structure, and direct WebAssembly entry
structure. Eligible cases SHALL preserve behavior and SHALL NOT retain foldable constructor calls or
an immediately materialized Effect environment. Ineligible controls SHALL preserve their ordinary
representation and behavior.

#### Scenario: Gate eligible constructor and run shapes

- **WHEN** direct map, flat-map, generic-provider, stored, and trapping cases compile
- **THEN** evaluator and Wasm behavior agree, native entries do not regress, and eligible direct-Wasm entries omit foldable constructor calls

#### Scenario: Keep an affine capture explicit

- **WHEN** an Effect environment directly captures an affine or exclusive value
- **THEN** the first normalization slice rejects that environment while the allocation-backed corpus preserves ordinary exactly-once Drop behavior

#### Scenario: Repeat structural evidence

- **WHEN** the normalization corpus runs in fresh compiler processes
- **THEN** behavioral results, verdicts, MIR, entry structures, and binary sizes are deterministic
