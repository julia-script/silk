## ADDED Requirements

### Requirement: Generic ownership is checked once and specialized exactly

Ownership SHALL classify canonical type parameters through compiler-owned Copy and cleanup
properties, check whole-value moves and cleanup once on generic HIR, and substitute that proof for
each concrete instance. A specialization MUST NOT duplicate cleanup or re-check the source body with
concrete-only behavior.

#### Scenario: Specialize move-only and Copy uses
- **WHEN** a checked generic whole-value transfer is instantiated once with `I32` and once with a move-only struct
- **THEN** each instance receives the correct concrete copy or cleanup actions from one generic ownership proof

