## ADDED Requirements

### Requirement: Active variants store concrete callable representations inline

A callable-bounded field in a nominal-union variant SHALL use the same finite specialized callable
representation, invocation-access, capture ownership, layout, direct-target lowering, cleanup, and
storage-fence rules as a callable field in a nominal struct. Only the active variant's callable
environment SHALL exist, be invocable after pattern selection, or participate in cleanup.

#### Scenario: Store and invoke a capturing callable variant

- **WHEN** a concrete variant stores a capturing section and a borrowing pattern selects that variant
- **THEN** the selected field invokes its static target under the match access mode while inactive variants contribute no callable environment

#### Scenario: Preserve an unsupported callable fence

- **WHEN** one reachable variant's callable representation cannot be realized by every required phase and backend
- **THEN** the complete nominal-union application remains unavailable before MIR rather than falling back to a universal callable ABI
