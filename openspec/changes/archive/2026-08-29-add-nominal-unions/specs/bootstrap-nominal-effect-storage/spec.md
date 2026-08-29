## ADDED Requirements

### Requirement: Active variants store concrete Effect environments lazily

An Effect-bounded field in a nominal-union variant SHALL use the same finite specialized runner,
environment, run-access, suspension, ownership, layout, cleanup, and storage-fence rules as an Effect
field in a nominal struct. Construction SHALL remain lazy, and only the active variant's Effect
environment SHALL exist, be runnable after pattern selection, or participate in cleanup.

#### Scenario: Store and run one selected Effect variant

- **WHEN** a concrete variant stores an Effect with owned captures and a consuming pattern selects it
- **THEN** construction runs nothing, selection transfers the exact environment once, and execution preserves its success, failure, requirement, access, suspension, and cleanup facts

#### Scenario: Preserve an unsupported Effect fence

- **WHEN** one reachable variant's Effect environment cannot be realized by every required phase and backend
- **THEN** the complete nominal-union application remains unavailable before MIR rather than gaining a standalone structural Effect ABI
