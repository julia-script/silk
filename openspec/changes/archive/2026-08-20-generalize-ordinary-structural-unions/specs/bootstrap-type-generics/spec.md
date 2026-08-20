## ADDED Requirements

### Requirement: Monomorphic ordinary unions renormalize after substitution

Every complete generic application SHALL substitute its concrete ordinary type arguments into each
reachable union and normalize the resulting member set before instance discovery, layout,
ownership, or lowering. Members that become identical SHALL collapse to one member; canonical order,
conversion mappings, and runtime tags SHALL be recomputed from the concrete set. Generic checking
SHALL NOT require symbolic parameters to prove they remain distinct under every specialization.

#### Scenario: Collapse a generic union to one member

- **WHEN** a declaration containing `A | B` is specialized with `A = i32` and `B = i32`
- **THEN** the concrete application carries `i32` and no union layout or duplicate runtime tag is produced

#### Scenario: Preserve distinct specialized members

- **WHEN** the same declaration is specialized with `A = i32` and `B = string`
- **THEN** the concrete application carries the canonical `i32 | string` member set and recomputed mappings
