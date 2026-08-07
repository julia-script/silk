## ADDED Requirements

### Requirement: Semantic facts distinguish scopes, storage, and initialized values

Semantic analysis SHALL publish canonical facts for `Layout`, `SlotLayout<T>`, named scope identities,
allocator role requirements, affine allocations, unsafe lexical `Slot<T>` places, restricted drop
hooks, explicit consuming drop, and `OutOfMemory`. It SHALL retain unavailable facts and originating
diagnostics for invalid layouts, missing providers, incompatible slot types, inactive scopes, or
rejected provider/scope lifetime relationships rather than fabricating a valid allocation or slot.

#### Scenario: Resolve a valid typed allocation

- **WHEN** unsafe code allocates repeated storage for concrete `T` in an active named scope with a satisfiable allocator role
- **THEN** semantic facts retain the concrete element identity, slot layout, allocator role, scope identity, affine result, private origin relationship, and `OutOfMemory` failure

#### Scenario: Keep an invalid scope explicit

- **WHEN** an allocation names an inactive or unknown destination scope
- **THEN** its allocation fact is unavailable with the scope diagnostic and downstream phases receive no placeholder resource
