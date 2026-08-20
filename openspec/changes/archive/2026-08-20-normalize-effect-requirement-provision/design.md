## Context

Requirement identity and access are currently entangled. The confirmed model needs stable keys for row algebra and a separate access relation for provider matching.

## Goals / Non-Goals

**Goals:** canonical keys; explicit roles with `at`; deterministic row algebra; exact provision; `provideEffect` naming.

**Non-goals:** optional dependencies, ambient defaults, dynamic service registries, or compatibility aliases.

## Decisions

1. Define a key as canonical service identity plus canonical optional role identity.
2. Store access demand alongside the key and merge repeated demands through the declared access lattice.
3. Perform provider selection by key, then validate access and acquisition contracts.
4. Implement subtraction once over normalized keys and reuse it for all provision helpers.
5. Make `flatten` union outer and inner requirements before normalization.
6. Rename the effectful helper to `provideEffect` and migrate source atomically.

## Risks / Trade-offs

- Role and access diagnostics may shift spans as facts become canonical; the source selector remains the primary location.
- Provider loan ownership must remain explicit even though access no longer participates in key identity.

## Migration Plan

Add key facts and normalization, adapt provider matching, migrate helpers and stdlib signatures, update HIR/MIR requirement records, migrate all callers, then remove access-bearing legacy selectors.

## Open Questions

Optional/default service behavior remains deliberately outside this change.
