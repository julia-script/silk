# Reconcile explicit Effect suspension

Type: implementation-handoff
Status: resolved
Blocked by: 05

Reconfirm that the foundational Effect audit did not invalidate the canonical
[effect-suspension rules](../../../docs/language/effect-suspension.md), then implement
`align-effect-suspension-coroutine-storage` without expanding the feature into async execution,
parking, or scheduling.

## Answer

The effect-suspension rules remained coherent with the confirmed Effect model. The implementation
replaced source allocator-visible continuation records with reusable compiler-owned coroutine
frames, preserved exact Effect channels and logical CallDepth, aligned evaluation/native/Wasm
behavior, passed the full repository and release-candidate checks, and was synced and archived as
`2026-08-19-align-effect-suspension-coroutine-storage`.
