# Enforce return-contract soundness

Type: OpenSpec implementation handoff
Status: resolved
Blocked by: —
OpenSpec: [2026-08-19-enforce-return-contract-soundness](../../../openspec/changes/archive/2026-08-19-enforce-return-contract-soundness/proposal.md)

Stop every invalid ordinary, effectful, generic, or conformance body at semantic analysis, keep it out of HIR/MIR, and close issue 226 with the correct diagnostic.
