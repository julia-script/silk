# Enforce return-contract soundness

Type: OpenSpec implementation handoff
Status: open
Blocked by: —
OpenSpec: [enforce-return-contract-soundness](../../../openspec/changes/enforce-return-contract-soundness/proposal.md)

Stop every invalid ordinary, effectful, generic, or conformance body at semantic analysis, keep it out of HIR/MIR, and close issue 226 with the correct diagnostic.
