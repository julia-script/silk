## ADDED Requirements

### Requirement: Lowering constructs MIR from elaborated instances

Lowering SHALL construct one MIR program module from the discovered instances in discovery
order: each instance's HIR body linearized into basic blocks in evaluation order (arguments
before their call), concrete drops and cleanup edges inserted exactly as the ownership phase's
cleanup plan directs (none in the frozen slice, where every exit releases nothing), and source
provenance attached to every lowered operation. An instance whose HIR body is unavailable SHALL
lower to an explicit generated trap rather than a fabricated body. Lowered programs SHALL verify
clean against the MIR structural verifier and SHALL encode deterministically, gated by committed
golden files.

#### Scenario: Lower a nested call program

- **WHEN** `main` returning `identity(identity(42))` is lowered
- **THEN** the program contains `main` and `identity` functions whose blocks evaluate arguments before calls, reference canonical targets, and end in returns, verifying clean

#### Scenario: Lower an unavailable body to a trap

- **WHEN** a discovered instance's HIR body is unavailable
- **THEN** its lowered function is a single block ending in a generated trap carrying the causative span

#### Scenario: Match the lowered golden encoding

- **WHEN** a committed fixture program is lowered and encoded
- **THEN** the encoding equals the committed golden text byte-for-byte and repeated fresh runs are byte-identical
