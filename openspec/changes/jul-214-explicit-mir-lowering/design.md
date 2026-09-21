# Context

Lowering already consumes concrete instances and runtime layout, builds provisional MIR, normalizes,
checks suspension ownership/native assembly, finalizes suspension/coroutine frames, and optionally
runs foreign planning. The coordinator previously supplied several inputs ambiently.

# Decisions

- `Mir.LoweringInput` names every artifact and policy input.
- Admission is a closed `Admitted | Rejected(diagnostics)` value; rejected requests perform no
  lowering work.
- Normalization is `Normalize | Preserve`, replacing the test-only boolean at the operation edge.
- Audit is `None | ForeignPlanning`; an audit failure withholds the program and returns diagnostics.
- `Mir.lower` lazily loads the implementation to keep the MIR data actor's import graph acyclic.

# Risks

The implementation currently lives beside the realization helpers. The public operation and input
contract prevent callers from depending on that placement, and the obsolete operation name is gone.
