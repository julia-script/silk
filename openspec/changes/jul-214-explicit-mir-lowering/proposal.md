# Why

MIR lowering is still a private step of realization and infers policy from coordinator state. It
must become an independently callable producer with explicit artifact inputs and admission policy.

# What Changes

- Add `Mir.lower` with explicit instance, runtime-layout, declaration, opaque-realization,
  presentation, profile, admission, normalization, and audit inputs.
- Move optional foreign-planning audit into the MIR operation.
- Return unavailable MIR plus diagnostics for rejected admission or failed audit.
- Remove the old coordinator-owned `Realization.lowerMir` operation identity.

# Capabilities

## New Capabilities

- `explicit-mir-lowering`: independently callable MIR production with visible admission and audit.

# Impact

MIR and realization modules, phase traces, foreign planning, normalization, compiler docs, and
focused lowering fixtures.
