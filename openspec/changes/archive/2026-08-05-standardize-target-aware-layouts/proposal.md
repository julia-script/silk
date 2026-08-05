## Why

The compiler currently carries a caller-supplied layout beside target-neutral MIR and lets the
LLVM backend choose physical details such as `Bool` representation during emission. Structs and
unions would multiply that split ownership, so target selection and concrete Silk layout need one
canonical compiler phase before those data features land.

## What Changes

- Add compiler-owned canonical target profiles for the three required bootstrap hosts plus the
  existing WebAssembly emission target, and reject unsupported or internally inconsistent target
  requests as typed compiler outcomes.
- Add an explicit layout phase after concrete instance discovery and before MIR lowering. It
  computes a deterministic, backend-neutral plan for every reachable concrete runtime type.
- Embed the selected target and complete layout plan in MIR and its textual encoding so analysis,
  interpretation, labs, and code generation observe the same facts.
- **BREAKING** Remove the separate target-layout argument from the `Backend` emission contract;
  backends must realize the plan carried by MIR and may not choose alternate Silk layouts.
- Move the existing physical `I32` and `Bool` decisions into the compiler plan as the scalar proof
  of the boundary. Later struct and union changes extend the same plan rather than creating a new
  backend seam.
- Thread the canonical target through object and link planning, preserving deterministic output for
  identical source, target, profile, and pinned-toolchain inputs.
- Expose target and layout facts through the supported analysis facade and the existing MIR/LLVM
  labs; the interpreter consumes the same target-aware MIR without constructing a shadow layout.

## Capabilities

### New Capabilities

- `bootstrap-target-layout`: Canonical bootstrap target selection and deterministic compiler-owned
  concrete layout planning between instance discovery and MIR lowering.

### Modified Capabilities

- `bootstrap-mir`: MIR carries the canonical target and layout table instead of prohibiting all
  physical layout facts and accepting a separate emission-time layout.
- `bootstrap-backend`: Backend emission consumes target-aware MIR without a second layout argument;
  the existing LLVM and WebAssembly backends realize the compiler-selected scalar representations
  and reject incompatible target profiles.
- `bootstrap-compiler-driver`: The driver selects a target, runs layout planning before lowering,
  and reports the new phase.
- `bootstrap-analysis-facade`: Snapshots expose immutable target and layout queries and use the same
  plan for lowering and code generation.
- `bootstrap-evaluation`: The interpreter accepts the target-aware MIR program and never derives an
  independent representation plan.
- `bootstrap-native-toolchain`: Object and link command planning use the compiler-selected canonical
  target and reject incompatible artifacts.
- `bootstrap-syntax-inspector`: MIR and LLVM labs display the selected target and layout plan from
  the same analysis snapshot.

## Impact

This breaks the current compiler-internal `Mir.TargetLayout`, `Backend.emit`,
`Driver.CompileRequest`, and `Analysis.codegen` boundaries. It introduces target and layout actor
modules, changes MIR construction, verification, encoding, samples, goldens, interpreter inputs,
LLVM and WebAssembly scalar lowering, toolchain command planning, driver phase reports, facade
queries, docs labs, and their tests. It adds no external runtime dependency and deliberately does
not add structs, unions, a public ABI guarantee, or a new backend.
