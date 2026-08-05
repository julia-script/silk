## Why

MIR is the pipeline's second owned IR — the monomorphic, backend-neutral control-flow graph both
the LLVM backend and the future WebAssembly backend consume, and the level the interpreter will
execute. Defining MIR, its invariants, and its encoder *before* lowering exists lets the data
model, verifier, and golden tests stabilize against hand-built samples instead of co-evolving
with a lowering pass.

## What Changes

- Define the MIR data model: basic-block control-flow graph over logical Silk types, with
  explicit moves, borrows, drops, cleanup paths, success/failure branches, service slots, witness
  calls, matches, traps, and runtime-helper calls — restricted to the frozen slice's needs, but
  shaped for the full vocabulary.
- MIR contains no LLVM types, instructions, intrinsics, attributes, metadata nodes, or physical
  field offsets — and adopts no WebAssembly control shapes in anticipation either. A small
  explicit target-layout input (triple, pointer width, endianness, size/alignment, private ABI
  decisions) is defined alongside, consumed only at emission time.
- Every operation carries source and semantic provenance; compiler-generated operations inherit
  the nearest causative span and are marked generated.
- Add a MIR verifier for structural invariants, and the deterministic textual encoder with golden
  tests over hand-built sample programs.
- Add the inspector's CFG lab rendering encoded MIR samples: blocks, edges, and per-op provenance
  hover — resolving the roadmap's open question in favor of landing the view here, so MIR is
  visually inspectable from its first day.
- No optimization machinery: MIR may only remove lowering-created unreachable blocks, fold
  already-constant branches, merge mechanically identical cleanup blocks, and verify invariants.

## Capabilities

### New Capabilities

- `bootstrap-mir`: The monomorphic backend-neutral CFG representation, its invariants and
  verifier, the target-layout input, and the deterministic textual encoder.

### Modified Capabilities

- `bootstrap-syntax-inspector`: CFG lab over encoded MIR programs.

## Impact

Purely additive: a new representation with samples, verifier, encoder, and lab. Lowering from HIR
is the next proposal; nothing existing consumes MIR yet.

## Plan References

- [Roadmap — Track 4, proposal 8](../../../roadmaps/compiler-realignment.md)
- [Issue 06](../../../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md),
  IR paragraph: "MIR is a monomorphic, backend-neutral control-flow graph. It makes moves,
  borrows, drops, cleanup paths, success/failure branches, service slots, witness calls, matches,
  traps, and runtime-helper calls explicit without containing LLVM types, instructions,
  intrinsics, attributes, metadata nodes, or physical field offsets."
- Same ticket, layout paragraph: "MIR uses logical Silk types and operations. A small explicit
  target-layout input supplies the target triple, pointer width, endianness, size and alignment
  rules, and private ABI decisions. … MIR does not adopt LLVM control flow merely because LLVM is
  the bootstrap backend, nor WebAssembly stack and structured control flow in anticipation of a
  future backend."
- Same ticket, optimization paragraph: "MIR performs no general optimization. It may remove
  lowering-created unreachable blocks, fold branches whose conditions are already constant, merge
  mechanically identical cleanup blocks, and verify its invariants while preserving provenance."
- Same ticket, provenance paragraph: "Compiler-generated cleanup, failure-forwarding, and
  witness-dispatch operations inherit the nearest causative source span and are marked generated."
