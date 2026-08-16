## Context

Six enabling changes can each pass locally while their composition still fails through tooling,
joins, cleanup, determinism, or one backend. This change is an integration and characterization gate,
not a proposal for a CLI standard library.

## Goals / Non-Goals

**Goals:**

- Prove the complete capability set in one ordinary-source, formatter-stable fixture.
- Enforce evaluator/native/Wasm parity and zero indirect dispatch.
- Establish deterministic static-tree growth baselines before numerical limits.

**Non-Goals:**

- Finalize `Command`, `Cli`, `Schema`, `Decoder`, or `Encoder` APIs.
- Add a production CLI library, shell grammar, help policy, or heterogeneous executable storage.
- Treat performance concerns as permission for erasure or compiler-known actors.

## Decisions

### Make one executable fixture the integration contract

Create `static-composition-acceptance.silk` with complete definitions, not pseudocode. It uses final
syntax, named callables, reusable borrowed decoding, explicit providers/rows, static nested command
fields, help traversal, branch-local consumption, one application union, and handler once/never
assertions. Formatting the fixture twice must be idempotent.

### Keep the library shape replaceable

The fixture's actors are pressure vocabulary only. Add a renamed equivalent control and inspect HIR,
MIR, and backend artifacts for forbidden actor-name branches. The compiler may know only sealed
intrinsics, never standard-library declarations by spelling.

### Require differential engine evidence

Run success, help, selection failure, decode failure, uncalled cleanup, called cleanup, and suspended
Effect cases through evaluator, LLVM, and direct Wasm. The evaluator provides the detailed handler,
failure, suspension, and cleanup trace contract. Native and Wasm execution must match its observable
result or failure. Native runtime parity and trapping cleanup witnesses live in the shared driver
corpus rather than a feature-local compile loop; emitted Wasm artifacts prove the selected direct
target and cleanup path. Inspect Wasm for zero tables and `call_indirect`.

### Characterize two static-tree shapes

Generate left-associated and balanced command trees at 1, 8, 32, 64, and 128 distinct leaves, with
per-leaf transforms and one normalized application-action union. Record canonical byte size,
semantic/representation/instance/layout/MIR counts, phase time, phase-boundary heap samples, LLVM
bitcode bytes, and Wasm bytes twice in fresh processes. Counts and artifacts are hard deterministic
gates; timing, memory, and size trends establish the first measured baseline. Any unexplained
superlinear semantic growth fails and returns to implementation analysis.

This characterization is an opt-in benchmark, not a default correctness test. The checked-in report
and a deliberate benchmark run carry the empirical evidence without adding fresh-process and
performance-count work to `pnpm check`.

### Keep evidence failures scoped

The four prerequisite vertical/characterization spikes and this separate fixture gate can return a
change to design review. They cannot silently weaken settled semantics or introduce runtime erasure,
dictionaries, allocation, or actor-specific compiler privilege.

## Risks / Trade-offs

- [Fixture accidentally defines the public CLI API] → Keep names explicitly non-normative and use a
  renamed equivalent control.
- [Stress results vary with host noise] → Hard-gate deterministic structural outputs first and record
  environment metadata for empirical timing/memory baselines.
- [Integration change hides prerequisite failures] → State all six required changes and refuse to
  duplicate unfinished feature implementation here.

## Migration Plan

1. Land only after every prerequisite change is complete and its fences are correctly narrowed.
2. Add and format the executable fixture plus renamed control.
3. Add evaluator trace and cross-engine result plus structural artifact assertions.
4. Add generated growth inputs and checked-in baseline report.
5. Run the complete release matrix and classify every finding.

The fixture and reports can be removed without changing language semantics; failing them blocks release.
