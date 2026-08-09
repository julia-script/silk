# Silk algorithm examples

These programs are familiar end-to-end probes for Silk rather than compiler-shaped fixtures. Each
directory contains readable Silk source, a short explanation, and an `example.json` manifest with
its deterministic input, expected result, capability inventory, declared execution targets, and
checked status.

- `executable` means analysis, evaluation, native execution, and direct WebAssembly execution must
  continue to agree.
- `frontier` means the complete program is retained alongside exact, machine-checked blocker
  evidence. The harness never changes an example's status automatically.

Run the suite with `pnpm --filter @silk-effect/compiler exec vitest run
test/AlgorithmExamples.test.ts`.
