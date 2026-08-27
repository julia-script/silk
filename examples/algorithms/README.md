# Silk algorithm examples

These programs are familiar end-to-end probes for Silk rather than compiler-shaped fixtures. Each
directory contains readable Silk source, a short explanation, and an `example.json` manifest with
its deterministic input, expected result, capability inventory, declared execution targets, and
checked status.

- `executable` means analysis, evaluation, native execution, and direct WebAssembly execution must
  continue to agree.
- `frontier` means the complete program is retained alongside exact, machine-checked blocker
  evidence. The harness never changes an example's status automatically.

Pure entries use their deterministic fingerprint as the process result. Effectful entries return
`()`: their manifest records process success separately from the algorithm result that the program
checks before returning, while unhandled reportable failures become runtime execution errors.

Run the suite with `pnpm --filter @silklang/compiler exec vitest run
test/AlgorithmExamples.test.ts`.
