# Compiler findings

These findings were encountered while compiling the experiment against bootstrap revision
`03ec67f6` on 2026-09-06 and checked again after rebasing onto `dd4510fa` on 2026-09-07. Each entry
keeps the original evidence and records whether the latest compiler still needs a workaround.

## OWN0020 after several ownership-sensitive suspensions

**Status:** fixed on `main`; the workaround has been removed from `src/main.silk`.

**Tracking:** [JUL-152](https://linear.app/juliaortiz/issue/JUL-152)

Putting the complete CLI flow in one effectful function caused every suspension after the first
combined input operation to fail with:

```text
error[OWN0020] Cannot preserve ownership across suspension: suspendable MIR run has no exact provisional control
```

The rejected function performed these ordinary steps in order:

1. Collect the source argument, working directory, and portable root with `Effect.zip3`.
2. Borrow the working-directory bytes as UTF-8.
3. Create `OsFileSystem` and join the relative argument with `Effect.zip`.
4. Read the file, borrow its `Bytes` into `Lexer`, and print tokens.

The diagnostic appeared on the invalid-UTF-8 branch and then on `Effect.zip`, `FileSystem.readFile`,
and the final token-printing `run`. All owned values had lexical owners and were used or moved in
order, so the diagnostic did not identify an ownership rule the source could act on.

The original workaround split the flow into `workingFileSystem`, `sourcePath`, `readSource`, and
`lexOwned`, with each helper tail-running its final ownership-sensitive suspension. JUL-152 repaired
the lost per-run classification. After rebasing to `dd4510fa`, the experiment again keeps these
operations in one sequential `program` body, and `silk check --manifest-path compiler/silk.toml`
passes.

## Semantic diagnostic cascade after a missing nominal-union field comma

**Status:** reproducible diagnostic-recovery problem; source mistake fixed.

**Tracking:** [JUL-153](https://linear.app/juliaortiz/issue/JUL-153)

Omitting a comma between fields in a nominal-union variant correctly emitted a leading `PAR0001`
(`Expected ','`). The compiler then emitted a large cascade of `SEM0169` invalid-union-construction,
unknown-field, and unreachable-match errors in downstream files that used the union. Those semantic
diagnostics were consequences of the one malformed declaration and obscured the actionable parser
error.

**Workaround:** fix the first parser diagnostic before examining semantic diagnostics. Nominal-union
payload fields use commas even though struct declaration fields do not.

A compiler-side recovery improvement would suppress semantic analysis derived from the invalid
union shape, or retain an explicit error member so downstream construction and match sites do not
fan out into unrelated failures.

## LLVM emission dominates lexer builds and repeat runs miss the default cache

**Status:** reproducible performance problem; persistent cache and direct execution are available
workarounds.

**Tracking:** [JUL-154](https://linear.app/juliaortiz/issue/JUL-154) covers cold LLVM
construction cost. [JUL-155](https://linear.app/juliaortiz/issue/JUL-155) covers cross-process
cache reuse for project builds.

On the current host, running `silk run -- fixtures/empty.silk` from this directory took 15.9
seconds and reached about 1.6 GB peak resident memory. The compiler realized 314 symbols. Its
reported phases identified LLVM backend emission as the largest phase at 5.8 seconds, followed by
elaboration at 2.2 seconds. CPU sampling divided backend emission into approximately 4.8 seconds
constructing LLVM functions through the in-process builder, 0.2 seconds verifying the module, and
0.8 seconds encoding bitcode. Object emission, runtime compilation, and linking together took less
than 0.5 seconds.

A one-function Silk executable completed in 1.2 seconds and spent 16.8 milliseconds in the backend,
so the cost scales with the realized program rather than Clang startup or a fixed compiler cost.

The default backend-emission cache is process-local when `SILK_NATIVE_CACHE_DIR` is unset. Since
each `silk run` starts a new process, an unchanged project repeats backend emission. Pointing
`SILK_NATIVE_CACHE_DIR` at a persistent directory changed the next unchanged run to a
`backend-cache` hit and reduced wall time from 15.9 to 9.9 seconds. Running the already-built
executable avoids compilation entirely.
