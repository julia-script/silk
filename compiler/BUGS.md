# Compiler findings

## Instance discovery multiplies call-path contexts for the full parser

**Status:** repaired in bootstrap instance discovery; the full self-hosted parser now checks,
builds, and executes without a discovery workaround.

The full parser's frontend checks successfully when imported by a trivial entry point. Making
`Parser.parse` reachable from the real executable causes bootstrap instance discovery to accumulate
hundreds of thousands of ancestry-sensitive work items. A debugger snapshot recorded 417,124
scanned contexts and 485,588 pending items. CPU sampling located the work in `Instances.discover`,
with substantial time spent shifting the pending array. The check was stopped after several minutes.

The minimal reproduction is a four-function monomorphic diamond: `main` calls `left` and `right`,
and both call `leaf`. The bootstrap reports four instances but five residual-body requests.
An eight-function Fibonacci-shaped graph plus `main` reports nine instances and 42 requests.
The repeated traversal matters far more in the parser's densely connected recursive grammar.

The regression lives in `packages/compiler/test/Instances.test.ts`:

```sh
pnpm --filter @silklang/compiler exec vitest run test/Instances.test.ts
```

The diamond now produces exactly four residual-body requests. The guard no longer records ordinary
monomorphic ancestors that cannot change type arguments. Generic declarations, hidden callable and
Effect identities, and structural-provider evidence still retain their ancestry. Shared contexts
are deduplicated before entering a cursor-based FIFO queue. Tests also cover generic recursion
through a shared monomorphic helper, so body reuse cannot silently erase a required recursion check.

The parser corpus in `scripts/test-parser.mjs` passes all 102 inputs: grammar fixtures, self-hosted
sources, and standard-library sources. It checks native AST structure, flat-tree invariants,
significant-token ownership, and recovery. `compiler/src/main.silk` produces 452 nodes and no syntax
diagnostics. Successful frontend checking alone would not establish these runtime claims.

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

**Status:** the full-parser reload expansion is repaired; cold compilation still has a measurable
cost, and default cross-process cache reuse remains a separate issue.

**Tracking:** [JUL-154](https://linear.app/juliaortiz/issue/JUL-154) covers cold LLVM
construction cost; [JUL-158](https://linear.app/juliaortiz/issue/JUL-158) records the follow-up
cold-compilation research without caching. [JUL-155](https://linear.app/juliaortiz/issue/JUL-155) covers cross-process
cache reuse for project builds.

The full parser exposed a larger construction problem: `NativeStorage.reloadRoots` reloaded every
mutable local in a function at every control-flow join, including locals discarded in earlier
blocks. This multiplied the local count by the join count. Lowering now computes a conservative
reference set for each linear block and reloads only its mutable members. The set includes
selector indices, cleanup owners and initialization flags, and hidden suspension frame payloads;
it is not an assumption that explicit operation operands are the only inputs.

On 2026-09-08, the normal cold command `silk build --manifest-path compiler/silk.toml`, run from the
repository root with the default Node heap and no persistent native cache, improved from 147.83
seconds and 4.57 GB peak RSS to 43.77 seconds and 3.17 GB peak RSS. Both builds emitted 720 symbols.
The baseline included a short inspector CPU sample. The new executable passes the complete parser
corpus, rather than merely completing LLVM construction.

There was also a separate retention issue in the LLVM builder: local-handle weak registries kept a
back-reference to the entire mutable function draft. One escaped value therefore retained all its
sibling handles. Entries now retain only the function owner's identity and local index. A
one-million-instruction probe reduced retained heap from about 1.09 GB to 0.63 GB. This change alone
did **not** materially improve the full build; the block-local reload repair accounts for the main
speedup.

The next cold profile confirmed JUL-158's representation-field lookup lead. Planning, nested
forwarding, and provenance resolution repeatedly flattened every module's structs and unions to
find a single declaration. They now use `DeclarationFacts.byCanonical`, which looks within the
owning module's member index. No new cache or cross-build reuse was added. Existing recursion
guards, substitution, field order, and recovery facts remain in place.

On the same host on 2026-09-08, two fresh-process CPU profiles of `build-exe compiler/src/main.silk`
with `--source-root compiler/src --optimization debug --timings` recorded 51.52 seconds before this
lookup change and 46.57 seconds afterward. Sampled self-time in `RepresentationField` fell from
3.87 seconds to 0.29 seconds. These are single profiled runs with other work active on the host,
not controlled medians or directly comparable to the earlier unprofiled 43.77-second project build.
The two native executables are byte-for-byte identical, and the new one passes all 102 parser
corpus inputs. LLVM backend emission still accounts for about 21 seconds of the profiled build.

The opt-in representation-field probe below uses 1,500 unrelated declarations and 2,000 planning
queries. Its query loop decreased from 967 ms to 95 ms with an identical result SHA-256; its first
query decreased from 1.59 ms to 0.87 ms. Those loop timings include JIT warmup, so they establish the
local scaling improvement rather than claiming a cold-build speedup by themselves.

Regression assertions live in `packages/compiler/test/Backend.test.ts` and
`packages/llvm/test/FunctionBody.test.ts`; `packages/compiler/test/RepresentationField.test.ts`
also rejects lookup attempts that enumerate unrelated aggregate collections. Reproducible, opt-in performance probes (after building
the workspace) are:

```sh
node packages/compiler/scripts/benchmark-native-reloads.mjs 40
node packages/compiler/scripts/benchmark-representation-fields.mjs 1500 2000
node --expose-gc packages/llvm/scripts/construction-benchmark.mjs 100 10000
```

The earlier lexer-only measurements below describe the smaller workload that first exposed
construction and cache costs; they are not timings for the full parser.

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
