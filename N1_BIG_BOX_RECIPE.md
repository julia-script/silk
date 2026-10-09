# N1 with #1164: diagnostic run recipe

Diagnostic branch only. Do not merge.

## Tree

This branch is the tree. It is `selfhost` 3e0761f with these PR heads merged on top, in this order:

| PR | Head |
| --- | --- |
| #1175 (revert of #1167's bare parameter guard) | 8e8f39e |
| #1166 | 1639a0a |
| #1169 | 49c09a2 |
| #1164 (union injection) | f4e2b4e (1acaf94 plus a selfhost merge) |

The only conflict was in `compiler/src/semantic/SemanticLoweringCases.silk`. Both sides only add a test, and both tests are kept.

The branch also adds this file and `n1-malloc-shim.c`.

## Why a big box

Without #1164, N1 finishes in about 92 s with a peak RSS of 4.0 GB. The only gap is union-form at `semantic/Query.silk` 4769-4777.

With #1164, `Query.demand`'s provider becomes reachable (`withRunning` -> `resolve`), and so does the whole semantic engine. N1 then needs about 1 MB of live memory per walked instance. On a 15 GB container the OOM killer stops it at 14.0 GB after about 6 to 13 minutes, before it prints anything. Swap does not help, because the container limit applies.

Use 32 GB at least, and preferably 64 GB.

## Commands

The pinned LLVM 22.1.8 `clang` must come first on `PATH`. Use absolute paths without `..`.

```sh
git fetch origin n1-big-box-recipe && git checkout n1-big-box-recipe
pnpm install
pnpm bundle:cli                      # writes packages/cli/dist/silk.mjs
cd compiler
NODE_OPTIONS="--max-old-space-size=11000 --max-semi-space-size=128" \
  /usr/bin/time -v node ../packages/cli/dist/silk.mjs build --optimization release-with-debug
# N0: about 14 min, bootstrap peak RSS about 9.3 GB

ROOT=$(cd .. && pwd)
/usr/bin/time -v build/llvm/x86_64-unknown-linux-gnu/release-with-debug/silk-compiler \
  build "$ROOT/compiler/src/main.silk" -o /tmp/N1 \
  --stdlib "$ROOT/packages/compiler/stdlib" --optimization none --debug false \
  > /tmp/n1.out 2> /tmp/n1.err
echo "exit $?"
grep -E "SILK_BUILD_ERROR|SILK_UNSUPPORTED_JSON" /tmp/n1.err
grep -E "Maximum resident|Elapsed" /tmp/n1.err
```

To report, send the `SILK_BUILD_ERROR` lines (every refusal), the `SILK_UNSUPPORTED_JSON` gap list, the exit code, the elapsed time and the peak RSS.

Already known: `Shared.with` at `silk/shared.silk` 4712-4763 (`Shared.withMut(self, inspect(move use))`) fails canonicalization with reason Resource in about 84 instances. Each one is reported as a `SILK_BUILD_ERROR` with code Unsupported. session_01AgVsheyYi8H8bgckEBejqp is fixing it.

## Measuring live memory per allocation site

The Silk code in N0 has no unwind tables, so gdb backtraces, heaptrack and perf cannot unwind it. `n1-malloc-shim.c` is an `LD_PRELOAD` shim. It tags every malloc block with the address of its immediate caller. For a Silk instance, that caller's mangled symbol names the element types, for example `Vector.append` of `semantic/DeclarationId.OwnerStep`. On `SIGUSR1` the shim writes the live bytes and block count for each caller to `/tmp/claude-0/shim/dump.<pid>`. Change the path in `dump()` if needed.

```sh
gcc -O2 -shared -fPIC -o shim.so n1-malloc-shim.c -ldl
LD_PRELOAD=$PWD/shim.so build/llvm/x86_64-unknown-linux-gnu/release-with-debug/silk-compiler build ... &
P=$!
# when RSS reaches the point of interest:
kill -USR1 $P    # writes dump.<pid>: "<caller address hex> <live bytes> <live blocks>"
```

Resolve the caller addresses against `nm -n` of the N0 binary. The binary is PIE, so you need the load base. Either read it from `/proc/<pid>/maps` before the process exits, or recover it the way the original analysis did. That analysis listed every `call malloc@plt`, `call realloc@plt` and `call calloc@plt` site in `objdump -d`. A caller address minus (call site + 5) gives a candidate base. The page-aligned candidate shared by every caller is the base. Two dumps, at 4 GB and 8 GB RSS, give each site's growth.

Result at 4 GB vs 8 GB RSS (growth, then live total):

| Allocating site | Growth | Live |
| --- | --- | --- |
| `Vector.appendBytes` (u8 buffers, about 4 KB each) | 1117 MB | 1513 MB |
| `Vector.append<DeclarationId.OwnerStep>` (4M live blocks) | 708 MB | 1707 MB |
| `Type.rewriteTypeInside` | 350 MB | 490 MB |
| `Vector.append<Semantic.Dependency>` | 212 MB | 334 MB |
| `Vector.append<Mir.Statement>` | 183 MB | 280 MB |
| `Vector.append<Semantic.BodyDependency>` | 158 MB | 241 MB |
| `Vector.append<u8>` (15M blocks) | 123 MB | 288 MB |

## Walk shape

A temporary stderr trace in `buildFrom`, printing one line per entered instance, showed the following:

- `build` re-runs `buildFrom` from the roots each time new source files are observed. The run made 62 passes.
- The last pass reached 14,365 instances at an admission depth of 51 or less when it was killed.
- The largest families were `Shared.with` (1969), `withMut` (1415) and `inspect` (1414). Each closure call site is its own instance.
