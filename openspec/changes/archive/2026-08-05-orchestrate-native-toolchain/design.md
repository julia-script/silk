# Design — orchestrate-native-toolchain

## Context

See proposal.md — Why. The backend produces deterministic bitcode; ticket 06 pins what stands
between it and a runnable binary: a pinned external Clang with `-c` under fixed optimization
profiles, build-scope-owned intermediates, the `NativeLinker` service driving the Clang link
driver with structured arguments, and the minimal C runtime shim (issue 07 owns the ABI's
future).

## Goals / Non-Goals

**Goals**

- `ToolchainPlan.ts` (pure, browser-safe): fixed profiles, planned object and link commands as
  structured argv data, the shim source.
- `NativeToolchain.ts` (Node-only deep import): pinned `Toolchain` config, named build scopes
  owning path-backed intermediates with save-temps promotion, `emitObject` completing the
  backend's object contract, `compileShim`, and the `NativeLinker` service with `ClangLinker` —
  failures as data with full command provenance.
- A toolchain lab presenting the planned commands, artifact sizes, shim source, and scope
  lifecycle.

**Non-Goals**

- No driver yet (`accept-end-to-end-pipeline` orchestrates the full chain and gates acceptance).
- No linking algorithms, no configurable pass pipeline, no PATH discovery — the caller pins the
  Clang path explicitly.
- No user-facing FFI: the shim is private and compiler-versioned.

## Decisions

1. **The module splits along the browser boundary.** Planning (profiles, argv construction, shim
   source) is pure data and lives in `ToolchainPlan.ts`, exported from the package index so the
   facade and lab can show exactly what will run. Execution (`node:fs`, `node:child_process`)
   lives in `NativeToolchain.ts`, reachable only as a deep import and deliberately absent from
   the index so the package root stays browser-safe. The release-candidate's namespace check
   exempts it the same way the llvm package exempts `LlvmError`.

2. **Profiles map to fixed argv suffixes**: debug `['-O0', '-g']`, release `['-O2']`,
   release-with-debug `['-O2', '-g']` — over `['-c', '-x', 'ir', input, '-o', output]`. The
   release-with-debug bitcode carries line information by emitting with the debug codegen mode;
   stripping versus keeping metadata is decided at bitcode emission, matching ticket 06.

3. **Build scopes are `mkdtemp` directories named by the caller**, removed recursively in
   `finally`; `promote(artifact, destination)` copies with `node:fs` (`copyFileSync`) so large
   Clang outputs stream through the filesystem, never through JavaScript buffers.

4. **Process invocation is `spawnSync(command, args)` with no shell**, capturing status, stdout,
   and stderr; every outcome — success or failure — carries the exact `{command, arguments}`
   issued, which is also what the lab renders.

5. **The shim is four lines of C** (`int main(void) { return silk_main(); }` behind the extern
   declaration), compiled per build by the pinned Clang. Exit-status delivery of the `I32` result
   is the slice's closed native entry; richer runtime capabilities stay with issue 07.

6. **Tests pin `/usr/bin/clang`** (present on the macOS toolchain this repo develops against) and
   exercise the real orchestration: object emission, failure provenance, scope cleanup,
   promotion, shim compile, link, and a run asserting exit status 42.

## Risks / Trade-offs

- [External Clang makes tests environment-dependent] → The path is pinned and asserted with a
  clear message; everything above this module stays hermetic.
- [Exit-status delivery truncates to 8 bits on POSIX] → Documented; the corpus keeps expected
  results in range, and issue 07 owns richer result delivery.

## Migration Plan

1. Land `ToolchainPlan.ts` + `NativeToolchain.ts` + tests; exports and release-candidate surface.
2. Add the toolchain lab.
3. Rollback is git-revert.

## Open Questions

None — the driver and acceptance gates are the final proposal's.
