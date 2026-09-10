# Verification

Verified 2026-09-07 with LLVM/Clang 22.1.8 first on PATH, on top of remote main `bb9994d4c1175ee924ba4bd8ad5ec3611e0f8078`.

## Independent native evidence

- All six source/C filesystem conformance lanes passed: Darwin AArch64 and GNU x86-64/AArch64, unoptimized and optimized. `conformance.json` records report hashes, source hashes, header hashes and execution results; `supplies.json` pins the selected supplies.
- Independent C checks cover stat/dirent layout, foreign signatures, open/openat mode promotion and constants. Scripted receivers exercise partial I/O, EINTR, immediate errno, primary-error preservation, failed close, descriptor transfer, bounded directory records, pending-name retries, raw bytes, 128 exclusive-name collisions and cancellation.
- Real filesystem acceptance covers descriptor-relative confinement, byte paths, read/write/list/stat/create/remove and temporary-directory cleanup. Darwin uses a valid non-ASCII name because APFS rejects invalid UTF-8; independent receivers verify lossless invalid-byte forwarding on all supplies.
- Analysis and MIR checks cover unsupported selections, consuming handles, readonly pointer byte projection and rejected mutable/non-byte projections.
- Shared native regressions verify narrow Effect success fields and initialized-field cleanup after a partial move. Filesystem policy remains ordinary source; the compiler fixes apply generically.

## Validation corrections

The full checks exposed three stale test/infrastructure assumptions, all corrected: Linux CI now selects the pinned LLVM 22.1.8 toolchain; the ordinary C fmod fixture explicitly requests libm; and the default lexer fixture verifies its independently calculated digest of 83, including the test helper substitution.

## Workspace gates

At implementation revision `793d78df61c2e592e5cb14f5f56c86f19a7b1079`, all required commands passed in order: `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`, then `pnpm release:candidate`.

- Compiler: 210 files / 2,367 tests passed; shared native acceptance: 323 tests passed.
- Workspace tests: all 22 tasks successful. `pnpm check` also passed all 17 repository-script tests.
- Release candidate: all 10 tests passed.
- Top-of-stack CI run [34077453127](https://github.com/julia-script/silk/actions/runs/34077453127) passed all 13 jobs on that implementation revision, including all native acceptance shards, all compiler shards, macOS native OS, browser and three platform supplies.
- Strict OpenSpec validation passed. The follow-up commit only records verification and completes the task checklist; implementation is unchanged.

Local logs: `/tmp/silk-jul131-{typecheck,format,lint,test,check,release}.log`.

Publication: gh stack #377, top PR #378. No merge performed.
