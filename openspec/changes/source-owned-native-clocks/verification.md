# Verification

Verified 2026-09-06 with LLVM/Clang 22.1.8 first on PATH.

- `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check` and `pnpm release:candidate`: passed in order at implementation revision 1de60687.
- Compiler: 210 files / 2361 tests; shared native acceptance: 321 tests. Workspace test tasks: 22 successful. Release-candidate validation: 10 tests passed.
- Independent clock C/source conformance: Darwin ARM64 and GNU x86-64/ARM64, debug and optimized, all executed successfully. Exact supplies and results are recorded alongside this file.
- Tests cover canonical civil/monotonic reads, resolution overflow, fixed GNU absolute EINTR retry, Darwin deadline recomputation and fatal invalid results without timing assertions.
- The earlier full run caught the inherited stale landing-page logger example. The streams fix was rebased into this layer and all gates passed afterward. The subsequent rebase adds only streams verification documentation.

Detailed logs: /tmp/silk-jul132-{workspace-typecheck,format,lint,test,check,release}.log.

Publication: stacked PR #375; no merge performed.
