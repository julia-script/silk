# Verification

Verified 2026-09-06 at implementation revision 72a5e39a with LLVM/Clang 22.1.8 first on PATH.

- `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check` and `pnpm release:candidate`: passed in order.
- Compiler: 211 files / 2363 tests; shared native acceptance: 321 tests; portable doctests: 54 passed. Darwin doctests: 59 passed in the focused native documentation run.
- Independent standard-stream C/source conformance: all three targets, debug and optimized, executed successfully; exact supply records are in conformance.json and supplies.json.
- Release-candidate validation: 10 tests passed.
- The full suite caught stale landing-page stdout constructors. Both live snippets now use selected StdoutLogger.make and explicitly select Darwin for analysis; all four introduction snippet checks pass.
- Earlier recovery fixture and two MIR golden failures were migrated and verified before the final run. No failures remain in the final gates.

Detailed local logs: /tmp/silk-jul128-{typecheck,format,lint,test,check,release}.log.

Publication: stacked PR #374; no merge performed.
