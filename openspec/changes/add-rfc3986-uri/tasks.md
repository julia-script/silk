## 1. URI actors

- [x] 1.1 Implement owned UriReference parsing, ranges, host classification, accessors, and typed errors; verify grammar and lossless fixtures.
- [x] 1.2 Implement Uri scheme enforcement and strict resolution; verify all RFC 3986 section 5.4 examples.
- [x] 1.3 Implement component-aware percent coding; verify arbitrary bytes, malformed escapes, and delimiter protection.

## 2. Integration and documentation

- [x] 2.1 Register modules and regenerate source and documentation surfaces; verify generation checks and public comments.
- [x] 2.2 Extend the prescriptive runtime reference and consolidated native acceptance corpus; verify the focused program and its unique evidence.

## 3. Handoff

- [x] 3.1 Run typecheck, format:check, lint, test, check, and release:candidate in required order; record exact results.
- [x] 3.2 Complete independent code review and mandatory test-economics review; verify approval of the final committed diff.
- [x] 3.3 Push a committed branch, confirm a draft PR, and update Linear to In Review with the exact head and evidence.

Verification: `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`,
`pnpm check`, and `pnpm release:candidate` passed. The compiler run passed 2,445
tests plus 331 shared native acceptance tests; all 57 standard-library doctests
and 10 release-candidate tests passed. `openspec validate add-rfc3986-uri --strict`
also passed.

Independent code and test-economics reviews approved the change with no findings.
The one added native corpus case adds approximately 10.33 seconds of test execution
(base: 3 ms tests / 3.80 s total; branch: 10.33 s tests / 14.24 s total, measured
sequentially while idle). Draft PR: https://github.com/julia-script/silk/pull/401.
JUL-164 is In Review with the verification evidence and exact PR-head baseline.
