# JUL-124 verification

Verified 2026-09-06 on the JUL-124 branch above JUL-122. Implementation head before this evidence
commit: `06084d6a`. No tool or fixture lane was skipped.

## Required repository gates

All gates passed in the required order:

- `pnpm typecheck`: 18 tasks passed.
- `pnpm format:check`: passed.
- `pnpm lint`: passed.
- `pnpm test`: 22 tasks passed. Compiler: 2,327 tests in 211 files, then **321 native acceptance
  cases actually executed**. LLVM: 74 tests in 15 files plus parity validation. The complete test
  run took 23m 6.774s; unchanged package tasks used Turbo's recorded results.
- `pnpm check`: passed, including all 17 repository script/workflow policy tests. This reused the
  successful compiler/native test tasks; it did not replace their actual execution.
- `pnpm release:candidate`: all 10 candidate checks passed, including the new ForeignContract
  public subpath and packed package consumers.

Local logs: `/tmp/silk-jul124-typecheck.log`, `format-check.log`, `lint.log`, `test-final.log`,
`check.log` and `release.log` with the same `/tmp/silk-jul124-` prefix.

The first lint attempt found new nested-conditional and JavaScript Effect parameter-typing errors,
and new JSON-boundary warnings; these were fixed. The lint suggestion named a Schema constant
absent from the installed declaration surface, so the supported `Schema.fromJsonString(Schema.Unknown)`
codec is used. The first full test run found two stale expected outputs introduced by this change:
the MIR foreign-signature assertion lacked behavioral identity, and the operator bitcode digest
predated personality-field serialization. Both were reconciled, and the full ordered gates above
passed afterward. These were change-related failures, not pre-existing failures. LLVM's existing
debug-info fixture warnings (anonymous file/debug version) did not fail verification.

## Independent native contracts

`pnpm --filter @silklang/compiler test:foreign-contracts` was rerun against the final built compiler
and passed all six debug/optimized lanes:

| Target                    | Debug  | Optimized |
| ------------------------- | ------ | --------- |
| aarch64-apple-darwin      | passed | passed    |
| aarch64-unknown-linux-gnu | passed | passed    |
| x86_64-unknown-linux-gnu  | passed | passed    |

The runner verifies the pinned authority/header/tool/image inputs in `supplies.json`, compiles Silk
IR and bitcode, runs LLVM verification, emits and inspects real native objects/unwind sections,
links a separately compiled C++ consumer, and executes normal, throwing and no-return modes.
`results.json` records fixture hashes, object/bitcode hashes and actual process outcomes. GNU/Linux
ARM64 ran in the pinned ARM64 container; it was not simulated by changing a target label.

Normal execution proves renamed foreign writes, read-only argument access, a returned raw alias,
no-capture pointer use and operation/accessor state ordering, including a deliberate intervening
state change. No-return exits with status 23. Foreign throws terminate with a native trap before
an enclosing C++ catch above the exported Silk callback can run. The unwind guard has a generated
personality and invoke/landingpad structure; a nounwind promise alone is not the enforcement.

The existing language does not admit first-class indirect calls to foreign declarations. Every
admitted immediate foreign call uses the guard, including outbound calls from exported Silk
callbacks. This change does not claim escaping/threaded callback lifetimes, retained reference
storage, nonlocal-jump support, permitted unwinding, variadic/aggregate ABI or LTO. Unsupported
contract fields and LTO profile input diagnose. Raw output pointers gain neither initialized-state
proof nor a checked reference from noCapture.

## Focused structural and admission evidence

Shared analysis tests cover defaults, ordinal normalization, aliases/property ordering, visible
contract mismatch origins, strict schema-2 imported interfaces, rejected retained/unwind/unknown
properties, invalid no-return results, raw/reference admission, overlapping loans, complete-call
loan endings, and initialization obligations. Driver tests reject incompatible supplied interfaces
before cache reads or native tool execution. MIR/cache/interface identities carry the normalized
contract. LLVM tests verify typed invoke/landingpad encoding, personality serialization and
edge-based dominance (including a second path into an invoke's normal destination).

Stack submission is tracked separately in tasks.md; this document does not claim a merged PR.

Final integrated verification on source head `a1bc63f3` (above origin/main `0ee2ed40`) passed all six
required repository gates. The final stack includes 2,343 compiler tests, 321 actually executed
native acceptance cases, 159 LSP tests, 89 CLI tests, 17 repository policy checks and 10 packed
release-candidate checks. See `../native-assembly-entry-contracts/verification.md` for integration
fixes, logs and the two unrelated baseline OpenSpec delta-validation failures. Submitted through
`gh stack` as draft PR #363; no merge is claimed.
