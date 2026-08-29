## 1. Establish the depth and diagnostic contracts

- [x] 1.1 Add one parser-owned expression-nesting actor with limit 256 and explicit root/child depth operations, keep it independent from `reservedForEnclosingCalls`, and verify focused unit tests pin root depth 0, child depth increments, and sibling reuse.
- [x] 1.2 Add `PAR0005`, the `ExpressionNestingLimitExceeded` reason payload, and its diagnostic constructor with limit, attempted depth, phase, severity, message, and exact first-significant-token span; verify compiler diagnostic tests assert code, reason, and span rather than message text.
- [x] 1.3 Regenerate the diagnostic catalog documentation and verify `pnpm --filter @silklang/compiler documentation:check` reports no drift.

## 2. Bound and recover recursive expression parsing

- [x] 2.1 Thread expression depth through expression, precedence, prefix, primary, projection, grouping, call/container, aggregate, match, statement, and declaration entry points; verify `pnpm --filter @silklang/compiler typecheck` passes and existing shallow parser fixtures retain their current shapes.
- [x] 2.2 Increment depth only when beginning an active child expression, preserve depth across parser-layer transitions, and derive every sibling from its common parent; verify focused tests cover operator operands, pipeline targets, indexes, arguments, arrays, aggregate initializers, and match children.
- [x] 2.3 Implement the non-recursive delimiter-aware over-budget scanner that consumes one maximal region into one `Error` node while leaving the owning delimiter or statement/declaration boundary untouched; verify a substantially deep grouped source returns without `RangeError` and advances to following syntax.
- [x] 2.4 Preserve original token objects and source order through the recovery branch and avoid quadratic rebuilding in its per-token loop; verify flattened tree tokens equal the `SyntaxFile` token stream by identity and reconstruct the original bytes exactly.
- [x] 2.5 Keep the depth comparison as the only `PAR0005` production path with no blanket `RangeError` catch; verify a parser invariant `RangeError` outside that branch still throws as a defect.

## 3. Lock down parser and facade behavior

- [x] 3.1 Add parser boundary cases generated from the shared limit at depths 255, 256, and 257; verify below-limit and boundary syntax are unchanged while the first over-limit token receives exactly one `PAR0005` with limit 256 and attempted depth 257.
- [x] 3.2 Add substantially deeper grouped, array, call/container, and direct-prefix cases plus structurally distinct operator/projection/aggregate/match cases; verify every case returns deterministic recovered syntax without relying on a host overflow threshold.
- [x] 3.3 Add recovery-cardinality cases for one maximal rejected region and two independent over-budget expressions, each followed by valid statements or declarations; verify diagnostic counts, precise spans, error-node ownership, and independent following syntax.
- [x] 3.4 Add one `it.effect` case that executes `Analysis.ofSource` on over-budget input and verifies a coherent frontend snapshot, parser diagnostics, and queryable following declaration without a defect.
- [x] 3.5 Add in-process committed-golden comparisons for recovered syntax and diagnostics and rely on the repository's existing fresh-process canary for process determinism; verify no new per-feature fresh-process test is introduced.

## 4. Document and verify the change

- [x] 4.1 Add the 256-edge depth/counting/recovery/diagnostic rule to `apps/docs/content/reference/expressions-and-operators.md`, including root depth, child edges, sibling independence, `PAR0005`, and defect isolation; verify reference links and compiler documentation policy checks pass.
- [x] 4.2 Run `openspec validate diagnose-excessive-expression-nesting --strict` and resolve every proposal, design, task, and delta-spec validation error.
- [x] 4.3 Run the required repository checks in order—`pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`—and record exact failures with whether they predate this change.
- [x] 4.4 Run `pnpm release:candidate` because compiler package contents change, then self-review the final diff against every JUL-29 acceptance criterion, deletion/simplicity, and regression-test sensitivity.
