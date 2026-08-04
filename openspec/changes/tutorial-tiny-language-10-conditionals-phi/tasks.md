## 1. Conditional lowering

- [x] 1.1 Lower the Tiny `i32` condition, compare it against zero, and produce the required LLVM `i1`.
- [x] 1.2 Create uniquely named true, false, and merge blocks; lower nested branch expressions; and terminate both actual predecessor blocks.
- [x] 1.3 Create, populate, and seal one `i32` PHI result for the conditional expression.

## 2. Control-flow lesson

- [x] 2.1 Create Lesson 10 with a CFG diagram and adjacent prose for predecessor, merge, beginner-level dominance, and PHI selection.
- [x] 2.2 Add the `abs` walkthrough and explicitly connect each PHI incoming value to the predecessor that ran.
- [x] 2.3 Add guided failure/recovery steps for wrong condition type, missing terminators, missing PHI edges, and insertion-point mistakes.

## 3. Verification

- [x] 3.1 Add IR snapshots for `abs` and nested conditionals, including unique block names and complete PHI inputs.
- [x] 3.2 Run consumer typecheck and compiler tests against the packed package.
- [x] 3.3 Run `pnpm check` and `pnpm release:candidate`, reporting any pre-existing failure separately.
