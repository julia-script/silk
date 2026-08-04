## 1. Transfer lesson

- [x] 1.1 Create Lesson 13 with the `%` requirements, affected-stage checklist, and success criteria while withholding finished implementation code.
- [x] 1.2 Specify multiplication-level, left-associative parsing and signed LLVM `srem` semantics.
- [x] 1.3 Add reflection prompts that ask why resolver, function-table, and conditional lowering code do not change.

## 2. Validation assets

- [x] 2.1 Add public learner tests for `%` tokenization, AST grouping, IR containing `srem`, and native execution.
- [x] 2.2 Add a non-published reference implementation or hidden validation fixture for `10 + 7 % 4 * 2` and `isOdd(7)`.
- [x] 2.3 Add recovery hints that become progressively specific without revealing the final patch.

## 3. Series completion

- [x] 3.1 Link Lesson 13 and optional factorial/playground next steps from the tutorial index.
- [x] 3.2 Pilot the transfer exercise or run an agent-free clean-room attempt to confirm the supplied guidance is sufficient but not solution-equivalent.
- [x] 3.3 Run `pnpm check` and `pnpm release:candidate`, reporting any pre-existing failure separately.
