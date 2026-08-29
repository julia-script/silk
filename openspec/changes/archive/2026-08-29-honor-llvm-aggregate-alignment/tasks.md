## 1. Data-layout representation and parsing

- [x] 1.1 Add the immutable public aggregate specification and pinned absent-rule default, and verify typecheck accepts every `DataLayout` constructor.
- [x] 1.2 Parse `a:<abi>[:<preferred>]` with zero/default, ordering, and last-entry semantics, and verify focused `DataLayout` tests cover valid, repeated, exact-render, and malformed inputs.

## 2. Aggregate type layout

- [x] 2.1 Apply the aggregate ABI minimum and tail padding to unpacked anonymous and named structures, and verify focused type tests match pinned LLVM sizes and alignments.
- [x] 2.2 Preserve empty structure size, packed structure layout, stronger-field alignment, and array behavior, and verify each case has a regression assertion.

## 3. Validation and handoff

- [x] 3.1 Validate the OpenSpec change strictly and run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check` in repository order.
- [x] 3.2 Run `pnpm release:candidate` because the public `DataLayout` value changes, then review the diff against every acceptance criterion and record exact results for handoff.
