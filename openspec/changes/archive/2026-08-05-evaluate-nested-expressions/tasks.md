## 1. Evaluate recursive expression facts

- [x] 1.1 Introduce one recursive evaluator operation for integer, parameter-reference, and call expression facts while preserving the closed outcome type
- [x] 1.2 Evaluate every call's arguments completely in concrete left-to-right order before creating positional bindings or entering the target body
- [x] 1.3 Propagate the deepest nested blocked reason and provenance without emitting enclosing bindings, reads, or returns that did not occur
- [x] 1.4 Remove the temporary unsupported-nested-expression branch and retain exhaustive handling of every semantic expression state

## 2. Preserve call and cycle semantics

- [x] 2.1 Extend active-declaration cycle checks to calls reached through nested arguments without prematurely entering the enclosing target
- [x] 2.2 Preserve completed earlier-argument work when a later nested argument blocks and ignore all unreachable nested expressions
- [x] 2.3 Add evaluator fixtures and tests for one nested call, ordered sibling calls, inner unavailable facts, wrong arity at depth, nested cycles, and representative deep input

## 3. Extend deterministic traces

- [x] 3.1 Record the enclosing call event before its arguments, complete nested argument events left to right, and emit enclosing bindings only after all values exist
- [x] 3.2 Retain distinct semantic identities and source provenance for repeated callees at different nested call sites
- [x] 3.3 Add exact successful and partial blocked trace assertions plus repeated-process determinism coverage without changing flat trace order

## 4. Make nested evaluation visible

- [x] 4.1 Add completed, inner-blocked, and nested-cycle evaluation presets to the hidden Syntax Inspector
- [x] 4.2 Render nested trace grouping, inner-result-to-outer-binding relationships, blocked endpoints, and exact source links from the authoritative trace sequence
- [x] 4.3 Provide equivalent accessible trace text and manually verify completed and blocked presets at desktop and narrow widths

## 5. Document and verify

- [x] 5.1 Update evaluator and inspector documentation with recursive call-by-value and trace-order semantics
- [x] 5.2 Add the required changeset and verify release-candidate contents if public outcomes, traces, or exports change
- [x] 5.3 Run focused evaluator and docs tests, then `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`
