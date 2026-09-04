## 1. Syntax and explicit arm bodies

- [x] 1.1 Parse immediate braced arm bodies with ordinary statement parsing and no implicit return;
      verify empty, sequential, guarded, mixed, nested, and transferring arms in the existing parser
      suite, including matches in initializers, arguments, and return operands.
- [x] 1.2 Preserve tokens, trivia, bounded nesting, and recovery before later arms and declarations;
      verify exact parser diagnostic codes/spans, lossless tokens, and continued rejection of bare blocks
      in general expression positions using distinct cases in the existing parser suite.
- [x] 1.3 Replace expression-only arm facts and HIR with explicit expression/block bodies, statement
      provenance, and completion facts; migrate construction, visitors, encoders, and specialization and
      verify typed snapshots retain both body alternatives without a second match or block expression.

## 2. Enclosing analysis and control flow

- [x] 2.1 Thread the current execution context and lexical loop stack through eager expression
      analysis so block statements share binding/region identities and return contracts; verify lexical
      scope, pattern-name conflicts, nested-loop targets, and SEM0038 while explicit callable/Effect
      boundaries remain separate.
- [x] 2.2 Compute block completion from structured flow and use existing reachable result joining;
      verify unit/unit, never/scalar, all-never, partial conditional transfer, inner-loop break, SEM0049
      for unit/scalar, SEM0087 for `{ 42 }`, and acceptance of `{ drop 42 }` with structured facts and
      exact diagnostic spans.
- [x] 2.3 Discover and validate returns through eager operands, guards, initializers, assignments,
      and return operands; verify return-type and returned-borrow checking against the enclosing body,
      all-arm transfer, and suppression of the outer return when its operand transfers.
- [x] 2.4 Update requirement/failure rows, provider traversal, callable-write tracking, representation
      discovery, and generic specialization for statement bodies; verify sequential eager `run`, legal
      and illegal `fail`, provider scope, guard transfer versus Boolean-false fallback, and existing
      explicit Effect/callable result joins in structured analysis.

## 3. Ownership and cleanup

- [x] 3.1 Share statement exit frames and loop outcomes with eager expression traversal; verify
      normal and nonlocal exits clean arm locals and pattern owners at their own lexical boundaries and
      post-match ownership joins only paths reaching the continuation.
- [x] 3.2 Preserve provisional ownership on Boolean-false guards and ordinary cleanup on guard
      transfers; verify later candidate availability, narrowed-borrow ending, consumed omitted fields,
      and no use or cleanup of inactive or already-transferred payloads through ownership plans.
- [x] 3.3 Track previously evaluated argument/aggregate temporaries until storage or transfer;
      verify return/failure/break/continue inside a nested match releases abandoned live temporaries
      exactly once in reverse acquisition order and emits no continuation-only cleanup or destination write.

## 4. MIR regions and LLVM control flow

- [x] 4.1 Replace inline-only match bodies and guards with region-backed execution and explicit
      normal-result availability; migrate existing expression arms and shared statement pattern
      selections and verify MIR retains canonical coverage, bindings, guard order, provenance, and
      zero-payload unit completion without an all-never result local.
- [x] 4.2 Compose expression lowering with the enclosing return/loop/cleanup context; distinguish
      successful noncompletion from lowering failure and verify structural MIR places later operands,
      calls, stores, and match joins exclusively on normal paths.
- [x] 4.3 Update MIR traversal, encoding, verification, inspection, and LLVM linearization to honor
      selected-region exits; verify forged inconsistent join/cleanup paths are rejected, completing
      scalar arms initialize their join, and transferring arms perform no join load or store.
- [x] 4.4 Preserve generic coverage specialization, Effect representation joins, lexical provider
      scope, and existing statement pattern behavior through lowering; verify the affected focused
      suites and intended LLVM IR structure without adding a per-feature native compile test.

## 5. Static evaluation, formatting, and documentation

- [x] 5.1 Implement canonical static match selection, provisional pattern/guard scope, and eager
      statement-arm execution; verify selected-only mutation, unit completion, source guard order, and
      unchanged static legality restrictions through the existing StaticEvaluation suite.
- [x] 5.2 Propagate explicit value-or-transfer outcomes through static expression evaluation;
      verify a transfer from an argument, initializer, or guard skips remaining work and reaches only
      the enclosing invocation or lexical loop, including the distinction from an arm's inner loop.
- [x] 5.3 Format arm statements using ordinary nested block layout; verify empty uncommented blocks,
      sequential/mixed/nested/transferring arms, delimiter comments, reparsing, idempotence, and typed
      refusal of damaged input in the existing formatter suite.
- [x] 5.4 Update control-flow and Effect reference pages and any affected diagnostic catalog
      examples, generated artifacts, fixtures, and public documentation; verify authored examples agree
      with analysis and the applicable documentation/catalog checks.

## 6. Integration evidence and delivery

- [x] 6.1 Add the smallest distinguishing programs to the shared native acceptance corpus for eager
      order, branch selection, outer mutation, expression/guard transfer, and cleanup; verify through
      DriverNativeAcceptance with independently pinned expected outcomes and no per-feature native or
      fresh-process determinism tests.
- [x] 6.2 Consolidate focused evidence so each source program has one shared Analysis snapshot per
      file and every new or expanded case has a distinct regression claim at the cheapest tier; record
      execution shape and focused runtime measurements for the dedicated test-economics review.
- [x] 6.3 Review the complete diff against every JUL-105 acceptance item and green-field cleanup;
      verify strict OpenSpec validation, no expression-only arm assumptions, no general block values,
      no compatibility paths, and no runtime evaluator or independent Wasm backend.
- [x] 6.4 Run `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, then `pnpm test`, followed by
      `pnpm check`; run `pnpm release:candidate` when package contents or exports change and record exact
      results, pre-existing failures, and any remaining gap without claiming completion prematurely.
- [x] 6.5 Obtain independent implementation review and a separate test-economics reviewer approval;
      verify findings, compare focused base/branch timings under equivalent conditions in an isolated
      base checkout, fix valid issues, and rerun affected checks until the final committed diff is
      approved.
- [x] 6.6 Commit only JUL-105 changes, push the feature branch, and create or reuse a draft PR to
      `main`; read back its URL and draft state, then update Linear with acceptance/check/test-review
      evidence and the exact PR-head Review baseline, move to In Review, and read back the issue.
