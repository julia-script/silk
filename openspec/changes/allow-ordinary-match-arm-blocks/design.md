## Context

See `proposal.md` for motivation and scope. The work base is
`8675336f1ee0110457682964ccd12801f0de7c97`; the issue's last reviewed commit is identical, so the
admission delta is empty. The checkout was clean before this change. Current demand remains
unimplemented: `Parser/Expression.parseMatchArm` parses an expression, `analyzeMatch` reads only
expression children, and `LowerExpression.lowerMatchExpression` requires a selected result local.
No open GitHub PR or existing active OpenSpec change owns this feature at admission.

The current architecture separates statement execution from expression values. In particular,
`analyzeStatements` receives `BodyContext` and a lexical loop stack, while expression analysis
receives resolution and binding scope. Ownership statement traversal owns lexical exit frames;
expression traversal does not currently return branch completion. MIR statement regions carry
transfers, while selected match arms carry inline operations and a mandatory result local.
StaticEvaluation has statement-flow outcomes but no match-expression selection case. These seams
must change together; parser support alone cannot satisfy the contract.

## Goals / Non-Goals

**Goals:** Preserve one match expression with explicit arm body alternatives, use existing ordinary
statement semantics, and make expression evaluation compose correctly with lexical transfers.
Keep semantic identities, specialization, cleanup, and emitted control flow consistent.

**Non-Goals:** No general block-value node, callable conversion, implicit Effect, extra match
statement, or compatibility representation. No runtime evaluator or independent Wasm backend.
Existing explicit callable/Effect bodies keep their own invocation and transfer boundaries.

## Decisions

### Parse ordinary statements only at the arm arrow

After `=>`, a significant `{` selects the ordinary block parser with both implicit-return options
disabled. Other tokens retain ordinary expression parsing. The arm body is identified explicitly
as expression or block in semantic facts and HIR; a block is not admitted to the general expression
union. Preserve the block's authored braces, statements, trivia, and spans. Extend bounded recovery
at the arm boundary so a missing closing brace does not swallow a recognizably subsequent arm or
declaration; keep the existing nesting limit effective when blocks contain nested matches.

Reusing callable-body parsing would synthesize an enclosing return at `}` and is rejected. Parsing a
new general block expression would admit unwanted block values and is also rejected.

### Share execution context and compute completion from flow

Thread an explicit current execution context through expression analysis when it is invoked from a
body. It owns the existing binding/region/loop allocation, enclosing return contract, static state,
provider context, and lexical loop stack. An ordinary arm reuses that context with an arm-local
binding scope. Explicit nested callable/Effect analysis creates its existing fresh execution
boundary. An expression analyzed outside an executable body cannot manufacture a transfer target.

Analyze block statements through `StatementAnalysis.analyzeStatements`. Store their typed facts and
completion in the arm body. Derive normal completion from the ordinary structured flow walk:
fallthrough contributes `()`, and absence of any normally completing path contributes `never`.
Partial conditional transfers retain fallthrough; an inner loop's `break` is consumed by that loop.
Preserve existing while scoping: the condition uses the surrounding transfer context and the body
establishes the current loop target. Ordinary arms do not introduce a target for the loop whose
condition is being evaluated.
Do not inspect only the last source token or coerce a block to an expected result type. Feed the
body contribution into the existing reachable-arm join; explicit expression arm contextual typing
remains available where already supported, such as named aggregate construction.

Generalize return discovery to traverse eager expression children in evaluation order and ordinary
arm statements. Stop at explicit execution boundaries. A return's operand is evaluated first: a
transfer within that operand suppresses the outer return on that path. Keep return checks tied to
the enclosing body's contract, including matches in guards, calls, initializers, writes, and return
operands. Failure and requirement-row collectors use the same eager traversal; a bare ordinary arm
adds neither a capture environment nor a deferred requirement boundary.

### Make arm bodies explicit throughout the typed pipeline

Use a discriminated arm body with either one typed expression or typed ordinary statements and
their completion information. Retain source provenance on either form. Update Elaboration and HIR
visitors, encoders, specialization, generated aggregate and executable-site discovery, effect-row
analysis, callable-write tracking, and ownership traversal in the same change. Traversal APIs must
distinguish structural inspection from eager execution and execution-boundary traversal; a generic
expression-children helper must not silently omit statements or flatten them across a callable.

This replaces the expression-only `arm.result` representation. A synthetic unit expression appended
to every block is insufficient: it obscures the distinction between unreachable completion and a
real unit contribution, and cannot represent nonlocal transfers safely.

### Compose ownership with expression transfers

Give eager expression traversal access to the same lexical frames, loop exits, and transfer
outcomes as statement traversal. Evaluate the scrutinee once and guards in source order. A guard
yielding Boolean false restores provisional binding/ownership state before the next candidate.
A transfer while evaluating a guard takes its ordinary exit after applicable provisional cleanup;
it never advances to another candidate. Selected bodies own
their pattern bindings and locals until normal completion or a transfer leaves their region.

Join only paths reaching the match continuation. Return, failure, break, and continue paths retain
their own exit plans; they do not contaminate the continuation's owner state. Keep earlier evaluated
argument and aggregate temporaries alive until transferred into storage or released by the selected
exit. The abandoned outer call, initializer, or assignment must not consume or store their values.
Release live arm owners and omitted consumed fields exactly once, end narrowed loans before owner
cleanup, and respect the existing reverse acquisition order. A local scope-copy around block
descendants alone cannot provide these transfer semantics.

### Lower selected bodies as regions with explicit normal completion

Replace the inline-only selected-arm representation with region-backed execution whose completing
paths yield a result and whose transferring paths preserve ordinary MIR region outcomes. Expression
arms use this same representation. A normally completing statement arm yields unit; a transferring
arm has no result assignment. The match join destination exists only when a completing path needs
it. Preserve canonical coverage identities, source order, access mode, binding extraction, and guard
fallthrough.

Make lowering of larger expressions sequence these regions in source evaluation order. Continuing
work is attached only to normal exits; a statically noncompleting expression has no usable value.
Do not overload the existing `undefined` lowering-failure result to mean a successful transfer, and
do not manufacture an initialized placeholder local for `never`. Partial transfers use branch
regions while their completing paths continue to the rest of the parent expression.

Guards also use region-backed evaluation: a nested match in a guard may transfer, so only a normal
Boolean false result advances to another candidate. Require Boolean type only on a guard's normally
completing paths; an all-transferring guard has no Boolean value or result local. Reuse the current
statement-region lowering and cleanup machinery with the enclosing loop and return destinations.
Update MIR verification, encoding, linearization, and LLVM lowering for the
new selected-region/result contract, including existing statement pattern selections that share
match lowering. The implementation must not accidentally change JUL-106's independent retained
if-let behavior. LLVM branches to existing exits and copies the join result only from completing
predecessors; it must not reinterpret source transfer rules.

### Extend compile-time evaluation with explicit outcomes

Implement canonical match selection in StaticEvaluation, including member/pattern bindings and
source-ordered guards, and execute the selected expression or ordinary statement body eagerly.
Use explicit value-or-transfer outcomes when composing expressions and statements so a transfer
inside an argument propagates to the enclosing execution boundary. Evaluate later arguments and
perform binding storage only after preceding expressions yield values. Consume break/continue only
at their target loop and return only at the current invocation. Preserve static value semantics,
existing finite evaluation budgets, diagnostics, and specialization traces. No runtime evaluator is
introduced. Existing static admission restrictions remain in force: match support does not admit
runtime Effect execution into static functions.

### Format and document the source behavior

Reuse ordinary block statement layout inside the canonical match arm layout: empty uncommented
blocks remain `{}`, sequential statements indent one level inside the arm, and nested matches
retain grouping. Preserve comments adjacent to patterns, arrows, braces, and statements. Valid
output must reparse and be idempotent; incomplete syntax follows the existing typed formatter
failure contract.

Update the control-flow and Effect reference pages with eager arm examples, unit/never joining,
ordinary transfers, and the distinction from explicit Effect/callable expressions. Keep SEM0087 for
non-unit expression statements, SEM0049 for incompatible arm contributions, and SEM0038 for an
out-of-loop transfer. Change diagnostic catalog entries or examples only if the implementation
requires a changed diagnostic contract; preserve codes and assert their source spans.

## Risks / Trade-offs

- Expression transfers affect many callers of expression traversal and lowering → migrate every
  caller under strict TypeScript and verify nested-expression exits structurally and in the corpus.
- Guard failure and partial transfers can duplicate cleanup or leak loans → assert ownership plans
  for normal/return/failure/loop exits and use a native cleanup-order oracle.
- Generic specialization can merge coverage members or invalidate a contributing arm → retain
  current canonical coverage revalidation and exercise expression/block mixtures after application.
- Added tests can multiply full compiler invocations → share one Analysis snapshot per program,
  keep semantic claims at analysis tier, and use only the shared native corpus for runtime claims.
- Broad traversal changes can alter explicit Effects/callables → preserve regression cases for
  capture, deferred execution, finite Effect joins, and their separate transfer targets.

## Verification Design

Use existing parser and formatter files for a small set of distinct grammar/recovery examples.
Use structured Analysis assertions for completion, joins, scope, return contracts, rows, transfer
targets, and ownership exit plans. Cover empty/sequential blocks, explicit drop versus bare non-unit
statements, partial versus all-arm transfer, inner-loop break versus outer-loop transfer, and a
match nested in an argument or initializer. Include both guard rejection and selected-body cleanup,
borrow ending, consumed omissions, generic specialization, and explicit Effect/callable boundaries.

StaticEvaluation proves compile-time selection and transfer. The native acceptance corpus pins
eager order, branch selection, mutation, abandoned outer-expression work, and cleanup with
independent expected outcomes. IR/MIR assertions prove no join write or result read on a transfer.
Do not add a per-feature native compilation test, a new determinism subprocess, or a runtime timing
assertion. An independent test-economics reviewer must inventory the exact committed diff, compare
focused base/branch timings under equivalent conditions, and approve before implementation handoff.

## Migration Plan

This is one green-field compiler change. Replace the arm representation and update all consumers,
tests, fixtures, generated output, and documentation together; keep no compatibility path. Strictly
validate all OpenSpec artifacts before source implementation. Run typecheck, format check, lint,
tests, and the aggregate check in the required order; run release-candidate validation when package
contents or exports change. Commit and publish the verified implementation as a draft PR and link
its exact head and evidence to JUL-105. If rollback is needed, revert the complete feature rather
than retaining both arm representations.
