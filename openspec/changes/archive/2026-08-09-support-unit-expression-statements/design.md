## Context

See [proposal.md](./proposal.md) for motivation. The parser already models `run` as a prefix
expression and can stop expressions at following-statement boundaries, but block dispatch only
enters a fixed set of statement forms. In a unit-returning block, an unrecognized expression start
therefore causes an implicit return to be inserted before the expression; right-brace recovery then
ejects the remaining tokens into top-level recovery. That is why one local source form currently
produces both an unexpected-token region and a phantom declaration cascade.

Statements are represented independently in concrete syntax, semantic facts, and HIR, then visited
by analysis, ownership, layout, deterministic encoding, and lowering. The implementation must add
one real statement variant through those layers rather than accepting the syntax only at the parser
edge. Silk also requires explicit ownership: accepting arbitrary discarded results would bypass the
language's binding and `drop` distinctions.

## Goals / Non-Goals

**Goals:**

- Add one compositional statement form for any expression compatible with unit or bottom.
- Preserve a standalone expression's identity and provenance through syntax, semantic facts, HIR,
  ownership, lowering, formatting, and inspection.
- Reuse existing expression evaluation, `run` propagation, and cleanup behavior.
- Make block recovery local and make unexpected-syntax diagnostics describe what was encountered,
  where, and what could validly follow.
- Lock down the reported `run foo()` cascade as a parser and end-to-end regression.

**Non-Goals:**

- Introducing semicolons or making newlines syntactically significant.
- Allowing arbitrary values to be silently discarded or implicitly dropped.
- Adding a `run`-specific statement grammar distinct from the existing run expression.
- Redesigning every existing parser diagnostic or introducing warning severities.
- Preserving the exact message or structured payload of the current generic unexpected-token
  diagnostic.

## Decisions

### 1. Add a general expression statement constrained to unit or bottom

The concrete form is an `ExpressionStatement` containing exactly one expression. Elaboration accepts
the statement when the expression type is compatible with `()` or `never`; all other available
types receive a dedicated semantic diagnostic carrying the actual type and actionable guidance.

This makes `run foo()` work without teaching the statement grammar about one expression operator,
and it also supports ordinary unit-returning calls and other future unit expressions. `never` is
accepted because a diverging expression has no value to discard.

Alternatives considered:

- **A dedicated run statement:** rejected because `run` already composes as an expression and a
  second grammar branch would drift in precedence, pipelines, recovery, and semantics.
- **Allow every expression and discard its value:** rejected because it hides mistakes and creates
  implicit destruction semantics for owned values.
- **Require a semicolon to express discard:** rejected because Silk currently uses neither
  semicolons nor significant newlines for statement boundaries, and the existing expression parser
  already stops at non-continuation tokens.

### 2. Give the statement first-class fact and HIR variants

Semantic analysis publishes an `ExpressionStatement` fact containing the elaborated expression,
region, and syntax provenance. HIR publishes an `Evaluate` statement with the expression, region,
and span. Every generic statement/expression traversal includes the new variant: statement queries,
semantic occurrences, unavailable checks, deterministic encoding, layout type discovery,
ownership use scanning, and call-target discovery.

The representation is not desugared to a nameless bind, return, or drop:

- a bind would invent an identity and liveness interval;
- a return would change control flow; and
- a drop would claim explicit consumption and cleanup that the author did not write.

An expression made unavailable by an earlier diagnostic produces the existing unavailable statement
shape with the originating cause retained where the surrounding model supports causes. This keeps
the statement in source order without issuing a second result-compatibility diagnostic.

### 3. Dispatch known statements before general expressions

Block parsing keeps explicit statement keywords (`let`, `return`, `if`, `while`, `fail`, `drop`,
`unsafe`, and transfers) at the highest dispatch priority. An identifier-led writable place followed
by `=` remains an assignment. Any remaining expression-start token enters expression-statement
parsing.

This order preserves existing assignment syntax while allowing identifier-led calls. Consecutive
expressions remain delimiter-free: the expression parser consumes one complete expression and stops
when the next token cannot continue it, after which block dispatch begins the next statement.

### 4. Recover malformed statement starts as block-owned branches

If a token cannot begin any supported statement, the block parser creates one error statement and
synchronizes at the next valid statement start or the current block's right brace. A malformed
expression statement similarly remains an expression/error statement in that block. The recovery
routine must never consume the owning right brace or hand a valid following statement to top-level
declaration recovery.

This fixes the cascade structurally instead of filtering the “Expected `{`” message after the
fact. Dependent missing elements may remain queryable in the CST, but only the independently
actionable primary diagnostic is published.

Alternative considered: retain current recovery and suppress the second diagnostic in presentation.
That was rejected because the syntax tree would still contain a phantom declaration and downstream
analysis would observe the wrong structure.

### 5. Make unexpected-token reasons contextual data

The unexpected-syntax reason carries:

- the non-empty ordered unexpected token kinds;
- a parser context such as statement, expression, parameter, or delimiter; and
- expected source-language spellings or grammatical roles available at the recovery site.

Diagnostic construction uses source-language descriptions to produce messages such as
“Unexpected `)` while parsing a statement” and notes describing valid starts. Low-level token
expectation supplies the expected token; construct-level recovery supplies the grammatical context.
The source span remains the authority for rendering the exact offending text.

The stable unexpected-token code may remain `PAR0002`, but its structured reason and message become
contextual. Purpose-built diagnostics continue to own cases where the parser already knows a more
specific source mistake.

### 6. Lower evaluate statements through the existing expression path

Lowering evaluates the HIR expression once, uses the existing expression and run-continuation
machinery, ignores only the unit representation, and forwards to the next statement on success.
Because semantic validation admits no owned non-unit result, this branch introduces no implicit
cleanup. Diverging expressions retain their terminal outcome rather than forwarding.

Ownership treats the evaluate expression as a statement root. Run-site propagation, loan endings,
and exit cleanup are therefore computed exactly as for the same expression under a bind or return.
The evaluator and native/Wasm backends need no new user-visible operation if the shared MIR lowering
produces the existing regions and operations.

### 7. Format and inspect the authored statement directly

The formatter prints `ExpressionStatement` through ordinary expression formatting inside the
existing block indentation and comment rules. Analysis and syntax-inspector projections expose the
new statement kind and HIR evaluate form rather than displaying a synthetic construct.

## Risks / Trade-offs

- **[Risk] Identifier-led malformed assignments could be reclassified as expressions.** → Keep the
  existing complete-place-plus-`=` lookahead ahead of expression dispatch and add damaged-assignment
  recovery tests.
- **[Risk] A new statement variant can be missed by one of the many exhaustive traversals.** → Let
  TypeScript exhaustiveness surface omissions, enumerate every known traversal in the task list,
  and add deterministic HIR plus analysis-surface coverage.
- **[Risk] Lowering a run expression without a destination could skip propagation cleanup.** → Route
  `Evaluate` through the same expression/run lowering and ownership run-site indexing used by bind
  and return, then test failure after live owned values across all engines.
- **[Risk] More detailed diagnostic payloads break snapshot or external consumers.** → Keep the
  stable code where practical, document the reason-shape change, update all presentations and
  fixtures atomically, and validate deterministic ordering.
- **[Trade-off] Whitespace alone separates adjacent statements.** → This is consistent with current
  Silk grammar; formatter output places each statement on its own line to keep the source readable.

## Migration Plan

1. Land the syntax, semantic, HIR, and lowering changes together so no intermediate revision accepts
   syntax it cannot analyze or execute.
2. Update diagnostic structured-reason consumers, snapshots, and inspector presentations in the
   same change.
3. Validate the original failing source, non-unit rejection, damaged-statement recovery, formatting
   idempotence, deterministic artifacts, and evaluator/native/Wasm parity.

Rollback is a source revert; there is no persisted data migration. Programs using standalone
expression statements would need to return or bind the expression again after rollback.
