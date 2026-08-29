## Context

See `proposal.md` for motivation. The parser is a synchronous immutable-state recursive descent
parser. Expression recursion is distributed across `parseExpression`, precedence descent,
prefix operands, grouped expressions, argument/container parsing, projections, aggregate values,
and match components. `reservedForEnclosingCalls` counts closing parentheses for recovery and is
not an expression-depth measure. `Parser.parse` currently has no exception boundary, while parser
invariants deliberately throw `RangeError` from helpers such as `syntaxNode`; those defects must
remain distinguishable from source diagnostics.

The current repro reaches the host stack through grouping, arrays, calls, and prefixes, and the
same defect escapes through the executed `Analysis.ofSource` Effect. Existing `SyntaxFile`
invariants require token-object identity, source order, and byte reconstruction even through error
nodes.

## Goals / Non-Goals

**Goals:**

- Make the 256-edge contract explicit at every recursive expression-child transition.
- Recover a rejected region in linear source time without further recursive descent.
- Preserve existing syntax shapes and diagnostics for sources whose maximum depth is at most 256.
- Keep the existing parser-invariant defect boundary intact.
- Make boundary and recovery behavior directly falsifiable at parser, syntax-file, and analysis
  seams.

**Non-Goals:**

- Rewriting the complete parser as an iterative parser or changing expression precedence.
- Applying this expression budget to recursive type or pattern grammars; those require separate
  evidence and contracts.
- Making the nesting limit configurable per request or target.
- Adding a public package export or compiler intrinsic for the implementation constant.

## Decisions

### 1. Thread an expression depth independently from parser state and call-delimiter recovery

Expression parsing receives an explicit depth, with non-expression callers supplying zero.
Parser-layer transitions that still describe the same expression—expression to precedence,
precedence to prefix, and prefix to primary—preserve that depth. Transitions into a syntactic child
expression pass `depth + 1`: group contents, prefix and operator operands, pipeline targets,
postfix indexes and arguments, container elements, aggregate initializers, and match children.
Sequential siblings all derive from the same parent depth.

The limit and depth operations live in one parser-owned expression-nesting actor so implementation
and tests share one value. `reservedForEnclosingCalls` remains separate and retains its current
delimiter-recovery meaning.

Alternatives considered:

- Reuse `reservedForEnclosingCalls`: rejected because it tracks only selected parentheses, resets in
  projection parsing, and cannot count arrays or prefixes correctly.
- Store active depth in immutable token state: rejected because depth follows nested calls rather
  than token position; restoring it on every return would couple stream state to call-stack state.
- Count all concrete syntax nodes: rejected because parser-layer and recovery nodes are not
  source-level expression-child edges and would make the public boundary implementation-shaped.

### 2. Check depth before recursive descent, never by catching host exceptions

Every entry that can begin an expression child checks the attempted depth before calling the
recursive parser. Depths zero through 256 continue normally. Attempted depth 257 enters explicit
source recovery and produces `PAR0005`; no `try`/`catch` is added around `Parser.parse`,
`parseExpression`, or `Analysis.ofSource`.

This keeps existing `RangeError` throws from invalid token ordering, invalid insertion positions,
and other parser invariants as defects. It also makes behavior independent of JavaScript engine
stack size.

Alternatives considered:

- Catch `RangeError` at `Parser.parse`: rejected because it cannot distinguish source depth from
  invariant bugs and would only run after the stack is already exhausted.
- Use a private thrown escape token: rejected because the budget is known before descent and normal
  return-based recovery is simpler; no stack-wide early termination is needed.
- Convert all expression parsing to an explicit work stack: rejected as unnecessary scope for the
  bounded bootstrap grammar; the over-budget scanner alone must be iterative.

### 3. Recover one maximal expression region with an iterative delimiter-aware scanner

The over-budget branch first retains any leading trivia, locates the first significant token for
the diagnostic span, and then scans original lexer tokens without recursive parser calls. A local
delimiter stack tracks parentheses, brackets, and braces. At delimiter depth zero, the scanner
stops before the owning expression's comma, closing delimiter, statement/declaration boundary, or
end-of-file. Balanced delimiters belonging to the rejected child remain inside its region; an
unmatched delimiter belonging to the parent remains for the parent parser.

The scanner consumes at least the first significant token, collects original token objects in
source order, creates one `Error` node, and returns one state update at the final token index. Its
per-token loop uses mutable local arrays and freezes once at the boundary, avoiding recursive calls
and repeated immutable-array copying for attacker-controlled depth. A focused comment records this
linear scanning requirement.

Because the entire maximal region becomes one branch, nested rejected syntax cannot produce
additional `PAR0005` diagnostics. Returning the parent delimiter untouched lets the ordinary parser
unwind and synchronize. A later independent expression begins again at depth zero and can produce
its own diagnostic.

Alternatives considered:

- Reuse ordinary `expect` recovery: rejected because it has no nested-delimiter stack and can stop
  too early or consume a parent boundary.
- Consume only the first offending token: rejected because the remaining deeply recursive tokens
  would immediately re-enter the parser and could produce cascades or another overflow.
- Consume through end-of-file: rejected because it destroys following statements and declarations.

### 4. Add a dedicated structured diagnostic and keep syntax ownership unchanged

`Diagnostic` gains code `PAR0005`, reason `ExpressionNestingLimitExceeded`, and a constructor that
records limit 256, attempted depth 257, and the half-open span of the first significant token in the
rejected child. The recovered `Error` node may include leading trivia and the full rejected region;
the diagnostic span remains the single decisive token.

The token stream and token objects are not copied or synthesized. `SyntaxTree.tokens` over the root
must equal the `SyntaxFile.tokens` sequence by identity and order, and the existing textual encoder
must expose the error branch and diagnostic deterministically. Generated diagnostic documentation
is regenerated from the same catalog source.

### 5. Test the contract at the cheapest seams that can falsify it

Parser tests generate sources from the shared limit rather than a host overflow threshold and
cover depth 255, 256, 257, and a much deeper value. Shape cases include grouped expressions,
arrays, calls/containers, and direct prefixes; additional assertions cover one versus two
independent diagnostics, exact spans, following-statement/declaration recovery, original-token
identity, and byte reconstruction. Existing shallow fixtures provide the unchanged-syntax control.

One `it.effect` analysis test executes `Analysis.ofSource` for an over-budget source and asserts the
snapshot diagnostic/recovery data. A focused invariant test invokes a parser invariant that throws
`RangeError` outside the depth branch and asserts that it remains thrown. No native or Wasm leg is
needed because the behavior ends in the frontend parser.

## Risks / Trade-offs

- [The scanner may stop at a delimiter owned by the rejected child] → Exercise every delimiter
  family with balanced, damaged, and following-syntax cases; assert exact token identity and byte
  reconstruction.
- [A recursive expression call site may fail to increment depth] → Route child transitions through
  the shared depth helper and keep independent stress cases for grouping, arrays, calls, prefixes,
  operators, projections, aggregates, and matches where the parser shape differs.
- [The 256 limit may reject machine-generated but otherwise valid source] → Keep the limit fixed,
  documented, and comfortably above ordinary authored syntax; the deterministic diagnostic and
  recovered artifact make the rejection actionable.
- [Scanning a very large rejected region may allocate proportionally to source size] → Retaining
  each original token is required for losslessness; use one linear pass and one final freeze without
  recursive frames or quadratic array rebuilding.
- [Recovery state may suppress an independent diagnostic] → End recovery when the owning delimiter
  is returned to ordinary parsing and test two separate over-budget expressions explicitly.

## Migration Plan

Implement the depth actor and diagnostic first, then update every expression-child call site and
add the iterative recovery branch. Add parser/syntax-file tests before the analysis-facade test,
update the prescriptive expression reference and generated diagnostic catalog, and run the full
repository verification order. This is a green-field contract change with no compatibility shim or
data migration. Rollback is removal of the change as one unit; partial rollback would restore the
host-stack defect and is not supported.
