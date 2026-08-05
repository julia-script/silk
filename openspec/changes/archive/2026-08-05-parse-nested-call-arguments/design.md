## Context

The parser currently distinguishes returned calls from integer and identifier expressions, but an
argument accepts only the latter two shapes. Semantic argument facts and inspector projections make
the same closed assumption. This change must introduce recursive concrete structure without letting
downstream phases misclassify a shape they do not understand yet.

## Goals / Non-Goals

**Goals:**

- Reuse the existing call-expression grammar inside argument positions.
- Keep nested recovery local and preserve lossless token ownership.
- Give semantic analysis one honest unavailable placeholder for the new transitional syntax.
- Make the transition visually obvious in the inspector.

**Non-Goals:**

- Nested call resolution, type propagation, contract checking, evaluation, or data-flow edges.
- Operators, grouping expressions, arbitrary new call targets, AST, HIR, or MIR.

## Decisions

### Parse arguments through the shared expression entry

Argument parsing will recognize the same integer, identifier, or call-expression forms used by a
function return, while keeping the current grammar subset explicit. Duplicating a separate nested
call parser was rejected because its recovery rules would drift from top-level returned calls.

### Bound inner recovery at the nearest argument-list delimiter

An inner argument list treats its own closing parenthesis as primary, while comma, an enclosing
closing parenthesis, closing brace, and following `pub` remain recovery boundaries. Greedily
consuming until the outer call ends was rejected because one missing inner token would erase valid
sibling arguments and declarations.

### Add one transitional unavailable semantic expression

The semantic argument union will gain an explicit syntax-owned unavailable form for a nested call
that has not yet been analyzed. The enclosing contract becomes unavailable from that exact fact.
Pretending the node is an unavailable integer or identifier was rejected because it would falsify
the source shape; resolving it in this ticket was rejected because it would collapse the planned
parser and semantic checkpoints.

### Keep the transition observable

Parser tests cover structure and recovery, semantic tests prove no invented facts, and inspector
presets show both the nested CST and its unavailable semantic state. This preserves the project rule
that each ticket delivers immediate visual feedback.

## Risks / Trade-offs

- [Recursive parsing exposes user-controlled nesting depth] → Keep recursion confined to the
  expression parser, add representative deep-nesting coverage, and record stack-safety as a
  graduation concern if measured limits become relevant.
- [The transitional semantic variant briefly expands the public prerelease API] → Name it by its
  honest unavailable meaning and replace its behavior in the immediately following analysis change
  without a compatibility alias.
- [Recovery can confuse inner and outer parentheses] → Test damaged inner syntax beside sibling
  arguments, reserve required outer closing tokens, and cover following declarations rather than
  relying on an ambiguous bare comma to infer delimiter ownership.

## Migration Plan

Add recursive concrete parsing and recovery first, update semantic consumers to publish the
unavailable placeholder, then add inspector fixtures. Existing non-nested programs and their facts
remain unchanged. The next change replaces placeholder behavior with recursive semantic facts.
