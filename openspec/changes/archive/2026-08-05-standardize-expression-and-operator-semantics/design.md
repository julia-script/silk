## Context

See `proposal.md` for motivation. The compiler currently parses expressions by choosing one leaf or
call form, while elaboration already resolves qualified `I32` arithmetic/comparison and `Bool.not`
calls into a closed HIR builtin vocabulary. MIR, the interpreter, LLVM, and WebAssembly already
implement the corresponding binary operations. Cross-module name resolution and target-aware layout
are complete, so operator and pipeline syntax can normalize into those authorities.

The design must preserve the lossless `SyntaxFile`, explicit recovery, one-pass closure-wide facts,
facade-only tooling, deterministic encoders, and Wayfinder's qualified data-first behavior model.

## Goals / Non-Goals

**Goals:**

- Give every expression position one recursive precedence parser with exact concrete ownership.
- Publish a closed operator vocabulary and immutable resolution facts rather than hiding mappings in
  parser control flow.
- Make operators and pipelines semantic sugar over canonical actor/module calls.
- Keep target selection, layout, MIR, interpretation, and backend emission unchanged in authority.
- Preserve useful partial facts after syntax and type errors.

**Non-Goals:**

- User-defined operators, overload sets, implicit conversions, numeric promotion, truthiness,
  short-circuit boolean operators, bitwise/shift operators, assignment, indexing, ranges, or method
  lookup.
- General callable-value pipelines, partial application objects, or runtime pipe values.
- Adding scalar types beyond the shipped `I32` and `Bool` slice.

## Decisions

### 1. Use precedence climbing over lossless concrete nodes

Replace expression-kind lookahead with a precedence-climbing parser. Primary parsing handles
literals, identifiers, moves, calls, and grouped expressions; prefix parsing handles `-` and `!`;
the loop then folds infix operators by precedence. Each fold constructs a concrete node containing
both operand subtrees, the operator token, and intervening trivia exactly once.

Relational and equality levels accept one operator. A second ungrouped operator is left for bounded
recovery, making their non-associativity visible instead of silently creating a type-error-shaped
tree. This is preferable to a semantic-only rejection because the concrete grammar owns grouping.

Alternative: parse every operator left-to-right and repair precedence during elaboration. Rejected
because it makes the concrete tree misleading and forces semantic tooling to reconstruct syntax.

### 2. Keep directly signed decimals as literal syntax

`-` directly applied to a decimal literal remains one `IntegerLiteralExpression` branch so
`-2147483648` stays representable without first constructing the out-of-range positive value.
Every other `-` creates a prefix expression resolving to `I32.negate`. `!` always creates a prefix
expression resolving to `Bool.not`.

Alternative: represent all negation uniformly as a call. Rejected because the signed minimum would
need an exceptional contextual literal rule later in elaboration and would lose the existing exact
literal fact.

### 3. Publish one closed Operator actor

Add an `Operator` actor defining the public closed prefix/infix vocabulary, token mapping,
precedence/associativity metadata, display spellings, and semantic operation names. The parser uses
only its syntactic metadata; elaboration combines its semantic mapping with operand facts. This
avoids duplicating the table across parser, semantic facts, encoders, and labs while keeping
operator lookup finite and compiler-known.

Equality is the only type-selected spelling: identical available `I32` operands map to
`I32.equals`/`notEquals`, and identical available `Bool` operands map to
`Bool.equals`/`notEquals`. All other spellings have one fixed actor contract. A mismatch is reported
through the existing `SEM0012` argument diagnostic, not a new overload-resolution diagnostic.

Alternative: desugar tokens into synthetic call syntax before elaboration. Rejected because
synthetic tokens would violate source ownership and obscure operator-specific semantic facts.

### 4. Model operator and pipeline facts, then erase them in HIR

Elaboration adds explicit `Operator` and `Pipeline` expression facts containing the surface syntax,
operand/argument facts, resolved operation or declaration reference, effective mappings, contract,
type, and causal unavailable state. HIR construction consumes those facts and emits the existing
`BuiltinCall` or `Call` variants.

The HIR builtin vocabulary gains `Negate`; lowering realizes it as a generated zero literal plus the
existing trapping `Subtract` MIR operation. `Bool.equals` and `Bool.notEquals` reuse MIR equality.
No MIR surface-operator or pipeline variant is added.

Alternative: retain operators in HIR. Rejected because actor-call normalization belongs in
elaboration and every downstream consumer already understands the canonical operations.

### 5. Parse a pipeline target as a constrained qualified call tail

At the lowest precedence level, `|>` consumes a required qualifier, dot, member, and optional
argument list. Absence of parentheses means zero explicitly supplied later arguments, matching
Wayfinder's `flag |> Bool.not`; parentheses retain their usual ordered argument list. Elaboration
analyzes the left expression first, then explicit arguments left-to-right, inserts the left fact at
ordinal zero, and uses existing builtin or namespace-qualified resolution.

Pipelines associate left-to-right. Selected bare functions and arbitrary callable values are not
pipeline targets because the accepted behavior model keeps actor operations qualified.

Alternative: rewrite `|>` as a general binary operator whose right operand is any expression.
Rejected because that would accidentally define callable values and partial application before
their contracts exist.

### 6. Verify surface equivalence at every retained artifact

Tests pair operator/pipeline programs with their complete qualified-call forms at semantic facts,
HIR, MIR operation structure, interpreter result/trap, native execution, and WebAssembly execution.
Syntax and HIR/MIR goldens cover precedence and provenance independently. Supply-order and
fresh-process checks continue to gate deterministic encodings.

The docs extend the existing syntax/HIR, MIR, LLVM, WebAssembly, and pipeline labs through the
analysis facade. No lab imports the Operator actor to recreate compiler decisions.

## Risks / Trade-offs

- [Precedence parsing can absorb statement or call delimiters during recovery] → Keep the existing
  statement/declaration synchronization set at every missing operand and add focused following-node
  preservation tests.
- [Grouping conflicts with the old recovered missing-callee `()` form] → Treat `()` as the existing
  damaged call only when no expression can begin inside; otherwise parentheses own a group.
- [Operator facts duplicate some call-contract shape] → Reuse the same immutable reference,
  argument, mapping, contract, and type vocabularies rather than creating parallel contracts.
- [Generated zero for negation changes MIR provenance detail] → Mark zero generated and the
  subtract operation source-authored at the complete prefix span; gate the encoding intentionally.
- [Pipeline syntax could be mistaken for general method dispatch] → Require a visible qualified
  path in grammar and diagnostics, and show the canonical actor/module target in the lab.

## Migration Plan

Implement the new public token/node/fact variants and migrate all exhaustive consumers in one
change; backward compatibility is intentionally not preserved during alpha. Refresh deterministic
goldens and the packed export contract, run the full differential corpus on native and WebAssembly,
then archive only after strict OpenSpec and release-candidate validation pass. Rollback is the
single change revert because no persisted user data or external protocol changes.
