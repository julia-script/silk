## Context

`@silklang/compiler` currently ends at `Parser.ParseResult`, which retains the lexical result,
a closed lossless concrete tree, and parser diagnostics. The grammar guarantees one recovered
function region, one return-type region, and one decimal-integer-expression region even when their
required tokens are missing. See `proposal.md` for motivation and the delta specs for observable
behavior.

The accepted compiler-pipeline decision calls for immutable fact products separate from syntax and
delays HIR until real semantic operations need it. The package is pre-release, so this first fact
shape may improve when a second declaration or expression form supplies contrary evidence.

## Goals / Non-Goals

**Goals:**

- Establish one small public semantic-analysis boundary whose result retains syntax provenance and
  makes unavailable knowledge impossible to confuse with valid knowledge.
- Give each diagnostic phase clear ownership while allowing one consumer to inspect the complete
  source-to-semantics chain.
- Keep deterministic identity, lookup, ordering, and numeric interpretation testable without a
  generalized database or query engine.

**Non-Goals:**

- A semantic tree, annotated CST, AST-lowering pass, HIR node vocabulary, or generic fact-table
  framework.
- Stable identity across source edits, multiple-source identity, dependency graphs, incremental
  invalidation, or memoization.
- A general type universe, integer-literal inference, coercion, mismatch recovery, constant
  evaluation, or target layout.

## Decisions

### 1. Add two public actors around one immutable analysis result

`SemanticAnalysis` owns the analysis result, the closed first-slice fact states, deterministic
declaration identity and name lookup, and the pure `analyze` operation. `SemanticDiagnostic` owns
the semantic diagnostic codes and reason data. Both are exported as namespaces from the compiler
root and through explicit package subpaths.

The result retains the exact `Parser.ParseResult` and contains one declaration fact, one declared
return-type fact, one integer-expression fact, one return-compatibility fact, and ordered semantic
diagnostics. It does not aggregate the three diagnostic collections into a new generic diagnostic
type; callers can reach lexical and parser diagnostics through the retained parse result.

Separate `Declaration`, `Type`, `Value`, and fact-table modules were rejected for this slice because
each would have one instance and no independent operation. A single semantic AST was rejected
because it would duplicate syntax shape and blur facts with structure before either HIR or a second
expression exists.

### 2. Model each fact with a small purpose-specific discriminated state

The analysis result uses closed immutable states rather than `undefined`, nullable fields, or one
generic `Fact<A>` abstraction:

- the declared name is `Present` with its ASCII spelling and original identifier token, or
  `Unavailable` with syntax provenance;
- the declared return type is `Resolved` to the single `I32` semantic type, `Unresolved` with its
  present identifier token and spelling, or `Unavailable` because syntax could not supply a token;
- the integer expression is `Available` with semantic type `I32`, a safe exact numeric value, and
  its original decimal token; `OutOfRange` with the original token; or `Unavailable`;
- return compatibility is `Compatible` or `Unavailable`.

The first declaration identity is a frozen structured value containing the source identity and
declaration ordinal zero. Equivalent source snapshots therefore produce equal identity data while
different logical sources do not collide. The declaration also retains its function node. A tiny
name lookup returns the declaration only when its name fact is present and exactly matches.

One reusable generic unavailable-state abstraction and stable syntax-derived IDs were rejected.
The former would hide why the individual fact is unavailable; the latter would promise edit
stability that this batch compiler neither needs nor can yet validate.

### 3. Read semantic roles from the fixed concrete node shape without rewriting it

Analysis locates the first `FunctionDeclaration`, its direct declared-name token, the nested
`ReturnType` identifier, and the nested `IntegerLiteralExpression` decimal token by node kind and
token kind. It does not depend on numeric child offsets because trivia and recovery error nodes can
change those positions. It retains the found nodes and tokens directly as provenance.

A matching concrete token is analyzed even when another part of the same function has parser
damage. A missing leaf or error region at the expected semantic role produces an unavailable fact.
The semantic phase does not copy parser diagnostics or manufacture a fallback name, type, or
value. Violating the parser's closed root-shape invariant remains a compiler defect; ordinary
missing and error elements do not.

An intermediate simplified AST was rejected because these four role lookups are direct, bounded,
and already tested by the concrete grammar. The decision must be revisited when repeated syntax or
desugaring makes direct role lookup ambiguous.

### 4. Interpret spellings and decimal values exactly at the semantic boundary

Identifier tokens are ASCII by lexer contract. Analysis obtains their bytes through their owning
source spans, compares the exact `I32` bytes, and derives display spelling without locale-sensitive
case folding. No other identifier is treated as a built-in type.

Decimal bytes are accumulated or parsed with an exact integer representation before comparison to
`2147483647`. Only an in-range result becomes a JavaScript `number`, which is exact for every valid
`I32` value in this grammar. The browser view therefore avoids serializing public `bigint` values
while analysis never passes through an imprecise host number. Negative values remain impossible in
the current grammar rather than being anticipated here.

### 5. Give semantic source mistakes stable ownership

`SEM0001` belongs only to a present identifier that is not the built-in `I32` spelling. `SEM0002`
belongs only to a present decimal token whose exact non-negative value exceeds the positive `I32`
maximum. Missing or recovered syntax belongs to the parser and produces no semantic duplicate.

Semantic diagnostics carry reason data and the original token span, then sort by span and code
using locale-independent comparisons. `SemanticAnalysis.analyze` is a pure total operation over a
valid parse result: source mistakes are immutable output data, not an Effect error channel.

A shared cross-phase diagnostic hierarchy was rejected because no caller yet needs to recover from
all phases uniformly, and merging would erase phase ownership that the inspector is meant to show.

### 6. Extend the inspector as a view of the analysis result

The client computes `SemanticAnalysis.analyze(Parser.parse(Lexer.lex(source)))` in memory. A compact
semantic section shows declaration identity and name, declared type, integer type/value, return
compatibility, and semantic diagnostics with their spans. Unknown-type and overflow presets join
the existing syntax-recovery presets.

The concrete tree remains independently visible and unchanged. The route stays outside Fumadocs
content, navigation, and search, and it gains no storage, worker, API route, or server compilation.

## Risks / Trade-offs

- **[Risk] One-slot facts could be mistaken for the permanent multi-declaration architecture.** →
  Document the closed first slice and require the next declaration change to reassess result shape
  before adding collections mechanically.
- **[Risk] Direct CST role lookup could silently select a token from an error region.** → Traverse
  only the specified direct or nested grammar node, distinguish ordinary tokens from `Error` and
  missing elements, and test damaged versions of every semantic role.
- **[Risk] Semantic diagnostics could echo syntax damage and overwhelm users.** → Emit only for
  present, grammatically assigned tokens whose meaning is invalid; test diagnostic ownership across
  all three phases.
- **[Risk] Host-number conversion could hide literal overflow.** → Interpret exactly first, convert
  only after the positive `I32` bound check, and test both the boundary and values well beyond host
  safe-integer precision.
- **[Risk] The inspector could imply a much broader language than exists.** → Label facts as the
  first bootstrap slice and state the explicit boundary before AST, HIR, and code generation.

## Migration Plan

The change is additive. Implement semantic data and diagnostics first, add analysis and fixtures,
publish the new namespaces and packed-import checks, then extend and browser-test the existing
hidden inspector. Rollback removes the new public exports and semantic view without changing source,
lexical, or concrete-syntax behavior.
