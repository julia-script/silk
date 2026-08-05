# `@silk-effect/compiler`

`@silk-effect/compiler` contains the bootstrap layer of the Silk Effect compiler: immutable
source bytes and source-owned spans, the lossless per-module `SyntaxFile` artifact, module
closure loading, the canonical declaration index, HIR elaboration with unified diagnostics, and
the closed bootstrap evaluator — all reachable through one supported analysis facade.

```ts
import { Analysis } from '@silk-effect/compiler'

const snapshot = Analysis.ofSource(
  'memory://example.silk',
  new TextEncoder().encode(`pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(42) }`),
)
const result = Analysis.rootAnalysis(snapshot)

console.log(result.syntax.root.kind) // SourceFile
console.log(result.functions.length) // 2
console.log(result.hir.functions[1]?.body) // { _tag: 'Call', target: { _tag: 'CanonicalDeclarationId', ... }, ... }
console.log(Analysis.declarationByName(snapshot, 'memory://example.silk', 'main')) // { _tag: 'Resolved', ... }
console.log(Analysis.diagnostics(snapshot)) // []
console.log(Analysis.evaluate(snapshot)._tag) // Completed
```

## The facade is the supported consumer surface

Tooling consumes compiler phases exclusively through `Analysis`: build a snapshot from a
compilation request (or one source), then query immutable facts — sources, syntax, imports and
cycles, declarations, references, types, contracts, HIR, evaluation, and the compilation's
diagnostics merged in deterministic driver order. The immutable data-model vocabularies
(`SyntaxTree`, `SourceFile`, `SourceSpan`, `Token`, `Diagnostic`, `Hir`, and the fact type
namespaces) are part of the facade's answers and remain importable, including as type-only
imports. Running phase modules directly (`Lexer`, `Parser`, `ModuleClosure`,
`DeclarationIndex`, `Elaboration`, `BootstrapEvaluation`) is not a supported consumer surface.
Bootstrap does not implement every future editor query, but its identities, recovery states,
provenance, and phase boundaries let the facade grow without reimplementing Silk semantics in a
separate tool.

## Byte and span conventions

- Source input is an arbitrary byte sequence, not assumed to be valid UTF-8.
- `SourceFile.make` copies its input and attaches a caller-provided logical identity.
- A `SourceSpan` is an owner-qualified half-open byte range `[start, end)`.
- Empty spans represent positions; EOF is `[sourceLength, sourceLength)`.
- A source only returns bytes for a span with the same identity and in-bounds offsets.

## Bootstrap lexer vocabulary

The lexer recognizes ASCII identifiers, the `pub`, `fn`, and `return` keywords, decimal integers,
parentheses, braces, colons, commas, `->`, whitespace, and `//` line comments. Trivia is retained as tokens.
Unsupported bytes form maximal `Invalid` tokens and ordered `LEX0001` diagnostics, so lexing always
makes progress and every input byte can be reconstructed from the non-EOF token spans.

## Bootstrap concrete grammar

The parser recognizes exactly this grammatical slice, with whitespace and `//` line comments
allowed between its elements:

```text
File                → FunctionDeclaration+ EOF
FunctionDeclaration → pub fn Identifier(ParameterList?) -> Identifier { return ReturnExpression }
ParameterList       → Parameter ( , Parameter )*
Parameter           → Identifier : Identifier
ReturnExpression    → DecimalInteger | Identifier | CallExpression
CallExpression      → Identifier ( ArgumentList? )
ArgumentList        → Argument ( , Argument )*
Argument            → DecimalInteger | Identifier | CallExpression
```

The result is a concrete syntax tree (CST), not a semantic AST. Its nodes group the source into one
or more direct function declarations in source order. Each declaration contains a parameter list,
return type, block, return statement, and an integer, bare-identifier, or call expression. Typed
parameter declarations and integer, identifier, or recursively nested call arguments retain their
own ordered concrete nodes, separators, and trivia. Nested calls reserve the closing tokens required
by their enclosing calls, so one missing inner `)` does not consume the outer call's delimiter.
Unexpected tokens inside lists remain explicit error regions. Every lexer token—including trivia,
invalid tokens, and EOF—remains the same object in the tree and appears exactly once in source order.
A following `pub` bounds both block and damaged-call recovery so the next declaration remains
separate.

Ordinary source mistakes remain data. A required absent token becomes a `MissingToken` leaf with an
empty span and a `PAR0001` diagnostic. Unexpected concrete input becomes a lossless `Error` node
and a `PAR0002` diagnostic. Lexical diagnostics remain separate on the retained lexical result;
`Parser.parse` does not throw or fail an Effect for these mistakes.

## Bootstrap semantic facts

`SemanticAnalysis.analyze` retains the exact parse result and publishes an immutable ordered
`functions` collection. Each `FunctionFact` groups one declaration, a closed integer, identifier, or call
`returnedExpression`, and return compatibility. Declaration identities combine the source identity with the function's
zero-based concrete-source ordinal; missing names do not change later ordinals. Each declaration
also retains public visibility, its exact concrete parameter count, a present or unavailable name, and a resolved,
unresolved, or unavailable declared return type.

Each declaration publishes its ordered parameter facts. A parameter identity nests its concrete
ordinal under the owning function identity, so same-spelled parameters in different functions stay
independent. Parameter names and declared types retain exact concrete provenance. The exact `I32`
type resolves through the same type rule as function returns; unknown present types produce
`SEM0001`, while parser-damaged names and types remain unavailable without duplicate diagnostics.

`SemanticAnalysis.parameterByName` performs function-local lookup with closed `Resolved`, `Missing`,
and `Ambiguous` outcomes. It never sees parameters from another function or top-level function
names. Every later duplicate present parameter name produces `SEM0005` at its declaration while all
matches remain available in source order.

`SemanticAnalysis.declarationByName` supports data-first and pipeable lookup with closed `Resolved`,
`Missing`, and `Ambiguous` outcomes. It never silently selects the first duplicate. Missing recovered
names do not enter lookup, while every present duplicate after the first produces `SEM0003` at the
later name span and retains the original name span in its reason data.

This slice recognizes only the exact ASCII type spelling `I32` and positive decimal values from
`0` through `2147483647`. It interprets token bytes without host-number precision loss. A present
unknown type produces `SEM0001`; a present integer above the boundary produces `SEM0002`. Every
function is analyzed independently. Missing or damaged syntax remains unavailable and belongs to
parser diagnostics, so lexical, parser, and semantic diagnostics remain separate ordered
collections.

Analysis collects every declaration header before resolving calls, so backward, forward, and self
references follow the same rule. A call reference is `Resolved`, `Missing`, `Ambiguous`, or
syntax-unavailable and retains exact call-site and target provenance. Resolution never silently
selects the first duplicate. A missing target produces `SEM0004`; an ambiguous target relies on the
declaration-owned `SEM0003` diagnostics without adding a redundant call-site error.

A uniquely resolved call uses its target declaration's resolved return type. That type is available
even when the target body has its own compatibility error, because this phase records a declaration
relationship rather than executing the function. Missing, ambiguous, syntax-damaged, or
unresolved-target-type calls remain unavailable. Integer returned expressions keep their existing
exact value and compatibility behavior.

Every usable bare identifier expression resolves only against the complete parameter collection of
its enclosing function. A unique match retains the exact parameter identity and supplies its
declared type to the expression; a missing match produces `SEM0006`; duplicate matches stay
ambiguous and rely on the declaration-owned `SEM0005`. Missing or damaged reference syntax remains
parser-owned and unavailable. The same rule applies to returned identifiers and identifier call
arguments.

Each usable call argument has a source-local identity, zero-based concrete ordinal, expression,
type, and exact syntax provenance. A uniquely resolved call maps argument ordinal `n` to target
parameter ordinal `n`. Its separate call contract is `Compatible` when counts match and every mapped
type is available, `ArityMismatch` when available counts differ, or `Unavailable` when syntax,
target resolution, or a mapped type is unavailable. Partial mappings remain visible across arity
mismatches. `SEM0007` reports the expected and actual counts at the complete call span without
changing the call's expression type or the caller's return compatibility.

`ExpressionFact` is recursive: integer literals, parameter references, and calls use the same
discriminated fact shape at returned and argument positions. Each nested call retains its own span,
target resolution, ordered argument identities, positional mappings, contract, and result type.
Analysis proceeds from nested leaves outward, so an unavailable inner type stops only the dependent
outer contract while all known inner facts remain inspectable. Argument identities combine the
enclosing function, owning call span, and concrete ordinal without introducing cyclic object
references.

These are direct semantic facts over the concrete tree—not a semantic AST or a general type
checker. The package intentionally does not yet contain an AST, HIR, MIR, LLVM lowering, or native
compilation. Top-level call resolution and positional contract checking exist, but conversions, a
general scope graph, and dependency scheduling remain deferred.

## Bootstrap evaluation is not compilation

`BootstrapEvaluation.evaluate` is a pure, direct interpreter over the existing semantic facts. It
selects one zero-parameter `main: I32`, follows only reachable decimal literals, resolved parameter
reads, and compatible calls, and returns either an exact `Completed` value or closed `Blocked`
reason. Its immutable trace records entry, call, positional binding, parameter read, and return
events with existing semantic provenance. Arguments, including nested calls, are evaluated fully
from left to right before their values are bound to the enclosing target. The trace records the
enclosing call first, then every nested argument event, then the enclosing bindings and target body.
An inner blocked reason propagates unchanged without claiming enclosing bindings or returns that did
not happen. Unreachable broken declarations do not block a valid entry path, and direct, mutual, or
nested-argument recursion becomes a bounded `RecursiveCycle` outcome.

This bootstrap evaluator proves that the frontend facts compose into one source-to-result vertical
slice. It is not lowering, bytecode, LLVM, native compilation, a process runtime, a general
interpreter, or a promise about future execution semantics. It performs no I/O and creates no AST,
HIR, MIR, runtime service, or persistent state.

Token families deliberately deferred with those later grammar decisions include string and
character literals, floating-point numbers, general operators, separators, attributes, and any
additional keywords. Until specified, their bytes recover predictably as `Invalid` regions.
