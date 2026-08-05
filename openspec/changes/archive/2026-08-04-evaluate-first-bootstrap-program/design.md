## Context

After the preceding changes, semantic analysis can describe a reachable bootstrap program using
exact integer values, local parameter references, resolved function calls, and checked positional
contracts. No execution representation or runtime exists. Because the expression slice is closed
and pure, the first vertical result can be evaluated directly from semantic facts without creating
an intermediate representation whose eventual shape is still unknown.

## Goals / Non-Goals

**Goals:**

- Evaluate one uniquely resolved zero-parameter `main` to an exact `I32` value.
- Bind arguments to parameters using already checked semantic identities and contracts.
- Bound recursive cycles and all unavailable states as deterministic data.
- Expose a trace that agrees with the inspector's static data-flow view.

**Non-Goals:**

- Native or bytecode compilation, LLVM, optimization, I/O, memory, effects, conditionals, or a stable runtime.
- Defining general recursion semantics, stack limits, entry-point configuration, or process exit codes.
- AST, HIR, MIR, dependency scheduling, or evaluation of unreachable declarations.

## Decisions

### Add a dedicated BootstrapEvaluation actor

The new public actor consumes a completed semantic-analysis result and returns a closed
`Completed | Blocked` outcome. Parsing and semantic analysis remain separate callers'
responsibilities. Folding
evaluation into `SemanticAnalysis` was rejected because relationships and execution have different
failure boundaries and the analyzer must remain non-executing.

### Treat expected inability to evaluate as result data

Missing entry points, unavailable semantic facts, wrong contracts, and recursive cycles are normal
closed blocked outcomes, not thrown exceptions or an untyped error channel. The operation is pure and
requires no service environment. Unexpected JavaScript exceptions remain defects. This mirrors the
frontend's existing policy that source problems return inspectable data.

### Evaluate only the reachable declaration graph

Evaluation begins at the exact `main` identity and follows resolved calls. Unreachable declarations
are ignored even if their bodies are invalid. Whole-file semantic validity was rejected because it
would make an unrelated experimental function prevent an otherwise complete entry path from
demonstrating the vertical slice.

### Use identity-keyed immutable call frames

Each call frame maps target parameter identities to evaluated `I32` values. Parameter reads use
their resolved identity rather than spelling. Arguments are evaluated left to right and bound only
after the call contract is compatible. String-keyed environments were rejected because duplicate
names and ownership would become ambiguous.

### Detect recursion with the active declaration path

Before entering a target, evaluation checks whether its declaration identity is already active. If
so, it returns the smallest closed cycle ending at the current call site. A general depth limit was
rejected because it would make outcomes depend on an arbitrary threshold and still obscure the
actual cycle supported by this grammar.

### Append a provenance-rich trace during interpretation

Entry, call, binding, read, and return events are accumulated in evaluation order and retained on
both completed and blocked outcomes. Events refer to semantic identities and source spans; they do
not include timestamps or host-specific data. Reconstructing a trace afterward was rejected because
blocked evaluation must retain the exact successful prefix.

## Risks / Trade-offs

- [A direct evaluator could be mistaken for the future runtime] → Name it bootstrap evaluation, document its closed grammar, and avoid runtime services or lowering abstractions.
- [Recursive implementation could overflow before cycle detection] → Check the active identity path before every call and test direct and mutual cycles.
- [Trace details could couple UI and evaluator] → Expose concept-oriented events and let the inspector project presentation labels from identities and provenance.
- [Future nested expressions change evaluation order] → Specify left-to-right only for the current argument list and break the prerelease API cleanly when the expression model expands.

## Migration Plan

Implement entry selection and literal completion first, then call frames and parameter reads, cycle
detection, blocked outcomes, and traces. Export the actor explicitly with its package subpath, add a
changeset and release-candidate coverage, then connect the inspector action and verify completed and
blocked programs. No compatibility facade is retained.
