# SLP-0002: Explicit non-unit result discard

SLP: 0002
Status: Draft
Revision: 2
Author: Julia Ortiz
Created: 2026-08-18
Updated: 2026-08-18
Discussion: —
Review record: —
Depends on: —
Split from: —
Split into: —
Supersedes: —
Superseded by: —
Revisit when: —
Resolution: —
OpenSpec handoff: —

## Summary

Selected direction: an expression statement is valid only when its result is `()` or `never`.
Discarding any other result requires an explicit `drop`. The uniform rule makes ignored scalar,
affine, and lazy Effect values visible in source instead of introducing a special list of
compiler-known “important” result types.

## Problem and evidence

Silk currently rejects every non-unit expression statement with `SEM0087`, but this policy was
introduced during implementation rather than confirmed as a language rule. The motivating failure
was a standalone effect-function call: it constructs an Effect value but does not execute it, so
silently accepting the statement would make code that looks effectful do nothing.

The same diagnostic also rejects an ignored `i32` or newly created affine owner. That uniformity is
simple, but Silk must decide whether the explicitness is worth rejecting otherwise harmless Copy
results.

## Driving examples: current and desired

### Case: Prevent a lazy Effect from looking executed

#### Intent

Execute `answer`, deliberately ignoring its successful `i32` value.

#### Current Silk

```silk
effect fn answer() -> i32 { return 42 }

pub effect fn main() {
  drop run answer()
}
```

A bare `answer()` reports `SEM0087` because it constructs and discards `Effect<i32>`.

#### Desired Silk

```silk
effect fn answer() -> i32 { return 42 }

pub effect fn main() {
  drop run answer()
}
```

This is the selected strict model. `drop answer()` explicitly discards the unrun Effect;
`drop run answer()` executes it and explicitly discards its success value.

#### Observable result

The Effect executes exactly once only in the `run` form. A bare `answer()` is rejected before
lowering, and its diagnostic says that the Effect was constructed rather than executed.

#### Boundary case

```silk
effect fn answer() -> i32 { return 42 }

pub effect fn main() {
  answer()
}
```

The expression produces `Effect<i32>`, not `()`, and reports `SEM0087`.

### Case: Ignore an ordinary value deliberately

#### Intent

Call a function for eager work while explicitly ignoring its returned value.

#### Current Silk

```silk
fn calculate() -> i32 { return 42 }

fn use() {
  drop calculate()
}
```

#### Desired Silk

Under the selected strict model, the source is unchanged. A bare `calculate()` remains invalid
even though `i32` is Copy.

#### Observable result

`calculate` executes once. The explicit `drop` has no cleanup work for `i32`, but records that the
result was intentionally ignored.

#### Boundary case

```silk
fn calculate() -> i32 { return 42 }

fn use() {
  calculate()
}
```

The expression reports `SEM0087`. A competing model would accept this case and reserve errors for
lazy or affine results.

## Goals and non-goals

### Goals

- Choose one programmer-visible rule for ignored expression results.
- Keep Effect construction distinct from Effect execution.
- Define the relationship between expression statements, `drop`, Copy values, and affine cleanup.
- Require a diagnostic that explains the actual discarded type and the available explicit forms.

### Non-goals

- Redesign `drop` or automatic lexical cleanup.
- Add warning severities or a general lint framework.
- Decide whether individual library result types should carry a future `must_use` annotation.
- Change expression-statement parsing, ordering, formatting, or HIR identity.

## Current language model

An expression statement is parsed for any complete expression. Semantic analysis accepts it only
when the result is compatible with `()` or `never`. Every available non-unit result receives
`SEM0087`; an unavailable expression retains its original diagnostic without a duplicate discard
error. Valid expression statements execute once in source order.

## Proposed language model

The selected model adopts the current semantic boundary as the intended rule. `()` has no value
to discard, and `never` produces no value. Every other type requires an explicit destination:
binding, return, transfer into another expression, or `drop`.

`drop expression` evaluates the expression once and intentionally discards its result. For Copy
values this has no cleanup effect. For affine values it performs ordinary cleanup. For Effect
values it discards the lazy value without running it; source must spell `drop run expression` to run
the Effect and discard its success.

## Worked language experience

An ignored generic result follows its declared contract rather than the eventual specialization:

```silk
fn produce<T>(value: T) -> T { return move value }

fn ignore<T>(value: T) {
  drop produce<T>(move value)
}
```

Omitting `drop` is invalid whether `T` later specializes to Copy or affine. The body does not change
meaning per specialization.

## Semantic sketch

- An available expression statement with result `()` or `never` is valid.
- Any other available result reports `SEM0087` at the complete expression.
- An unavailable expression does not receive an additional discard diagnostic.
- `drop` is an explicit consuming statement, not an expression-statement exception synthesized by
  the compiler.
- Expression evaluation and cleanup remain exactly-once and source-ordered.

## Compiler–standard library boundary

### Compiler necessity

Only semantic analysis sees an expression whose result is about to be discarded by statement
structure. Ordinary Silk library code cannot intercept that boundary or require the author to name
the result.

### Smallest target-neutral primitive

No source-callable intrinsic is needed. The smallest behavior is the existing semantic
compatibility check plus ordinary lowering for accepted expression and `drop` statements.

### Standard-library construction

No standard-library API is required. Libraries may return `()` when an operation has no meaningful
result and may offer ordinary functions whose callers explicitly discard advisory results.

### Privilege audit

The selected rule does not recognize Effect or any library declaration by spelling. It checks
only the result type's compatibility with `()` or `never`; `drop` applies ordinary ownership and
cleanup semantics.

## Whole-language interaction map

| Surface | Disposition | Analysis |
| --- | --- | --- |
| Syntax and names | Not affected | Expression-statement and `drop` syntax already exist. |
| Types and abstraction | Affected | Result compatibility is checked from the generic declaration contract. |
| Execution contracts | Affected | Effect construction remains lazy; only `run` executes it. |
| Ownership and resources | Affected | Explicit `drop` consumes affine results; implicit statement discard does not. |
| Runtime and targets | Not affected | Accepted statements already lower identically across targets. |
| Compiler | Affected | Semantic validation and diagnostics enforce the selected policy. |
| Standard library | Not affected | No privileged type or helper is required. |
| Tooling and diagnostics | Affected | `SEM0087` must distinguish ignored values from unrun Effects in its guidance. |
| Learning and use | Affected | The rule must fit in one sentence and make the common Effect mistake obvious. |

## Scope cohesion

This proposal asks one question: when may an expression result be omitted at statement position?
Effect laziness, Copy values, affine cleanup, generics, and diagnostics are consequences of that
single boundary rather than independent features.

## Complexity and subtraction budget

The strict model preserves one type-independent rule and requires no annotations, warning system,
or curated important-type list. Its cost is one explicit `drop` in cases where ignoring a Copy
result is harmless and intentional.

## Surface displacement

The rule changes no syntax or runtime representation. If adopted, it converts a disputed current
restriction into a confirmed language contract and may require only diagnostic wording changes.

## Drawbacks and risks

- Explicit `drop` around a Copy result may feel ceremonial.
- Authors unfamiliar with Effect laziness may read `drop answer()` as execution unless diagnostics
  and reference examples contrast it with `drop run answer()`.
- A strict compiler error cannot be relaxed selectively by libraries without a later language
  change.

## Alternatives and prior art

### Status quo

The status quo is implemented but disputed: only `()` and `never` may appear as expression-statement
results. The provisional model differs only by making that policy intentional and documented.

### Smaller primitive or library solution

A lint cannot prevent invalid lowering unless ignored values are otherwise assigned implicit
destruction semantics. A library wrapper such as `ignore(value)` is equivalent to explicit `drop`
but less fundamental and cannot protect a bare Effect call.

### Strongest competing language model

Allow ignored Copy results, but reject Effect values and potentially affine results. This removes
ceremony for harmless eager values while preserving the two highest-risk cases. Its cost is a
type-category matrix, special treatment for Effect, and generic behavior that depends on Copy and
ownership constraints.

A second competitor allows every result, evaluates it, and applies ordinary cleanup. Tooling warns
for suspicious discards. This is permissive and familiar, but a bare effect-function call silently
does no work—the exact mistake that exposed the rule.

## Falsifiers and acceptance blockers

- A realistic API where routinely ignored Copy results make explicit `drop` materially obscure the
  program rather than clarify intent would weigh against the strict model.
- If `drop expression` cannot uniformly represent deliberate discard for Copy, affine, and Effect
  values, the strict model needs another explicit source form.
- Candidate promotion still requires the author's explicit request and the process review bar.

## Open realization questions

- Should `SEM0087` add Effect-specific guidance without making Effect a distinct validity rule?
- Should tooling later warn when source explicitly drops an unrun Effect?

## Future directions

A future lint system could warn about explicit discards or introduce a library-authored `must_use`
contract. Neither is needed to select the bootstrap statement-validity rule.

## OpenSpec realization map

If the strict model is accepted, reconcile the existing expression-statement specification and
diagnostic catalog wording without changing syntax or lowering. A different model requires a
semantic-validity delta plus ownership and cross-engine scenarios for every newly accepted result
category.

## Revision and decision record

| Revision | Date | Change or decision |
| --- | --- | --- |
| 1 | 2026-08-18 | Initial Draft with the strict current rule as the provisional recommendation. |
| 2 | 2026-08-18 | Author selected explicit handling for every non-unit result, with `drop` as the intentional-discard form. |
