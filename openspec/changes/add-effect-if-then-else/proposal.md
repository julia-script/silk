## Why

Silk has no inline conditional in the effect world. #7 landed short-circuit `&&`/`||` with the right
operand restricted to pure, effect-free expressions, so an effectful conditional stays
statement-shaped: a caller who wants to choose between two Effects writes a statement `if` around
two `run`s, and cannot express the choice as a value that composes into a pipeline.

The difficulty that forced #7's restriction does not exist here. `&&` had to answer what "the right
operand did not run" means for an operand that is an ordinary expression with no defined un-run
form. A combinator's arms are values with defined un-run behavior — an un-run Effect is an affine
value with a cleanup obligation, and an un-invoked `once fn` is an affine value the body releases —
so both "branch not taken" cases are already-specified machinery rather than new semantics.

The mechanism is also already shipped. `Effect.catchAll` invokes a `once fn` arm only on the failure
path, and `Effect.mapBoth` carries two `once fn` parameters, so conditionally invoking one of two
suspended arms adds no interpreter behavior.

## What Changes

- Add `Effect.ifThenElse(condition, onTrue, onFalse)` to canonical standard-library source. It
  invokes exactly one arm — the one the condition selects — and runs the Effect that arm returns.
- Take the arms as **suspended** `once fn() -> Effect<...>` rather than as pre-built `Effect`
  values, which is the decision recorded on #98 on 2026-08-13. This gives *construction* laziness:
  an arm whose body is only well-defined under the condition is never invoked, so it is never even
  built. Pre-built arms would construct both branches eagerly, and construction-time work — or a
  construction-time trap — in the branch not taken would happen regardless of the condition.
- Release the unselected arm explicitly with `drop move` in the combinator body, so the affine
  obligation for the arm that is never invoked is discharged in the source rather than left to a
  generated release.
- Add `ifThenElse` to the closed list of combinators that resolve to ordinary Silk declarations, so
  no compiler-side name recognition is introduced.

The name is `ifThenElse` rather than `if` because `if` is lexed unconditionally as a keyword
(`Lexer.ts:106`) and Silk has no raw-identifier form, so `effect fn if(...)` does not parse — the
declaration fails, not merely a qualified call. `ifThenElse` names both arms, and leaves `when` and
`unless` free for the one-armed forms that would otherwise be the obvious names for them.

## Capabilities

### Modified Capabilities

- `bootstrap-flow-functions`: specify the two-armed conditional combinator — that it invokes only
  the selected arm, that the unselected arm is therefore never invoked and its branch never
  constructed, that the unselected arm is released exactly once, and that the result's failure and
  requirement rows are the union of the two arms' — and add `ifThenElse` to the closed list of
  library-defined combinators.

## Impact

The change affects canonical standard-library source (`silk/effects`), the compiler-shipped source
table generated from it, the generated standard-library documentation page, and acceptance tests. It
adds no intrinsic, no HIR or MIR operation, no diagnostic code, and no compiler-side name
recognition.

It does not add lambda literals — arms are named functions, as every other combinator's callbacks
already are. It does not add a `bool`-scrutinee `match`; `MatchOperation.scrutineeType` stays
`Nominal | Union`. It does not change #7's pure-only restriction on `&&`/`||`, which stands
regardless. It adds no one-armed `when`/`unless` form.
