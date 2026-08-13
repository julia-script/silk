## Context

Two questions had to be settled before this combinator could be written, and only one of them was
a design choice. The design choice — suspended arms or pre-built Effect values — was answered on
#98 by decision. The other was a fact about the language that the issue had assumed wrongly, and
resolving it changed what the acceptance criteria can even assert.

## Goals / Non-Goals

Goals:

- An inline conditional in the effect world, where the branch not taken is a defined un-run value
  rather than an expression with no un-run form.
- Laziness at *construction*, not merely at execution, so a branch that is only well-defined under
  the condition is safe to write.
- Exactly one release of the arm that is never invoked, visible in the source.

Non-Goals: lambda literals, a `bool`-scrutinee `match`, one-armed `when`/`unless` forms, any change
to #7's pure-only restriction on `&&`/`||`, and any change to `Effect.catch` or the propagation exit.

## Decisions

### The arms are suspended `once fn()`, not pre-built Effect values

Recorded on #98 on 2026-08-13. A pre-built pair is simpler and matches the `bindRequirement`/
`provide` argument style, and dropping the unselected `Effect` is an already-defined cleanup path.
But both branches would be *constructed* at the call, so construction-time work — or a
construction-time trap — in the branch not taken would happen regardless of the condition. That is
precisely the execution-vs-construction split that decided #7, and it is decided the same way here.

The mechanism is shipped behavior rather than new machinery: `Effect.catchAll` already invokes a
`once fn` arm only on the failure path, and `Effect.mapBoth` already carries two `once fn`
parameters.

### An arm takes no argument, and the zero-arity form is verified

The issue body objected that "a zero-argument `fn()` parameter form is not verified to exist
anywhere." It exists and works:

```silk
fn produce() -> i32 { return 42 }
fn invoke(make: once fn() -> i32) -> i32 { return make() }
pub fn main() -> i32 { return invoke(produce) }
```

That this works is recent. It depends on #104 (issue #100): before it, a call through a
function-typed parameter in an ordinary `fn` passed semantic analysis and then died at MIR
verification with `InvalidCallShape`. #98 was filed against a compiler that could not have run its
own recommended signature.

### The combinator is named `ifThenElse`

`if` is lexed unconditionally as `IfKeyword` (`Lexer.ts:106`, in `keywordSpellings`) and Silk has no
raw-identifier or escaped-identifier form. This is not a member-access problem that a qualified
spelling would dodge — the *declaration* is what fails:

```silk
effect fn if(condition: bool) -> i32 { return 1 }
```
→ `PAR0002`, ``Unexpected `if`; expected identifier``.

`ifThenElse` names both arms explicitly, is the name the Effect ecosystem this library mirrors used
for the same combinator before it became `Effect.if`, and leaves `when` and `unless` free for the
one-armed forms — which would otherwise be the obvious names to reach for and would then already be
taken by a two-armed combinator. `branch` and `cond` were the other candidates considered.

### The unselected arm is released with an explicit `drop move`

The body could rely on a generated release for the parameter that falls out of scope. Today it
cannot, because of a defect that is pre-existing and unrelated to Effect: the generated release for
a callable parameter emits `Drop` with a `NoCleanup` plan typed from the *declared* parameter type
(`mode: "Take"`), while the local's own type carries the *actual* argument's mode (`mode: "Shared"`,
because a reusable named `fn` was passed where `once fn` is declared — the weakening
`bootstrap-callable-values` explicitly permits). `Mir.ts:3211` compares the two with
`SilkType.equals`, which compares modes, so a no-op cleanup is rejected on a mode spelling mismatch.
Without the explicit releases this combinator produces zero diagnostics and then dies at MIR
verification with `InvalidAggregateOperation`, "drop cleanup disagrees with its local type or
canonical union cases".

The defect reproduces in six lines with no Effect involved:

```silk
fn a() -> i32 { return 7 }
fn pick(onTrue: once fn() -> i32) -> i32 { return 0 }
pub fn main() -> i32 { return pick(a) }
```
→ zero diagnostics, `Blocked(InvalidMir)`.

`Effect.catchAll` escapes it only because its `match`-shaped body emits no generated release for
`onFailure` at all, where an `if`-shaped body does.

The explicit `drop move` is kept as the decision rather than as a silent workaround: it puts the
affine obligation for the unselected arm in the source, where the reader can see that the arm not
taken is released exactly once. The underlying verifier defect is worth its own issue and is not
fixed here.

### The rows are unioned, and the success type is shared

`-> A ! E | F ? R | S` follows `flatMap` and `tap`, which already union two independent rows. The
caller discharges whatever either branch could need without knowing which is selected. The success
type must be shared: there is no runtime tag on the result that would let a caller discriminate two
different success types, and unioning them would change what the combinator returns rather than
what it requires.

## Risks / Trade-offs

- **A caller cannot write an arm inline.** Silk has no lambda literals, so each arm is a named
  function. This is the same cost every other combinator's callbacks already pay.
- **The explicit `drop move` is load-bearing.** If the verifier defect above is fixed later, the
  releases become redundant rather than wrong, and the body still reads correctly.

## Migration Plan

None. This is an addition; no existing source changes meaning.

## Open Questions

None blocking. The name is the one judgement call that a reviewer may want to overrule, and doing so
is a rename of one declaration and its call sites.
