# Statements and discarded values

An expression statement evaluates an expression without binding or returning its result. The
current compiler accepts this form only when the result is `()` or `never`.

## STMT-001 — A non-unit expression result cannot be ignored implicitly

**Status:** Disputed

The current compiler rejects a standalone expression whose type is neither `()` nor `never`. To
discard such a result, source must say so explicitly with `drop`; to preserve it, source must bind
or return it.

```silk
effect fn answer() -> i32 { return 42 }

pub effect fn main() {
  drop run answer()
}
```

**Boundary:** Calling an effect function as a standalone statement constructs and then attempts to
ignore its lazy Effect value. It does not execute the Effect.

```silk,ignore
effect fn answer() -> i32 { return 42 }

pub effect fn main() {
  answer()
}
```

The current compiler reports `SEM0087` because the statement produces `Effect<i32>`.

**Diagnostics:** A non-unit expression statement reports `SEM0087` at the expression and includes
the produced type. The error must distinguish constructing and discarding an Effect from executing
it; it must not imply that the Effect body ran.

**Why the current rule exists:** It prevents code that appears to perform work while actually
discarding a lazy Effect recipe, and it makes every discarded value explicit. This is not required
by the borrow checker: the same rule rejects an ignored `i32`, even though copying or discarding an
`i32` creates no ownership problem. It is a language safety and ergonomics policy introduced by
the existing specification and implementation, not a confirmed ownership law.

**Stabilization question:** Silk still needs to choose whether to reject every non-unit result,
reject only values whose discard is especially suspicious such as Effects and affine owners, or
allow implicit discard. Until the author confirms one of those policies, `SEM0087` remains
implemented but disputed.

**Evidence:** [expression-statement requirements](../../openspec/specs/bootstrap-expression-statements/spec.md),
[semantic implementation](../../packages/compiler/src/Elaboration.ts),
[elaboration tests](../../packages/compiler/test/Elaboration.test.ts).
