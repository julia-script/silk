# Statements and discarded values

An expression statement evaluates an expression without binding or returning its result. The
current compiler accepts this form only when the result is `()` or `never`.

## STMT-001 — A non-unit expression result must be handled explicitly

**Status:** Confirmed

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

This is a uniform statement rule rather than a borrow-checker requirement. It rejects an ignored
`i32` even though the value is Copy, and it rejects an affine result before automatic cleanup can
silently stand in for authored intent. The same rule prevents a lazy Effect construction from
looking like execution. `drop` is the explicit statement that the result is intentionally unwanted.

Dropping an Effect value does not run it. `drop answer()` constructs and discards the Effect;
`drop run answer()` executes it and discards its successful result.

**Evidence:** [expression-statement requirements](../../../../openspec/specs/bootstrap-expression-statements/spec.md),
[semantic implementation](../../../../packages/compiler/src/Elaboration.ts),
[elaboration tests](../../../../packages/compiler/test/Elaboration.test.ts).
