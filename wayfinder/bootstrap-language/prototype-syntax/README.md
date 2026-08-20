# Bootstrap syntax prototype

> **Throwaway prototype.** This is a comparison instrument, not a parser or an implementation of
> the language.

## Question

Which concrete notation makes a reified Silk Effect feel natural in both imperative and piped code?
In particular, can one notation make delayed execution, ownership-sensitive reuse, typed failures,
service specialization, deterministic ownership cleanup, explicit flattening, and stack-safe recursion readable
together without requiring an Effect-style runtime object for every operation?

The prototype now compares compact imperative and fully piped forms of the same language direction.
A third variant shows the real Effect source shapes that motivated the experiment. Its pure state
model is in `model.mjs`; `tui.mjs` is only a throwaway terminal shell.

The current iteration tests these decisions:

- failures use `! E1 | E2` and requirements use `? &Service | &mut Service@Role`;
- `effect {}` opens a lazy imperative body and `effect fn` applies that boundary to a whole function;
- `run` evaluates or binds exactly one Effect layer;
- `return` stays explicit, while a single-statement function body may omit braces;
- named functions are first-class; supplying the trailing arguments of a multi-argument function
  constructs a unary section, and a pipe invokes any unary callable after evaluating its left side;
- callable contracts expose shared `fn`, exclusive `mut fn`, and consuming `once fn` invocation;
- `run` owns the complete following expression, while grouping explicitly pipes an executed result;
- `provide` specializes an open Effect with an existing provider, while `provideEffect` acquires and
  releases a fresh provider for every execution;
- captured shared, exclusive, and consuming access determines whether a closed Effect may run again;
- `map` preserves nesting, while `flatten` and `flatMap` remove exactly one Effect layer;
- self-contained allocation owners escape provider calls without retaining the provider;
- tail-recursive effects lower to loops; every other recursive cycle must cross pipeable
  `Effect.suspend` and lowers through an explicit continuation stack.

The Effect references come from the adjacent upstream checkout:

- `packages/effect/src/Function.ts` (`Function.dual`);
- `packages/effect/src/Effect.ts` (`Effect.suspend`, `Effect.flatMap`, `Effect.provide`, and
  `Effect.scoped`);
- `packages/effect/src/internal/effect.ts` (the fiber run loop and suspended instruction).

## Run

```sh
pnpm prototype:bootstrap-syntax
```

Use `W/S` or up/down to change variants and `A/D` or left/right to change examples. The prototype
does not parse or execute the snippets; it exists to make awkward combinations visible before a
grammar is fixed.
