# Bootstrap syntax prototype

> **Throwaway prototype.** This is a comparison instrument, not a parser or an implementation of
> the language.

## Question

Which concrete notation makes a reified Silk flow feel natural in both imperative and piped code?
In particular, can one notation make delayed execution, ownership-sensitive reuse, typed failures,
service specialization, resource scopes, explicit flattening, and stack-safe recursion readable
together without requiring an Effect-style runtime object for every operation?

The prototype now compares compact imperative and fully piped forms of the same language direction.
A third variant shows the real Effect source shapes that motivated the experiment. Its pure state
model is in `model.mjs`; `tui.mjs` is only a throwaway terminal shell.

The current iteration tests these decisions:

- failures use `! E1 | E2` and requirements use `? &Service | &mut Service@Role`;
- invoking a `flow fn` packages its inputs without entering the body;
- `run` evaluates or binds exactly one flow layer;
- `return` stays explicit, while a single-statement function body may omit braces;
- actor functions are data-first and automatically dual, with a pipe inserting argument one;
- `provide` specializes an open flow with an existing provider, while `provideWith` acquires and
  releases a fresh provider for every execution;
- captured `view`, `edit`, and `take` access determines whether a closed flow may run again;
- `map` preserves nesting, while `flatten` and `flatMap` remove exactly one flow layer;
- values borrowing a per-run provider or scope cannot escape unless promoted first;
- tail-recursive flows lower to loops; every other recursive cycle must cross pipeable
  `Flow.suspend` and lowers through an explicit continuation stack.

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
