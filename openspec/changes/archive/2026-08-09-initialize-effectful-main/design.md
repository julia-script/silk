## Context

See `proposal.md` for motivation. The initializer owns one exact `src/main.silk` source literal,
and existing CLI acceptance coverage already checks that an initialized project loads, checks,
builds for native and both WebAssembly paths, and runs successfully.

## Goals / Non-Goals

**Goals:**

- Make the generated source the smallest valid effectful executable.
- Keep the initialized project's observable success status at zero on every supported path.
- Pin the canonical scaffold text and its end-to-end usability in tests.

**Non-Goals:**

- Removing or changing ordinary `main() -> i32` entry support.
- Adding a sample failure type, `Report` conformance, capability requirement, or console output.
- Adding an initializer flag for choosing the entry style.

## Decisions

### Generate an empty-failure effectful entry

The template will declare `pub effect fn main() -> ()` and return `()`. This teaches the effect
boundary without introducing a contrived failure or capability. A dummy error would add concepts
that the initial program does not use, while an ordinary entry would continue teaching the older
default.

### Keep one canonical initializer template

The initializer will replace its existing source literal rather than add an entry-style option.
One default keeps initialization sparse and makes the project's preferred application model clear.
Users can still edit the file to an ordinary integer-returning entry when explicit exit status is
the program's intended interface.

### Reuse the existing end-to-end project workflow test

The exact-source initializer assertion will pin the new text, while the existing CLI initialization
test will continue proving that the generated project checks, builds for native and WebAssembly,
and runs. This exercises the real command boundary instead of adding a parallel harness.

## Risks / Trade-offs

- [The empty program has no visible effect] → Keep the body minimal; the declaration itself teaches
  the correct boundary and is ready to grow into effects and typed failures.
- [Surface syntax can drift] → Pin the exact generated source and compile it through the full CLI
  acceptance workflow.

## Migration Plan

Only projects initialized after this change receive the new template. Existing projects and
ordinary entry points require no migration. Reverting the source literal restores the prior
initializer behavior.
