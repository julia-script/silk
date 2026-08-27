# Standard-library documentation workflow

The comments in `silk/*.silk` are the standard library's canonical API documentation. Follow the
[Silk doc comment style guide](../../../apps/docs/content/reference/documentation-style-guide.md) for comment
coverage, structure, examples, links, language, and review rules.

This file defines the additional repository workflow for standard-library documentation.

## Generated outputs

The documentation pipeline reads the same `//!` and `///` blocks for generated reference pages,
editor hovers, policy validation, and doctests. Do not edit generated standard-library pages.

After a source comment changes, run:

```bash
pnpm --filter @silklang/compiler documentation:policy
pnpm --filter @silklang/compiler documentation:examples
pnpm --filter @silklang/compiler documentation:generate
pnpm --filter @silklang/compiler documentation:check
```

The full repository gate also runs these checks through `pnpm check`.

## Example evidence

The doctest compiles each ordinary `silk` fence exactly as written. It adds no prelude or hidden
wrapper. Use `silk,ignore` only when an intentionally invalid example becomes less clear as a
complete valid program. Each ignored example needs a specific reason in the stdlib doctest audit.

A doctest proves compilation. It does not prove runtime behavior. A claim about results, ownership,
lifecycle, failure, or ordering must have a behavioral test.

## Public representation

Document the public surface that the compiler currently exposes. Do not change visibility during a
documentation-only pass. Record a likely visibility defect in the local Markdown tracker.

## Completion standard

A standard-library documentation change is complete when:

- each changed claim agrees with implementation and tests;
- each required summary is locally owned by its declaration;
- each symbol link resolves;
- each example passes policy and doctest validation;
- generated pages contain the intended heading structure; and
- the full repository checks pass.
