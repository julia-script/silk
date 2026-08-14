# Standard-library documentation guide

The comments in `silk/*.silk` are the standard library's canonical API documentation. Write them
for someone choosing and composing APIs, not for someone counting declarations. Generated module
pages, editor hovers, policy checks, and doctests all consume the same `//!` and `///` blocks.

## Shape of a document

Start every module and public declaration document with one concise summary paragraph. Name the
public concept and the useful distinction a caller needs first. Do not begin by repeating the
signature or describing an incidental implementation technique.

Add only sections that teach something useful, in this order:

1. `# When to use` — selection guidance when nearby APIs solve related problems.
2. `# Details` — stable behavior such as ownership, allocation, ordering, bounds, units, failure,
   lifecycle, portability, or provider selection.
3. `# Gotchas` — a concrete and likely mistake whose consequence is not obvious.
4. `# Examples` — selective complete programs, each under a distinct `## Scenario title`.
5. `# See also` — a short list of navigable relationships.

Omit empty sections, generic advice, speculative guarantees, and duplicated prose. Source headings
are document-local: use the depths above and let the generated reference rebase them below the
module or declaration heading.

## Put contracts on their owner

Attach parameter or type-parameter documentation immediately above that declaration when a caller
needs to know ownership, units, interpretation, bounds, mutation, defaults, or selection behavior.
Do not document an obvious operand merely to increase coverage, and do not substitute an
`@param`-style list in the owning function's prose.

Every public field and every service or interface operation needs its own semantic summary. Return
types, failure rows, requirement rows, and other signature facts are compiler-owned; prose should
explain how to use those facts rather than transcribe them. Private helpers need comments only when
a maintainer-facing invariant benefits from one.

## Link related APIs semantically

Write a symbol relationship as ``[`Symbol`]`` so the documentation project can resolve it to a
canonical declaration and the generated reference can link to its page and anchor. Use links when
they help a reader choose or continue, especially between checked and unsafe forms, borrowed and
owned forms, portable services and native providers, or representative family operations.

Prefer the shortest spelling that resolves from the documented module. Do not add decorative links,
and do not leave unresolved stdlib links as an editing convention: the policy checker reports them.

## Write examples as maintained programs

An example earns its maintenance cost by teaching composition, inference, failure handling,
lifecycle, ownership, ordering, or another contract that prose and a signature do not make clear.
Each executable example is one complete Silk module in an ordinary `silk` fence under a titled
subsection of `# Examples`. It uses public imports, explicit setup, deterministic inputs, bounded
work, and one observable idea. The doctest compiles the fence exactly as authored; it adds no
prelude or hidden wrapper.

Use `silk,ignore` only for an intentionally non-compilable illustration that would become less
clear as a complete program. The skipped example remains visible in doctest counts and needs a
specific justification in the stdlib doctest audit. No other fence attribute is supported.

Doctest proves compilation, not runtime behavior. A claim about runtime results, ownership,
lifecycle, failure, or ordering must point to an existing behavioral test or gain a narrow test
that establishes it.

## Evidence checklist

Before publishing or revising a module family:

- Read the declaration and its implementation branches, including unsafe preconditions and
  allocation or mutation paths.
- Read behavioral tests for success, boundaries, failure, ownership, ordering, and target-specific
  behavior.
- Inspect representative call sites to learn how the API composes and which distinctions matter in
  real code.
- Compare related public APIs so summaries explain selection rather than repeat a family template.
- Consult maintained design prose in this directory, relevant OpenSpec capabilities, and archived
  changes; keep only behavior that the current implementation and tests still support.
- Cross-reference every example's behavioral claim with a test, then run the scoped policy,
  doctest, and generation checks.
- Classify exposed representation state before describing it. Document the current public surface
  neutrally, record likely visibility defects in the local Markdown tracker, and do not change
  visibility as part of a documentation pass.

## Review standard

A documentation pass is complete when the module teaches its shared mental model, required public
surfaces have locally owned summaries, non-obvious contracts live on the declarations they qualify,
links resolve, examples are selective and verified, and generated output shows the full intended
hierarchy in source order. A mechanically complete but unhelpful comment pass is not complete.
