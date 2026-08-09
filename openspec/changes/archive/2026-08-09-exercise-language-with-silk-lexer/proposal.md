## Why

Silk's small algorithm corpus proves individual features, but not whether they compose into a
larger stateful transformation over runtime-sized input and owned output. A recognizable lexer can
pressure that boundary now without making self-hosting—or a parser port—the delivery goal.

## What Changes

- Add a real lexer written in ordinary Silk source that consumes borrowed bytes and produces an
  owned `Vector<Token>` through the public allocation and standard-library surface.
- Differentially compare its token kinds and byte spans with the canonical TypeScript lexer across
  representative valid and invalid Silk inputs.
- Exercise deterministic allocation, rollback, cleanup, evaluator, native, and direct WebAssembly
  behavior without lexer-specific compiler primitives.
- Check in a findings report that classifies every wall as a language gap, standard-library gap,
  compiler defect, tooling/ergonomics issue, or performance/cost concern and recommends whether it
  deserves a separate change.
- Keep the TypeScript lexer canonical. Replacing it, porting the parser, adding filesystem services,
  and beginning continuous self-hosting are explicit non-goals.

## Capabilities

### New Capabilities

- `bootstrap-language-pressure-programs`: Defines how complete recognizable Silk programs are used
  as differential, cross-engine language-pressure evidence and how their findings are recorded.

### Modified Capabilities

None.

## Impact

- Adds a checked-in Silk lexer fixture, differential acceptance corpus, allocation/cleanup checks,
  and a durable findings artifact under the compiler package.
- Uses the existing lexer, analysis, evaluator, LLVM, WebAssembly, `Allocator`, and `Vector` seams;
  it does not change the public compiler API or introduce a runtime dependency.
- Any genuinely general blocker discovered during implementation is recorded for a subsequent
  focused proposal unless a small repair is required to complete this pressure exercise.
