## Why

Silk clock and waiting APIs express elapsed time as raw `u64` nanoseconds, which obscures intent and
makes common values tedious and error-prone to write. Duration literals can provide compact,
readable source notation while preserving that existing runtime representation.

## What Changes

- Add duration literals composed from whole decimal components and the closed suffix vocabulary
  `ns`, `us`, `ms`, `s`, `m`, `h`, `d`, and `w`.
- Support canonical compact compounds such as `1h30m30s`: units descend without repetition,
  skipped units and zero-valued components remain valid, and subordinate components stay within
  their natural radix.
- Give every duration literal the fixed type `u64` and exact value of its elapsed nanoseconds,
  including fixed 24-hour days and seven-day weeks.
- Reject fractions, non-decimal components, unknown units, invalid component order or bounds, and
  totals outside the `u64` range with focused diagnostics.
- Commit numeric text immediately followed by letters to duration recognition so malformed forms
  such as `3sec` do not degrade into an integer followed by an identifier.
- Preserve authored duration spelling during formatting, including digit separators, leading-zero
  padding, and zero-valued alignment components.
- Lower valid duration literals to the existing ordinary `u64` literal representation without a
  duration-specific intrinsic, runtime operation, or backend type.

## Capabilities

### New Capabilities

- `duration-literals`: Defines duration grammar, canonical component rules, fixed-unit scaling,
  `u64` typing and range behavior, formatting preservation, and backend-neutral lowering.

### Modified Capabilities

- `bootstrap-lexer`: Extends deterministic token recognition and recovery for valid and malformed
  duration-looking numeric text.

## Impact

- Compiler frontend token, syntax, parser, expression-analysis, constant-surface, diagnostic, HIR,
  and formatting actors.
- Lexer, parser, semantic, formatter, module-surface, and evaluator-focused compiler tests, plus the
  prescriptive language reference and generated diagnostic catalog.
- No new standard-library abstraction is required: `silk.monotonic_clock.waitFor` already consumes
  `u64` nanoseconds.
- No new compiler-known standard-library actor, intrinsic, MIR operation, runtime ABI, Wasm
  instruction, or native backend behavior is introduced.
