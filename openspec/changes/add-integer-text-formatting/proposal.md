## Why

Silk cannot turn a number into text. No standard-library module declares a rendering or a reading
function, and the intrinsic catalog has none, so no Silk program can write `"LEX0001 at line 12"`
where the line number is a runtime value. That is the wall issue #40 records, and it is a
self-hosting gate: a compiler's whole output is diagnostics and artifact text.

The Silk lexer example shows the workaround. It cannot render a token or a diagnostic, so it reduces
its entire output to one `i32` fingerprint and compares the number instead
(`examples/language-pressure/lexer/main.silk`).

The roadmap places this work in **Next** — "Add owning String and formatting" — and instructs that it
"Keep it a separate OpenSpec change so ownership, allocation, Unicode validity, formatting failures,
cost, dispatch, and provider presentation remain explicit" (`roadmaps/project.md:130-134`). This is
that change.

Three claims that shaped the issue's plan were measured against `main` and are answered here rather
than restated.

- **Float rendering does not ship.** Requirement 9 of the issue asks for "the shortest text that
  parses back to the same value" — a shortest-round-trip algorithm of the Ryū / Grisu / Steele-White
  class, against a requirement that the bytes be identical on the evaluator, the Wasm backend, and
  the native backend. Nothing of the kind exists in the codebase, and whether it is written in Silk
  over `toBits`/`fromBits` or added as an intrinsic implemented three times is an unmade
  architectural decision comparable in size to the `Transcendental` work. Julia's 2026-08-13
  decision on #40 splits it out. Integers are what self-hosting needs.
- **The out-of-range rejection is writable now.** The earlier triage recorded it as impossible
  because the only `pub const` declarations in the whole standard library were `usize.ZERO` and
  `usize.ONE`. PR #106 (issue #38) shipped `MAX`, `MIN` and `BITS` across all eight fixed-width
  integer modules, so the accumulator can name the bound it must not cross.
- **`usize` and `isize` are not blocked on #109.** They have no limit constants — those are
  target-dependent — but reading them needs none: the digits accumulate in `u64`/`i64` and the
  result narrows through the existing checked conversion, which is exactly a range test that names
  no literal.

## What Changes

- **A new `silk/format` module holds the whole engine**, written twice — once over `u64` and once
  over `i64` — with every integer module reaching it by widening. Writing it once and generically is
  not available: an interface may carry only operations an operator spells, so `checkedMultiply` and
  its kin have no call surface through a bound (the same wall #34 hits, tracked in #118).
- **Every integer module gains `toText` and `parse`**, so `i32.toText(code)` and `usize.toText(line)`
  read the way the issue writes them. The pair is reached through the ambient namespace rather than
  an import, so no integer module grows an import edge.
- **`String` gains `appendOwned`**, the companion to `append` for text a program computed rather
  than wrote, and **`append` stops copying**. Appending now grows the existing storage instead of
  copying the whole string into fresh storage first. The atomicity is unchanged — the byte append
  underneath builds its replacement buffer in full before committing — but a copy per piece made
  composing a message quadratic in its length and linear in allocations, which is precisely the
  operation this change exists to make possible.
- **A reading failure is data, not one opaque error.** `ParseFailure` narrows to `NotANumber`,
  carrying the byte offset at which reading stopped, or `OutOfRange`.
- **The Wasm backend learns to move a value between lanes of different widths.** A union's payload
  slot is as wide as its widest member, so `Result<u64, ParseFailure>` puts a 32-bit member in a
  64-bit slot on a 32-bit target. The backend moved one into the other unchanged, which is not a
  valid instruction sequence, so every such program failed to emit while the evaluator and the
  native backend ran it correctly. This is a prerequisite, not a bonus: without it, `u64.parse` and
  `i64.parse` have no Wasm engine to be identical on.
- **The lexer example composes a real diagnostic message** carrying the line and column its
  diagnostic offsets resolve to, and the value the pressure harness checks now covers that message's
  bytes, so a wrong line number is a failing test rather than a different number.

## Capabilities

### Modified Capabilities

- `bootstrap-silk-stdlib`: ship decimal rendering and reading for every integer type at radix 10,
  the typed reading failure, and the owned-text append companion.
- `bootstrap-backend`: require a union member narrower than its payload slot to survive the round
  trip through that slot on every backend.

## Impact

Affects the standard-library manifest and its generated source table, ten integer modules, `String`,
one new module, the Wasm backend's lane transfer, and the lexer pressure example with its harness.

The closure grows: naming `usize` now reaches the formatting stack, so a program importing
`silk.vector` resolves 17 modules where it resolved 8. That is an analysis cost, not an artifact
cost — the emitted module for such a program is byte-identical, because codegen emits only what the
entry point reaches.

Out of scope, unchanged: float rendering and reading, which requirement 9 of #40 still owns and
which is split to a follow-up; a format string with placeholders, which needs a parser; a `Display`
or `Debug` interface for a user type, which needs the bound form from #34 and is itself blocked on
#118; a locale-sensitive format; and any radix other than 10. A union member that owns a droppable
value and is narrower than its payload slot still refuses to emit on the Wasm backend rather than
releasing nothing — the reclaim path reads slots directly and has no local of the member's own type
to bridge into, so refusing keeps the missing release loud instead of leaking it silently.
