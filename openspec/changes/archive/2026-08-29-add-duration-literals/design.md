## Context

See `proposal.md` for motivation and the delta specs for normative behavior. The compiler currently
scans decimal integers and floats in the lexer's digit branch, classifies primary expressions from
token kinds, contextually types ordinary integer literals, and lowers scalar literals through a
shared HIR/MIR representation. Character literals provide the closest fixed-type precedent:
source syntax remains distinct until analysis has established its value and type, after which
lowering uses general scalar machinery.

The standard library already defines `MonotonicClock.waitFor(howLong: u64)` in whole nanoseconds.
The formatter emits complete literal tokens as indivisible source content, and public constants
carry literal facts through deterministic module-surface encoding. Those contracts make duration
literals a cross-cutting frontend change but not a runtime feature.

## Goals / Non-Goals

**Goals:**

- Keep duration scanning, structural validation, unit scaling, and source spans under one owning
  frontend actor.
- Preserve one lossless token for valid and malformed duration-looking source so diagnostics do not
  cascade through integer-plus-identifier recovery.
- Establish exact `u64` type and value before HIR while preserving public-constant determinism.
- Reuse every existing `u64` operation and backend path after frontend lowering.
- Preserve authored literal spelling through formatting and inspection.

**Non-Goals:**

- Introduce a nominal `Duration` type, dimensional type checking, or implicit conversion between
  duration values and another integer type.
- Add fractional, calendar-relative, signed, full-name, plural, Unicode-unit, or user-extensible
  duration syntax.
- Add a new sleep API, compiler intrinsic, MIR operation, runtime ABI, or backend representation.
- Extend pattern syntax, enum discriminants, fixed-array lengths, or other integer-token-only
  grammar positions to accept duration spellings.
- Normalize a literal to the shortest spelling or rewrite aligned zero and padding components.

## Decisions

### One DurationLiteral actor owns recognition and exact decoding

Add one internal duration-literal actor beside the existing integer-literal support. It owns the
ordered unit catalog, scale factors, component bounds, maximal candidate boundary, structural
parser, and exact `bigint` nanosecond calculation. The lexer uses its boundary and structural result;
semantic analysis uses the same parsed representation for exact value calculation and `u64` range
checking. This avoids duplicating unit precedence or component rules across lexer and analyzer.

The actor distinguishes structural failure from total-value overflow. Unknown units, invalid
decimal forms, ordering, repetition, and subordinate bounds are source grammar failures available
to the lexer. A structurally valid value beyond `u64.MAX` remains available as an exact mathematical
total until semantic analysis emits the range diagnostic.

Alternative considered: recognize an integer token followed by one or more identifier tokens in
the parser. That loses the agreed lexical commitment, makes trivia-sensitive compounds awkward,
and produces cascading generic syntax errors for malformed units.

### Valid and malformed candidates each remain one token

Introduce `DurationLiteral` and `InvalidDurationLiteral` token kinds. Once a completed numeric scan
has trailing duration-looking letters, the lexer asks the duration actor for the complete candidate
extent. Valid compounds receive the former token; malformed compounds receive the latter plus one
focused diagnostic at the first determinable violation. Both retain the candidate's complete source
span, and punctuation or trivia terminates the candidate so normal lexing resumes deterministically.

`InvalidDurationLiteral` remains an expression-start recovery token, analogous to invalid static
literal recovery, so the parser can retain one unavailable duration expression without adding a
second parser diagnostic. Numeric forms without a trailing unit keep the existing integer or float
path; in particular, `1e5` remains a float.

Alternative considered: use the generic `Invalid` token. A dedicated invalid token makes recovery
intent explicit and prevents parser behavior from depending on diagnostic side tables.

### Duration syntax receives a distinct frontend expression

The parser builds `DurationLiteralExpression` from either duration token kind and includes it in the
ordinary primary-expression and expression-start catalogs. The syntax node remains atomic: its
components are retained in the token slice rather than expanded into additions or multiplication
nodes. This keeps source reconstruction, formatting, diagnostics, and editor inspection faithful to
the authored literal.

Alternative considered: desugar `1h30m` into generated arithmetic syntax. Generated operators and
integers would have no honest source spans, would expose intermediate overflow behavior, and would
make formatting and inspection report syntax the user did not write.

### Analysis fixes the type before ordinary integer context can participate

Expression analysis decodes a valid duration token to an exact mathematical total, checks it against
the fixed `u64` range, and publishes a fact whose semantic type is unconditionally `u64`. Expected
integer context is ignored for type selection; a non-`u64` boundary receives the ordinary mismatch
diagnostic after the literal fact is established. Invalid lexical structure publishes an unavailable
fact without a duplicate semantic diagnostic.

Constant collection retains a distinct duration-literal fact so a source duration cannot be
mistaken for a context-selectable integer literal. Deterministic module-surface encoding records the
duration value and fixed `u64` meaning; source spelling and offsets remain excluded from semantic
surface identity, as they are for other constants.

Alternative considered: reuse the integer-literal fact immediately. That would permit existing
contextual integer selection to retype the duration and would make `const timeout: i64 = 3s`
incorrectly admissible.

### HIR normalizes duration facts to existing u64 literals

Once analysis has produced a valid value and type, HIR lowering emits the existing integer-literal
form with type `u64`. MIR therefore receives the existing general scalar `Literal` operation and all
evaluation, Wasm, native, arithmetic, and trap behavior follows established `u64` paths. No backend
switch gains a duration case.

Alternative considered: retain a duration tag through MIR or add a compiler-known clock operation.
The source unit has no runtime identity after scaling, so either choice would add privilege without
observable semantics.

### Diagnostics separate source grammar from semantic range

Lexical diagnostics cover unknown units, non-whole or non-decimal amounts, digit separators,
ordering, repetition, and subordinate-field bounds. Their primary spans target the first offending
component while the invalid token retains the whole candidate span. Semantic analysis owns the one
diagnostic for a structurally valid total above `u64.MAX`, preserving the exact mathematical value.
Downstream type incompatibility and arithmetic overflow continue using existing `u64` diagnostics
and traps. New stable diagnostic entries are added through the generated catalog workflow.

Alternative considered: report every failure during semantic analysis. That would classify malformed
token structure as type behavior and would allow parser errors to obscure the intended diagnostic.

### Formatting and editor views preserve the literal token

The source formatter prints both valid duration nodes and complete valid duration tokens atomically,
without normalizing components. Invalid-duration tokens continue to make the artifact unformattable
under the existing complete-syntax rule. Syntax/token inspection exposes the new token and node kinds;
semantic inspection reports the established `u64` type and exact value through existing scalar views.

### Verification follows the cheapest falsifying tier

Lexer tests cover token extent, every unit, malformed commitment, exact diagnostic spans, recovery,
and lossless byte reconstruction. Existing parser and formatter suites cover the new atomic node and
spelling preservation. Semantic tests build one analysis snapshot per source and cover fixed `u64`
typing, constants and module surfaces, canonical component rules, exact bounds, and ordinary
arithmetic. Evaluation proves language semantics. One Wasm structural assertion may prove that HIR
reaches the existing `u64` literal operation; no per-feature native execution or fresh-process
determinism test is added because those properties already have global acceptance and canary suites.

## Risks / Trade-offs

- [A `u64` carries no dimensional identity after analysis] → Document that any `u64` API accepts a
  duration and keep a future nominal duration abstraction outside this syntax change.
- [Nanosecond scaling limits the largest literal to roughly 584 years] → Diagnose exact overflow at
  compile time and avoid calendar-duration claims.
- [Committed recognition changes diagnostics for currently invalid text such as `3sec`] → Consume
  one lossless candidate and provide a more specific diagnostic; valid existing programs are not
  reinterpreted.
- [The `m`/`ms` and `s`/`ms` boundaries can be implemented inconsistently] → Keep unit matching,
  order, scale, and bounds in one catalog and test longest matching at every adjacent pair.
- [New exhaustive token and syntax variants affect many frontend switches] → Update every caller in
  the same green-field change and delete any temporary fallback rather than adding compatibility
  paths.

## Migration Plan

Land the token catalog, duration actor, syntax/parser support, analysis and constant-surface support,
HIR normalization, formatter/inspection updates, diagnostics, tests, and reference documentation as
one atomic language change. No stored data, runtime ABI, generated artifact migration, or user-source
migration is required because previously valid source cannot contain a duration-looking token in an
expression position. Rollback is a source-level revert of the complete frontend feature.
