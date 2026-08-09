## Context

See proposal.md — Why and the `bootstrap-language-pressure-programs` delta. The canonical
TypeScript lexer is a single byte-indexed loop that recognizes trivia, comments, literals,
identifiers and keywords, decimal numbers, punctuation, unsupported runs, and EOF. The existing
Silk scanner fixture already proves `&[T] -> Vector<Token>` with allocator failure sweeps, but it
only maps three artificial byte values to integer kinds and therefore does not pressure real lexical
logic.

Silk has `u8`, runtime slices, structs, loops, matching, effects, `Allocator`, and the source-shipped
`silk.vector` module. It does not yet have enums, an owning String, iterators, or a test bridge that
can directly return a dynamic Silk vector to TypeScript.

## Goals / Non-Goals

**Goals:**

- Keep one readable, complete lexer program as the durable pressure artifact.
- Compare exact token and diagnostic observations without adding a foreign collection bridge.
- Reuse current allocation tracing and three-engine harnesses so resource claims remain measurable.
- Separate general findings from complexity inherent to writing a lexer at a low level.

**Non-Goals:**

- Installing the lexer in the compiler, publishing it as stdlib, or making its token model public.
- Adding enums, String, iterators, filesystem access, Logger, or Stream preemptively.
- Matching diagnostic prose or source identities; lexical diagnostic kind and byte span are the
  semantic comparison boundary.
- Benchmarking TypeScript versus unoptimized bootstrap Silk as a release-quality performance claim.

## Decisions

### D1: The program is a visible example with a replaceable input expression

Store the lexer under a visible language-pressure example directory rather than hiding it in a
TypeScript string or installing it as stdlib. Its checked-in `main` lexes one representative byte
literal; the differential harness substitutes only that literal and an expected fingerprint to run
the corpus. This keeps the source directly inspectable while avoiding a filesystem or host-string
service. A test-only generated lexer was rejected because it would recreate the source-visibility
problem the stdlib work already removed.

### D2: Token kinds use a local numeric representation

Represent a token as `{ kind: u8, start: usize, end: usize }` and a lexical diagnostic as its invalid
half-open span. The harness owns the explicit mapping between those local codes and canonical
`TokenKind` values. This pressures structs, Copy values, slices, vectors, and exact integer
conversions without pretending Silk already has an accepted enum design. A structural union per
token kind was rejected: dozens of empty nominal variants would test union ceremony more than
lexing and would inflate every vector element for no semantic benefit.

### D3: Differential detail comes from evaluation observations; compiled engines check a fingerprint

The program calls small observation functions for every token field and diagnostic span. The
evaluator trace exposes those calls to TypeScript, allowing exact ordered comparison with the
canonical lexer. The program also computes a bounded deterministic fingerprint; native and Wasm
check that same result. Adding a host ABI for returning dynamic vectors was rejected as unrelated
platform work. Exact observations plus compiled fingerprint parity distinguish semantic mismatch
from backend mismatch.

### D4: The corpus is systematic but finite

Use table-driven cases that collectively cover the complete current `TokenKind` surface, boundary
lookahead, escaped literals, exponent forms, adjacent tokens, and multiple invalid runs. Keep one
valid and one invalid case large enough to force token-vector growth; the invalid case also forces
diagnostic-vector growth. Evaluator differential checks run for the whole corpus, while those two
representatives carry native, Wasm, allocation, and failure-ordinal gates. Exhaustive byte-sequence
fuzzing was rejected for this change because it obscures the readable acceptance contract and adds
no new lexer rule beyond the systematic cases.

### D5: Findings are a first-class implementation artifact

Maintain a Markdown report beside the example with a fixed category table: language, standard
library, compiler defect, tooling/ergonomics, and performance/cost. Record a finding when evidence
appears, including source/test references and disposition. Small general defects required for the
exercise can be repaired here; new language or library designs become separate proposals. This
prevents the example from silently accumulating workarounds or turning into an implicit parser
roadmap.

## Risks / Trade-offs

- [Numeric token codes can drift from the TypeScript union] → keep the mapping centralized in the
  differential harness and include every canonical token kind in a completeness assertion.
- [Trace observation details could couple the test to evaluator internals] → restrict extraction to
  named public observation calls, and use the fingerprint as an engine-independent second oracle.
- [Substituting the input literal could accidentally mutate lexer logic] → use unique, asserted
  sentinels and fail source generation unless each sentinel occurs exactly once.
- [Failure sweeps over two owned vectors may be expensive] → sweep only the representative cases
  and assert allocation ordinals from the successful baseline before injecting failures.
- [The lexer may expose a substantial missing language feature] → retain the complete source and
  exact blocker in findings, then stop and propose the smallest general change rather than adding a
  lexer-specific intrinsic.

## Migration Plan

This is additive. Land the visible example, differential and engine gates, and findings together.
Rollback removes those artifacts without changing compiler behavior because the TypeScript lexer
remains canonical throughout.
