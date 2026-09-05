# Lexer benchmark

Run `pnpm --filter @silklang/compiler lexer:bench` to scan a deterministic one-mebibyte source
snapshot ten times after three warm-up runs. The script reports median and minimum throughput and
retains the observed token count so the work cannot be discarded.

This focused measurement supports the single imperative exception in `Lexer.lex`: classification
is a per-byte hot path whose state is naturally one cursor and two output buffers. No filesystem,
process, or other external boundary is part of the lexer itself.

The most recent local result is checked in as [`latest.json`](latest.json). It is evidence for the
loop-shaped implementation, not a CI threshold: shared runners and developer machines have very
different performance profiles.

The separate [lifetime and partial ownership benchmark](lifetimes.md) measures source proof
work, sparse move paths, and incremental body-query reuse across independent growth families.
