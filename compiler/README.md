# Streamed self-hosted lexer experiment

This directory is an experimental native Silk program that lexes one Silk source file and writes
each token directly to stdout. It is deliberately separate from the official bootstrap compiler.
The experiment is meant to exercise the language and standard library while developing a reusable
pull-based `Stream` shape.

From the repository root, run:

```sh
silk run --manifest-path compiler/silk.toml -- compiler/fixtures/keywords.silk
```

The source path must be one normalized relative path beneath the directory where the command is
launched. Missing arguments, extra arguments, absolute paths, `.` or `..` components, unreadable
files, allocation failures, host-input failures, and writer failures remain typed failures and
propagate from `main`.

The lexer implementation lives under `src/lexer/` and borrows the file-owned `Bytes` slice.
`Stream.take` scans and returns one nominal `Token` at a time, so `main` can print it immediately
without retaining a token vector. Token lines use
`<Variant> <start>..<end>`. Invalid variants are followed by
`diagnostic <code> <reason> <start>..<end>`. Spans are half-open byte offsets. EOF is emitted once
at `source.length`; the next pull returns `Option.None` and ends the loop.

The bootstrap lexer at revision `dd4510fa` is the compatibility authority for this experiment.
The fixtures in `fixtures/` cover keywords, trivia, punctuation, numbers, durations, static
literals, lifetimes, unsupported bytes, and empty input. Their token kinds, spans, diagnostic codes,
reason tags, and focused spans were compared byte-for-byte with the bootstrap lexer.

| Finding                     | Result                                                                                                                                                                                                                 |
| --------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Keyword vocabulary          | The running compiler also reserves `static`, `compileError`, `tuple`, `type`, `extern`, and `export`; the lexical reference's current closed list omits them. This lexer follows the running compiler.                 |
| Contextual spellings        | `where` and `place` remain `Identifier` tokens.                                                                                                                                                                        |
| Contextual integer literals | An integer literal such as `44` is selected as `u8` when compared with a `u8`; an explicit `u8.toU8(44)` conversion is unnecessary.                                                                                    |
| Character literals          | Character literals have type `char`, a Unicode scalar type distinct from `u8`. The byte-oriented lexer therefore uses numeric ASCII literals for byte comparisons.                                                     |
| Stream receiver             | A caller that already has `&mut Lexer` passes that reference as `Stream.take(lexer)`. Taking `&mut lexer` again requests a mutable reference to the reference binding.                                                 |
| Storage                     | Scanning is byte-oriented and allocation-free after the source file has been read. Tokens are printed as they are pulled.                                                                                              |
| Ownership across suspension | The original compiler rejected the straight-line CLI flow with `OWN0020`. The repair tracked by JUL-152 is now on `main`, and the sequential `program` body checks without helper splitting. See [`BUGS.md`](BUGS.md). |

Focused checks:

```sh
silk check --manifest-path compiler/silk.toml
silk run --manifest-path compiler/silk.toml -- compiler/fixtures/empty.silk
silk run --manifest-path compiler/silk.toml -- compiler/fixtures/numbers-durations.silk
silk run --manifest-path compiler/silk.toml -- compiler/fixtures/literals-lifetimes.silk
```

This first slice does not decode literal values, retain tokens, or integrate with the parser.
