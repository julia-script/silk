# Why text and bitcode share one model

This explanation is about the relationship between module construction, textual LLVM IR, and LLVM
bitcode. It does not describe the bitstream record format or provide output instructions.

LLVM assembly and bitcode are two representations of the same module. `@silk-effect/llvm` reflects
that relationship by keeping one semantic builder state and placing both output paths at its edge:
`IrText.render` interprets the snapshot as text, while `Bitcode.encode` interprets it as bitstream
records.

## One source of semantic truth

Types, constants, global ordering, function bodies, attributes, and metadata are validated before
they reach either output path. The renderers do not maintain parallel public models. As a result, a
handle cannot be valid for text but invalid for bitcode merely because it was constructed through a
different API.

Maintaining separate text-first and bitcode-first models would make simple writers easier in
isolation. It would also duplicate numbering, ordering, escaping, and reachability rules—the exact
places where two encodings tend to drift apart.

## Determinism is structural

LLVM bitcode uses table indices and relative identifiers extensively. Textual IR also exposes
declaration, block, value, and metadata ordering. Deterministic output therefore begins when values
are interned and mutations are serialized, not when the final bytes are packed.

The output operations take snapshots and never consume the builder. Repeated encoding of unchanged
state produces equal bytes; rendering before encoding does not affect the later encoding.

## Why the writer still has imperative loops

Bit packing is a measured byte-level hot path. The package keeps construction, validation, and
failure handling in Effect, while the innermost packing loops operate imperatively behind
`Bitcode.encode`. This is a narrow performance boundary rather than a second architecture.

An Effect operation per emitted bit or record would add tracing and allocation where there is no
useful recovery boundary. Errors that callers can act on have already been classified before the
packing loop runs.

## Parity has a fixed reference point

The implementation is a port of Zig's standard-library LLVM builder and writer, pinned to the
revision recorded in `UPSTREAM.md`. The parity manifest records which upstream operations, enum
cases, records, and unsupported paths are implemented or intentionally different.

The goal is not to make the TypeScript API resemble Zig. The goal is to preserve LLVM semantics and
encoded output while presenting an idiomatic Effect boundary: typed failures, scoped body drafts,
immutable settings actors, and builder-owned handles.
