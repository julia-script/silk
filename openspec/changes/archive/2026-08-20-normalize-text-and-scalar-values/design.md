## Context

Text currently crosses semantic and backend seams through dedicated exceptions. Ordinary provenance plus one checked scalar conversion can remove those exceptions while keeping UTF-8 guarantees.

## Goals / Non-Goals

**Goals:** ordinary string value semantics; provenance-correct views; checked `char` production; cross-engine parity.

**Non-goals:** a second text type, locale collation, grapheme segmentation, implicit lossy conversions, or mutable strings.

## Decisions

1. Keep `string` as a logical immutable UTF-8 type and remove compatibility shortcuts.
2. Create byte views through the generalized ordinary borrow path with source provenance.
3. Expose `char.fromU32(value) -> Option<char>` and `char.toU32(value) -> u32` as the only
   integer/scalar conversions in this change. The source wrappers delegate to sealed,
   target-neutral `Intrinsic.charFromU32` and `Intrinsic.charToU32` operations. The checked
   conversion accepts values through `0x10ffff` except the surrogate range
   `0xd800...0xdfff`; the reverse conversion is total.
4. Keep debug presentation type-directed so text and binary bytes remain visibly distinct.
5. Use shared UTF-8 traversal source and require evaluator/backend agreement.
6. Represent `char` in the existing 32-bit scalar lane. Lower the checked conversion as an
   explicit target-neutral MIR operation whose result is the ordinary `Option<char>` union;
   evaluators and backends implement the same scalar-validity predicate. This does not make a
   standard-library declaration compiler-known by spelling.

## Risks / Trade-offs

- Removing exceptions may expose missing imports or explicit conversions in existing sources.
- Unicode scalar traversal is not grapheme traversal; documentation must keep that boundary clear.

## Migration Plan

Migrate string semantic facts and borrows, add checked conversion, rewrite traversal source, align engines/debug metadata, replace diagnostics/tests, and delete `SEM0094` exception paths.

## Open Questions

Higher-level grapheme and normalization APIs remain standard-library future work.
