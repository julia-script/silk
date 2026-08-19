## Context

Text currently crosses semantic and backend seams through dedicated exceptions. Ordinary provenance plus one checked scalar conversion can remove those exceptions while keeping UTF-8 guarantees.

## Goals / Non-Goals

**Goals:** ordinary string value semantics; provenance-correct views; checked `char` production; cross-engine parity.

**Non-goals:** a second text type, locale collation, grapheme segmentation, implicit lossy conversions, or mutable strings.

## Decisions

1. Keep `string` as a logical immutable UTF-8 type and remove compatibility shortcuts.
2. Create byte views through the generalized ordinary borrow path with source provenance.
3. Add or reuse a narrow checked integer-to-char primitive exposed through ordinary standard-library source.
4. Keep debug presentation type-directed so text and binary bytes remain visibly distinct.
5. Use shared UTF-8 traversal source and require evaluator/backend agreement.

## Risks / Trade-offs

- Removing exceptions may expose missing imports or explicit conversions in existing sources.
- Unicode scalar traversal is not grapheme traversal; documentation must keep that boundary clear.

## Migration Plan

Migrate string semantic facts and borrows, add checked conversion, rewrite traversal source, align engines/debug metadata, replace diagnostics/tests, and delete `SEM0094` exception paths.

## Open Questions

Higher-level grapheme and normalization APIs remain standard-library future work.
