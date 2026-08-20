## Context

Union normalization is shared by values, failures, branch joins, and patterns, but storage and membership still assume nominal payloads in places. One canonical member plan must serve every phase.

## Goals / Non-Goals

**Goals:** ordinary detached members; deterministic normalization; exact ownership/layout/tagging; deterministic monomorphic renormalization; engine parity.

**Non-goals:** open runtime unions, runtime reflection, borrowed union storage, or source-visible representation identities.

## Decisions

1. Normalize member semantic identities before layout and assign canonical order independent of spelling order.
2. Require detached storable members and preserve executable finite representations as ordinary members.
3. Derive one member plan containing tags, payload layout, Copy/cleanup, and exact membership evidence.
4. Carry the plan through HIR/MIR and consume it unchanged in evaluation and backends.
5. Substitute and renormalize generic unions at every monomorphic instance, collapsing equal members and recomputing mappings before runtime.

## Risks / Trade-offs

- Changing tag order affects golden artifacts; deterministic canonical ordering intentionally replaces historical ordering.
- Maximum payload layout can increase for heterogeneous ordinary members.

## Migration Plan

Build ordinary normalization, migrate compatibility and inference, migrate ownership/layout, migrate HIR/MIR and engines, expose membership evidence, update tests/diagnostics, and delete nominal-only paths.

## Open Questions

None for finite closed unions.
