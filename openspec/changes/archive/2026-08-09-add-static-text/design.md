## Context

`add-static-text` follows complete integers because literal lengths and views use `usize`. It precedes output so `StandardStreams` can consume immutable bytes without defining an owning String.

## Goals / Non-Goals

**Goals:** exact UTF-8/byte constants, no allocation, deterministic target data, and immutable byte views.

**Non-Goals:** owning/growable String, concatenation, formatting, interning promises, JS String representation, or logging.

## Decisions

### Literal data is static, immutable, and non-owning

Text validates to UTF-8; bytes preserve exact `u8` values. Both lower to compiler-owned static data plus a logical borrowed view and `usize` length. No public owning layout is exposed.

_Alternative considered:_ construct a growable String. Rejected because it would settle ownership and target representation without evidence.

### MIR owns a deterministic static-data table

HIR retains semantic identity/content; MIR orders reachable entries canonically. Backends may coalesce placement only when observable semantics remain identical.

### The future String seam remains explicit

A later owning String may be ordinary UTF-8, target-selected native data, or service-backed. Conversion from static text belongs to that future layer.

## Risks / Trade-offs

- [Static view becomes accidental String ABI] → expose no owning operations or public layout.
- [Escape decoding differs by phase] → decode once after lossless syntax and carry exact bytes forward.
- [Target coalescing changes identity] → specify content/view semantics independent of storage address equality.

## Migration Plan

Add syntax/diagnostics, then HIR/MIR static data, evaluator behavior, layouts, and both backend placements with exact parity fixtures.

## Open Questions

None.
