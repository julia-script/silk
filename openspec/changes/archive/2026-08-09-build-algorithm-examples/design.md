## Context

This change consumes the focused stdlib, integer, float, static-text, and standard-stream changes. It adds no hidden language features: examples either execute through existing public behavior or remain frontier evidence.

## Goals / Non-Goals

**Goals:** recognizable source, deterministic fixtures/outcomes, executable parity, and durable blocker evidence.

**Non-Goals:** algorithm-specific intrinsics, benchmark claims, exhaustive algorithm coverage, or silently expanding compiler scope.

## Decisions

### Every example owns source, fixture, expected outcome, capability inventory, and status

Use one small machine-checkable manifest per algorithm. The harness computes executable evidence and checks frontier diagnostics; it never rewrites status automatically.

### Four programs are the executable baseline

Game of Life, Sieve, matrix multiplication, and CRC-32 must execute. Quicksort and FFT remain recognizable frontier programs only when genuine missing capabilities block them.

### Frontier evidence is first-class

Normalized diagnostics and capability notes are committed and exercised in CI. Regressing an executable example fails instead of changing its status.

## Risks / Trade-offs

- [Examples become fixture puzzles] → prioritize readable ordinary source and keep harness details outside programs.
- [Frontier evidence goes stale] → run analysis in CI and review any evidence change.
- [Algorithms pressure unrelated scope] → create follow-up proposals rather than changing compiler behavior inside this change.

## Migration Plan

Create the harness/status format, add all six complete programs, graduate the required four, record honest blockers for the remaining two, and add CI regression gates.

## Open Questions

None; observed quicksort/FFT blockers are outputs of the change, not prerequisites.
