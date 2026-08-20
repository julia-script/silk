## Context

Existing match syntax already contains much of the needed structural information, but parsing, coverage, ownership, and lowering are match-specific. Let and if-let must reuse—not clone—that model.

## Goals / Non-Goals

**Goals:** one pattern AST/fact; irrefutable let; if-let; exact ordinary union members; ownership and cleanup correctness; LSP support.

**Non-goals:** executable extractors, user-defined matching, guard expressions in this batch, or compiler errors for merely redundant irrefutable conditionals.

## Decisions

1. Parse a shared syntax tree and elaborate it against a known scrutinee type into a typed pattern.
2. Compute coverage, irrefutability, binding types, and access mode in one semantic pass.
3. Lower let as a pattern proven total; lower if-let as one match plus deterministic ownership join.
4. Use exact normalized union member evidence, then substitute and renormalize selectors for each complete monomorphic application before lowering.
5. Route standalone wildcard result discard through the expression-statement rule.

## Risks / Trade-offs

- Moving existing match code behind a shared abstraction can regress nominal coverage unless old cases remain in the corpus.
- Move-pattern mismatch cleanup must consume exactly once without exposing hidden partial moves.

## Migration Plan

Create shared syntax/facts, migrate match, add let, add if-let, generalize union selectors, implement HIR/MIR/engines, add formatter/LSP, update diagnostics/tests, and delete match-only representations.

## Open Questions

Pattern guards and richer conditional expressions remain future extensions.
