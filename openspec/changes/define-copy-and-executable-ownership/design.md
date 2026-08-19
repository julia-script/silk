## Context

Copy facts are currently inferred differently by ownership, declaration indexing, and representation-bearing types. A single sealed fact must own the answer before downstream derivation.

## Goals / Non-Goals

**Goals:** one validated Copy property; derive compound types consistently; make executable representation fields ordinary aggregates.

**Non-goals:** custom copy code, cloning allocated memory, implicit reference counting, or weakening capture access rules.

## Decisions

1. Resolve `impl Copy` as a sealed marker conformance with zero operations.
2. Validate the complete stored-field graph and absence of Drop/cleanup before publishing Copy evidence.
3. Make arrays, unions, represented callables/Effects, and generic bounds query only that evidence.
4. Derive aggregate partial moves and cleanup from realized fields; access restrictions remain separate ownership constraints.
5. Reject cyclic or unavailable Copy proofs deterministically rather than assuming move-only or structural Copy.

## Risks / Trade-offs

- Many existing types may change classification and expose latent duplicate-use errors or unnecessary moves.
- Recursive proof evaluation needs a monotone unavailable/invalid/valid state.

## Migration Plan

Introduce the sealed fact, migrate plain nominals, arrays and unions, migrate generic bounds, migrate executable representations, update layout/cleanup/diagnostics, and remove every alternative Copy classifier.

## Open Questions

Explicit cloning APIs for affine owners remain ordinary library design and are not part of Copy.
