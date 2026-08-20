## Context

Unsafe acknowledgement currently attaches to sealed intrinsics only. Ordinary low-level APIs need to transfer caller obligations while remaining ordinary functions and preserving all other checks.

## Goals / Non-Goals

**Goals:** unsafe declaration qualifier; lexical call acknowledgement; first-class preservation; conformance safety variance; no check bypass.

**Non-goals:** FFI, unchecked blocks, ambient unsafe modules, compiler-known wrapper names, or a definition of new undefined behavior.

## Decisions

1. Add unsafe to callable contracts, not to nominal function identity by spelling.
2. Require acknowledgement when invoking the qualified callable; forming, storing, or passing it preserves the qualifier without acknowledgement.
3. Carry qualification through sections, generic substitution, HIR/MIR callable types, and interface operations.
4. Permit safer implementations for unsafe contracts and reject unsafe implementations for safe contracts.
5. Share acknowledgement diagnostics between source callables and intrinsic calls.

## Risks / Trade-offs

- Function type compatibility gains another dimension and every callable representation consumer must preserve it.
- Unsafe documentation quality remains a style/tooling concern, not a semantic escape hatch.

## Migration Plan

Add syntax and callable facts, migrate intrinsic call checking, propagate through values/sections/generics, add conformance variance, update HIR/MIR/tooling/tests, and add low-level standard-library examples.

## Open Questions

Future FFI may consume this qualifier but is not required to complete it.
