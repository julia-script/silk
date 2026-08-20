## Why

Silk currently allows some function bodies whose result does not satisfy the declared return type to survive semantic analysis and reach MIR or a backend. The resulting invalid-MIR or LLVM failure hides the actual source error and is the root class behind issue 226. The stabilized language requires every body, including an interface-dispatched body, to satisfy its signature before lowering.

## What Changes

- Check every reachable explicit return and fallthrough path against the resolved declared return type for ordinary, effect, generic, and conformance operations.
- Treat `Effect<A>` as an ordinary value: returning it where `A` is declared is a mismatch, while returning it where `Effect<A>` is declared remains valid.
- Prevent declarations with unavailable or invalid bodies from entering reachable HIR/MIR and emit one source diagnostic at the mismatching expression or missing return boundary.
- Add issue 226 and nested-Effect cases as semantic regressions rather than backend tests.

## Capabilities

### Modified Capabilities

- `bootstrap-flow-functions`: require every reachable return path to satisfy the resolved signature.
- `bootstrap-complete-interface-contracts`: apply the same body and return validation to inline and mapped operation implementations.
- `bootstrap-hir`: admit only declarations whose executable bodies are semantically available.
- `bootstrap-mir`: reject lowering requests that contain unresolved or invalid return contracts.

## Impact

This is the first stabilization implementation batch and has no prerequisite. It changes semantic checking, declaration availability, HIR/MIR construction, diagnostics, and focused analysis tests. It adds no syntax, implicit Effect execution, or backend coercion.
