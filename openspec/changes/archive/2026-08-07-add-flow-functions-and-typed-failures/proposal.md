## Why

Silk can execute ordinary functions, but it cannot yet describe lazy executable work or propagate a
recoverable owned failure. Scoped allocation therefore has nowhere honest to place `OutOfMemory`:
without this substrate it would have to become a trap, result-data convention, or allocator-specific
compiler exception.

## What Changes

- Add `flow fn` declarations. Calling one captures its supplied arguments without entering the body;
  `run` evaluates exactly one flow layer.
- Add declared failure rows with the accepted `! E1 | E2` spelling. Rows contain canonical owned
  nominal types, normalize deterministically, and are checked as upper bounds on a flow body.
- Add `fail move value` as the sole failure origin. It consumes one owned nominal payload, has success
  type `Never`, and propagates abortively through `run` without becoming a trap.
- Add exact-member recovery through `Flow.catch<E>(flow, handler)` or the equivalent pipeline
  `flow |> Flow.catch<E>(handler)`. The handler is a statically known `flow fn` accepting the owned
  `E`; the resulting failure row removes `E` and adds the handler's failures. Matching is by
  canonical nominal identity, not inheritance or runtime type lookup.
- Track captures and failure payloads through ordinary ownership. A delayed flow owns moved captures,
  borrows borrowed captures, and cannot silently copy an affine payload or failure.
- Extend HIR, instance discovery, target layout, the structured MIR DAG, evaluator, LLVM, direct Wasm,
  determinism fixtures, and `/labs` with explicit flow construction, one-layer execution, success,
  propagation, and recovery.
- Reject unhandled failure rows at an ordinary function or executable entry boundary. Traps remain
  separate and cannot be caught by typed handlers.
- Keep this prerequisite deliberately closed: no service requirement rows, roles, provision,
  `Scope.scoped`, suspension/trampolines, arbitrary closures or dynamically selected flows,
  source-spelled flow/function contract types, nested flow results, higher-order row parameters,
  `Flow.map`/`flatMap`/`flatten`/`tap`, host capability adapters, or allocator behavior. Those
  compose in later changes once this executable failure ABI exists.

## Capabilities

### New Capabilities

- `bootstrap-flow-functions`: Lazy statically shaped flow values, normalized nominal failure rows,
  one-layer execution, owned failure origin and propagation, exact-member recovery, and the boundary
  between typed failures and traps.

### Modified Capabilities

- `bootstrap-syntax`: Parse, recover, traverse, and format `flow fn`, failure rows, `run`, and
  consuming `fail` while preserving exact source tokens and spans.
- `bootstrap-declaration-index`: Publish function kind and declared normalized failure-row facts in
  every function header.
- `bootstrap-semantic-facts`: Type flow construction/execution, checked failure origins, residual
  handler rows, and unavailable contracts without fabricating downstream values.
- `bootstrap-ownership`: Transfer capture and failure-payload ownership and clean exited lexical
  regions exactly once during typed propagation and recovery.
- `bootstrap-hir`: Retain backend-neutral flow, run, fail, and catch operations with canonical
  success/failure contracts and provenance.
- `bootstrap-instances`: Discover statically selected flow bodies and handlers without specializing
  by runtime outcome or capture value.
- `bootstrap-target-layout`: Select one compiler-owned tagged success/failure calling shape for each
  reachable flow contract before lowering.
- `bootstrap-mir`: Represent flow execution and typed outcomes explicitly in the structured control
  DAG and verify row, payload, ownership, and cleanup consistency.
- `bootstrap-evaluation`: Evaluate lazy flow construction, one-layer run, propagation, and exact
  recovery as the deterministic semantic oracle.
- `bootstrap-backend`: Realize the compiler-selected typed outcome shape consistently in native LLVM
  and direct WebAssembly without exceptions, unwinding, or backend-chosen tags.
- `bootstrap-compiler-driver`: Exercise success, propagation, recovery, ownership cleanup, rejection,
  three-engine parity, and fresh-process determinism.
- `bootstrap-syntax-inspector`: Expose the canonical flow fixture through the unified `/labs`
  projections, including failure rows and stopped downstream states.

## Impact

- Lexer, parser, syntax tree, formatter, declaration indexing, semantic elaboration, ownership, HIR,
  instance discovery, target layout, MIR/lowering/verifier, evaluator, native LLVM backend, and Wasm
  backend under `packages/compiler`.
- Compiler acceptance and determinism fixtures plus one coordinated preset under `apps/docs/app/labs`.
- The active scoped-allocation proposal can depend on an ordinary `OutOfMemory` failure ABI; service
  access and cleanup scopes remain separate prerequisites.
