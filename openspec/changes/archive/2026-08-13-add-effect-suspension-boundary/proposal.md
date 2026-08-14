## Why

Recursive Effects currently consume one native or host-Wasm call frame per logical invocation, so a
terminating non-tail Effect can end in `SIGSEGV` or a host `RangeError` instead of producing its
typed outcome. Silk needs an explicit suspension boundary now that deep source-defined Effect
programs are practical, while preserving the synchronous shape and cost of programs that never use
that boundary.

## What Changes

- Add public `Effect.suspend`, implemented in ordinary Silk over one minimal sealed `Intrinsic`
  operation, to defer one Effect execution through a stack-safe compiler-owned continuation runner.
- Make continuation storage explicit at the suspension boundary: `Effect.suspend` adds
  `OutOfMemory` and an exclusive `Allocator` requirement, while Effects that cannot suspend keep
  their existing rows and runtime shape.
- Add stable target-neutral MIR suspension identities and provisional suspendable-run control forms,
  normalize them using concrete suspendability, then derive continuation descriptors from the exact
  specialized MIR locals live across each transfer. An explicit suspension originates transfer;
  an ordinary call to a suspendable runner may complete synchronously or relay transfer and saves
  its caller state only on the transfer path. Direct WebAssembly realizes finalized descriptors
  with a private iterative runner, while the evaluator maps them onto its existing heap activation
  machine. Before native implementation, compare direct LLVM emission with LLVM switched-resume
  lowering behind the same descriptor; returned-continuation is a conditional fallback only when
  it could plausibly address a switched-resume feasibility failure or material measured regression.
- Preserve logical `CallDepth` accounting, typed outcomes, ownership, and cleanup across suspended
  self-recursion, mutual recursion, and ordinary `Effect.map` / `Effect.flatMap` composition.
- Prove pay-for-use structurally: non-suspending programs contain no suspension MIR, private runner
  ABI, continuation allocation path, or complete-versus-pending dispatch.
- Add reproducible native and Wasm characterization that isolates deep `Box` chain construction,
  traversal, and destruction. These measurements identify separate ordinary-recursion defects and
  are not acceptance criteria for `Effect.suspend`.

## Capabilities

### New Capabilities

- None.

### Modified Capabilities

- `bootstrap-flow-functions`: Define `Effect.suspend`, its explicit channels, suspended recursion,
  source-defined combinator composition, and the synchronous pay-for-use guarantee.
- `bootstrap-intrinsic-boundary`: Admit exactly one minimal target-neutral suspension primitive and
  prohibit name-based privilege for the public wrapper.
- `bootstrap-mir`: Represent suspendable calls, continuation state, resume points, ownership, and
  verification without target ABI details.
- `bootstrap-backend`: Require bounded-stack private native and Wasm realizations and absence of
  their machinery from non-suspending artifacts, without exposing or standardizing the selected
  native lowering ABI.
- `bootstrap-evaluation`: Preserve source-logical `CallDepth` and deterministic allocation behavior
  while executing suspension through the heap activation machine.
- `bootstrap-owned-allocation`: Route continuation storage through the existing explicit allocator,
  typed `OutOfMemory`, and self-contained reclaim authority contract.
- `bootstrap-ownership`: Transfer live values into continuation frames and clean each obligation
  exactly once on every exit where the language promises cleanup.

## Impact

The change affects Effect intrinsic admission and elaboration, HIR/MIR lowering and verification,
MIR normalization, evaluator activation records and traces, native LLVM emission, direct Wasm
emission, ownership/cleanup planning, the shipped `silk.effects` source, runtime artifact inspection,
spike-local LLVM coroutine-intrinsic construction for a bounded native comparison, and three-engine
conformance tests. Rejected experimental construction does not become a public LLVM-package or
compiler surface. The change introduces no scheduler, fiber, cancellation, public pending state,
universal Effect interpreter, LLVM coroutine ABI in the language contract, or automatic stack
safety for ordinary functions and Drop hooks.
