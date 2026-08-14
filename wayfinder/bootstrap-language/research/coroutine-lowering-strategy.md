# Coroutine lowering strategy

Status: research finding, not an accepted language design or implementation commitment.

Last reviewed: 2026-08-13.

## Question

Silk needs suspension for more than stack-safe recursive Effects. The same control boundary must
eventually support parked producer/consumer execution, Deferred, Queue, Latch, Semaphore, and
concurrent Stream behavior. Should the compiler lower this control flow itself, use LLVM coroutine
intrinsics, or combine the two?

## Finding

The language-level architecture should be a target-neutral, stackless resumable state machine in
MIR. LLVM coroutine intrinsics may still be useful as a private native-backend implementation, but
they cannot be Silk's semantic model.

This is not equivalent to deciding to implement every backend mechanically by hand. Silk must own
the suspension points, live state, ownership cleanup, allocation contract, and park/wake semantics.
The native emitter may then either emit that machine directly or ask LLVM to perform some of the
function splitting and frame optimization. Direct WebAssembly and the evaluator still require
their own realizations of the same MIR contract.

## What Rust actually does

Rust performs a coroutine `StateTransform` in MIR before backend lowering. It computes the frame
from captured values, a state discriminant, and locals live across suspension; rewrites yields and
returns into ordinary state updates and returns; and creates resume/poll and state-sensitive drop
control flow. The saved field types also participate in borrow checking, trait predicates, and
region analysis. LLVM therefore receives an already lowered state machine rather than Rust
suspension semantics expressed as `llvm.coro.*` operations.

This follows Rust's documented goals: a stackless compiler-generated state machine, no implicit
allocation, and no required runtime. It also lets destruction drop exactly the values live at the
last suspension point and keeps the representation usable by LLVM, Cranelift, and GCC backends.
These are strong architectural reasons for owning the transform, but no primary Rust source found
in this research records a formal evaluation and rejection of LLVM coroutine intrinsics. That
historical comparison should not be claimed.

Rust's solution does not make arbitrary recursion stack-safe. Recursive `async fn` needs explicit
indirection to avoid an infinitely sized future type, and deeply nested polling can still form a
call chain. Rust is evidence for where coroutine semantics belong, not evidence that its execution
contract solves Silk issue #24 unchanged.

Primary sources:

- [rustc coroutine state transform](https://doc.rust-lang.org/stable/nightly-rustc/src/rustc_mir_transform/coroutine.rs.html#1-51)
- [rustc coroutine layout and borrow/type predicates](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_transform/coroutine/layout/index.html)
- [rustc MIR-to-backend lowering](https://rustc-dev-guide.rust-lang.org/backend/lowering-mir.html)
- [Rust backend-agnostic code generation](https://rustc-dev-guide.rust-lang.org/backend/backend-agnostic.html)
- [RFC 2033: stackless, allocation-free coroutines](https://rust-lang.github.io/rfcs/2033-experimental-coroutines.html)
- [Rust recursive async diagnostic](https://doc.rust-lang.org/error_codes/E0733.html)
- [Rust 2026 state-machine optimization goal](https://rust-lang.github.io/rust-project-goals/2026/async-statemachine-optimisation.html)

## What LLVM offers

LLVM 22 implements switched-resume, returned-continuation, returned-continuation-once, and async
coroutine lowerings. LLVM explicitly warns that compatibility of its coroutine support across LLVM
releases is not guaranteed, so none of these ABIs should appear in Silk's public or target-neutral
contract.

| LLVM lowering | Benefit | Cost or limitation | Fit for Silk |
| --- | --- | --- | --- |
| **Switched-resume** (`llvm.coro.id`) | Most exercised path; ramp, resume, and destroy lifecycle; LLVM's established heap-allocation-elision path; a Silk driver loop need not rely on tail calls. | Coroutine object carries resume/destroy dispatch and a suspension index; recursive escaping frames are commonly not elided; allocation protocol is non-trivial. | Safest first native experiment. |
| **Returned-continuation** (`llvm.coro.id.retcon`) | Returns the next continuation and values; frontend controls continuation ABI, inline buffer, allocator, and deallocator; closely resembles “complete or continue.” | Less commonly used; LLVM documents weak allocation elimination after full inlining; one outlined continuation per suspension point; no separate save point. | Best conceptual match, but a measured alternative rather than the default. |
| **Async** (`llvm.coro.id.async`) | Frontend-owned async context and direct continuation transfer; proven in Swift's runtime model. | Couples the frontend to a larger async-context ABI; lifetime and allocation remain frontend work; transfer is intended to use `musttail`. LLVM's own default wasm32 test falls back to an ordinary call without the Wasm tail-call feature. | Reject for the current requirement: it does not establish bounded stack on Silk's default Wasm target. |

Switched-resume can sometimes replace a heap coroutine frame with caller stack storage and
devirtualize resume/destroy. That optimization is valuable, but it is not a correctness mechanism:
Silk must still provide explicit allocation failure and cleanup behavior when the frame escapes or
the optimization does not fire. Returned-continuation gives Silk more direct ABI control, but its
documented allocation-elision weakness is particularly relevant to a pay-for-use Effect model.

Primary sources:

- [LLVM 22 coroutine documentation](https://releases.llvm.org/22.1.0/docs/Coroutines.html)
- [LLVM 22 coroutine ABI variants](https://github.com/llvm/llvm-project/blob/llvmorg-22.1.6/llvm/include/llvm/Transforms/Coroutines/CoroShape.h#L26-L48)
- [LLVM switched-resume allocation elision](https://releases.llvm.org/22.1.0/docs/Coroutines.html#avoiding-heap-allocations)
- [LLVM returned-continuation lowering](https://releases.llvm.org/22.1.0/docs/Coroutines.html#returned-continuation-lowering)
- [LLVM async lowering](https://releases.llvm.org/22.1.0/docs/Coroutines.html#async-lowering)
- [LLVM 22 async lowering without Wasm tail calls](https://github.com/llvm/llvm-project/blob/llvmorg-22.1.6/llvm/test/Transforms/Coroutines/coro-async-notail-wasm.ll#L1-L31)

## Consequences for Silk

Silk already has the right semantic seam. MIR is the common input to the native LLVM backend,
direct WebAssembly backend, and heap-activation evaluator. Ownership cleanup and explicit
allocation are compiler-visible before backend lowering. The evaluator can be the semantic oracle
for suspension without consuming host stack, while the two compiled backends realize the same
continuation descriptor using target-private layouts.

The smallest compiler privilege is therefore not a generator object, scheduler, Queue, or public
continuation API. It is one sealed, target-neutral transfer/suspension operation plus the MIR facts
needed to resume or destroy the live computation. Ordinary Silk should build safe `Effect`
operations and, later, Deferred, Queue, Latch, Semaphore, and Stream protocols over that boundary.
Those synchronization actors also need a registration-versus-wakeup rule so completion before
parking cannot strand a continuation; LLVM coroutine intrinsics do not supply that policy.

Deep `Box` build, walk, and drop remain separate characterization. A suspension boundary can make
Effect execution stackless; it does not silently transform arbitrary recursive source algorithms
into loops.

## Recommended experiment

Do not prototype the whole Effect runtime yet. Build a throwaway native lowering comparison behind
one fixed target-neutral MIR fixture:

1. Use one suspending computation with two suspension points, live affine state, normal completion,
   typed failure, and destruction while suspended.
2. Lower it once to an explicit Silk-managed resume loop and once with LLVM switched-resume.
3. Add returned-continuation only as a third measured variant if switched-resume overhead is
   material.
4. Compare correctness, exact cleanup order, frame bytes, hidden allocations, optimized object
   size, compile time, resume throughput, and source debugging at the pinned LLVM 22 profiles.
5. Require the synchronous control fixture to contain no coroutine declarations, continuation
   helpers, scheduler symbols, or new allocation sites.

In parallel with that native-only comparison, retain a small direct-Wasm continuation fixture and
the evaluator model. Otherwise the experiment could select a convenient LLVM ABI that cannot
express the actual cross-engine contract.

## Decision rule

- If switched-resume preserves Silk's explicit allocator and cleanup semantics and materially
  improves native code or compiler complexity, use it as a replaceable private native lowering.
- If it does not, emit the shared MIR state machine directly on native as well as Wasm.
- Do not choose returned-continuation without measurements overcoming its optimizer and deployment
  disadvantages.
- Do not choose LLVM async lowering unless Silk intentionally adopts a Swift-like async-context ABI
  and changes its Wasm tail-call requirement.

This research supports revising the current suspension proposal so the target-neutral contract is
approved independently of the private native lowering. It does not yet select the native lowering.
