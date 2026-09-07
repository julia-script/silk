## Context

See proposal.md. OsRandom already provides an infallible secure service over initialized mutable slices; the generated adapter currently contains completion/error policy. JUL-123/124 provide the required generic pointer and ordered foreign-call operations. No compiler extension is needed.

## Goals / Non-Goals

**Goals:** ordinary selected source with truthful platform signatures, complete initialized-buffer semantics, adapter deletion and independent native evidence.

**Non-Goals:** raw uninitialized storage, entropy readiness polling, fallback devices/syscalls, weak random data, statistical tests or new distribution/PRNG APIs.

## Decisions

### Keep the native boundary in OsRandom

The selected OsRandom module owns private unsafe C declarations and its ordinary fill implementation. Darwin uses arc4random_buf(nullable many mutable u8 pointer, usize) returning unit; GNU uses getrandom(nullable many mutable u8 pointer, usize, u32 flags) returning isize, plus __errno_location returning a mutable C int pointer. Only buffer parameters are noncapturing. External state remains conservatively observable. There is no uniform fictional failure return for Darwin and no new compiler-known provider actor.

### Borrow initialized bytes and preserve every partial state

Save the slice length and its raw pointer once while the exclusive borrow remains live. Empty fills return before strengthening a nullable pointer or making a foreign call. GNU offsets are formed only within the same live allocation. Positive counts no larger than the request advance the committed prefix. Every remaining byte retains its initialized prior value. Zero or excessive counts trap, and only a negative call reads errno immediately. EINTR retries the same offset. EAGAIN, ENOSYS, EPERM and other failures trap, even after earlier progress; no partial secure result returns. No allocation, raw temporary storage or byte logging is introduced.

### Bound GNU requests to 256 bytes

Use a source policy cap of 256, below the Linux getrandom limit and matching the documented small-request behavior. The Linux man-pages 6.18 getrandom(2) documentation recommends this size and still requires checking returned counts. Retain defensive short/EINTR handling without treating scripted unusual counts as proof they occur after a ready small real request. This replaces the old 32MiB-minus-one cap; call count is not the public contract. GRND_NONBLOCK=1 refuses entropy-initialization waiting. It is not a wall-clock latency guarantee.

Darwin makes one void arc4random_buf call for a nonempty slice. It exposes neither a readiness flag nor a recoverable failure result. The source layer does not promise a stronger no-wait guarantee than that selected native contract, invent a readiness check or substitute weaker output.

### Pin authorities and compare prior art without importing their policies

supplies.json records Apple SDK 15.5/deployment 11.0, GNU glibc 2.36/GCC12, LLVM22.1.8 and exact stdlib/random/error/scalar headers. GNU requires glibc2.25 and Linux3.17; unsupported older supplies are outside the admitted lane. Independent C fixtures validate size_t/ssize_t/C unsigned flags and the void Darwin signature.

Pinned Zig Threaded.randomSecure directly uses arc4random_buf where present and otherwise loops over getrandom with flags0 and a typed EntropyUnavailable error. Silk retains its explicit nonblocking GNU readiness and fatal public outcome. Rust Linux has weak/raw syscall and device fallback policies, while Rust Apple uses CCRandomGenerateBytes; none is copied. Rust's interposed-error fixture motivates deterministic native receiver tests, but its fallback and catchable panic are not Silk contracts. Pinned Zig randomSecure tests compare two random buffers; Silk deliberately excludes this probabilistic assertion. Prior-art tests are reviewed, not executed.

### Test source policy and actual ABI separately

Retain portable Random/derived-operation tests and the shared native corpus's successful fill without inspecting random values. Replace OsRandomRuntime's generated-C simulation with independent C/header receivers linked to source exports. A fixed initialized source buffer crosses the 256-byte boundary; scripts verify exact requested lengths, pointer offsets, short progress, retry, zero/overcount and partial-then-error state. Child processes verify fatal outcomes while receiver assertions distinguish initialized tails from fresh prefixes before returning failure. Darwin has only empty/no-call and full void-call cases; no fabricated error case. All three native targets execute O0/O2 with physical supply evidence, and missing supplies fail. LTO is explicitly rejected.

## Risks / Trade-offs

- Small chunks increase calls for large buffers → accept the documented bounded source policy; performance is not tested with timing thresholds.
- Native errors can be overwritten → read errno only immediately after negative getrandom.
- Partial writes can be mistaken for full entropy → only whole-range completion returns; failure tests prove fatal outcome after prefix mutation.
- Darwin has no error return → preserve its actual void contract instead of inventing a shared status protocol.

## Migration Plan

Implement selected source, delete the intrinsic/runtime/prelude families, migrate fixtures and documentation, verify all native/portable checks and required repository gates, and publish through gh stack. Keep no fallback or compatibility path; rollback reverts the complete layer.
