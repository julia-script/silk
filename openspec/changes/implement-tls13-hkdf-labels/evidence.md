# TLS HKDF evidence

## Fixture origins and reproduction

`fixtures.json` (SHA-256 `57f63de8f53af1d982eb225ebcb2ba49b22701bbe7f0467bc82d0b528f79a4ae`) commits seven bounded inputs and their exact HKDF info and expected output (or a
SHA-256 fingerprint for maximum output). The two `rfc8448-*` records are RFC 8448 section 3's server
handshake-derived secret and server handshake traffic key. The remaining records are independently
generated inputs, including SHA-384; they are not RFC SHA-384 vectors.

Authoritative framing is [RFC 8446 section 7.1](https://www.rfc-editor.org/rfc/rfc8446.html#section-7.1).
The trace source is [RFC 8448 section 3](https://www.rfc-editor.org/rfc/rfc8448.html#section-3).
The selected trace values are numeric test data. The implementation contains no copied RFC prose
or reference-library algorithm source.

`oracle.zig` independently verifies every committed output using Zig's generic HKDF instantiated
with HMAC-SHA-256 or HMAC-SHA-384. SHA-384 uses the generic instantiation because the named HKDF
exports do not include it. Reference library: Zig revision
`e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa`. Compiler used:
`0.17.0-dev.1503+1f1bee62e`.

From the repository root, with `ZIG_SOURCE` set to that exact Zig checkout:

```sh
zig build-obj --zig-lib-dir "$ZIG_SOURCE/lib" \
  openspec/changes/implement-tls13-hkdf-labels/oracle.zig -fno-emit-bin
```

This performs all comparisons during compile-time evaluation and emits no executable. It avoids
mixing the newer compiler's runtime calling-convention enum with the pinned library revision;
a runtime `zig test` with that combination fails at platform startup declarations. No reference
algorithm is patched. The compile-time oracle passed. Default Silk tests use committed values and
perform no network or host-crypto operation.

The bounded native corpus owns success, byte framing, message/from-hash equivalence, and rejection
with destination preservation. A separate small LLVM-to-Wasm witness uses only two successful
operations to cover both hash widths; it does not duplicate the native boundary corpus.

## Assurance boundary

Wrapper branches and addresses depend on public lengths and indices, not secret bytes. Generic
SHA/HMAC/HKDF retain their existing ownership and assurance boundaries. The wrapper does not retain
borrowed inputs. From-hash derivation uses a bounded local digest copy to convert the fixed-width
reference to a slice; this does not hash the digest again. No erasure guarantee applies to that
copy or other compiler-created secret copies.

Generated-output inspection, required checks, and independent review results are recorded below
when completed. Functional vectors alone do not prove side-channel resistance or production
security.

## Verification progress

The focused shared native corpus passed (23.76 seconds process, 19.34 seconds test body),
the two-operation Wasm witness passed (13.22 seconds process, 8.85 seconds body), and
the fixed-width/overlap analysis witness passed (7.31 seconds process, 3.05 seconds body).
The native corpus includes zero-output invalid label/context cases and a numeric-only
source extraction of the SHA-256 message-length admission predicate at its maximum,
maximum plus one and u64 maximum. It constructs no invalid slice or enormous input.
Generated documentation policy passed for all 104 registered modules.

A retained-main witness compiled to nonempty LLVM bitcode for aarch64-apple-darwin,
x86_64-unknown-linux-gnu, aarch64-unknown-linux-gnu and wasm32-unknown-unknown in
debug, release and release-with-debug modes. LLVM 22.1.8 compiled all twelve to target
assembly. Native executable and Wasm module artifact kinds retained the witness;
an earlier NativeObject attempt retained no exported function and was discarded.
GNU targets were inspected, not executed. LLVM-dis warns about debug version zero
in debug-bearing bitcode; it still disassembles the retained program.

Inspection of optimized validation on all four targets found only comparisons of
public label/context/output lengths. The Wasm framing loops copy label/context bytes
with addresses controlled by public indices; their branches test loop bounds and
checked numeric conversions, without branching on loaded bytes. This is bounded
wrapper evidence, not an independent audit of every SHA/HMAC/HKDF instruction.

Independent general review approved the implementation subject to completed delivery
gates; its requested zero-output negative cases and numeric SHA-256 boundary seam
are included. The separate committed-diff economics verdict is recorded below.

## Independent review outcome

General reviewer investigator_a approved exact commit
`9e5f18a14076edf7ff08337a8ab3a724945ab198`, including the private numeric-boundary seam
and final two-operation Wasm witness. No correctness finding remains. Separate economics
reviewer investigator_b approved the same commit after removing redundant Wasm transcript
and from-hash calls. Its complete inventory, justification, paired measurements and load
limitations are in `test-economics-review.md`. The full native corpus remains intact.

Full repository test/check/release verification is the remaining delivery gate.

## Repository verification retry

The initial ordered typecheck, format and lint checks passed. A subsequent full test run
was stopped after six unrelated compiler tests exceeded their 60/120-second budgets while
two whole-machine Vitest pools overlapped. The affected cases were two receiver-bound member
checks, OS directory-list lowering, two native-call/union target checks and sort-scratch release.
The partial log is retained at `/tmp/jul181-test-full.log`; it is not a passing full-suite result.
No timeout or test configuration was changed. The final gate is being rerun with only one
broad test suite scheduled by this task; this note does not classify those failures as
pre-existing source defects.
