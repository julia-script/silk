## Context

See proposal.md. JUL-123 supplies invariant qualified raw pointers, ordinary reference loans and explicit output storage. Foreign declarations currently classify machine types, emit direct calls and reload address-taken locals. No measured reorder bug is presumed. The governing source is Native OS Integration Plan updated 2026-09-04T21:17:22.942Z, WS-13/WS-09/D-020 and WS-25–28.

## Goals / Non-Goals

Admit explicit immediate scalar/pointer contracts, preserve conservative calls and stop a platform foreign exception at the first Silk foreign boundary. Do not introduce retained storage, callback lifetime promises, permitted unwind, source TLS, platform-specific error policy or implicit pinning. Nonlocal jumps that bypass the platform exception unwinder are outside this exception subset.

## Decisions

### Sealed declaration clause

Use an optional contextual `with Intrinsic.foreign(...)` tail after the optional symbol rename. This is a compiler-owned declaration property, not a source function execution or a library actor recognized by name. Ordinary unsafe acknowledgement remains mandatory. Arguments are literal named properties, with exact field/type validation and source spans:

- `memory`: `"none" | "read" | "write" | "readwrite"`, default `"readwrite"`.
- `locality`: `"external" | "arguments"`, default `"external"`. `memory: "none"` canonicalizes locality to external because there are no accesses. Arguments means only memory reachable through pointer parameters; it does not permit hidden globals or inaccessible allocator state.
- `noCapture`: tuple of raw-pointer parameter names, default empty. The assertion forbids all pointer capture, including return capture; it does not promise non-freeing or make the pointer owning.
- `borrow`: tuple of single-value reference parameter names, default empty. Each named parameter asserts noncapture and non-freeing for the complete call. Every reference parameter requires this assertion. Slices and borrowed results remain unadmitted. This gives immediate native code one C pointer while keeping the existing reference loan checker and initialization rules authoritative.
- `returned`: one raw-pointer parameter name, absent by default. Result must have the identical pointer type. It asserts exact pointer equality, not fresh allocation or ownership. It conflicts with noCapture on the same parameter and with noReturn.
- `noReturn`: Boolean, default false; true requires a unit result. It permits no successful continuation. A foreign implementation that returns despite this unsafe assertion violates its declaration contract.

Unknown, duplicate, wrong-kind, unknown-parameter and conflicting properties diagnose at the property/parameter origin. Assertions on exported source functions and function-pointer types are not admitted by this clause; their delivered contracts remain conservative. No generalized attribute strings or arbitrary LLVM properties enter source. Named sets normalize to parameter ordinals, not source aliases or property order.

The alternative of treating raw pointer formation as a borrow is rejected: JUL-123 deliberately made raw pointers non-owning and loanless. Explicit reference parameters make the loan lifetime visible to existing checking without manufacturing provenance for arbitrary native pointers. Existing output storage still uses raw write-only pointers and explicit unsafe initialization acknowledgement; a memory assertion never initializes an owner.

### One normalized behavioral identity

A concept module owns data, validation and canonical encoding. Declaration collection retains property syntax, completion validates types and parameter identities, and semantic surface/callable keys include the normalized facts. Classified C signatures carry the same behavioral record into executable inventory, MIR, LLVM and backend/cache identity. Native ABI JSON advances to schema 2 and records normalized contracts for both imported and exported functions; data entries remain data. Every existing consumer migrates. Visible declaration/interface mismatches compare machine and behavioral facts and retain both source origins. No claim is made about unavailable contracts in arbitrary binary objects. LTO input remains explicitly unsupported by profile validation.

### Immediate loans

Only `&T` and `&mut T` reference parameters named by `borrow` enter this new ABI admission. They lower to the existing nonnull single-pointer representation and preserve pointee type, mutability and target alignment. Passing references reuses ordinary call-argument loan creation, overlapping-access checks, initialized-state checks and end-of-call release. No return borrow is admitted. Capture-capable declarations cannot accept reference parameters. Raw pointers stay Copy non-owners; noCapture affects the optimizer but cannot keep an owner alive or prove an unknown implementation correct.

### LLVM memory and control contracts

Emit memory(read/write/readwrite/none) and argument locality only from the normalized assertion. Explicit noCapture uses captures(none); call-only borrowed arguments additionally use nofree and the applicable immutable-access restriction. Returned aliases use returned only on the named raw pointer. NoReturn applies only to the explicit declaration. Defaults supply no optimistic memory/capture/alias/termination property. Keep address-root reloads and ordinary external loads; neither volatile accesses nor hardware fences are required. Any later reload minimization is outside this change.

### Forbidden-unwind boundary

Every emitted immediate foreign call goes through an internal signature-specific guard that invokes the actual foreign function. Add typed LLVM invoke and cleanup landingpad support across builders, verifier, IR and bitcode. The guard has a local platform personality with the Itanium/DWARF native signature. That personality performs the compiler fatal trap when called by the unwinder, in search or cleanup phase; the landingpad also traps. Therefore an enclosing C++ catch cannot intercept an exception after it has crossed the guard. The original declaration is never marked nounwind merely because propagation is forbidden. The guard may be nounwind because its unwinding path terminates. Preserve the guard frame (noinline and no tail transfer) and inspect actual object unwind records in optimized fixtures. Normal foreign calls do not acquire a C++ library dependency. Existing indirect calls receive the same guard mechanism where required without new callback lifetime promises.

A bare nounwind call was rejected because it makes a violating unwind undefined instead of guaranteeing termination. A generated C++ noexcept wrapper would work but would add a C++ compilation/runtime supply to every ordinary C build. A compiler-generated personality uses the platform unwinder supplied by the actual throwing implementation and needs no foreign exception object decoding.

### Authorities and comparative evidence

- LLVM LangRef 22.1.8, pinned local digest `0f62c10776c9017e8805e561fdfdb780cfb814d3ff509a66ca14f3900c7101c4`: memory, captures, returned, noreturn, invoke and landingpad contracts. LLVM is the optimizer contract authority, not the C ABI oracle.
- Itanium C++ ABI exception handling revision 1.22, https://itanium-cxx-abi.github.io/cxx-abi/abi-eh.html, sections 1.1 and 1.6: platform personality invocation and two-phase unwind. Pin fetched bytes in the fixture supply record before execution.
- Retain JUL-123's AAPCS64 2025Q1, x86-64 psABI e1ce098331da5dbd66e1ffc74162380bcc213236 and Apple ARM64 authority records for admitted machine lanes.
- Rust c33d8f3b5a50b56466998e8c5ed8a077d2caed84: tests/codegen-llvm/cffi/ffi-pure.rs and ffi-const.rs map to compiler/rustc_codegen_llvm/src/attributes.rs FFI_PURE/FFI_CONST memory attributes. ffi-out-of-bounds-loads.rs is an aggregate ABI regression outside this scalar subset. abort_unwinding_calls.rs supplies the explicit-abort-on-unwind analogue; Rust's ordinary C nounwind assumption is not Silk's required runtime outcome. asm.rs is not an error-accessor implementation.
- Zig e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa: lib/std/c.zig and behavior/extern.zig preserve explicit C linkage and renamed symbol identity. They do not supply an analogue for the admitted Silk declaration property or prove its unwind outcome.

Use pinned Clang/LLVM 22.1.8 and existing JUL-123 Darwin SDK 15.5 / deployment 11.0 and Debian/GCC/glibc/binutils images. Add independently compiled C/C++ fixtures restricted to the signatures above. Run Darwin ARM64 and GNU/Linux x86-64 and ARM64 in debug/optimized modes, including actual execution using the available Docker runners. Required missing tools or fixtures fail the lane. Record exact versions, header hashes, compiler/linker commands and results; do not label planned evidence verified.

## Risks / Trade-offs

- The unwinder might omit a guard frame after optimization → retain the frame and verify a throw through a Silk export called inside a C++ catch in each native lane.
- Captures and returned are subtle LLVM promises → reject their overlap and distinguish raw noncapture from borrowed nonfreeing storage.
- Reference parameters must remain single machine pointers → use structural ABI/LLVM checks plus independent C writes; reject slices and borrowed results.
- Conservative memory effects may inhibit optimization → preserve them unless an explicit unsafe assertion narrows the contract.

## Migration Plan

Publish this contract in the reference before implementation. Update all representations and codecs in the same branch, regenerate diagnostics and interface goldens, extend independent fixtures, run focused analysis/LLVM checks then every repository gate. No compatibility decoder or obsolete type-only behavioral identity remains. This is source development with no deployed data migration.
