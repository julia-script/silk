## Context

Baseline native support already has one-lane data pointers, scalar/pointer/void C signatures, function pointers, nested external structs and fixed arrays. JUL-120 supplies audited revision-1 target descriptions. This change uses those foundations rather than replacing proven ABI and storage machinery.

## Goals / Non-Goals

Goals: represent descriptor buffers, scalar accessor pointers and nested pointer elements without erasing nullability, extent or alignment; make target facts authoritative; expose a source-owned initialization proof boundary.

Non-goals: arbitrary pointer/integer casts, volatile or opaque access, retained-address pinning, foreign retention/no-capture or unwind contracts, variadic/aggregate ABI, new union/packed forms and OS services.

## Decisions

### Qualified pointers

`*const T` and `*mut T` become non-null single-object raw pointers. `[*]const T` and `[*]mut T` are non-null many-item pointers. Prefix `?` admits the foreign null representation: `?*const T`, `?*mut T`, `?[*]const T`, `?[*]mut T`. Null is address zero in the audited ordinary data address space and occupies no extra tag lane. Nested pointees retain every qualifier independently.

Optional `align(N)` and `addrspace(0)` follow the access keyword and precede the pointee. Omitted alignment means the pointee's natural semantic alignment for the selected target. Explicit alignment is a positive power of two through 536870912 bytes. Only ordinary address space zero is admitted; other spaces receive a structured diagnostic. Pointers retain all axes in keys, substitution, surface encoding and verification even though the machine representation remains one pointer lane.

Pointees remain invariant. Safe implicit weakening may remove mutation capability, add nullability, or lower a statically guaranteed alignment; it never changes single/many extent or admits a slice as a pointer. Natural alignment can always weaken to one; strengthening an alignment whose implication is not established requires an explicit unsafe conversion. Nullable access must narrow explicitly before a non-null access primitive can be called.

Raw pointers are Copy and hold no ownership or loan. Formation is not a proof of initialization, liveness after a move/drop, pinning, foreign retention permission or ownership transfer. No new LLVM lifetime/noalias/retention promise is inferred.

### Small primitive boundary and source wrappers

Keep formation, null testing, indexed address computation and Copy loads/stores behind sealed Intrinsic operations. A minimal pointer qualifier conversion preserves pointee and address space while making the stronger qualifier proof an unsafe caller obligation. Ordinary source wrappers implement checked null-to-Option conversion; the compiler does not recognize a library declaration by spelling.

Many-item addresses carry no length. Descriptor APIs pass length separately; slices keep their address-plus-length representation. Indexing a non-null many pointer returns a single-object pointer and requires an unsafe bounds/liveness proof. Unaligned reads/writes use explicit weak-alignment pointer contracts and LLVM load/store alignment derived conservatively from the pointer's guarantee and lane offset.

Expose one raw-slot address primitive so output storage never constructs a reference to uninitialized T. It consumes the Slot selection, copies its address and neither loads memory nor updates initialization state.

### Output storage

Use ordinary source `Uninitialized<T>` and `Initialized<T>` owners over existing RawBuffer allocation, initially restricted to Copy values needed by scalar and admitted external output records. Private fields prevent safe fabrication of the initialized state. A safe initialization operation writes a value and consumes Uninitialized into Initialized. Taking/passing its raw address leaves the state unchanged. An explicitly unsafe assumption consumes Uninitialized into Initialized only under the caller's proof that the external operation wrote a valid T. Extracting consumes Initialized and returns T once; ordinary ownership rejects reuse. Raw storage deallocation remains scoped by its existing owner. No runtime compiler-maintained flag or source-known special actor is introduced.

### Semantic target layout

Replace scalar-catalog physical alignment decisions with the target description's primitive facts. The scalar catalog continues to own language categories and arithmetic semantics. Fixed arrays and existing external records derive offsets and stride from semantic entries. Validate the description before layout and ABI classification; missing/inconsistent machine facts cannot reach LLVM and have no ambient-host fallback. C classification preserves pointer qualifiers in semantic symbol contracts while reusing the existing scalar calling convention and one-lane pointer lowering.

### Conformance and supplies

Authorities reuse the content-pinned Apple ARM64 document, AAPCS64 2025Q1 and x86-64 psABI e1ce098331da5dbd66e1ffc74162380bcc213236 from target-facts/authorities.json. LLVM LangRef is content-pinned to llvmorg-22.1.8 for loads, stores, alignment, pointers and data layout. Local Clang, LLVM tools and LLD are 22.1.8. A Darwin probe compiled, linked and executed against the explicit macOS 15.5 SDK despite a failing xcrun version lookup; SDKSettings and necessary header hashes are recorded in supplies.json.

The exact authority/tool/header pins and successful pre-implementation C supply probes are in supplies.json. Both GNU architectures use Debian bookworm at the recorded image digest, GCC 12.2.0-14+deb12u1, glibc 2.36-9+deb12u14, binutils 2.40-2 and linux-libc-dev 6.1.180-1; both runners execute successfully. These probes are supply verification only, not Silk boundary conformance. Designated lanes must fail when supplies are absent; no skip-as-pass. Each target gets independently compiled C and separately compiled Silk objects, link and object inspection. Darwin ARM64 and GNU x86-64 execute; GNU ARM64 executes when a runner is available. Debug and optimized fixtures distinguish buffer writes, scalar accessor/null boundaries, nested pointers, and unaligned access. LTO is rejected for this boundary unless a tested lane establishes it.

Prior art is pinned to Zig e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa and Rust c33d8f3b5a50b56466998e8c5ed8a077d2caed84. Zig ptrcast tests cover align(1) reinterpretation; its pointer forms inform extent/nullability distinctions. Rust core/src/mem/maybe_uninit.rs and tests/mir-opt/const_prop/maybe_uninit.rs inform the separation of storage from validity. Silk deliberately uses ordinary owning source wrappers, initially Copy-only, and does not infer that arbitrary external C initialized an output. Neither implementation is an ABI oracle; foreign ABI fixtures do not prove initialization or pinning.

## Risks / Trade-offs

- Every reconstruction of a Pointer must retain its axes; type-key and substitution tests target this failure.
- Weak alignment must reach every LLVM lane load/store without introducing an over-alignment promise.
- Null-to-Option wrappers are safe only for the nullness claim; subsequent dereference remains unsafe.
- Output state transitions must consume ownership once and release storage on all exits.
- A target fixture compiled without headers cannot establish SDK/libc interfaces; the conformance lane records exactly which header-backed signatures it verifies.

## Migration Plan

Publish the reference and exact spec deltas before code. Replace constructors, syntax facts, encodings, intrinsic contracts and all consumers in one stack layer. Update every source fixture and wrapper whose old pointer silently admitted null or many-item access. Regenerate catalogs and goldens, run required gates, then submit the two-ticket stack with complete evidence.

The Rust abi-x86_64_sysv.rs fixture concerns aggregate-by-value widths and is not an analogue for this scalar-only boundary. Rust aarch64.rs separates DarwinPCS and AAPCS and informs the platform split; the existing Silk scalar convention is retained. Zig c.zig errno access and align.zig pointer weakening inform source wrappers. Their compiler tests are inspected design evidence, not tests executed for Silk.
