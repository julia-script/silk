## Context

The implementation baseline is main `851a87e3`, including selected physical supplies, artifact roots, native pointer/layout contracts and integer C variadics. See proposal.md for motivation. Existing bitcode emission and final link-plan identity are reusable. The pre-object runtime symbol list remains the account of language runtime operations, not evidence about LLVM legalization.

## Goals / Non-Goals

**Goals:** Explain every external reference in the emitted program object, retain exactly the required helper providers, and independently prove source memory support closes without libc in debug and optimized builds.

**Non-Goals:** Move coroutine allocation, reporting, startup, OS service policy or the Wasm allocator; implement atomics, permitted foreign unwind, sanitizer runtimes or arbitrary compiler-rt installation. LTO remains rejected.

## Decisions

### Account after object emission

Use a bounded object reader for the admitted little-endian ELF64, ARM64 Mach-O and wasm32 relocatable formats. Preserve object-format spelling in evidence and normalize only the ABI's Mach-O C prefix when classifying support symbols. Record definitions, undefined symbols and relocation references; malformed or unsupported data fails explicitly. Ordinary declared foreign functions/statics and the backend's exact runtime list remain separate origins. A helper-looking declared foreign symbol remains a foreign declaration, with its own contract. Unknown references never cause host-library guessing.

Post-legalization reports contain object content identity, symbol, C signature, family, provider identity, source/object origin, target availability, linkage, visibility and retention. The final physical plan includes those reports and selected roots. The plan identity includes helper contracts/provider versions in addition to already-hashed object and library bytes. Static archives and intermediate native objects expose unresolved support requirements honestly, rather than silently pretending an object is a closed executable.

### Separate capability selection from physical realization

Independent families are memory, arithmetic, atomics, stack probes, stack protection, sanitizers and unwind. The initial contracts are C memcpy/memmove/memset/memcmp and fmod/fmodf, subject to actual emitted references. Unsupported family/provider requests fail before linking. Providers carry explicit dependencies; closure uses deterministic traversal with an origin-rich cycle path. A provider's actual emitted references are checked against its declared dependency edges, including references back to its own exported helper.

Memory helpers use ordinary Silk source and raw byte-pointer operations. Each selected export is an artifact root in a separate restricted support compilation; no unrequested memory routine is retained. Their ABI uses target size_t/pointer width, C int for memset's fill and memcmp's result, and default C calling convention. The source object is explicitly retained through the ordinary physical object input, so archive extraction cannot accidentally discard it. Source is the permanent implementation, with no bootstrap C copy.

Arithmetic remainder uses the selected hosted platform's verified fmod/fmodf contract. GNU needs an explicit libm requirement; Darwin uses the already-selected libSystem contract. This is a permanent platform implementation choice for this hosted subset, avoiding an unverified floating-point reimplementation. A no-libc arithmetic helper request is unavailable. No arithmetic library is added for helper-free programs or merely because another family is selected.

The existing wasm32 memory implementation is an explicit versioned bootstrap component, distinct from its allocator/coroutine policy. Its admitted helper contract and source content are accounted for; native libc math providers are unavailable there. Later source-runtime composition owns its replacement, and this change preserves the existing Wasm capability set without adding an OS dependency.

### Restrict the helper bootstrap, then audit the result

The source support profile has object form, no application/loader entry, no runtime root, no libc, no sanitizers or foreign unwind, and only explicitly selected helper exports. Scalar byte loads/stores and pointer arithmetic require no allocator or service. The profile keeps the requested O0/O2 mode. For support objects only, disable LLVM's memcpy/memset loop-idiom synthesis so byte-loop implementations cannot be rewritten into the helpers they implement. This narrowly scoped restriction leaves the remaining optimization pipeline enabled and is checked against LLVM 22.1.8; the post-object dependency audit remains authoritative even with those flags. Do not turn off optimization globally or rely solely on a source call graph.

The provider dependency validator rejects declared direct/transitive cycles and emitted undeclared/self dependencies. A deliberately recursive object fixture proves the latter. User objects with legitimate external declarations are not freestanding support providers and are not subject to that stricter empty dependency policy.

### Evidence and authority

Pinned LLVM is 22.1.8. LLVM 22.1 LangRef supplies memory intrinsic and floating remainder semantics; LLVM 22.1.8 `LoopIdiomRecognize.cpp` verifies the exact scoped anti-recursion flags and target-library dependency, and `TargetLibraryInfo.cpp` provides target-specific library lowering evidence. Actual object/ABI fixtures remain the oracle.

Zig revision `e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa`: reviewed `lib/compiler_rt.zig`, `lib/compiler_rt/memcpy.zig` and `lib/compiler_rt/udivmoddi4_test.zig`. Zig's selected C exports and byte-loop implementation support the source-provider design; Silk uses explicit raw pointers and its own safety model, not Zig's runtime-safety switch. The integer division tests are an arithmetic edge-case example, not proof of Silk fmod or freestanding closure.

Rust revision `c33d8f3b5a50b56466998e8c5ed8a077d2caed84`: reviewed `library/compiler-builtins/README.md`, `tests/run-make-cargo/compiler-builtins/rmake.rs` and `tests/run-make/no-builtins-lto/rmake.rs`. Rust's relocation-based exclusion of unresolved core dependencies is the closest bootstrap witness. Silk excludes every unexplained support dependency, not only a core-name subset. Rust explicitly retaining no-builtins objects across LTO motivates explicit root retention; Silk rejects LTO rather than claiming equivalent support.

Retain exact downloaded source hashes in the conformance record. Required real lanes use the already-pinned LLVM/LLD 22.1.8, macOS SDK 15.5/deployment 11.0 and GNU glibc 2.36/GCC 12 supplies from JUL-126/140. Independent C probes verify memory signatures, overlap/zero-length behavior and floating remainder, and object fixtures verify which helpers legalization actually requests at O0/O2. No prior-art test execution is claimed.

## Risks / Trade-offs

- Object formats can hide references in relocation tables → bound every parse and compare real fixtures with LLVM's independent object tools; keep final symbol resolution with the linker.
- New LLVM versions introduce new helpers or remove scoped flags → reject unexplained requirements and pin the required matrix rather than accept a silent fallback.
- Source support compilation could accidentally pull in a language runtime → require no-runtime/no-libc roots and audit actual provider objects before linking.
- Floating remainder has subtle NaN/sign/zero behavior → retain the verified platform implementation and use independent C values as the ABI/semantic witness.

## Migration Plan

Introduce typed records and object inspection, implement and audit ordinary source support, integrate helper selection before final-plan/cache admission, remove unconditional libm, update public/packed exports and conformance, and run the required gates. No compatibility path is retained.

### Address observation for overlap

The existing raw-pointer API has no address observation or ordering operation. Add the sealed, target-neutral `Intrinsic.pointerAddress<P>(pointer: P) -> usize`, accepting only data pointers in address space zero. It observes the unsigned numeric address without reading memory, retaining storage, or enabling integer-to-pointer reconstruction. The standard-library `Pointer.address` wrapper exposes this operation. Memory helpers use it only to choose an overlap-safe direction; validity and byte-range proofs remain caller obligations. Constant evaluation cannot observe runtime addresses.

### Verified target-specific memory idioms

Additional LLVM 22.1.8 object probes show that equality-only memcmp calls become bcmp on both GNU architectures, while zero-valued memory fill becomes bzero on Darwin. Add ordinary source bcmp (GNU) and bzero (Darwin), verified against the selected strings.h header. bcmp returns zero for equal bytes and nonzero otherwise; bzero takes destination/count and returns void. These are independently selected memory helpers, not implicit libc dependencies. The existing Wasm bootstrap does not admit these aliases. Extend the required debug/optimized object and C matrix with equality and zero-fill witnesses.
