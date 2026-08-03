# Practical LLVM native target matrix for bootstrap

## Question

Given a small, single-threaded self-hosting compiler with few platform services, what work remains
after choosing LLVM, and which targets are cheap enough to require for the bootstrap milestone?

This report assumes the bootstrap compiler needs one thread, dynamic memory, command-line
arguments, source-file input, diagnostics/output, and either a way to invoke an installed LLVM
tool or an external build harness that does so. Concurrency, atomics, networking, public FFI, a
standalone SDK, and the direct WebAssembly backend are outside the milestone.

## Recommendation

Require full native self-hosting on this three-target matrix:

1. `arm64-apple-darwin` (the development host);
2. `x86_64-unknown-linux-gnu`;
3. `aarch64-unknown-linux-gnu`.

This is the smallest useful portability matrix, not merely the smallest number of targets. It forms
a partial CPU/OS cross:

| Target | CPU backend | OS/runtime adapter | Object/link format |
| --- | --- | --- | --- |
| arm64 macOS | AArch64 | Darwin | Mach-O |
| x86_64 Linux | X86-64 | Linux | ELF |
| arm64 Linux | AArch64 | Linux | ELF |

The two Linux targets share one platform adapter while exercising both CPU backends. The two Arm
targets share a CPU backend while exercising both platform adapters. All three are 64-bit and
little-endian, so they expose accidental CPU/OS coupling without forcing a 32-bit or endian-aware
language design into the first milestone.

Defer `x86_64-pc-windows-msvc` and `wasm32` from the required self-host matrix. Add object-emission
smoke tests for both as soon as target-machine emission exists, but do not call either target
supported until a produced compiler runs there and completes the staged self-host test.

This recommendation assumes that bootstrap builds may depend on an installed, pinned LLVM/Clang
toolchain. Requiring a standalone compiler distribution with LLVM bundled would change the cost
substantially and should be a later decision.

## LLVM removes backend work, not platform work

LLVM already owns instruction selection, register allocation, target-specific frame lowering, and
machine-code emission. A `TargetMachine` represents the complete machine description, while
`DataLayout` describes target pointer sizes, alignment, structure layout, and endianness
([LLVM code-generator documentation](https://llvm.org/docs/CodeGenerator.html#target-description-classes)).
LLVM's MC layer emits all formats needed by the assessed matrix: AArch64/X86 Mach-O, AArch64/X86
ELF, X86 COFF, and WebAssembly
([LLVM object-format table](https://llvm.org/docs/CodeGenerator.html#the-mc-layer)).

The target triple and data layout are nevertheless semantic inputs, not decorative headers. The
triple selects the backend; the data layout must agree with the eventual code generator, and LLVM
IR cannot be made target-neutral by omitting it because optimization then uses defaults and still
introduces target specificity
([LLVM Language Reference: target triple and data layout](https://llvm.org/docs/LangRef.html#data-layout)).

The committed Silk LLVM builder already accepts caller-supplied `targetTriple` and `dataLayout`
values and serializes them into text and bitcode
([`Builder.ts`](../packages/llvm/src/Builder.ts),
[`IrText.ts`](../packages/llvm/src/IrText.ts),
[`Bitcode.ts`](../packages/llvm/src/Bitcode.ts)). It does not yet derive the canonical layout from a
target machine or emit native objects. The next backend layer therefore needs either:

- a pinned external LLVM path (`llc`/`clang`) that owns target-machine selection and object
  emission; or
- a native LLVM boundary that initializes the required targets, creates their target machines, and
  returns canonical data layouts and object bytes.

LLVM supports both native-only and all-configured-target initialization, and an LLVM installation
contains only the targets it was configured to build
([`TargetSelect.h`](https://llvm.org/doxygen/TargetSelect_8h_source.html),
[`llvm-config --targets-built`](https://llvm.org/docs/CommandGuide/llvm-config.html)).
Thus "LLVM supports the target" and "the LLVM shipped with this compiler contains the target" are
separate facts that the driver must check and diagnose.

### Practical object-emission probe

As a bounded check rather than proof of complete support, the LLVM 22 installation on the current
Arm macOS host emitted a minimal module as all five relevant object kinds: Arm Mach-O, x86-64 ELF,
Arm64 ELF, x86-64 COFF, and Wasm. This matches LLVM's documented object-format table. It proves the
CPU/object backends are not the blocking work; it does **not** prove that the corresponding
executables link, start, access files, allocate, or self-host.

## Compiler-language portability work

This work is common to every additional target and belongs in the compiler rather than an OS shim.

### 1. Make target layout an input to lowering

- Obtain the canonical data layout from the selected LLVM target machine before lowering source
  types.
- Derive `usize`/pointer width, size, alignment, aggregate padding, and union payload placement from
  that layout. Do not bake the current host's offsets into semantic IR or serialized compiler data.
- Put the target triple, CPU baseline, feature set, relocation model, code model, and deployment/ABI
  environment in one explicit compilation-target value.
- Start with a conservative generic CPU rather than host autodetection. LLVM's object-code tutorial
  demonstrates a target machine using `generic` plus no extra features
  ([LLVM object-code tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl08.html#target-machine)).

The recommended matrix intentionally stays within 64-bit little-endian targets. The language
should not claim that `usize` is universally 64-bit, but bootstrap tests need not yet validate a
32-bit representation or big-endian byte order.

### 2. Separate the private Silk ABI from the platform boundary ABI

Silk-to-Silk calls inside one program may use a deliberately private, compiler-controlled ABI.
Calls into the bootstrap runtime should use LLVM's target C calling convention (`ccc`), which maps
to the target's C convention
([LLVM Language Reference: calling conventions](https://llvm.org/docs/LangRef.html#calling-conventions)).

For bootstrap, make that boundary scalar-only: fixed-width integers, byte pointers, lengths,
opaque handles, and explicit out-pointers. Do not pass Silk structs, type unions, strings, errors,
or generic values by value across it. This avoids prematurely implementing every platform's
aggregate ABI classification and keeps public FFI out of scope.

The ABIs are genuinely different even where pointer width matches. Arm publishes AAPCS64 as the
base Arm64 procedure-call standard
([Arm AAPCS64](https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst)); Apple documents
platform-specific deviations from that standard
([Apple Arm64 ABI guidance](https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms));
and Windows x64 uses four argument registers, shadow space, restricted prologues/epilogues, and
Windows unwind metadata
([Microsoft x64 calling convention](https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention)).
LLVM can lower these conventions, but only after the frontend describes the boundary correctly.

### 3. Account for compiler-generated runtime calls

Target code generation can emit helper calls for operations unavailable as single instructions.
LLVM's compiler runtime supplies such target-specific builtins, and Clang chooses compiler-rt or
libgcc according to the target
([Clang complete-toolchain documentation](https://clang.llvm.org/docs/Toolchain.html#compiler-runtime)).
Even freestanding Clang builds must supply `memcpy`, `memmove`, and `memset`
([Clang freestanding-build documentation](https://clang.llvm.org/docs/UsersManual.html#freestanding-builds)).

The bootstrap language/runtime must therefore do one of the following:

- let the native Clang driver select and link the target's compiler runtime and C library; or
- ship its own required builtins and memory routines, then continuously audit emitted undefined
  symbols per target and optimization level.

The first option is materially cheaper and is recommended for bootstrap.

### 4. Keep the semantic compiler target-independent

Lexer, parser, type checking, ownership checking, monomorphization, diagnostics, and the
language-level IR should not branch on OS or CPU. Target-dependent decisions begin at concrete
layout, ABI boundary lowering, and LLVM emission. This separation is what makes Arm Linux a cheap
increment once Arm macOS and x86 Linux work.

## Runtime and platform-integration work

LLVM does not provide the compiler's process entry, argument decoding, allocator, file API,
terminal streams, exit behavior, or subprocess launching. A bootstrap runtime needs a very small
per-platform adapter for:

- process entry and command-line arguments;
- allocate/reallocate/deallocate;
- open/read/write/close and filesystem error translation;
- stdout/stderr and process exit;
- optionally spawning the pinned LLVM/Clang driver and collecting its exit status.

The last item is avoidable only if the Silk compiler stops at LLVM IR/bitcode/object output and an
external build harness performs native code generation and linking. That can prove the language
self-hosts, but it produces a less useful compiler driver. The acceptance definition should say
which model is intended.

Prefer a private C-shaped runtime object or library per OS and keep all boundary functions
scalar-only. This is an implementation boundary, not a user-facing FFI feature. It lets the first
self-hosted compiler exercise Silk allocation, ownership, errors, services, and scopes while
deferring a general foreign-function system.

### Link through a compiler driver

Emit an object (or LLVM input) and ask the target's Clang driver to link it. Do not call LLD
directly in the bootstrap driver. LLD supports ELF, PE/COFF, Mach-O, and WebAssembly, but on Unix
its own documentation says linkers are normally invoked by compiler drivers
([LLD overview](https://lld.llvm.org/)). The driver is the component that locates startup objects,
the target compiler runtime, C runtime, sysroot/SDK, default dynamic linker, and linker.

For cross-compilation, an LLVM backend is still insufficient: Clang requires a correct target
triple and generally a target sysroot, libraries, and headers; host dependencies cannot be reused
as target dependencies
([Clang cross-compilation guide](https://clang.llvm.org/docs/CrossCompilation.html)). For the
bootstrap acceptance matrix, native builds on each target are simpler and more meaningful than
maintaining cross sysroots.

## Per-target assessment

### `arm64-apple-darwin`: required baseline

**Language/backend:** already the development architecture and OS. Obtain the exact Darwin triple,
canonical data layout, conservative CPU features, and a minimum macOS deployment target from the
toolchain rather than hardcoding the current machine's full host triple.

**Runtime/platform:** implement the Darwin adapter and link against the macOS SDK through Apple's
Clang driver. Apple provides the command-line tools specifically for terminal builds and CI
([Xcode command-line tools](https://developer.apple.com/documentation/xcode/command-line-tools)).

**Link/test:** native execution is available locally. Make both unoptimized and optimized stage
builds pass so target-runtime calls introduced by optimization are visible.

**Distribution:** bootstrap may require Xcode Command Line Tools. A standalone binary and SDK/LLVM
redistribution are not part of this recommendation. An `x86_64` macOS slice is a cheap later
artifact: Apple documents compiling both targets on one Mac and combining them with `lipo`
([Apple universal-binary guidance](https://developer.apple.com/documentation/Apple-Silicon/building-a-universal-macos-binary)).
It should not expand the bootstrap acceptance matrix unless Intel-native/Rosetta testing is wanted.

**Marginal cost:** baseline, unavoidable.

### `x86_64-unknown-linux-gnu`: required

**Language/backend:** adds the mature LLVM X86 backend and catches accidental dependence on Arm
layout/lowering. LLVM documents ELF object support for both X86 and AArch64
([LLVM object-format table](https://llvm.org/docs/CodeGenerator.html#the-mc-layer)).

**Runtime/platform:** add the Linux adapter once. Use fixed-width boundary types; do not encode C
`long` or host-native structure layouts in the boundary. Link through native Clang against the
runner's GNU/Linux sysroot and runtime.

**Link/test:** run the entire staged build natively on x86-64 Linux, not merely cross-emit an ELF
object on macOS.

**Distribution:** for bootstrap, support the CI/runtime image's glibc rather than promising a
portable Linux binary. Choosing a minimum glibc baseline versus a musl/static distribution is a
separate packaging decision.

**Marginal cost:** low. It adds one CPU backend, the Linux adapter, and a native CI job. It is the
highest-value second target.

### `aarch64-unknown-linux-gnu`: required

**Language/backend:** reuses AArch64 code generation from macOS and the source-level compiler from
the other targets.

**Runtime/platform:** reuses the Linux adapter from x86-64. Only ABI-sensitive runtime build output,
compiler builtins, and architecture-specific distribution artifacts differ.

**Link/test:** a native Arm64 Linux stage job is necessary to claim support. GitHub currently lists
standard `ubuntu-24.04-arm` and `ubuntu-22.04-arm` runners for public and private repositories,
although they are marked public preview
([GitHub-hosted runner reference](https://docs.github.com/en/actions/reference/runners/github-hosted-runners)).
The preview status is an infrastructure risk, not language work; a self-hosted or other native
Arm64 Linux runner is a fallback.

**Distribution:** same unresolved libc baseline as x86-64 Linux, plus a distinct compiler and LLVM
toolchain artifact.

**Marginal cost:** low after the first two required targets. Its main new cost is native execution
capacity. It is worth including because it proves the CPU and OS abstractions compose rather than
merely existing as two hardcoded cases.

### `x86_64-pc-windows-msvc`: defer full support

**Language/backend:** LLVM emits X86-64 COFF and supports the Windows C calling convention. LLD's
Windows driver can read import/static libraries, create executables and DLLs, and emit PDBs
([LLD Windows status](https://lld.llvm.org/windows_support.html)). Object emission is therefore a
cheap early smoke test.

**Runtime/platform:** Windows requires a third adapter for process entry/arguments, allocation,
filesystem/stdio, error mapping, and process launch. The driver must also choose an MSVC/UCRT or
MinGW environment. In the MSVC environment, Clang finds components from three places: the Windows
SDK, UCRT, and Visual C++ tools
([Clang Windows system-library lookup](https://clang.llvm.org/docs/UsersManual.html#windows-system-headers-and-library-lookup)).

**Link/test:** link with `clang-cl`/`lld-link` or an explicitly selected MinGW toolchain, then run a
native Windows stage build. This also introduces PE entry/subsystem choices and Windows-specific
unwind/debug validation even if Silk typed failures do not use stack unwinding.

**Distribution:** Windows 10 and later include UCRT as an OS component, but SDK import libraries are
still build/link inputs and down-level/local deployment has additional rules
([Microsoft UCRT deployment](https://learn.microsoft.com/en-us/cpp/windows/universal-crt-deployment)).
A target decision must pin MSVC versus MinGW, minimum Windows version, dynamic versus static runtime,
and whether users provide the Windows SDK.

**Marginal cost:** medium-to-high. LLVM removes the machine-code problem, but Windows adds an OS
adapter, a different toolchain discovery model, native CI, and distribution policy. It does not
help validate the minimal language enough to justify putting it on the bootstrap critical path.

### `wasm32`: keep as stretch architecture, not a bootstrap host

**Language/backend:** LLVM and `wasm-ld` can emit/link `wasm32-unknown-unknown` objects. `wasm-ld`
also makes host imports, memory, exports, entry point, and unresolved-symbol policy explicit
([WebAssembly LLD documentation](https://lld.llvm.org/WebAssembly.html)). This makes Wasm object
emission a useful backend-neutrality smoke test.

**Runtime/platform:** `wasm32-unknown-unknown` has no filesystem, allocator host, CLI, or process
service by itself; imports must provide them. WASI supplies capability-oriented CLI and filesystem
interfaces, but selecting a WASI version/world, runtime, bindings model, and preopened filesystem is
real platform design, not just another LLVM triple
([WASI releases](https://wasi.dev/releases),
[WASI capability model](https://github.com/WebAssembly/WASI/blob/main/docs/Capabilities.md)).

There is also a bootstrap-specific obstacle: if the Silk compiler delegates code generation or
linking to an external LLVM executable, a Wasm-hosted compiler needs a host-defined process service,
an externally orchestrated build, or LLVM compiled into the Wasm program. Native targets do not
have this extra layer.

**Marginal cost:** high for full self-hosting despite cheap object emission. The desired custom
direct Wasm backend is a separate stretch milestone; forcing LLVM/WASI self-hosting first would not
materially de-risk that backend and could prematurely decide the eventual host interface.

## Bootstrap acceptance procedure per required target

A target belongs to the required matrix only when all of these run on that target's native OS/CPU:

1. **Toolchain probe:** verify the pinned LLVM version, requested target, canonical data layout,
   compiler runtime, linker, and SDK/sysroot are present. Fail with a structured diagnostic rather
   than silently using the host triple.
2. **Runtime conformance:** execute small programs covering arguments, allocation/reallocation/free,
   file read/write/close, stdout/stderr, nonzero exit, and representative error paths.
3. **Codegen conformance:** compile and run layout-sensitive structs/unions/generics, pointer-sized
   integers, recursive calls, and bulk memory operations at `-O0` and the milestone's release
   optimization level.
4. **Stage 1:** the Effect/TypeScript bootstrap compiler compiles the Silk compiler for the native
   target; the result starts and passes compiler tests.
5. **Stage 2:** that native Silk compiler compiles the same compiler sources for itself; the result
   starts and passes the same tests.
6. **Reproducibility check:** compare stage outputs under a specified normalization policy. Exact
   byte identity is desirable but should not be made an unstated requirement; functional
   equivalence and deterministic compiler-owned output are the minimum.

Cross-emission jobs from macOS should additionally ensure all assessed triples remain selectable
and produce the expected object format. They are fast regression tests, not substitutes for steps
2–5.

## Cost summary

| Target | LLVM object emission | New language work | New platform/link work | Native stage infrastructure | Bootstrap decision |
| --- | --- | --- | --- | --- | --- |
| Arm64 macOS | already available | baseline | Darwin adapter + SDK driver | local/macOS CI | require |
| x86-64 Linux | mature | CPU/layout portability | Linux adapter + GNU sysroot | common x64 Linux CI | require |
| Arm64 Linux | mature | little after above | reuse Linux adapter; Arm runtime artifacts | Arm64 Linux runner | require |
| x86-64 Windows | mature | ABI-boundary audit | Windows adapter + SDK/UCRT/toolchain policy | Windows CI | defer |
| wasm32 | mature object backend | address/runtime assumptions | WASI/import/runtime/orchestration design | Wasm runtime tests | stretch/defer |

## Decisions this research supports

- Treat target selection and canonical data layout as explicit backend inputs now.
- Design one target-independent compiler core and tiny OS runtime adapters.
- Keep the private runtime ABI scalar-only; do not make general FFI a bootstrap feature.
- Use a pinned native Clang driver to link and select compiler/C runtimes during bootstrap.
- Require Arm64 macOS, x86-64 Linux, and Arm64 Linux stage-2 self-hosting.
- Smoke-test Windows COFF and Wasm object emission, but do not promise those hosts in the milestone.
- Keep standalone LLVM bundling, Windows support, Linux portable-distribution policy, Effect
  integration, and the custom Wasm backend outside this target-matrix decision.

## Unresolved facts and follow-up decisions

1. **LLVM consumption model:** Will the self-hosted compiler emit LLVM input for external
   `clang`/`llc`, spawn those tools itself, or link LLVM in-process? This determines whether process
   spawning is a runtime requirement and whether per-target LLVM libraries must be distributed.
2. **Runtime shim implementation:** Will the private bootstrap shim be C/assembly compiled by the
   host toolchain, or unsafe Silk code once minimal low-level calls exist? The surface can remain the
   same either way.
3. **Linux ABI/distribution baseline:** `gnu` is appropriate for native bootstrap CI, but a release
   policy must later choose a minimum glibc version, musl/static artifacts, or both.
4. **Pinned LLVM compatibility:** The current direct bitcode writer is pinned. The project must pin
   the consumer LLVM version and test bitcode/IR compatibility rather than assume arbitrary system
   LLVM installations can consume its output.
5. **Arm64 Linux runner stability:** GitHub's standard Arm64 Linux runners are currently public
   preview. Confirm availability for this repository or provision an alternative before making the
   target a release gate.
6. **Optimization baseline:** Decide the release optimization level and whether debug/unwind
   metadata is required for bootstrap. Those settings affect runtime helper calls and platform
   validation.
7. **What “compiler” orchestrates:** Decide whether stage acceptance permits an external script to
   invoke LLVM and the linker. If yes, subprocess support can remain out of the language/runtime;
   if no, it is one of the compiler's minimal platform services.

## Primary sources

- [LLVM target-independent code generator](https://llvm.org/docs/CodeGenerator.html)
- [LLVM Language Reference](https://llvm.org/docs/LangRef.html)
- [LLVM target initialization source](https://llvm.org/doxygen/TargetSelect_8h_source.html)
- [`llc` command guide](https://llvm.org/docs/CommandGuide/llc.html)
- [LLVM object-code tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl08.html)
- [Clang complete-toolchain documentation](https://clang.llvm.org/docs/Toolchain.html)
- [Clang cross-compilation guide](https://clang.llvm.org/docs/CrossCompilation.html)
- [LLD overview](https://lld.llvm.org/)
- [LLD Windows support](https://lld.llvm.org/windows_support.html)
- [WebAssembly LLD port](https://lld.llvm.org/WebAssembly.html)
- [Arm AAPCS64](https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst)
- [Apple Arm64 ABI guidance](https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms)
- [Microsoft x64 calling convention](https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention)
- [WASI releases and specifications](https://wasi.dev/releases)
- [GitHub-hosted runner reference](https://docs.github.com/en/actions/reference/runners/github-hosted-runners)
