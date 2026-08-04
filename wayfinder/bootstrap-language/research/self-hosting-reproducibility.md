# Deterministic native self-hosting acceptance

Research date: 2026-08-04. This note is scoped to the settled direct-LLVM-bitcode plus external-Clang
pipeline and the three required native hosts. Sources are LLVM, Clang, Apple, the ELF gABI, and GNU
binutils documentation or source.

## What can be compared

- LLVM bitcode is a serialization of LLVM IR, not a canonical semantic normal form. The format
  permits self-described abbreviations and records module data in a defined order; LLVM IR also
  preserves `source_filename` through bitcode, and use-list-order directives preserve an in-memory
  order that has no IR semantics. Equivalent modules can therefore have different bytes.
  [`LLVM Bitcode File Format`](https://llvm.org/docs/BitCodeFormat.html),
  [`source_filename` and use-list order](https://llvm.org/docs/LangRef.html#source-filename)
- This does not weaken Silk's fixed-point gate. Stage 1 and stage 2 use the same Silk bitcode writer,
  compiler sources, target, profile, and build recipe. That controlled case should require raw `.bc`
  byte equality. The writer must give every table and declaration a canonical order and use fixed
  logical module identifiers and source filenames rather than build paths.
- Do not call `llvm-dis | llvm-as` canonicalization. It is a useful diagnostic representation, but
  it does not turn semantically equivalent arbitrary modules into one canonical byte stream.
  `llvm-diff` is also diagnostic only: its documentation says it ignores ordering and local names
  while missing differences including linkage and function attributes.
  [`llvm-dis`](https://llvm.org/docs/CommandGuide/llvm-dis.html),
  [`llvm-diff` limitations](https://llvm.org/docs/CommandGuide/llvm-diff.html)
- Compare release artifacts for the fixed point. Use `-g0`; keep debug-build breakpoint, stepping,
  and stack-trace acceptance separate. LLVM's reproducible-build guidance recommends clean builds
  followed by direct artifact comparison, identifies the date/time macros as nondeterministic, and
  says Clang does not need GCC's `-frandom-seed` workaround. The C shim should reject date/time
  macros with `-Werror=date-time`; its paths should use `-ffile-prefix-map` and
  `-fdebug-prefix-map` where applicable. Silk-emitted IR must normalize its own path-bearing
  metadata before Clang receives the bitcode.
  [`Deterministic builds with Clang and LLD`](https://blog.llvm.org/2019/11/deterministic-builds-with-clang-and-lld.html),
  [`Clang path-mapping options`](https://clang.llvm.org/docs/ClangCommandLineReference.html#cmdoption-clang-ffile-prefix-map)

## Platform metadata

### Arm64 macOS

- Modern Apple `ld` generates `LC_UUID` from an output-content hash by default specifically for
  reproducible builds. `-random_uuid` makes it random; `-no_uuid` removes information used to pair
  an image with its symbols. Retain the default content-derived UUID in acceptance artifacts.
  Apple also notes that a code signature covers the UUID.
  [`ld` UUID options](https://github.com/apple-oss-distributions/ld64/blob/main/doc/man/man1/ld-classic.1#L860-L868),
  [Apple TN3178](https://developer.apple.com/documentation/technotes/tn3178-checking-for-and-resolving-build-uuid-problems)
- `LC_BUILD_VERSION` records the platform, minimum OS, SDK version, and build-tool versions. Pin the
  exact target triple, deployment target, SDK, Clang, and linker; otherwise a toolchain upgrade can
  legitimately change the bytes even when Silk input is unchanged.
  [`build_version_command`](https://github.com/apple-oss-distributions/xnu/blob/main/EXTERNAL_HEADERS/mach-o/loader.h#L1255-L1273),
  [`ld -platform_version`](https://github.com/apple-oss-distributions/ld64/blob/main/doc/man/man1/ld-classic.1#L566-L586)
- Apple Silicon requires executable code to be signed, and Clang/`ld` automatically add an ad-hoc
  signature at link time. Keep that linker-generated signature: Apple says it is not tied to the
  build machine and satisfies the native execution requirement. Fix the output leaf name and other
  code-signing identity inputs, then compare the signed linker outputs byte-for-byte.
  [`macOS Big Sur universal-app release notes`](https://developer.apple.com/documentation/macos-release-notes/macos-big-sur-11_0_1-universal-apps-release-notes/),
  [`ld` ad-hoc signing options](https://github.com/apple-oss-distributions/ld64/blob/main/doc/man/man1/ld-classic.1#L805-L811)
- Identity-based development or Developer ID signing is a distribution step, not bootstrap
  evidence. It changes `LC_CODE_SIGNATURE` and may add signing time or a secure timestamp. Compare
  before that step; if distribution signing is later required, verify it separately and never feed
  the signed file back into the fixed-point comparison.
  [Apple TN2206](https://developer.apple.com/library/archive/technotes/tn2206/),
  [Apple TN3161](https://developer.apple.com/documentation/technotes/tn3161-inside-code-signing-certificates)

### x86-64 and Arm64 Linux

- The ELF header has no timestamp member. GNU `ld`'s current-time insertion option is PE/COFF
  specific, not a normal ELF concern.
  [`Elf64_Ehdr`](https://refspecs.linuxfoundation.org/elf/gabi4%2B/ch4.eheader.html),
  [GNU `ld` PE timestamp option](https://sourceware.org/binutils/docs/ld/Options.html#index-_002d_002dinsert_002dtimestamp)
- A `.note.gnu.build-id` is optional metadata. GNU `ld --build-id=uuid` uses random bits, while
  `--build-id=sha1` hashes normative output content and is stable for identical output. Pass
  `-Wl,--build-id=sha1` explicitly instead of inheriting a distribution default. Use the same rule
  for `x86_64-unknown-linux-gnu` and `aarch64-unknown-linux-gnu`.
  [GNU `ld --build-id`](https://sourceware.org/binutils/docs/ld/Options.html#index-_002d_002dbuild_002did)
- If an archive becomes part of the shim/runtime input, require deterministic archive mode. LLVM's
  archiver uses zero timestamps and UID/GID by default; still record the exact archiver and argv.
  [`llvm-ar` deterministic archives](https://llvm.org/docs/CommandGuide/llvm-ar.html#deterministic-archives)

## Native versus cross-target evidence

- Clang is a cross-compiler when given `--target`, so a host can emit an object for another target.
  Linking and running require that target's startup objects, libraries, sysroot, and machine; object
  emission alone proves none of those. [`Clang cross-compilation guide`](https://clang.llvm.org/docs/CrossCompilation.html)
- A cross-target smoke test should emit an object and have pinned `llvm-readobj --file-header`
  confirm its format and machine. It must be reported as **object-emission smoke only**.
  [`llvm-readobj`](https://llvm.org/docs/CommandGuide/llvm-readobj.html)
- Runnable self-hosting requires a native compile, native link, and execution on each matching host:
  Arm64 macOS, x86-64 Linux, and Arm64 Linux. An emulator may be useful additional coverage but does
  not satisfy this native matrix. LLVM's `lli` is explicitly not a cross-architecture emulator.
  [`lli`](https://llvm.org/docs/CommandGuide/lli.html)

## Required pinning and provenance

Use one immutable toolchain/sysroot bundle per native runner. A version string is not enough: a
locally rebuilt binary can retain the same version text. Each accepted run should store a build
recipe and a build record containing at least:

1. The canonical sorted source manifest and digest for the compiler, standard library, C shim, and
   runtime inputs; target, CPU/features, profile, and logical source/build roots.
2. Resolved paths, SHA-256 digests, and `--version` output for Clang, the selected assembler,
   linker, `llvm-readobj`, and any archiver. Store Clang's normalized and effective target triples,
   resource directory, runtime-library path, and the complete `-###` command expansion. Clang
   documents all of these inspection operations.
   [`Clang driver options`](https://clang.llvm.org/docs/ClangCommandLineReference.html#driver-options),
   [`Clang toolchain and -###`](https://clang.llvm.org/docs/Toolchain.html)
3. The SDK/sysroot identity plus a content digest or immutable image digest; macOS SDK and
   deployment versions; the exact startup objects, runtime archives, dynamic-linker choice, and
   linked system libraries selected by the driver.
4. Complete ordered argv for bitcode-to-object compilation and native linking, the fixed output
   leaf name, a minimal environment allowlist (`PATH` resolved away, locale and timezone fixed),
   and every allowed environment value. Undeclared host state invalidates the run.
5. The producing compiler digest, every emitted artifact digest, diagnostics, exit status, and
   compiler/Clang/link timing and memory measurements. Stage 1 and stage 2 have different producer
   digests, but stage 1-to-2 and stage-2 fixed-point builds must consume the same recipe.

## Smallest credible bootstrap acceptance

Run this independently on each required native host:

1. Stage 0, the pinned Effect/TypeScript seed under pinned Node.js, compiles the canonical Silk
   compiler sources to release bitcode. The pinned native Clang emits the object and links the
   native stage-1 compiler with the pinned shim/runtime inputs.
2. Run stage 1 with Node.js and TypeScript unavailable. It consumes the same build recipe and
   produces the stage-2 bitcode, object, and native compiler through external Clang.
3. Run stage 2 once more with the same recipe to emit a fixed-point bitcode and object. This is
   verification evidence, not a separately distributed stage 3. Require raw byte equality between
   stage-2 and fixed-point bitcode and object artifacts.
4. Link the fixed-point object with the identical recorded linker invocation and require byte
   equality with the stage-2 executable: Mach-O including its content UUID and ad-hoc signature on
   macOS, ELF including its content-derived build ID on Linux.
5. Inspect each object and executable's format, machine, and platform metadata. Run stage 1 and
   stage 2 over the same minimum conformance/compiler corpus and require the same structured
   diagnostics, outputs, exit statuses, and artifact manifests; run the resulting programs.
6. Repeat the stage-2 fixed-point emission in a second clean physical directory mapped to the same
   logical roots. Any byte difference is a release blocker; disassembly and metadata inspection may
   diagnose it but do not weaken the comparison.

## GNU/Linux compatibility floor

Use **glibc 2.28 on an EL8 userland** as the ABI floor for both Linux targets. Assemble separate
`x86_64` and `aarch64` sysroots from the current UBI 8.10 BaseOS packages, then freeze the resolved
RPM NEVRAs, package and repository-metadata hashes, signing-key fingerprints, sorted sysroot
manifests, and per-architecture OCI digests in project-controlled storage. Do not name a mutable
`ubi8:latest` tag as the baseline: Red Hat exposes only current RPM versions on the public UBI CDN.
UBI content is freely redistributable, receives updates on the underlying RHEL schedule, and
provides public BaseOS repositories; RHEL 8 uses glibc 2.28, supports both required architectures,
and remains in its ten-year maintenance lifecycle until the transition on 2029-06-01.
[`RHEL 8 glibc 2.28`](https://docs.redhat.com/en/documentation/red_hat_enterprise_linux/8/html/8.0_release_notes/overview),
[`RHEL 8 architectures`](https://docs.redhat.com/en/documentation/red_hat_enterprise_linux/8/htmlsingle/considerations_in_adopting_rhel_8/hardware-enablement_considerations-in-adopting-rhel-8),
[`RHEL lifecycle`](https://access.redhat.com/support/policy/updates/errata),
[`UBI content and lifecycle`](https://access.redhat.com/support/policy/updates/ubi),
[`UBI repositories`](https://access.redhat.com/articles/4238681),
[`UBI 8 Arm64 image`](https://catalog.redhat.com/en/software/containers/ubi8/ubi-minimal/5c359a62bed8bd75a2c3fba8?architecture=arm64)

This is the oldest common maintained floor, not an arbitrary convenience floor. RHEL 7's glibc
2.17 line is no longer a candidate: its Arm64 port was retired and current RHEL 7 extended support
covers x86-64 and IBM Z, not Arm64. Newer Debian and Ubuntu LTS lines start at glibc 2.31 or later
and therefore cannot improve compatibility. Review and deliberately raise the floor before EL8's
maintenance window ends; do not silently keep an unmaintained sysroot.
[`RHEL 7 architectures`](https://docs.redhat.com/en/documentation/red_hat_enterprise_linux/7/html/7.9_release_notes/architectures),
[`RHEL 7 extended-support architectures`](https://access.redhat.com/support/policy/updates/errata_legacy),
[`Debian 11 glibc 2.31`](https://packages.debian.org/bullseye/libc6),
[`Ubuntu 20.04 glibc 2.31`](https://packages.ubuntu.com/focal/libc6)

The narrow C shim makes this older floor practical. Compile the fixed allocation, file/path,
directory, process, standard-stream, clock, and startup boundary against the pinned sysroot and
forbid Silk-generated code from importing arbitrary libc APIs. Account for compiler-introduced
`memcpy`, `memmove`, `memset`, arithmetic, and atomic helpers; link compiler-rt builtins into the
artifact and avoid dynamic LLVM, C++, `libgcc_s`, or `libatomic` dependencies. Do not statically
link glibc. glibc symbol versioning preserves old entry points in newer runtimes, but it cannot make
a binary linked to a new entry point load on an older runtime; building against the oldest supported
sysroot is the supported direction. [`Clang runtime libraries`](https://clang.llvm.org/docs/Toolchain.html),
[`glibc maintainer guidance`](https://sourceware.org/pipermail/libc-alpha/2023-July/150165.html),
[`RHEL 8 ABI compatibility`](https://access.redhat.com/articles/rhel8-abi-compatibility)

Add this exact gate for every stage-1, stage-2, fixed-point, and release ELF:

1. Build and link through the pinned Clang bundle with the matching immutable EL8 sysroot. The
   Clang executable itself must run in that userland if it is part of the claimed native bootstrap
   runner. Pin conservative ISA floors independently of libc: `-march=x86-64 -mtune=generic` and
   `-march=armv8-a`, unless a later ticket explicitly raises them.
2. Inspect with pinned `llvm-readelf --file-header --program-headers --dynamic-table --dyn-symbols
   --version-info --notes`. Require the target machine and ISA, the interpreter
   `/lib64/ld-linux-x86-64.so.2` or `/lib/ld-linux-aarch64.so.1`, no `RPATH`/`RUNPATH`, and an exact
   `DT_NEEDED` allowlist (prefer only `libc.so.6`). Resolve every undefined dynamic symbol to an
   allowed DSO. Reject `GLIBC_PRIVATE`, unexpected unversioned imports, and every numeric
   `GLIBC_*` requirement newer than `GLIBC_2.28`; reject undeclared `GCC_*` requirements too.
   [`llvm-readelf`](https://llvm.org/docs/CommandGuide/llvm-readelf.html),
   [`glibc binary inspection`](https://sourceware.org/glibc/manual/2.43/html_node/Dynamic-Linker-Hardening.html)
3. On native x86-64 and native Arm64, run the complete stage-0-to-stage-2 fixed-point procedure and
   generated conformance corpus inside the corresponding digest-pinned, fully patched UBI 8.10
   userland. Remove `LD_PRELOAD`, `LD_LIBRARY_PATH`, and `GLIBC_TUNABLES`; record `uname -m`,
   `/etc/os-release`, `getconf GNU_LIBC_VERSION`, and the loader/libc hashes. A cross-build, `--version`
   smoke, or emulation does not pass. A container proves the userland/loader floor but shares the
   host kernel; if Silk also claims an EL8 kernel floor, repeat on native per-architecture EL8 VMs.
4. Run the unchanged EL8-built artifacts on maintained newer glibc userlands on both architectures
   as supplemental forward-compatibility evidence. Keep a negative fixture importing a known newer
   symbol, such as `close_range@GLIBC_2.34`, and require both the static audit and EL8 loader to reject
   it, proving that the gate itself detects floor drift.

This is intentionally narrower than general supply-chain reproducibility. It proves deterministic
self-hosting with the declared native dependency boundary; independent rebuilds from source of
Clang, the OS SDK, libc, or distribution-signed packages belong to a later reproducible-release
milestone.
