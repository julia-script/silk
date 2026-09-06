## Context

See proposal.md for motivation. Logical profiles and artifact roots are already implemented.
JUL-148 makes all incomplete native final inputs ineligible; Wasm has a separate existing policy.
The toolchain is a Node-only boundary and must not leak into the browser-safe compiler barrel.

## Goals / Non-Goals

Goals: resolve one physical contract per build, preserve exact linker semantics and ordered input
closure, expose evidence, and make complete native final artifacts eligible under the existing rule.
Non-goals: runtime policy, OS APIs, managed installation, arbitrary linker command strings, general
linker language interpretation, and LTO (not admitted by the logical profile).

## Decisions

### Request and provider boundary

Use pure PlatformSupply records and a Node-only resolver. A request selects Automatic, Native,
Explicit, or Managed. Artifact pin wins over project pin; an explicit top-level request wins over
both. Automatic uses the winning pin or Native only when the declared host target equals the
requested canonical target. Managed fails with a corrective diagnostic. Explicit failure is final.
Explicit supplies name their target, sysroot/SDK, linker, and optional compiler-support installation
roots; every exception to the platform root has a named compatible provenance claim. Tool paths
remain toolchain inputs and do not enter compiler semantic selection.

Capture only consulted PATH, SDKROOT, DEVELOPER_DIR, and tool query results. Execute tools with a
frozen controlled environment, no implicit Clang configuration, no CPATH/LIBRARY_PATH or ambient
loader overrides. Resolve executable paths and hash full bytes plus full version output. Discovery
queries use argument arrays; never invoke a shell. File identities include resolved path, content
digest, role, and selection origin. Revalidate selected bytes before consumption; a changed file
requires explicit re-resolution instead of silently changing the build.

### Capabilities and platform compatibility

Darwin: SDKSettings.json supplies version and supported architectures/deployment range. An absent
logical deployment uses the admitted ARM64 minimum 11.0.0, never the host OS version. SDKROOT is an
explicit discovery input and invalid values fail. Otherwise query xcrun with DEVELOPER_DIR. Resolve
libSystem and requested frameworks independently, retaining TBD install names and reexport closure.
Inline TBD subdocuments satisfy their own reexports without a second filesystem lookup. Headers
are not part of the link contract merely because the SDK contains them.

GNU: query the selected Clang for its library/search/tool paths and dry-run link command. An
explicit sysroot restricts libc/CRT/loader components to that installation, with compiler support
roots separately declared. Inspect ELF architecture, glibc version requirements and interpreter;
reject mixtures and a deployment below a selected component requirement. A logical libc-none
plan supplies tools only, no implicit CRT/libc/interpreter. This does not introduce startup code.

### Concrete link closure

Use Clang --no-default-config -### to obtain the platform's complete linker argv, then resolve and
execute that selected linker directly. Pin its path with --ld-path. This retains toolchain CRT
ordering without hard-coding Linux distribution directories. Static archives use the selected
LLVM archiver directly. Reject unexpected commands and unsupported input-bearing options.
Resolve every -l and framework against the recorded ordered roots. Recursively account for GNU
INPUT/GROUP/AS_NEEDED/INCLUDE/SEARCH_DIR scripts and preserve their group/as-needed semantics.
Retain layout scripts as scripts; rewrite resolved references into scope-owned scripts where needed
so execution cannot perform a different search. Record original bytes and transformed semantics.
Dynamic import/reexport closure is accounted for by target format, not by assuming undefined
symbols must occur in directly listed objects. The actual selected linker is the symbol resolution
oracle for archive extraction, weak definitions, shared imports, duplicate definitions and conflicts;
failures retain full commands and component origins. Compatibility validation happens before link.

### Identity and consumers

Keep semantic and bitcode/object identities dependent on logical code generation only. Physical
resolution happens after semantic planning. Final identity uses ordered selected file contents,
linker/tool bytes, concrete flags, generated objects, entry source, and embedded path identities
(interpreter, rpath, install-name, and path-valued imports). Search roots and non-emitted equivalent
supply paths do not distinguish identities. A complete plan is the evidence consumed by JUL-148's
permanent admission rule. Do not create a second policy. Native lookup occurs after complete
physical resolution; Wasm retains its existing independent path.

C compilation consumes the frozen compiler/platform subset. Scan actual dependencies with the same
flags, hash the preprocessed translation unit and consumed headers plus compiler bytes, and compile
that frozen translation unit. This prevents a header edit between dependency scan and compilation
from changing cached bytes. No whole-SDK hash enters runtime object identity. Compiler object
emission uses the frozen compiler command/environment without SDK header discovery.

### Research and authority

Pinned LLVM/Clang/lld 22.1.8; Darwin SDK15.5 with ARM64 deployment11.0.0; GNU glibc
2.36-9+deb12u14, GCC12.2.0-14+deb12u1 support objects, binutils2.40-2 and Linux headers
6.1.180-1. Exact component digests and commands are generated by conformance, not inferred from
version labels. ABI authorities are the existing pinned AAPCS64 2025Q1, x86 psABI revision
e1ce098331da5dbd66e1ffc74162380bcc213236 and LLVM22.1.8 LangRef.

Read Zig e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa LibCDirs/LibCInstallation and
standalone/glibc_compat: adopt component discovery, but deliberately reject explicit failures instead
of Zig's bundled fallback. Rust c33d8f3b5a50b56466998e8c5ed8a077d2caed84 apple.rs/link.rs
and run-make apple-sdk-version, apple-deployment-target, link-under-xcode, linkage-attr-framework
cover SDK metadata, environment and frameworks. Rust's warning-and-continue SDK failure is not
adopted. Neither project provides Silk's exact immutable transitive closure/cache contract; use
structural fixtures and actual linker inspection for that contract. GNU ld2.40's linker-script and
archive semantics and LLVM22.1.8 TextAPI/ELF/Mach-O formats govern physical resolution.

## Risks / Trade-offs

- Driver argv evolution → parse only admitted commands and fail closed for unknown input forms;
  pinned CI proves the actual supported tools.
- Linker scripts and TBD reexports → recursively preserve semantics, detect cycles, and report
  unsupported constructs with the original component origin.
- Mutable supply directories → hash selected bytes and validate before use; frozen preprocessed C
  avoids header rediscovery. No implicit re-resolution after a cache miss or provider failure.
- More resolution work before a final cache hit → necessary for correctness; share one snapshot
  within a build and cache only with its complete relevant input subset.
- Target runners unavailable → required CI fails; designated ARM64 Linux execution uses its runner.

## Migration Plan

Replace all native Driver/finalizer/build callers together, add typed project request decoding and
inspection, remove ambient finalization and incomplete native key generation, and update tests and
public exports. No compatibility shims. Implement conformance and gates before publishing the stack.
