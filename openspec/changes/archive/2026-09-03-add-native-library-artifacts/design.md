## Context

See `proposal.md` for motivation. Today the driver always discovers `main`, LLVM lowering always
assigns one MIR machine entry the symbol `silk_main`, and native finalization always compiles a C
process shim before a Clang executable link. Manifest native dependencies are only string library
names. The repository is green-field, so the old arrays are removed rather than adapted.

## Goals / Non-Goals

**Goals:**

- Keep artifact kind and each link input as immutable domain data from manifest decoding through the
  final pure command plan.
- Give executable and library builds distinct root and runtime policies without teaching semantic
  analysis any standard-library or project spelling.
- Guarantee the public shared-library surface at LLVM visibility and C-runtime compilation
  boundaries, then prove it with a platform symbol dumper and an external C consumer.
- Keep every filesystem and subprocess failure inside the typed native-toolchain boundary.

**Non-Goals:**

- Producing universal/fat libraries, import libraries, package-manager metadata, or versioned SONAME
  policy.
- Accepting arbitrary linker options or discovering third-party libraries.
- Treating static archive members as a stable ABI beyond their explicit C thunk names.

## Decisions

### Artifact kind is a compiler-owned actor

`ArtifactKind` owns `NativeExecutable`, `NativeSharedLibrary`, `NativeStaticLibrary`, and
`WebAssemblyModule`, their manifest spellings, target compatibility, and conventional filenames.
Driver requests carry the final kind directly. This avoids string branching across CLI, driver, and
toolchain. A CLI-only enum was rejected because embeddings need the same closed contract.

### Native link inputs are one ordered tagged union

`NativeLinkInput` owns constructors and canonical encoding for Object, StaticArchive, Library
(Static or Dynamic), SearchPath, and Framework. Manifest decoding resolves path forms relative to
the manifest; program and compiler-generated objects remain explicit prefix inputs to finalization.
Tool planning folds the supplied union in order. Separate arrays were rejected because they erase
relative ordering and encourage each layer to grow another ad-hoc parameter.

### Entry policy is selected before instance discovery

Realization receives an executable or library root policy. Executable policy uses the existing
`main` resolver. Library policy inventories valid C exports first, rejects an empty inventory, and
seeds the worklist only with those export keys. MIR gains an explicit library entry with no machine
entry; LLVM symbol naming therefore never invents `silk_main` for a library. This is preferable to a
synthetic main because a synthetic process contract would contaminate reachability, termination,
and the exported surface.

### Runtime support and finalization depend on artifact kind

Executables compile the current process shim plus runtime fragments. Libraries compile only the
runtime fragments their reachable functions require. Shared libraries link the program and runtime
objects through target Clang shared mode. Static libraries archive those objects and any supplied
object inputs through pinned `llvm-ar rcsD`; link-only input forms are rejected for static output.
The toolchain actor holds both `clang` and `llvmAr` paths so tests and embeddings can pin them.

### Visibility is explicit at definition sites

Every compiler implementation function and generated helper definition is emitted with hidden or
internal LLVM visibility; only C export thunks retain default visibility. C runtime fragments are
compiled with hidden default visibility. This makes shared-library dynamic symbol tables closed by
construction rather than relying on a generated platform export list. Linker allow-lists were
rejected because they duplicate the export inventory in platform-specific temporary files and do
not protect intermediate artifacts.

### Target-specific link syntax remains pure planning

The pure planner maps dynamic libraries and search paths for all native targets, frameworks only for
Apple targets, and static named libraries only where the target driver has an unambiguous scoped
static-selection syntax. Unsupported combinations return typed planning failures before spawn.
This is preferable to guessing platform behavior or silently weakening requested static linkage.

## Risks / Trade-offs

- [Static archives expose member-level global symbols to archive tooling even when shared-library
  visibility is hidden] → Treat explicit C thunk names as the supported archive ABI and test the
  stricter default-visible guarantee on the loadable shared artifact.
- [Platform link syntax differs] → Centralize it behind canonical target facts and test both pure
  plans and host acceptance; unsupported combinations fail explicitly.
- [Changing entry shape touches evaluator and Wasm assumptions] → Admit the library entry only for
  LLVM native requests and make consumers exhaustively handle it rather than fabricating a machine
  entry.
- [Archive reproducibility can inherit object nondeterminism] → Retain existing deterministic LLVM
  emission gates and invoke archive deterministic mode with stable member ordering.

## Migration Plan

Replace all internal callers, tests, fixtures, manifest examples, and documentation in this change.
Delete `nativeObjects`, `nativeLibraries`, and `native-libraries`; there is no compatibility parser
or adapter. Rollback is a full stack-layer revert because the repository has no released
compatibility contract.
