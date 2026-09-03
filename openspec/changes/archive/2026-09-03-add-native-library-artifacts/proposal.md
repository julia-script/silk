## Why

Silk can currently finalize LLVM output only as an executable, and its native-link contract reduces external inputs to object paths plus unstructured library names. Projects need first-class shared and static library artifacts with a closed, typed link model and a deliberately small exported ABI.

## What Changes

- Add explicit native artifact kinds for executables, shared libraries, and static libraries to project manifests, CLI planning, compiler requests, and final artifacts.
- Add an immutable structured native-link-input union covering object files, static archives, named static or dynamic libraries, search paths, and frameworks.
- Plan shared-library links through the target Clang driver and deterministic static archives through `llvm-ar rcsD`, reporting unsupported combinations as typed toolchain failures.
- Discover `export "C"` declarations as library roots without requiring `main`; retain the existing entry contract for executables.
- Hide compiler implementations, helpers, and runtime symbols while exposing only explicit C export thunks from native libraries.
- Add native acceptance coverage using a separately compiled C consumer and update the project-manifest and CLI reference.
- **BREAKING**: replace `build.native-libraries` and the driver’s separate native object/library arrays with `build.native-link-inputs` and the structured link-input API.

## Capabilities

### New Capabilities

- None.

### Modified Capabilities

- `bootstrap-native-toolchain`: Add shared-library and deterministic static-archive finalization, structured native link inputs, and exported-symbol control.
- `bootstrap-compiler-driver`: Make artifact kind and structured link inputs part of the end-to-end compilation request and outcome.
- `bootstrap-instances`: Permit library realization to root discovery at explicit C exports without a `main` entry.
- `silk-project-manifest`: Define native artifact-kind selection and structured native link inputs in `[build]`.
- `silk-cli-workflows`: Plan native library filenames and forward artifact/link configuration through project builds.

## Impact

The compiler’s driver, instance discovery, LLVM lowering, native toolchain, and public exports change together with CLI build planning and project-manifest decoding. Tests, fixtures, generated native commands, cache identities, and the user-facing manifest/CLI reference are updated in the same change. LLVM’s archive tool becomes a named member of the native toolchain boundary.
