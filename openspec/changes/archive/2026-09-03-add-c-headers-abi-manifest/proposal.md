## Why

Native Silk libraries expose a verified C ABI, but consumers must still copy prototypes by hand
and automation has no stable description to compare. Generating companions from the same verified
inventory as the library makes the published interface directly consumable and reviewable.

## What Changes

- Emit `<package>.h` and `<package>.abi.json` beside every successfully committed native shared or
  static library, including cache hits.
- Render fixed-width C declarations for exported functions and immutable data symbols, including
  valid nested function-pointer declarators and the ABI's opaque pointer convention.
- Publish a canonical, versioned JSON manifest for imported and exported functions and data symbols.
- Keep executables and WebAssembly modules free of C-library companion artifacts.
- Pin companion bytes for every supported native target and compile the existing native C consumer
  against the generated header.

## Capabilities

### New Capabilities

- `native-library-interface-artifacts`: The filenames, content, ordering, and commit behavior of C
  headers and ABI manifests emitted for native libraries.

### Modified Capabilities

- `bootstrap-compiler-driver`: A successful native-library compilation returns the identities of
  all durable library artifacts and commits them consistently on cached and uncached paths.
- `silk-cli-workflows`: Project builds report and preserve generated native-library companions.

## Impact

This adds focused `CHeader`, `AbiManifest`, and library-artifact actors in `packages/compiler`,
extends the driver and CLI build outcome data, updates native library acceptance without another
toolchain process, and documents the generated public surface. No new runtime dependency or
compiler-known source declaration is introduced.
