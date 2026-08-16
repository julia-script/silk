# bootstrap-native-toolchain Delta

The disk artifact cache the toolchain already implements becomes reachable through configuration:
the default artifact cache honors `SILK_NATIVE_CACHE_DIR`, so identical compilation requests skip
the external Clang toolchain across processes and runs.

## ADDED Requirements

### Requirement: The default artifact cache persists to a configured directory

When the `SILK_NATIVE_CACHE_DIR` environment variable names a directory, the toolchain's default
artifact cache SHALL persist finalized native and WebAssembly artifacts in that directory, keyed by
the content of the compilation request: artifact kind, target triple, profile, Clang identity,
runtime shim, and input bitcode. A request whose key matches a stored artifact SHALL reuse it
without invoking the external toolchain. When the variable is unset, the default cache SHALL retain
its process-local behavior unchanged. A corrupted or missing cache entry SHALL cause recompilation,
never a failed or incorrect build.

#### Scenario: A second process reuses a cached artifact

- **WHEN** two processes compile an identical request with `SILK_NATIVE_CACHE_DIR` set to the same directory
- **THEN** the second process produces a byte-identical artifact without invoking Clang

#### Scenario: A changed input misses the cache

- **WHEN** the bitcode, profile, target, shim, or Clang identity of a request differs from every stored entry
- **THEN** the toolchain compiles the request through Clang and stores the new artifact under its own key

#### Scenario: The variable is unset

- **WHEN** `SILK_NATIVE_CACHE_DIR` is not set
- **THEN** the default cache remains process-local and no artifact is written outside the build's own scope
