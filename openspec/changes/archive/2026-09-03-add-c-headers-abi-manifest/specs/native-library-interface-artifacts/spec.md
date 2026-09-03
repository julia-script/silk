## Purpose

Defines the deterministic C header and machine-readable ABI manifest that make a native Silk
library consumable and reviewable without copying declarations from Silk source.

## ADDED Requirements

### Requirement: Native libraries publish sibling interface artifacts

A successful native shared- or static-library build SHALL commit `<package>.h` and
`<package>.abi.json` beside the platform library and SHALL report all three durable paths.
Executable and WebAssembly-module builds SHALL emit neither companion. Cached and uncached library
builds SHALL produce byte-identical companions from the current verified ABI inventory.
Once the primary library has committed, a failed or interrupted companion commit SHALL remove the
library, header, and manifest destinations, including stale companions from an earlier build.

#### Scenario: Commit a shared-library interface

- **WHEN** a native shared library named `answer` builds successfully
- **THEN** its output directory contains the platform library, `answer.h`, and `answer.abi.json`

#### Scenario: Commit a static-library interface

- **WHEN** a native static library named `answer` builds successfully
- **THEN** its output directory contains the archive, `answer.h`, and `answer.abi.json`

#### Scenario: Omit companions for non-library artifacts

- **WHEN** the same package builds as a native executable or WebAssembly module
- **THEN** no C header or ABI manifest is emitted or reported

#### Scenario: Reproduce companions on a cache hit

- **WHEN** a native-library request is satisfied from either backend or final-artifact cache
- **THEN** the companions are regenerated from the verified cached inventory with the same bytes as an uncached build

#### Scenario: Roll back a failed companion rebuild

- **WHEN** stale header and manifest siblings exist and staging the replacement manifest fails
- **THEN** the primary library and both companion destinations are absent after cleanup

### Requirement: The generated header is valid canonical C

The header SHALL include `<stdint.h>` and declare every exported C function and immutable exported
data symbol in canonical symbol order. Integer classes SHALL use exact-width `intN_t` or `uintN_t`
types, `f32` and `f64` SHALL use `float` and `double`, immutable and mutable opaque pointers SHALL
use `const void *` and `void *`, and C callback classes SHALL use valid nested function-pointer
declarators. A no-argument function SHALL use `(void)`. The header SHALL use a package-derived
collision-safe include guard and C++ linkage guards without changing the C ABI.

#### Scenario: Render scalar and data exports

- **WHEN** a library exports `increment(i32) -> i32` and immutable data `silk_abi_version: u32`
- **THEN** the header declares `int32_t increment(int32_t arg0);` and `extern const uint32_t silk_abi_version;`

#### Scenario: Render pointer and callback exports

- **WHEN** an exported function receives immutable and mutable pointers plus a C callback
- **THEN** the header uses `const void *`, `void *`, and a nested `(*argN)(...)` declarator with exact scalar classes

### Requirement: The ABI manifest is versioned canonical data

The JSON manifest SHALL contain schema marker `silkForeignAbi: 1`, the canonical target id, and
`exports` and `imports` arrays. Every entry SHALL contain its symbol, explicit ABI `C`, lowercase
direction, and kind. Function entries SHALL contain canonical parameter and result classes; data
entries SHALL contain their canonical type class. Entries SHALL be ordered by symbol and then kind,
object fields SHALL have one stable order, and the document SHALL end with one newline.

#### Scenario: Record the complete native ABI

- **WHEN** a library artifact retains imported and exported functions and data symbols
- **THEN** the manifest records every retained entry once under its direction with the target-qualified ABI classes

#### Scenario: Emit deterministic target-specific bytes

- **WHEN** the same admitted source is rendered repeatedly for one native target
- **THEN** the manifest bytes are identical and target-sized integers use that target's fixed-width class
