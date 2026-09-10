## Purpose

Define source-owned native filesystem calls, byte-preserving paths and resource/error contracts independently of compiler filesystem policy.

## ADDED Requirements

### Requirement: Native filesystem declarations match each selected C ABI

Declarations SHALL exist only for the three supported native target/libc pairs. Open calls MUST use actual C variadics and target-correct promoted mode arguments. Record layout, flag constants and signatures MUST be established independently for each target. All eleven former filesystem intrinsics, compiler handle operations and generated filesystem C policy SHALL be removed.

#### Scenario: GNU architectures use different directory flags

- **WHEN** filesystem source is selected for GNU ARM64 or x86-64
- **THEN** calls use the selected architecture's verified flags and stat layout

#### Scenario: Unsupported supply

- **WHEN** a program requests OsFileSystem on Wasm or a native no-libc profile
- **THEN** ordinary selected-source availability rejects the missing member before emission

### Requirement: Handles have one source-owned close obligation

Successful descriptor acquisition SHALL create one affine source owner. fdopendir SHALL transfer ownership only on success. Explicit consuming cleanup SHALL disarm before one close or closedir attempt and MUST NOT retry close after EINTR. Structured unwinding and cancellation SHALL release remaining owners. Primary failures SHALL survive secondary cleanup failures; otherwise cleanup failure SHALL become the operation result. Fatal-trap cleanup is not guaranteed.

#### Scenario: Failed stream transfer

- **WHEN** fdopendir fails
- **THEN** its errno is captured before cleanup and the original descriptor is closed once

#### Scenario: Canceled traversal

- **WHEN** structured cancellation exits an operation owning a descriptor or directory stream
- **THEN** each still-owned resource receives exactly one cleanup attempt

### Requirement: Native errors are captured only by their call protocol

Failed calls SHALL capture errno before any subsequent native call or resource release. Successful metadata indicating an unsupported kind SHALL produce a logical error with native code zero. Directory reads SHALL clear errno before readdir and distinguish null EOF from null failure; positive records SHALL not read errno.

#### Scenario: Successful metadata with stale errno

- **WHEN** fstat succeeds for a non-regular file while errno contains an unrelated failure
- **THEN** the provider returns WrongType with code zero

### Requirement: Directory entries remain owned across retries

Directory reads SHALL inspect only valid prefix fields and record-bounded name bytes, never load a nominal full dirent. Names SHALL be copied before another readdir. A short output buffer SHALL retain the same pending entry and report its required length. Byte names SHALL be preserved without UTF-8 validation. Symlinks and unsupported kinds SHALL be rejected through no-follow metadata inspection.

#### Scenario: Short allocation and long name

- **WHEN** readdir supplies a record smaller than sizeof(dirent) whose name exceeds the caller buffer
- **THEN** only valid bytes are read and a larger retry returns the owned same name without advancing readdir

### Requirement: Unique directory allocation is bounded and exclusive

Unique directory creation SHALL validate the prefix and attempt at most 128 exclusive creations using a provider-local wrapping counter encoded in sixteen hexadecimal digits. It SHALL retry collisions only, return other failures, and report logical AlreadyExists on exhaustion. Names are predictable and SHALL NOT require entropy, clocks or process identifiers.

#### Scenario: Collision followed by success

- **WHEN** a generated name already exists
- **THEN** the counter advances and a later successful exclusive creation returns its owned path

#### Scenario: Exhausted candidates

- **WHEN** all 128 candidates collide
- **THEN** creation returns AlreadyExists with native code zero and terminates

### Requirement: Native conformance proves boundary behavior

Independent C fixtures SHALL verify all three target layouts, flags and call signatures and execute all six target/optimization lanes. Evidence SHALL cover partial I/O, immediate errno capture, short directory records, pending names, resource transfer, cleanup failure ordering, byte paths, symlinks and bounded collisions. Shared native acceptance SHALL retain real filesystem effects.

#### Scenario: Exact boundary evidence

- **WHEN** a source declaration or layout changes
- **THEN** independently compiled C receiver and layout checks can fail without duplicating the source implementation
