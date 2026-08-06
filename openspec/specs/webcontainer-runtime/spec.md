# WebContainer Runtime Specification

## Purpose

Provides an Effect-native, resource-safe boundary for acquiring and operating an in-browser WebContainer runtime without exposing raw promises or manual teardown to consumers.

## Requirements

### Requirement: Scoped runtime lifecycle
The package SHALL provide a scoped runtime service whose acquisition boots a WebContainer and whose release tears down that same instance exactly once. Release SHALL run after successful use, typed failure, defect, or interruption and SHALL NOT replace the original exit.

#### Scenario: Runtime is released after successful use
- **WHEN** a consumer acquires the runtime service and its enclosing scope closes normally
- **THEN** the acquired WebContainer is torn down exactly once

#### Scenario: Runtime is released after interrupted use
- **WHEN** a fiber using the runtime service is interrupted and its enclosing scope closes
- **THEN** the acquired WebContainer is torn down exactly once without replacing the interruption

#### Scenario: Runtime boot fails
- **WHEN** WebContainer boot rejects or throws
- **THEN** service acquisition fails with a typed WebContainer error identifying the boot operation and preserving diagnostic ancestry

### Requirement: Lazy and shareable acquisition
Constructing the runtime layer SHALL NOT boot a WebContainer or access browser-only runtime state. A single built layer instance SHALL share one acquired runtime throughout its layer graph so that dependent capabilities do not attempt competing boots.

#### Scenario: Layer is constructed outside acquisition
- **WHEN** a consumer constructs or composes the runtime layer without opening its scope
- **THEN** no WebContainer boot is attempted

#### Scenario: Multiple capabilities use one layer
- **WHEN** runtime, filesystem, process, and event capabilities are derived from the same built layer instance
- **THEN** they use the same acquired WebContainer

### Requirement: Configurable boot
The runtime acquisition SHALL accept the supported WebContainer boot options for cross-origin embedding mode, working-directory name, and preview-error forwarding and SHALL pass only explicitly supplied options to boot.

#### Scenario: Custom boot options
- **WHEN** a consumer acquires a runtime with custom boot options
- **THEN** the WebContainer is booted with those values

#### Scenario: Default boot options
- **WHEN** a consumer acquires a runtime without boot options
- **THEN** WebContainer defaults determine the boot behavior

### Requirement: Runtime metadata
The acquired runtime service SHALL expose its WebContainer working directory and executable search path as immutable metadata.

#### Scenario: Read runtime metadata
- **WHEN** a consumer accesses runtime metadata after acquisition
- **THEN** the consumer receives the working directory and executable search path of the acquired instance

### Requirement: Mount filesystem content
The runtime service SHALL mount either a filesystem tree or a binary snapshot, optionally at a requested mount point, and SHALL report failures through the typed WebContainer error channel.

#### Scenario: Mount a filesystem tree at the root
- **WHEN** a consumer mounts a valid filesystem tree without a mount point
- **THEN** the content is mounted at the runtime filesystem root

#### Scenario: Mount a snapshot at a mount point
- **WHEN** a consumer mounts binary snapshot content with an existing mount point
- **THEN** the content is mounted at that location

#### Scenario: Mount fails
- **WHEN** WebContainer rejects a mount operation
- **THEN** the operation fails with a typed WebContainer error identifying the mount operation

### Requirement: Export filesystem content
The runtime service SHALL export a requested path as a filesystem tree, binary snapshot, or ZIP byte sequence according to the requested format and SHALL preserve the format in the result type.

#### Scenario: Export JSON content
- **WHEN** a consumer exports a path using the JSON format
- **THEN** the operation returns a filesystem tree

#### Scenario: Export binary or ZIP content
- **WHEN** a consumer exports a path using the binary or ZIP format
- **THEN** the operation returns a byte sequence

#### Scenario: Export fails
- **WHEN** WebContainer rejects an export operation
- **THEN** the operation fails with a typed WebContainer error identifying the export operation and path

### Requirement: Configure preview injection
The runtime service SHALL allow a consumer to configure an optional preview script and its supported script attributes through a typed Effect.

#### Scenario: Configure a preview script
- **WHEN** a consumer supplies preview script source and attributes
- **THEN** future or reloaded previews use that configuration after the operation completes

#### Scenario: Preview configuration fails
- **WHEN** WebContainer rejects preview-script configuration
- **THEN** the operation fails with a typed WebContainer error identifying the preview configuration operation

### Requirement: Effect-only external boundary
Public fallible runtime operations SHALL return Effects with a precise WebContainer error type. The package SHALL NOT expose the raw WebContainer instance, its bare promises, or manual teardown as part of the public service contract.

#### Scenario: Consumer composes a runtime operation
- **WHEN** a consumer mounts, exports, configures, or otherwise operates the runtime
- **THEN** the operation composes in Effect without awaiting a raw promise or catching an unknown thrown value
