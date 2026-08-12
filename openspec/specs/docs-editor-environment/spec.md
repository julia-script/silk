# Docs Editor Environment Specification

## Purpose

Defines one browser-scoped execution environment whose WebContainer runtime and standard Effect filesystem are shared by every subsystem in the docs editor.

## Requirements

### Requirement: One editor execution environment
The docs editor SHALL acquire at most one WebContainer runtime for an editor application instance, and every editor subsystem requiring runtime or filesystem capabilities SHALL resolve them from that same acquired environment.

#### Scenario: Multiple editor consumers mount
- **WHEN** the terminal and another editor subsystem both request execution-environment capabilities
- **THEN** both consumers use the same acquired WebContainer rather than initiating competing boots

#### Scenario: Environment is constructed outside the browser
- **WHEN** the docs application imports or constructs the editor environment during server rendering or build-time evaluation
- **THEN** no WebContainer boot or browser-global access occurs

### Requirement: Standard filesystem is globally available to editor subsystems
The editor environment SHALL provide Effect's standard `FileSystem` service at the editor application boundary, backed by the same WebContainer filesystem used by terminal processes. Editor subsystems SHALL consume this standard service without importing the raw WebContainer filesystem API or creating their own runtime layer.

#### Scenario: Terminal and filesystem consumer share changes
- **WHEN** a terminal process creates or modifies a file and an editor subsystem reads that path through the standard `FileSystem` service
- **THEN** the subsystem observes the file content written by that terminal process

#### Scenario: Future subsystem requests filesystem access
- **WHEN** a file tree, source editor, package manager, or preview actor requires `FileSystem`
- **THEN** the actor can resolve the existing editor-scoped service without changing the application composition or booting another WebContainer

### Requirement: Environment lifecycle follows the editor application
The editor execution environment SHALL remain available while editor consumers are mounted and SHALL release its WebContainer and filesystem resources when the editor application scope closes after success, failure, defect, or interruption.

#### Scenario: User leaves the editor
- **WHEN** the `/editor` application unmounts with no remaining editor consumers
- **THEN** its running processes are released before the shared WebContainer runtime is torn down

#### Scenario: Editor initialization fails
- **WHEN** environment acquisition fails
- **THEN** the failure is represented through the typed editor initialization state and no partially acquired runtime is retained

### Requirement: Replaceable environment for deterministic tests
The editor application SHALL permit tests to replace the live editor environment with a deterministic implementation while preserving the same runtime and standard filesystem requirements.

#### Scenario: Atom behavior is tested without browser boot
- **WHEN** a test supplies a deterministic editor environment to a fresh application state registry
- **THEN** terminal and filesystem behavior can be exercised without booting WebContainer or sharing state with another test
