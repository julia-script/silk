## Purpose

Provides scoped, Effect-native command execution over WebContainer while preserving the runtime's combined-terminal-output model rather than pretending to be a POSIX child process.

## ADDED Requirements

### Requirement: Spawn a WebContainer process
The runtime service SHALL spawn a command with an argument list and supported working-directory, environment, output, and terminal-dimension options through a typed Effect.

#### Scenario: Spawn with command options
- **WHEN** a consumer supplies a command, arguments, working directory, environment values, and terminal dimensions
- **THEN** the returned process runs with those values in the acquired WebContainer

#### Scenario: Spawn fails
- **WHEN** WebContainer rejects process creation
- **THEN** the operation fails with a typed WebContainer error identifying the command and spawn operation

### Requirement: Scoped process lifecycle
Every spawned process SHALL be scoped. When its scope closes while the process is still running, release SHALL request process termination and SHALL NOT replace the scope's original success, failure, defect, or interruption.

#### Scenario: Scope closes around a running process
- **WHEN** a process remains active when its enclosing scope closes
- **THEN** the process receives one termination request

#### Scenario: Process already exited
- **WHEN** the enclosing scope closes after the process has exited
- **THEN** release completes without attempting to reinterpret the exit code as a cleanup failure

### Requirement: Await process exit
The process actor SHALL expose an Effect that completes with the integer exit code and SHALL not treat a nonzero exit code as an Effect failure.

#### Scenario: Successful command exits
- **WHEN** a process exits with code zero
- **THEN** awaiting exit succeeds with zero

#### Scenario: Command exits nonzero
- **WHEN** a process exits with a nonzero code
- **THEN** awaiting exit succeeds with that nonzero code

### Requirement: Stream combined terminal output
The process actor SHALL expose WebContainer's combined stdout-and-stderr text output as an Effect stream in emission order. When output is disabled at spawn, the stream SHALL complete without emitting chunks.

#### Scenario: Read process output
- **WHEN** a process writes interleaved standard output and standard error
- **THEN** the process output stream emits the combined text in the order delivered by WebContainer

#### Scenario: Output is disabled
- **WHEN** a process is spawned with output disabled
- **THEN** its output stream completes without emitted chunks

### Requirement: Write terminal input
The process actor SHALL expose a sink for ordered string input to the process pseudoterminal and SHALL close or release the underlying writer when sink use completes or is interrupted.

#### Scenario: Send multiple input chunks
- **WHEN** a consumer runs the input sink with multiple strings
- **THEN** the process receives the strings in input order

#### Scenario: Input use is interrupted
- **WHEN** a fiber writing process input is interrupted
- **THEN** the underlying stream writer lock is released

### Requirement: Control the process
The process actor SHALL provide typed Effect operations to request termination and resize the terminal.

#### Scenario: Kill a running process
- **WHEN** a consumer invokes kill on a running process
- **THEN** one WebContainer process termination request is issued

#### Scenario: Resize a terminal
- **WHEN** a consumer supplies new positive column and row dimensions
- **THEN** the attached WebContainer terminal is resized to those dimensions

#### Scenario: Invalid dimensions
- **WHEN** a consumer supplies nonpositive or non-finite terminal dimensions
- **THEN** resize fails with a typed invalid-input WebContainer error without invoking WebContainer

### Requirement: Preserve WebContainer process semantics
The package SHALL model a WebContainer-native process and SHALL NOT claim to provide separate stdout and stderr, process identifiers, operating-system signals, extra file descriptors, or reference and unreference semantics that WebContainer does not expose.

#### Scenario: Consumer inspects process capabilities
- **WHEN** a consumer uses the public process actor
- **THEN** only combined output, terminal input, exit, kill, and resize capabilities are available
