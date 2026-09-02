# bootstrap-entry-termination Delta

## ADDED Requirements

### Requirement: The native entry receives the process command line

The compiler-owned native runtime shim's entry point SHALL receive the process argument count and
argument vector and retain them for the host-input runtime before running the user entry. Silk `main`
SHALL keep its zero-parameter, empty-requirement-row shape in both its effectful and its ordinary
status form: a program SHALL reach its arguments through a service rather than through the entry
signature. Retaining the command line MUST NOT change any termination status, report byte, or
cleanup behavior, and MUST NOT make the entry unavailable for a program that reads no host input.

#### Scenario: Keep a zero-parameter entry

- **WHEN** a program reads its command line through the host-input service
- **THEN** its `main` still declares no parameters and no requirement row, and entry discovery accepts it unchanged

#### Scenario: Preserve every status with arguments present

- **WHEN** a program that reads no host input is run with arguments
- **THEN** it exits `0` on success, `1` with the same report bytes on a reported typed failure, and `2` on a failed standard-error write

#### Scenario: Leave standalone WebAssembly unchanged

- **WHEN** a standalone WebAssembly artifact is produced
- **THEN** it keeps its import-free boundary and its exported `silk_main` contract, with no process-input import and no entry parameters
