## MODIFIED Requirements

### Requirement: OS intrinsics report low-level outcomes without library values

Unsafe handle-producing open operations SHALL receive exact success and failure `once fn` carriers.
Success SHALL invoke its carrier with one newly initialized affine `OsHandle`; failure SHALL create no
handle, write a stable low-level numeric reason plus optional native `u32` code to initialized scalar
outputs, and invoke its zero-argument failure carrier. Every other fallible OS operation SHALL return
`bool` and write transferred counts, required capacity, reason, or native code to explicit initialized
scalar outputs as its contract requires. The compiler MUST NOT construct or recognize `Option`,
`Path`, `Bytes`, `DirectoryEntry`, `FileError`, or the portable `FileSystem` service. Read and write
SHALL report transferred byte counts and MAY complete partially.

#### Scenario: Report a failed open

- **WHEN** the host refuses a file open
- **THEN** the intrinsic creates no handle, writes the normalized low-level reason and native code, and invokes the failure carrier without constructing a standard-library error

#### Scenario: Transfer a successful open

- **WHEN** a file or directory open succeeds
- **THEN** the intrinsic invokes the success carrier exactly once with the new affine handle and transfers one explicit close obligation

#### Scenario: Report a partial write

- **WHEN** the host accepts fewer bytes than the supplied slice
- **THEN** the write intrinsic returns `true` and writes the exact positive byte count so ordinary source can continue or translate a later failure

### Requirement: Directory iteration is retryable and deterministic at the protocol boundary

Directory-next SHALL return `true` and write `n > 0` for one entry, return `true` and write zero for
end of directory, and return `false` with normalized reason outputs for failure. When the supplied
name buffer is too small, it SHALL report the stable buffer-too-small reason and required capacity
without advancing the iterator. The intrinsic MUST NOT construct an optional carrier, sort entries,
or construct portable paths.

#### Scenario: Retry an oversized directory name

- **WHEN** the next entry does not fit the supplied buffer
- **THEN** the call reports the required capacity, leaves the iterator on the same entry, and a sufficiently sized retry returns that entry

#### Scenario: Reach directory end

- **WHEN** every host entry has been consumed
- **THEN** directory-next returns `true` with a zero count without fabricating an empty-name entry
