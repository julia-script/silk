## ADDED Requirements

### Requirement: MIR contains only primitive intrinsic operations

After instance discovery and specialization, MIR SHALL lower source wrappers to ordinary control
and calls, service operations to general witness dispatch, and explicit intrinsic calls to the
smallest backend-neutral primitive operations. MIR MUST NOT contain operations named for
`Allocator`, `SystemAllocator`, `StandardStreams`, `Logger`, `FileSystem`, numeric interfaces, or
other standard-library policy.

#### Scenario: Inspect system allocation MIR

- **WHEN** a source-defined SystemAllocator handles an allocation request
- **THEN** MIR contains ordinary service and source-call structure plus only the primitive storage operation

#### Scenario: Inspect generic addition MIR

- **WHEN** generic integer addition specializes to `i32`
- **THEN** MIR contains the concrete integer primitive and no generic numeric dispatch
