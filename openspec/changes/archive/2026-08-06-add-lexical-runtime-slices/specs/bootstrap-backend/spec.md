## ADDED Requirements

### Requirement: Backends lower compiler-planned slice shapes

Native LLVM and direct WebAssembly emission SHALL consume the target-aware logical slice type,
typed address-and-length calling shape, element stride, loan-validated operations, and structured
control DAG supplied by the compiler. Neither backend MAY specialize a slice-taking function by
source array length, flatten an unknown-length slice into fixed element parameters, or choose an
independent slice ABI.

#### Scenario: Emit one callee for distinct source lengths

- **WHEN** the same shared-slice function is called with two fixed-array lengths
- **THEN** each backend emits one callee symbol with the target-selected address-and-length signature

#### Scenario: Preserve target-specific address lanes

- **WHEN** native and Wasm backends emit the same logical slice program
- **THEN** native uses the planned pointer-width address lane and Wasm uses the planned linear-memory address lane without changing the logical MIR

### Requirement: Address-taken arrays have authoritative contiguous storage

A backend SHALL materialize each address-taken fixed-array root in contiguous storage using the
compiler-planned element layout and SHALL treat that storage as authoritative for the duration of
its loans. Reads after a potentially mutating exclusive-slice call MUST observe storage rather than
stale scalarized values. Arrays never borrowed as slices MAY retain their existing value lowering.

#### Scenario: Reload after exclusive native mutation

- **WHEN** an LLVM caller reads an array in the same block immediately after an exclusive-slice helper returns
- **THEN** the read observes the helper's stored value rather than a pre-call SSA snapshot

#### Scenario: Preserve aggregate stride

- **WHEN** a slice views aggregate elements with target padding
- **THEN** native and Wasm address each logical index using the compiler-planned element stride and field offsets

### Requirement: Wasm frames isolate address-taken locals per invocation

Direct Wasm emission SHALL reserve aligned private linear-memory frame storage for address-taken
fixed arrays on each function invocation, including recursive and nested invocations, and SHALL
restore the previous frame state on every normal structured exit. Frame exhaustion or failed memory
growth SHALL trap deterministically. This private mechanism MUST NOT appear as Silk allocation or an
allocator requirement.

#### Scenario: Keep recursive frames distinct

- **WHEN** nested or recursive Wasm calls each borrow their own local fixed array
- **THEN** every live invocation receives distinct backing storage and slice mutation cannot alias another invocation's local array

#### Scenario: Restore a frame after early return

- **WHEN** a Wasm function with address-taken locals exits through an early structured return
- **THEN** its private frame is restored exactly once before control returns to the caller

### Requirement: Slice backend artifacts remain deterministic

Equivalent slice-bearing programs and target inputs SHALL produce byte-identical native IR,
symbols, object artifacts, Wasm text, Wasm bytes, and private frame layouts across fresh processes.

#### Scenario: Repeat slice emission

- **WHEN** the multi-length and exclusive-mutation fixtures are compiled repeatedly in fresh processes
- **THEN** their native and Wasm artifacts, symbols, layout decisions, and execution results are identical
