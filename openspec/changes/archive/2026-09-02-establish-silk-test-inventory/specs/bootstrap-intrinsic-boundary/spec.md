## ADDED Requirements

### Requirement: Test privilege is limited to opaque inventory and invocation primitives

The sealed `Intrinsic` namespace SHALL expose only the smallest target-neutral operations needed to
borrow the current compilation's ordered opaque test inventory, read canonical metadata, invoke one
closed handle, and inspect an owned logical path through length and checked frame lookup. A frame
lookup beyond the path length SHALL return checked absence rather than trap, fail, allocate, move,
or mutate the path. The compiler MUST NOT recognize `Test`,
`Reporter`, `AssertionError`, assertion, equality, filtering, presentation, or runner declarations by
spelling. The primitive surface MUST NOT add general erased function pointers, failure-value
rendering, runner policy, filesystem-path policy, service selection, or engine selection.

#### Scenario: Audit the minimal test seam

- **WHEN** the intrinsic catalog is compared with syntax, semantic, HIR, MIR, evaluator, and backend branches
- **THEN** every test-specific branch maps to marker, inventory, metadata, opaque invocation, or logical-path inspection and none names a standard-library actor

#### Scenario: Give same-named source no privilege

- **WHEN** ordinary source defines or renames actors called Test, Reporter, AssertionError, StandardRunner, or equalBytes
- **THEN** every actor retains ordinary source semantics and receives no inventory or invocation privilege by spelling

#### Scenario: Refuse erased callable expansion

- **WHEN** a consumer attempts to convert an opaque test handle to a general function value or invoke an arbitrary source function through the test primitive
- **THEN** the operation is unavailable and the closed per-entry adapter remains the only invocation boundary

#### Scenario: Check an invalid path index

- **WHEN** source requests a logical frame at the path length or a larger index
- **THEN** the sealed inspection operation returns checked absence without changing ownership of the path
