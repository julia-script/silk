## MODIFIED Requirements

### Requirement: Availability is checked after executable reachability

Target validation SHALL inspect only intrinsic calls retained after each reachable concrete
application has been statically specialized and the resulting executable closure has been closed.
Merely parsing, loading, importing, indexing, or retaining an uncalled declaration that mentions an
unsupported intrinsic MUST NOT reject an otherwise portable executable. A call in an unselected
`static if` arm MUST NOT enter semantic call facts, the executable closure, target validation, or a
backend inventory.

#### Scenario: Ignore an unreachable target-specific declaration

- **WHEN** the loaded module graph contains a function calling a native-only intrinsic but the function is absent from the executable closure
- **THEN** a direct-WebAssembly request succeeds without a target-unavailable diagnostic

#### Scenario: Ignore an inactive target-specific arm

- **WHEN** a reachable function places a native-only intrinsic in the arm not selected for direct WebAssembly
- **THEN** specialization omits that call and target validation succeeds without retaining native support

#### Scenario: Reject the same declaration when reachable

- **WHEN** the selected static arm and executable closure retain a native-only intrinsic for direct WebAssembly
- **THEN** target validation rejects the concrete residual specialization with the stable intrinsic-availability diagnostic

#### Scenario: Keep residual availability deterministic

- **WHEN** identical source, target, generic arguments, evidence, and static arguments are compiled in fresh processes
- **THEN** the retained intrinsic inventory and target-availability diagnostics are byte-for-byte identical
