## ADDED Requirements

### Requirement: Entrypoints use the confirmed public shapes and statuses

The compiler SHALL accept public ordinary `main() -> ()`, public ordinary `main() -> i32`, and public effectful `main() -> () ! E ? never`. It SHALL map unit and Effect success to status zero, every unhandled typed failure to status one, and only ordinary `i32` success to its returned custom status.

#### Scenario: Run an ordinary unit entry

- **WHEN** a public ordinary `main` explicitly returns `()`
- **THEN** the program is a valid entry and terminates successfully with status zero

#### Scenario: Diagnose a private entry accurately

- **WHEN** a correctly typed `main` is not public
- **THEN** entry discovery reports the visibility error rather than claiming its return type is unresolved

#### Scenario: Reject unresolved entry dependencies

- **WHEN** an effectful `main` retains one or more requirement keys
- **THEN** compilation lists those unresolved dependencies and does not begin execution

### Requirement: Termination is structured target-neutral data

Success, unhandled typed failure, and fatal trap SHALL produce one target-neutral outcome carrying classification, failure identity or trap reason, source provenance, stable logical call path, and causal recovery history where present. No `Report` conformance SHALL be required. Standalone adapters MAY render this data; embedding hosts SHALL receive it without ambient output.

#### Scenario: Preserve a failure trace through suspension

- **WHEN** an unhandled typed failure crosses suspended logical invocations in an optimized build
- **THEN** evaluator, native, and Wasm outcomes retain the stable logical path rather than compiler driver frames

#### Scenario: Keep traps outside typed recovery

- **WHEN** execution reaches a fatal trap
- **THEN** the outcome is classified as a trap, `Effect.catch` cannot recover it, and no successful source cleanup is claimed

#### Scenario: Keep a trivial adapter minimal

- **WHEN** a closed trivial entry reaches no reporting output or runtime service
- **THEN** its artifact contains no console, scheduler, allocator, provider-container, or unrelated adapter machinery
