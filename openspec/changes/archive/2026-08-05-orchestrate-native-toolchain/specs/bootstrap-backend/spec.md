## MODIFIED Requirements

### Requirement: The Backend service is a nominal contract

The `Backend` service SHALL expose one emission operation consuming the whole monomorphized MIR
program, the explicit target layout, and a codegen request (debug or release), producing one
program artifact. Consumers MUST NOT inspect backend identity: one compilation request produces
one MIR program, one backend module, and one artifact — source modules are semantic namespaces,
not codegen units. The relocatable-object half of the contract SHALL be fulfilled by the pinned
native toolchain orchestration, which turns the artifact's bitcode into one target object under
a fixed optimization profile.

#### Scenario: Emit one artifact per program

- **WHEN** a lowered program with several functions is emitted through the service
- **THEN** exactly one artifact results, containing every function's symbol, regardless of which source modules the instances came from

#### Scenario: Complete the object contract

- **WHEN** the artifact's bitcode is passed through the pinned toolchain's object emission
- **THEN** one relocatable object for the requested profile results, completing the backend contract for the compilation
