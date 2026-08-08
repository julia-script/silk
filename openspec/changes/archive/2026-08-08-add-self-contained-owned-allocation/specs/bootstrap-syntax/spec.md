## ADDED Requirements

### Requirement: Unsafe allocation and Drop forms are explicit and bounded

The lexer and parser SHALL preserve explicit `unsafe { ... }` boundaries containing ordinary
qualified calls to the raw allocation or typed-storage seam, restricted `impl Drop for Name`
declarations, ordinary `impl Allocator for Name` conformance declarations, and consuming
`drop value` statements for early cleanup. These forms SHALL reuse
ordinary qualified calls, type arguments, Effect failure and requirement rows, roles, blocks, and
expression precedence. The grammar MUST NOT introduce a named lifetime scope, allocator-specific
block, or special allocation-call syntax. Missing unsafe delimiters, Drop declaration parts, call
delimiters, type arguments, or drop operands SHALL recover locally and deterministically.

#### Scenario: Preserve an allocating Effect

- **WHEN** source contains `Allocator.allocate(layout)` inside `unsafe { ... }` under an allocator requirement and later `drop allocation`
- **THEN** syntax and canonical formatting retain the unsafe boundary, qualified call, requirement, failure path, drop statement, and every source span

#### Scenario: Preserve a restricted Drop declaration

- **WHEN** a nominal actor declares `impl Drop for Guard { ... }`
- **THEN** the syntax tree distinguishes the restricted conformance from an ordinary interface implementation and retains its target, hook body, and tokens losslessly

#### Scenario: Preserve an allocator conformance

- **WHEN** source declares `impl Allocator for TestAllocator` and maps its operation to the provider actor
- **THEN** the syntax tree retains the capability, nominal provider, operation mapping, and source spans without erasing the provider type

#### Scenario: Recover after a damaged unsafe call

- **WHEN** an unsafe typed-slot operation omits an argument or closing delimiter before a valid following statement
- **THEN** parsing records explicit missing syntax inside the operation and preserves the following statement without inventing scope syntax
