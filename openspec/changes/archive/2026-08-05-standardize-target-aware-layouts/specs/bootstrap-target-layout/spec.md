## Purpose

Define canonical bootstrap targets and one deterministic, backend-neutral compiler layout plan that
all later phases and consumers share for reachable concrete Silk types.

## ADDED Requirements

### Requirement: Compilation selects one canonical bootstrap target

Each compilation SHALL select exactly one canonical target before runtime instance discovery. The
required native-host targets SHALL be `aarch64-apple-darwin`, `x86_64-unknown-linux-gnu`, and
`aarch64-unknown-linux-gnu`; the supported non-host emission target SHALL be
`wasm32-unknown-unknown`. An explicit request SHALL resolve only to one of these four profiles. A
request without an explicit target SHALL resolve the current native host only when it matches one
of the three required hosts. Unsupported or inconsistent requests SHALL produce a closed typed
compiler outcome before MIR lowering or backend emission.

#### Scenario: Select an explicit Linux target

- **WHEN** a compilation requests `x86_64-unknown-linux-gnu`
- **THEN** the compiler selects that exact canonical profile independently of the host running the compiler

#### Scenario: Default to a supported host

- **WHEN** no target is requested and the compiler runs on `aarch64-apple-darwin`
- **THEN** the compiler selects the canonical `aarch64-apple-darwin` profile

#### Scenario: Select WebAssembly explicitly

- **WHEN** a compilation requests `wasm32-unknown-unknown`
- **THEN** the compiler selects the non-host WebAssembly profile and never treats it as the native host default

#### Scenario: Reject an unsupported target

- **WHEN** a compilation requests a target outside the four supported profiles
- **THEN** compilation returns a typed unsupported-target outcome before layout, MIR, or backend work begins

### Requirement: Layout planning follows concrete instance discovery

The compiler SHALL compute one layout plan after concrete runtime instance discovery and before MIR
lowering. The plan SHALL include every concrete logical runtime type reachable through discovered
function signatures, operations, runtime helpers, and cleanup behavior, and SHALL omit types that
have no concrete runtime instance. Each entry SHALL identify its canonical logical type and record
its concrete size, alignment, and representation facts for the selected target.

#### Scenario: Plan the scalar bootstrap program

- **WHEN** discovery finds a program whose reachable operations use `I32` and `Bool`
- **THEN** the plan contains canonical entries for `I32` and `Bool` before either type is lowered to MIR

#### Scenario: Ignore an unreachable concrete type

- **WHEN** a source declaration mentions a concrete type that no discovered runtime instance reaches
- **THEN** layout planning does not add that type merely because its declaration was analyzed

### Requirement: Bootstrap scalar layouts are canonical

For all four supported targets, the layout plan SHALL represent `I32` as a four-byte,
four-byte-aligned signed integer and `Bool` as a four-byte, four-byte-aligned scalar whose valid
runtime values are zero and one. Pointer-sized layout facts SHALL be eight bytes with eight-byte
alignment for the three native profiles and four bytes with four-byte alignment for
`wasm32-unknown-unknown`. All four profiles are little-endian. These facts are private Silk compiler
ABI decisions and SHALL NOT be advertised as a stable external ABI.

#### Scenario: Plan Bool before LLVM emission

- **WHEN** a reachable program uses `Bool` on any supported bootstrap target
- **THEN** the compiler plan fixes its size, alignment, and zero-or-one representation before the backend runs

#### Scenario: Plan native pointer width consistently

- **WHEN** any required native bootstrap target is selected
- **THEN** its target facts report eight-byte pointers with eight-byte alignment and little-endian byte order

#### Scenario: Plan WebAssembly pointer width consistently

- **WHEN** `wasm32-unknown-unknown` is selected
- **THEN** its target facts report four-byte pointers with four-byte alignment and little-endian byte order

### Requirement: Layout plans are deterministic and backend-neutral

A layout plan SHALL order entries by canonical logical type identity and SHALL encode
deterministically. Identical reachable instances and canonical target inputs SHALL produce
byte-identical plans across fresh processes. A plan MUST NOT contain LLVM types, WebAssembly value
types, backend instructions, backend handles, or backend-specific metadata.

#### Scenario: Repeat layout planning

- **WHEN** identical discovery and target inputs are planned repeatedly in fresh processes
- **THEN** their target facts, ordered entries, and textual encodings are byte-identical

#### Scenario: Keep backend vocabulary out of the plan

- **WHEN** a consumer inspects every layout entry
- **THEN** it finds only canonical Silk type identities and target representation facts, with no backend-owned type or instruction
