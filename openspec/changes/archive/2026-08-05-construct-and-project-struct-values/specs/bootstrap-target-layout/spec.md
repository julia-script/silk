## ADDED Requirements

### Requirement: Reachable struct values reuse catalog layouts

When runtime discovery reaches a nominal struct, the runtime plan SHALL include the exact available
catalog entry for that struct and recursively required nominal field entries. It MUST NOT recompute,
reorder, resize, or omit fields. An unavailable catalog entry SHALL make the dependent runtime plan
explicitly unavailable before MIR or backend work.

#### Scenario: Select a nested runtime aggregate

- **WHEN** a reachable value has an outer struct containing an inner struct
- **THEN** the runtime plan includes both canonical catalog entries with byte-identical sizes, alignments, offsets, and padding

#### Scenario: Refuse an unavailable runtime aggregate

- **WHEN** a reachable nominal type has an unavailable declaration-wide layout
- **THEN** runtime layout planning stops that value path with the catalog's original cause and creates no placeholder ABI

### Requirement: Aggregate calling shape is compiler-owned target data

For every reachable parameter and result type, target planning SHALL publish a deterministic
backend-neutral calling shape. In this bootstrap slice, a nominal struct SHALL recursively flatten
to its Copy scalar leaf lanes in canonical declaration order; an empty struct SHALL have zero lanes.
The shape SHALL retain each lane's canonical field path and scalar representation. Calls and returns
MUST use that same selected shape in MIR evaluation and every backend.

#### Scenario: Plan a nested struct result

- **WHEN** a reachable function returns a nested struct with three scalar leaves
- **THEN** the selected target plan records three scalar result lanes ordered by canonical nested field path

#### Scenario: Plan an empty marker parameter

- **WHEN** a reachable function accepts an empty struct
- **THEN** its calling shape retains the nominal parameter identity with zero runtime lanes

#### Scenario: Repeat aggregate ABI planning

- **WHEN** identical declarations, discovery, and target inputs are planned in fresh processes
- **THEN** aggregate parameter and result shapes, lane paths, and encodings are byte-identical

### Requirement: Backends cannot choose aggregate ABI independently

The runtime plan SHALL express aggregate call and return shapes without LLVM types, WebAssembly
value types, registers, instructions, or handles. A backend SHALL either realize the selected shape
exactly or reject the plan as target-incompatible; it MUST NOT choose a different flattening,
field order, padding rule, or indirect convention.

#### Scenario: Compare native and WebAssembly planning authority

- **WHEN** native and WebAssembly backends receive plans for the same logical aggregate program
- **THEN** each consumes its compiler-selected target plan and neither derives aggregate calling shape from its own type system
