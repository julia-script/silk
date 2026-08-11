## ADDED Requirements

### Requirement: String layout is compiler-owned but source-abstract

Target planning SHALL retain `string` as a canonical logical type and select one deterministic
calling shape and runtime representation for each supported target. Current native and WebAssembly
profiles MAY realize the view as one immutable address-provenance lane followed by one target-sized
byte-length lane, but source MUST NOT observe addresses, lane count, storage identity, padding, or
an ABI promise. Backends MUST consume the selected plan rather than deriving string layout from a
byte-slice rule.

#### Scenario: Plan current target string lanes

- **WHEN** a reachable `string` crosses a function boundary on a current native or Wasm target
- **THEN** the plan retains canonical string identity and selects the target's immutable storage and byte-length lanes deterministically

#### Scenario: Keep representation abstract

- **WHEN** source uses every public `string` operation
- **THEN** no operation can distinguish equivalent target realizations or inspect a backing address

#### Scenario: Separate string and slice shapes

- **WHEN** one program reaches both `string` and `&[u8]` with physically equivalent current lanes
- **THEN** target planning retains two distinct logical types and never makes them interchangeable
