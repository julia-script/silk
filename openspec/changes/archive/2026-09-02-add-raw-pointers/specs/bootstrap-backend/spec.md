## ADDED Requirements

### Requirement: Backends realize raw pointers as one address lane

Native LLVM emission SHALL lower a raw pointer to one LLVM pointer lane, null to the null pointer
constant, formation to the address of the source place's authoritative storage, offset to a typed
element index, and read and write to a load or store of the pointee's lanes. Direct-WebAssembly
emission SHALL lower a raw pointer to one linear-memory address lane with the same operations over
its heap. A place from which a pointer is formed SHALL be materialized in memory for its live range
and reloaded after every call, foreign or Silk, exactly as borrowed roots are today; the
direct-WebAssembly reload reachability SHALL include pointer lanes so a callee writing through a
`*mut` parameter is observed by its caller. Pointer artifacts SHALL be
deterministic and both backends SHALL agree with the evaluator on every program that reaches no
foreign call.

#### Scenario: Reload after a foreign call

- **WHEN** a native program forms a pointer to a local, passes it to a foreign function that writes through it, and reads the local
- **THEN** the emitted code loads the local from its storage after the call and the read observes the write

#### Scenario: Pointer parity without foreign calls

- **WHEN** the pointer corpus program (form, offset, write, read over a local array) runs on the evaluator, LLVM, and direct Wasm
- **THEN** all three report the same exit status
