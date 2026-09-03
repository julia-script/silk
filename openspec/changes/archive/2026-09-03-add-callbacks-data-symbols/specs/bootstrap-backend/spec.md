## ADDED Requirements

### Requirement: Native artifacts lower C callbacks as function addresses

The native backend SHALL represent a C function pointer as one LLVM pointer lane, SHALL use the
generated `export "C"` thunk address for an admitted callback conversion, and SHALL pass that address
unchanged to a foreign call.

#### Scenario: Lower qsort callback invocation

- **WHEN** a native program passes an exported comparator to `qsort`
- **THEN** emitted LLVM calls `qsort` with the comparator thunk address and the linked executable
  sorts through callbacks into Silk

### Requirement: Native artifacts declare imported and exported data symbols

The native backend SHALL declare an imported static as an external LLVM global and SHALL define an
exported static as a C-visible LLVM global with its initializer. Both forms SHALL use the selected
target's exact ABI layout and SHALL appear in deterministic artifact symbol metadata.

#### Scenario: Import environ

- **WHEN** a native executable reaches an imported `environ` static
- **THEN** emitted LLVM contains one external global declaration and loads its value at the read

#### Scenario: Export initialized data

- **WHEN** a native library reaches an exported `u32` static initialized to `1`
- **THEN** emitted LLVM contains one externally visible global definition initialized to `1`
