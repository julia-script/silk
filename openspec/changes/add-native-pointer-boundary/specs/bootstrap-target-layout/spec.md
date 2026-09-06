## MODIFIED Requirements

### Requirement: Bootstrap scalar layouts are canonical

The selected audited target description SHALL supply primitive physical size/alignment and endianness before layout or ABI classification. The scalar vocabulary SHALL determine language width/category, but SHALL NOT supply a competing physical-layout fallback. `bool` SHALL use the target's four-byte zero-or-one storage; fixed integers and floats SHALL use their audited primitive facts; `usize`/`isize` SHALL use audited pointer facts; `()`/`never` SHALL have no runtime lane. External structs and fixed arrays SHALL derive offsets and stride from those semantic entries. Missing or inconsistent target facts SHALL produce a typed rejection before lowering. LLVM SHALL consume semantic layout rather than infer it from the host.

#### Scenario: Plan the integer family

- **WHEN** a program reaches every fixed-width integer
- **THEN** layout fixes width, alignment, signedness, and calling lane before backend emission

#### Scenario: Plan unit and bottom

- **WHEN** unit or bottom occurs in control flow
- **THEN** layout assigns no runtime value lane
