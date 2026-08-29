## MODIFIED Requirements

### Requirement: Explicit target and data layout
The system SHALL accept an explicit target triple and LLVM data-layout string, SHALL parse supported layout components exactly, and SHALL expose layout queries for integer, floating-point, vector, pointer, aggregate, size, and alignment properties. Aggregate alignment SHALL follow the pinned LLVM semantics: an absent rule behaves as `a:0:64`; `a:0` has one-byte ABI and preferred alignment; each encoded alignment MUST fit LLVM's unsigned 16-bit field; an explicit preferred alignment MUST be nonzero, power-of-two, and at least the ABI alignment; and the final repeated aggregate rule is authoritative.

#### Scenario: Parse a valid data layout
- **WHEN** a caller supplies a supported LLVM data-layout string
- **THEN** layout queries return the widths, address spaces, ABI alignments, and preferred alignments described by that string

#### Scenario: Preserve an aggregate rule
- **WHEN** a caller supplies `a:<abi>[:<preferred>]`
- **THEN** the parsed layout exposes its effective ABI and preferred alignments and renders the original data-layout bytes exactly

#### Scenario: Resolve repeated aggregate rules
- **WHEN** a supported data-layout string contains more than one aggregate-alignment component
- **THEN** the final component determines the observable aggregate ABI and preferred alignments

#### Scenario: Apply aggregate ABI alignment to structures
- **WHEN** a caller queries a non-empty unpacked anonymous or named structure
- **THEN** its ABI alignment and tail-padded allocation size use the greater of the aggregate ABI minimum and the strongest field ABI alignment

#### Scenario: Preserve zero-sized and unaffected aggregate layouts
- **WHEN** a caller queries an empty unpacked structure under a nonzero aggregate ABI minimum
- **THEN** the structure uses the aggregate ABI alignment and retains a zero allocation size

#### Scenario: Preserve packed structure and array layouts
- **WHEN** a caller queries a packed structure or array
- **THEN** aggregate ABI minimums do not change that type's LLVM allocation size and alignment behavior

#### Scenario: Reject a malformed data layout
- **WHEN** a caller supplies a malformed or unsupported data-layout component, including a zero explicit preferred alignment, a non-power-of-two alignment, a preferred alignment below the ABI alignment, or an alignment outside LLVM's unsigned 16-bit field
- **THEN** parsing and builder creation fail with an `InvalidInput` `LlvmError` identifying the rejected component
