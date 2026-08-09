## ADDED Requirements

### Requirement: Native composite affine returns preserve ownership

LLVM emission SHALL realize every compiler-planned lane of a composite result containing multiple
generic affine fields at both the callee return and caller extraction sites. Native execution SHALL
preserve each field's value and active union discriminants, transfer every cleanup obligation to
the caller exactly once, and agree with MIR evaluation and direct WebAssembly without a
Vector-specific or generic-owner-specific backend branch.

#### Scenario: Return two empty affine owners

- **WHEN** a native callee returns a composite containing two empty generic affine owners
- **THEN** the caller receives both valid empty values and cleans both without a trap or invalid discriminant

#### Scenario: Return two allocated affine owners

- **WHEN** a native callee returns a composite containing two independently allocated generic affine owners
- **THEN** the caller observes both payloads and eventually releases both owners exactly once in declaration order

#### Scenario: Compare composite affine returns across engines

- **WHEN** the same multi-owner return programs run through evaluation, native LLVM, and direct WebAssembly
- **THEN** all three engines produce the same scalar observations and successful cleanup outcome

