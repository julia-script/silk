## MODIFIED Requirements

### Requirement: Union layout is a compiler-owned target fact

For every discovered concrete union, the target-aware layout plan SHALL assign a compiler-owned
discriminant representation, canonical tag for every normalized member, payload offset, payload
size and alignment sufficient for the largest member, total size and alignment, and deterministic
padding. Canonical ordinary member identity SHALL determine member/tag order. Exact and opaque
executable members SHALL use their compiler-private finite representation plans rather than a
universal closure ABI. Numeric tags, executable identities, and padding SHALL have no public ABI or
serialization promise, and backends MUST NOT independently choose or reorder them.

#### Scenario: Lay out differently sized members

- **WHEN** a union contains a scalar, fixed array, and nominal struct with different target sizes and alignments
- **THEN** its payload storage fits and aligns the largest requirement and every tag follows canonical member order

#### Scenario: Lay out a represented executable member

- **WHEN** a union contains an exact callable or opaque Effect value with a finite capture environment
- **THEN** its member payload uses that representation's target-aware capture layout without exposing the private executable identity in source types

#### Scenario: Repeat equivalent layout requests

- **WHEN** equivalent permuted and nested union spellings reach layout planning in fresh processes
- **THEN** they produce one byte-identical layout entry with the same tags, payload placement, and padding
