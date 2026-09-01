## ADDED Requirements

### Requirement: Union layout is a compiler-owned target fact

For every discovered concrete union, the target-aware layout plan SHALL assign a compiler-owned
discriminant representation, canonical tag for every member, payload offset, payload size and
alignment sufficient for the largest member, total size and alignment, and deterministic padding.
Canonical nominal identity SHALL determine member/tag order. Numeric tags and padding SHALL have no
public ABI or serialization promise, and backends MUST NOT independently choose or reorder them.

#### Scenario: Lay out differently sized members

- **WHEN** a union contains two nominal structs with different target sizes and alignments
- **THEN** its payload storage fits and aligns the larger requirement and both tags follow canonical identity order

#### Scenario: Repeat equivalent layout requests

- **WHEN** equivalent permuted and nested union spellings reach layout planning in fresh processes
- **THEN** they produce one byte-identical layout entry with the same tags, payload placement, and padding

### Requirement: Union calling shape is fixed by the layout plan

The layout plan SHALL publish one backend-neutral union calling shape containing the discriminant
lane, fixed payload slots, and a complete mapping from each canonical member's logical calling shape
into those slots. Injection and widening SHALL use that same mapping across calls and returns. An
unavailable member layout or impossible mapping SHALL make the union shape unavailable before MIR
or code generation rather than allowing backend-specific fallback.

#### Scenario: Plan a union call boundary

- **WHEN** a function accepts `Token | End` and each member has a different aggregate calling shape
- **THEN** the plan fixes one tag-plus-payload shape and a complete mapping for both members

#### Scenario: Reject an unavailable member layout

- **WHEN** one nominal union member has an invalid recursive inline layout
- **THEN** the union layout names that member dependency and no executable calling shape is produced
