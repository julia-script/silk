## ADDED Requirements

### Requirement: Nominal union layout is a compiler-owned tagged payload plan

Every complete non-generic nominal union SHALL receive a target-aware catalog entry before runtime
reachability, including unused private declarations. Every reachable concrete generic application
SHALL receive one specialized entry, while an open generic declaration SHALL receive no speculative
physical layout. Each available entry SHALL contain an inaccessible variant tag, one payload offset,
and storage aligned and sized for its largest concrete variant payload. Unit variants SHALL require
no payload bytes. The plan SHALL retain canonical parent, variant, field, ordinal, availability,
size, alignment, and padding metadata; source semantics SHALL expose no numeric tag, stable external
ABI, or serialization representation.

#### Scenario: Plan mixed unit and payload variants

- **WHEN** a concrete union contains one unit variant and payload variants with distinct sizes and alignments
- **THEN** the layout contains one tag and one correctly aligned payload region sufficient for every variant with deterministic padding

#### Scenario: Specialize a generic union layout

- **WHEN** `Option<T>` is reachable as `Option<u8>` and `Option<Large>`
- **THEN** layout planning produces separate finite concrete entries from the same canonical variant set and each calling shape consumes its selected entry

#### Scenario: Catalog an unused non-generic union

- **WHEN** a module declares a valid private non-generic union that no runtime instance reaches
- **THEN** the nominal catalog exposes its complete target-aware layout while the runtime plan omits it

#### Scenario: Preserve an unavailable union catalog entry

- **WHEN** one variant field has an unresolved type
- **THEN** the catalog retains the parent entry and originating unavailable cause without publishing a partial tag or payload plan

### Requirement: Each variant payload reuses nominal field layout

Each named-field variant SHALL lay out its specialized fields in declaration order under the same
target-aware offset, alignment, padding, represented-callable, represented-Effect, and unavailable-
dependency rules as a nominal struct. The enclosing union payload region SHALL satisfy the maximum
size and alignment of those complete variant payload layouts. Unit variants SHALL contribute an
empty payload layout and SHALL NOT create source-visible fields.

#### Scenario: Lay out a padded multi-field variant

- **WHEN** one variant contains multiple fields whose target alignments require internal and tail padding
- **THEN** its variant plan records the ordinary declaration-ordered field offsets and the union payload region preserves that complete aligned layout

### Requirement: Nominal union calling shape is compiler-owned target data

For every reachable nominal-union parameter or result, target planning SHALL publish one
backend-neutral tag-plus-payload calling shape and a complete canonical mapping from every variant's
logical field calling shape into fixed payload slots. Construction, calls, returns, matching, and
cleanup SHALL consume that same mapping. An unavailable variant layout or impossible mapping SHALL
make the calling shape unavailable before MIR or backend emission.

#### Scenario: Plan a nominal union call boundary

- **WHEN** a function accepts and returns a union whose variants have different aggregate field shapes
- **THEN** the plan fixes one tag-plus-payload shape and complete per-variant field mappings for both the parameter and result

### Requirement: Union layout recursion follows nominal aggregate rules

Layout dependency analysis SHALL reject every inline recursive cycle through union and struct fields
and SHALL accept a cycle only when an existing explicit finite indirection breaks storage recursion.

#### Scenario: Reject a mixed struct-union cycle

- **WHEN** a struct stores a union inline and one variant stores the struct inline
- **THEN** layout analysis reports the complete canonical cycle and publishes no partial layout for either declaration
