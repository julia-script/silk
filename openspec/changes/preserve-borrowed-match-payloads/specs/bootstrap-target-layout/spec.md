## MODIFIED Requirements

### Requirement: Nominal union layout is a compiler-owned tagged payload plan

Every complete non-generic nominal union SHALL receive a target-aware catalog entry before runtime
reachability, including unused private declarations. Every reachable concrete generic application
SHALL receive one specialized entry, while an open generic declaration SHALL receive no speculative
physical layout. Each available entry SHALL contain an inaccessible variant tag, one payload offset,
and an aligned payload region sized for the largest canonical variant aggregate layout. Each active
variant SHALL occupy that region using its ordinary field offsets. Unit variants SHALL add no logical
payload lanes. The plan SHALL separately retain the fixed calling-lane mapping and every concrete
canonical variant payload layout, deriving stored payload size and alignment from those layouts.
The plan SHALL retain canonical parent,
variant, field, ordinal, availability, size, alignment, and padding metadata; source semantics SHALL
expose no numeric tag, stable external ABI, or serialization representation.

#### Scenario: Plan mixed unit and payload variants

- **WHEN** a concrete union contains one unit variant and payload variants with distinct sizes and alignments
- **THEN** the layout contains one tag and one correctly aligned payload region sufficient for every canonical variant layout with deterministic padding

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
dependency rules as a nominal struct. The representation plan SHALL derive the maximum size and
alignment of those complete variant payload layouts for stored values. An address-based operation
on the active payload SHALL address its original canonical aggregate fields directly. Mutations
through borrowed fields and Drop hooks SHALL affect the same stored payload, without temporary
materialization or deferred writeback. Unit variants SHALL contribute an empty payload layout and
SHALL NOT create source-visible fields.

#### Scenario: Lay out a padded multi-field variant

- **WHEN** one variant contains multiple fields whose target alignments require internal and tail padding
- **THEN** its variant plan records the ordinary declaration-ordered field offsets and address-based operations observe that complete aligned layout in the original active payload

### Requirement: Nominal union calling shape is compiler-owned target data

For every reachable nominal-union parameter or result, target planning SHALL publish one
backend-neutral tag-plus-payload calling shape and a complete canonical mapping from every variant's
logical field calling shape into fixed payload slots. Construction, calls, returns, matching, and
cleanup SHALL consume that same mapping. Loads and stores SHALL translate between those logical calling lanes and the active variant's
canonical stored field offsets. Borrowing and cleanup SHALL use the original canonical field
addresses; calling-lane slot offsets SHALL NOT be interpreted as stored variant field offsets. An
unavailable variant layout or impossible mapping SHALL make the calling shape unavailable before
MIR or backend emission.

#### Scenario: Plan a nominal union call boundary

- **WHEN** a function accepts and returns a union whose variants have different aggregate field shapes
- **THEN** the plan fixes one tag-plus-payload shape and complete per-variant field mappings for both the parameter and result

#### Scenario: Borrow a narrow aggregate beside a wider alternative

- **WHEN** one variant stores two `i32` fields and another stores two `i64` fields
- **THEN** borrowing the narrow variant observes its ordinary field offsets and writes both original fields even though the unified calling lanes are wider

## ADDED Requirements

### Requirement: Private native aggregate results use caller-owned storage

Native functions SHALL transport recursively growing result representations through caller-owned
result records selected by the compiler's typed-place classification. The record SHALL retain all
source result lanes and owned failure metadata. Synchronous calls SHALL return without an aggregate
SSA payload; suspension steps SHALL return their status separately and receive a fresh result
destination for each invocation. No transient result destination SHALL be retained across a park.
Foreign and machine signatures SHALL remain governed by their own ABI contracts, with private
result adaptation at the boundary.

#### Scenario: An owned aggregate crosses a private call boundary

- **WHEN** a native function returns an owned aggregate through normal or resumed completion
- **THEN** the caller receives every source lane and diagnostic owner through its result record
- **AND** generated private returns do not build a flat aggregate SSA chain for that representation

#### Scenario: A suspended invocation transfers without a completed payload

- **WHEN** a suspension step originates or relays a transfer
- **THEN** its result record contains the defined empty payload and empty diagnostic metadata
- **AND** subsequent resume steps receive fresh invocation-local result storage
