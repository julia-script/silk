# bootstrap-target-layout Specification

## Purpose

Define canonical bootstrap targets and one deterministic, backend-neutral compiler layout plan that
all later phases and consumers share for reachable concrete Silk types.

## Requirements

### Requirement: Compilation selects one canonical bootstrap target

Each compilation SHALL select exactly one canonical target before runtime instance discovery. The
required native-host targets SHALL be `aarch64-apple-darwin`, `x86_64-unknown-linux-gnu`, and
`aarch64-unknown-linux-gnu`; the supported non-host emission target SHALL be
`wasm32-unknown-unknown`. An explicit request SHALL resolve only to one of these four profiles. A
request without an explicit target SHALL resolve the current native host only when it matches one
of the three required hosts. Unsupported or inconsistent requests SHALL produce a closed typed
compiler outcome before MIR lowering or backend emission.

#### Scenario: Select an explicit Linux target

- **WHEN** a compilation requests `x86_64-unknown-linux-gnu`
- **THEN** the compiler selects that exact canonical profile independently of the host running the compiler

#### Scenario: Default to a supported host

- **WHEN** no target is requested and the compiler runs on `aarch64-apple-darwin`
- **THEN** the compiler selects the canonical `aarch64-apple-darwin` profile

#### Scenario: Select WebAssembly explicitly

- **WHEN** a compilation requests `wasm32-unknown-unknown`
- **THEN** the compiler selects the non-host WebAssembly profile and never treats it as the native host default

#### Scenario: Reject an unsupported target

- **WHEN** a compilation requests a target outside the four supported profiles
- **THEN** compilation returns a typed unsupported-target outcome before layout, MIR, or backend work begins

### Requirement: Layout planning follows concrete instance discovery

The compiler SHALL compute one layout plan after concrete runtime instance discovery and before MIR
lowering. The plan SHALL include every concrete logical runtime type reachable through discovered
function signatures, operations, runtime helpers, and cleanup behavior, and SHALL omit types that
have no concrete runtime instance. Each entry SHALL identify its canonical logical type and record
its concrete size, alignment, and representation facts for the selected target.

#### Scenario: Plan the scalar bootstrap program

- **WHEN** discovery finds a program whose reachable operations use `I32` and `Bool`
- **THEN** the plan contains canonical entries for `I32` and `Bool` before either type is lowered to MIR

#### Scenario: Ignore an unreachable concrete type

- **WHEN** a source declaration mentions a concrete type that no discovered runtime instance reaches
- **THEN** layout planning does not add that type merely because its declaration was analyzed

### Requirement: Bootstrap scalar layouts are canonical

For all four supported targets, the layout plan SHALL represent `I32` as a four-byte,
four-byte-aligned signed integer and `Bool` as a four-byte, four-byte-aligned scalar whose valid
runtime values are zero and one. Pointer-sized layout facts SHALL be eight bytes with eight-byte
alignment for the three native profiles and four bytes with four-byte alignment for
`wasm32-unknown-unknown`. All four profiles are little-endian. These facts are private Silk compiler
ABI decisions and SHALL NOT be advertised as a stable external ABI.

#### Scenario: Plan Bool before LLVM emission

- **WHEN** a reachable program uses `Bool` on any supported bootstrap target
- **THEN** the compiler plan fixes its size, alignment, and zero-or-one representation before the backend runs

#### Scenario: Plan native pointer width consistently

- **WHEN** any required native bootstrap target is selected
- **THEN** its target facts report eight-byte pointers with eight-byte alignment and little-endian byte order

#### Scenario: Plan WebAssembly pointer width consistently

- **WHEN** `wasm32-unknown-unknown` is selected
- **THEN** its target facts report four-byte pointers with four-byte alignment and little-endian byte order

### Requirement: Layout plans are deterministic and backend-neutral

A layout plan SHALL order entries by canonical logical type identity and SHALL encode
deterministically. Identical reachable instances and canonical target inputs SHALL produce
byte-identical plans across fresh processes. A plan MUST NOT contain LLVM types, WebAssembly value
types, backend instructions, backend handles, or backend-specific metadata.

#### Scenario: Repeat layout planning

- **WHEN** identical discovery and target inputs are planned repeatedly in fresh processes
- **THEN** their target facts, ordered entries, and textual encodings are byte-identical

#### Scenario: Keep backend vocabulary out of the plan

- **WHEN** a consumer inspects every layout entry
- **THEN** it finds only canonical Silk type identities and target representation facts, with no backend-owned type or instruction

### Requirement: Nominal layout facts precede runtime reachability

After declaration type dependencies resolve and a target is selected, the compiler SHALL compute
one immutable nominal layout catalog for every concrete non-generic struct in the loaded closure,
including unused private declarations. The catalog SHALL retain available and unavailable entries
under canonical nominal identities so analysis and tooling can inspect physical representation
before instance discovery or backend work. Runtime layout planning SHALL select reachable entries
from this catalog rather than recomputing their fields.

#### Scenario: Inspect an unused struct layout

- **WHEN** a module declares a valid private struct that no runtime instance reaches
- **THEN** the nominal catalog contains its target-aware layout while the runtime layout plan omits it

#### Scenario: Reuse a catalog entry in the runtime plan

- **WHEN** instance discovery reaches a struct already present in the nominal catalog
- **THEN** the runtime plan uses the catalog's identical size, alignment, and offsets without a second layout decision

#### Scenario: Catalog an unavailable declaration

- **WHEN** a struct contains an unknown type or inline recursive dependency
- **THEN** the catalog retains that struct's unavailable layout state and cause while other entries remain available

### Requirement: Nominal struct layouts are target-aware compiler facts

For each concrete nominal struct, its catalog entry SHALL recursively include every field type
needed for its representation and SHALL record the struct's size, alignment, and each field's
physical offset in declaration order. Each field offset SHALL be the smallest offset satisfying its
field alignment after the preceding field; the completed size SHALL include tail padding to the
struct alignment. An empty struct SHALL have size zero and alignment one. These facts SHALL be
selected by the compiler before MIR lowering and MUST NOT be recomputed or changed by a backend.

#### Scenario: Lay out scalar fields with padding

- **WHEN** a reachable struct declares fields whose selected-target alignments require padding
- **THEN** the layout records declaration-ordered offsets, internal padding, maximum field alignment, and tail-padded size

#### Scenario: Lay out a nested struct

- **WHEN** a reachable struct contains another available nominal struct
- **THEN** the plan contains both canonical entries and computes the outer offset and size from the inner compiler-owned layout

#### Scenario: Lay out an empty struct

- **WHEN** an empty marker struct is reachable
- **THEN** its canonical layout entry records size zero, alignment one, and no fields

### Requirement: Struct layout planning is finite and deterministic

Struct layout SHALL follow canonical nominal type dependencies rather than source traversal order.
An unavailable field type or inline-recursive dependency SHALL make only dependent struct layouts
unavailable with their originating causes; unrelated scalar and struct layouts SHALL remain
available. Identical target and declaration inputs SHALL produce byte-identical ordered entries and
field offsets across fresh processes.

#### Scenario: Refuse an inline recursive layout

- **WHEN** a reachable nominal struct participates in a direct or mutual inline dependency cycle
- **THEN** its layout remains unavailable and no placeholder size or backend type is created

#### Scenario: Propagate an unavailable nested layout

- **WHEN** an outer struct contains a struct whose field type is unavailable
- **THEN** the outer layout is unavailable with that dependency cause while unrelated entries remain complete

#### Scenario: Repeat aggregate layout planning

- **WHEN** the same nested nominal types are planned repeatedly for one target
- **THEN** their canonical entry order, sizes, alignments, field offsets, and encoding are byte-identical


### Requirement: Reachable struct values reuse catalog layouts

When runtime discovery reaches a nominal struct, the runtime plan SHALL include the exact available
catalog entry for that struct and recursively required nominal field entries. It MUST NOT recompute,
reorder, resize, or omit fields. An unavailable catalog entry SHALL make the dependent runtime plan
explicitly unavailable before MIR or backend work.

#### Scenario: Select a nested runtime aggregate

- **WHEN** a reachable value has an outer struct containing an inner struct
- **THEN** the runtime plan includes both canonical catalog entries with byte-identical sizes, alignments, offsets, and padding

#### Scenario: Refuse an unavailable runtime aggregate

- **WHEN** a reachable nominal type has an unavailable declaration-wide layout
- **THEN** runtime layout planning stops that value path with the catalog's original cause and creates no placeholder ABI

### Requirement: Aggregate calling shape is compiler-owned target data

For every reachable parameter and result type, target planning SHALL publish a deterministic
backend-neutral calling shape. In this bootstrap slice, a nominal struct SHALL recursively flatten
to its Copy scalar leaf lanes in canonical declaration order; an empty struct SHALL have zero lanes.
The shape SHALL retain each lane's canonical field path and scalar representation. Calls and returns
MUST use that same selected shape in MIR evaluation and every backend.

#### Scenario: Plan a nested struct result

- **WHEN** a reachable function returns a nested struct with three scalar leaves
- **THEN** the selected target plan records three scalar result lanes ordered by canonical nested field path

#### Scenario: Plan an empty marker parameter

- **WHEN** a reachable function accepts an empty struct
- **THEN** its calling shape retains the nominal parameter identity with zero runtime lanes

#### Scenario: Repeat aggregate ABI planning

- **WHEN** identical declarations, discovery, and target inputs are planned in fresh processes
- **THEN** aggregate parameter and result shapes, lane paths, and encodings are byte-identical

### Requirement: Backends cannot choose aggregate ABI independently

The runtime plan SHALL express aggregate call and return shapes without LLVM types, WebAssembly
value types, registers, instructions, or handles. A backend SHALL either realize the selected shape
exactly or reject the plan as target-incompatible; it MUST NOT choose a different flattening,
field order, padding rule, or indirect convention.

#### Scenario: Compare native and WebAssembly planning authority

- **WHEN** native and WebAssembly backends receive plans for the same logical aggregate program
- **THEN** each consumes its compiler-selected target plan and neither derives aggregate calling shape from its own type system

### Requirement: Fixed arrays have compiler-owned repeated-element layout

The target-aware layout phase SHALL compute an array's element stride, total size, alignment, and
index offsets from the selected element layout and canonical length before MIR lowering. Total size
or offset overflow SHALL make the array layout explicitly unavailable. A zero-length array SHALL
have size zero while retaining the element alignment and canonical type.

#### Scenario: Lay out a padded struct array

- **WHEN** `Array<Pair, 3>` uses a selected `Pair` size and alignment
- **THEN** the array layout records three equal element strides and a checked total size derived once by the compiler

### Requirement: Array calling paths use canonical element selectors

Compiler-owned calling shapes SHALL recursively flatten Copy scalar leaves in ascending array-index
order. Each lane path SHALL distinguish canonical field selectors from array-element selectors, so
nested arrays and structs have one unambiguous deterministic path vocabulary. Backends MUST NOT
derive or reorder these paths.

#### Scenario: Flatten an array of structs

- **WHEN** `Array<Pair, 2>` is reachable and `Pair` has two scalar fields
- **THEN** its calling shape contains index-zero fields in declaration order followed by index-one fields in declaration order

#### Scenario: Preserve zero lanes

- **WHEN** `Array<I32, 0>` crosses an internal function boundary
- **THEN** its calling shape retains the logical array identity with zero scalar lanes

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

### Requirement: Layout planning specializes reachable generic types

The target layout catalog and runtime plan SHALL compute physical facts for each reachable concrete
application of a generic nominal type from its normalized substituted fields. Open generic types
MUST NOT receive speculative physical layouts, and equivalent concrete applications SHALL reuse one
canonical layout entry before MIR and backend selection.

#### Scenario: Plan two concrete boxes
- **WHEN** runtime discovery reaches `Box<I32>` and `Box<Token>`
- **THEN** the selected target plan contains two canonical entries with independently derived concrete layouts

#### Scenario: Omit an open generic layout
- **WHEN** the compiler analyzes `Box<T>` without a concrete runtime instance
- **THEN** no physical layout is invented for the open type

### Requirement: The compiler plans one target-aware slice representation

For every reachable concrete slice element type, target layout SHALL publish one logical slice
entry containing an internal correctly aligned address lane followed by one `I32` length lane,
including exact offsets, padding, total size, alignment, and element stride. Shared and exclusive
slices of the same element type SHALL reuse the same physical representation. The address lane MUST
remain an internal layout scalar and MUST NOT resolve as a safe Silk type.

#### Scenario: Plan native and Wasm slice layouts

- **WHEN** the same `&[I32]` program is planned for a 64-bit native target and a 32-bit Wasm target
- **THEN** both plans retain the same logical slice type while selecting their target address widths and exact resulting layouts before backend emission

#### Scenario: Plan a zero-sized element slice

- **WHEN** a slice element has zero byte size and positive logical length
- **THEN** the plan retains its canonical element alignment, stride, address provenance lane, and independent logical length

### Requirement: Slice calling shapes carry heterogeneous typed lanes

The compiler-owned calling shape for a slice SHALL contain one typed address lane and one typed
`I32` lane in deterministic order. Callers, callees, evaluators, and backends MUST consume that
shape rather than flattening the source array or reconstructing a backend-private slice ABI.

#### Scenario: Preserve one multi-length calling shape

- **WHEN** arrays of different fixed lengths are borrowed for the same slice parameter
- **THEN** both calls use the same two-lane slice calling shape and neither array length expands the callee signature

#### Scenario: Keep native addresses pointer-typed

- **WHEN** a native target uses a pointer width different from `I32`
- **THEN** its slice address lane remains pointer-width and is not narrowed to the source-visible length type

### Requirement: Usize layout and calling lanes are compiler-owned target facts

The target-aware layout phase SHALL represent `Usize` as size eight, alignment eight, and one
unsigned 64-bit scalar lane on each native target, and as size four, alignment four, and one unsigned
32-bit scalar lane on `wasm32-unknown-unknown`. It SHALL validate exact literal magnitudes against
that width before MIR lowering. Backends MUST consume the selected layout and calling lane rather
than choosing or narrowing them independently.

#### Scenario: Plan native Usize

- **WHEN** a reachable native signature contains `Usize`
- **THEN** the plan publishes one 64-bit unsigned lane and an eight-byte layout before MIR lowering

#### Scenario: Leave unrelated layouts byte-stable

- **WHEN** a reachable program contains no `Usize`
- **THEN** layout planning does not eagerly add a `Usize` entry or perturb its existing encoding
