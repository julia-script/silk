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

- **WHEN** discovery finds a program whose reachable operations use `i32` and `bool`
- **THEN** the plan contains canonical entries for `i32` and `bool` before either type is lowered to MIR

#### Scenario: Ignore an unreachable concrete type

- **WHEN** a source declaration mentions a concrete type that no discovered runtime instance reaches
- **THEN** layout planning does not add that type merely because its declaration was analyzed

### Requirement: Bootstrap scalar layouts are canonical

The layout plan SHALL retain `bool` as the existing four-byte zero-or-one scalar; fixed-width integers SHALL use their named byte width and natural alignment; `usize`/`isize` SHALL use pointer width and alignment; `()`/`never` SHALL have no runtime lane. All supported targets remain little-endian and these are private ABI facts.

#### Scenario: Plan the integer family

- **WHEN** a program reaches every fixed-width integer
- **THEN** layout fixes width, alignment, signedness, and calling lane before backend emission

#### Scenario: Plan unit and bottom

- **WHEN** unit or bottom occurs in control flow
- **THEN** layout assigns no runtime value lane

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

- **WHEN** `Array<i32, 0>` crosses an internal function boundary
- **THEN** its calling shape retains the logical array identity with zero scalar lanes

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

- **WHEN** runtime discovery reaches `Box<i32>` and `Box<Token>`
- **THEN** the selected target plan contains two canonical entries with independently derived concrete layouts

#### Scenario: Omit an open generic layout

- **WHEN** the compiler analyzes `Box<T>` without a concrete runtime instance
- **THEN** no physical layout is invented for the open type

### Requirement: The compiler plans one target-aware slice representation

For every reachable concrete slice element type, target layout SHALL publish one logical slice
entry containing an internal correctly aligned address lane followed by one `i32` length lane,
including exact offsets, padding, total size, alignment, and element stride. Shared and exclusive
slices of the same element type SHALL reuse the same physical representation. The address lane MUST
remain an internal layout scalar and MUST NOT resolve as a safe Silk type.

#### Scenario: Plan native and Wasm slice layouts

- **WHEN** the same `&[i32]` program is planned for a 64-bit native target and a 32-bit Wasm target
- **THEN** both plans retain the same logical slice type while selecting their target address widths and exact resulting layouts before backend emission

#### Scenario: Plan a zero-sized element slice

- **WHEN** a slice element has zero byte size and positive logical length
- **THEN** the plan retains its canonical element alignment, stride, address provenance lane, and independent logical length

### Requirement: Slice calling shapes carry heterogeneous typed lanes

The compiler-owned calling shape for a slice SHALL contain one typed address lane and one typed
`i32` lane in deterministic order. Callers, callees, evaluators, and backends MUST consume that
shape rather than flattening the source array or reconstructing a backend-private slice ABI.

#### Scenario: Preserve one multi-length calling shape

- **WHEN** arrays of different fixed lengths are borrowed for the same slice parameter
- **THEN** both calls use the same two-lane slice calling shape and neither array length expands the callee signature

#### Scenario: Keep native addresses pointer-typed

- **WHEN** a native target uses a pointer width different from `i32`
- **THEN** its slice address lane remains pointer-width and is not narrowed to the source-visible length type

### Requirement: usize layout and calling lanes are compiler-owned target facts

The planner SHALL represent `usize` as unsigned 64-bit on required native targets and unsigned 32-bit on Wasm, validate literals against that width, and require backends to consume the selected lane.

#### Scenario: Plan native usize

- **WHEN** a native signature contains `usize`
- **THEN** the plan publishes one unsigned 64-bit lane before MIR lowering

### Requirement: Typed outcomes have one compiler-owned target shape

For every reachable flow contract, target planning SHALL publish a deterministic private outcome
shape containing a discriminant and payload storage sufficient for the success value or any failure
member. Canonical nominal identity SHALL determine failure tags. Evaluator and backends MUST consume
that shape without independently choosing tags, lanes, or padding.

#### Scenario: Plan mixed success and failure payloads

- **WHEN** a flow returns `usize` and may fail with differently shaped nominal errors
- **THEN** the selected target plan fixes one tag and payload-lane mapping before MIR lowering

### Requirement: Target planning owns allocation and Effect physical facts

Target layout SHALL compute validated byte and repeated-element layouts, raw-buffer lanes, reclaim
ticket shapes, concrete Vector layouts, Effect outcome shapes, and Drop calling shapes before MIR.
Evaluator and backends SHALL consume those facts unchanged and MUST NOT derive stride, alignment,
failure transport, or cleanup representation independently.

Target layout SHALL separately plan each reachable hidden Effect capture environment. Borrowed
captures use target-width provenance-bearing references; Copy and moved captures use their ordinary
value layouts. Effect values and Effect outcomes are distinct physical facts.

#### Scenario: Plan the same Vector for native and Wasm

- **WHEN** `Vector<Token>` is reachable for a 64-bit native target and `wasm32`
- **THEN** each target receives one compiler-owned address-width layout while retaining identical logical ownership and cleanup semantics

### Requirement: Target planning owns allocation and typed-storage shapes

After concrete instance discovery, target layout SHALL plan validated `Layout` values,
repeated-element stride and total bytes, affine allocation handles, private reclaim tickets,
`RawBuffer<T>`, lexical Slot addresses, Drop calling shapes, and typed allocation outcomes using the
selected target's address and `usize` width. Zero-sized allocations SHALL retain distinct logical
ownership without requiring nonzero physical bytes. Evaluator and backends SHALL consume these
facts unchanged and MUST NOT choose stride, alignment, ticket shape, failure transport, or cleanup
representation independently.

#### Scenario: Plan padded elements on two targets

- **WHEN** the same repeated aggregate type is reachable for a 64-bit native target and `wasm32`
- **THEN** each plan uses its selected address width and compiler-derived padded stride while retaining identical logical ownership and Drop order

#### Scenario: Plan zero-sized ownership

- **WHEN** a valid runtime count of a zero-sized element type is allocated
- **THEN** layout records zero physical bytes with the exact logical count and a distinct affine allocation identity

### Requirement: Float layouts and calling lanes are canonical

The target layout plan SHALL represent `f32` as IEEE binary32 with four-byte size/alignment and `f64` as IEEE binary64 with eight-byte size/alignment on every supported target. Backends MUST consume those planned lanes.

#### Scenario: Plan both float widths

- **WHEN** a reachable signature contains `f32` and `f64`
- **THEN** layout publishes both canonical lanes before MIR lowering

### Requirement: Static data placement is compiler-owned target data

Layout planning SHALL retain exact bytes, required alignment, immutable address lane, and target-selected `usize` length lane without publishing an owning String ABI.

#### Scenario: Plan Wasm static text

- **WHEN** a Wasm program reaches a text literal
- **THEN** layout plans one immutable byte region and a 32-bit `usize` length lane

### Requirement: String layout is compiler-owned but source-abstract

Target planning SHALL retain `string` as a canonical logical type and select one deterministic
calling shape and runtime representation for each supported target. Current native and WebAssembly
profiles MAY realize the view as one immutable address-provenance lane followed by one target-sized
byte-length lane, but source MUST NOT observe addresses, lane count, storage identity, padding, or
an ABI promise. Backends MUST consume the selected plan rather than deriving string layout from a
byte-slice rule.

#### Scenario: Plan current target string lanes

- **WHEN** a reachable `string` crosses a function boundary on a current native or Wasm target
- **THEN** the plan retains canonical string identity and selects the target's immutable storage and byte-length lanes deterministically

#### Scenario: Keep representation abstract

- **WHEN** source uses every public `string` operation
- **THEN** no operation can distinguish equivalent target realizations or inspect a backing address

#### Scenario: Separate string and slice shapes

- **WHEN** one program reaches both `string` and `&[u8]` with physically equivalent current lanes
- **THEN** target planning retains two distinct logical types and never makes them interchangeable

### Requirement: Target layout plans the exact combined execution package

Target layout SHALL produce one validated Layout keyed by the target and concrete `A`, `F`, `O`,
and `R` plus the body's normalized suspension summary. The logical contents SHALL cover the owner
record, body environment, exact invoke/drop metadata, endpoint, stable wake-control state when
external parking is reachable, alignment and padding, and any statically selected initial
continuation segment. Physical field order, continuation placement, growth increments, and pooling
SHALL remain target-private. Layout planning SHALL detect size/alignment overflow and SHALL retain
canonical provenance consumed by the initializer.

#### Scenario: Plan a direct explicit body

- **WHEN** an explicit Execution body reaches no suspension and uses a zero-sized no-op endpoint
- **THEN** layout still covers erased-body ownership and invoke/drop metadata but omits continuation and readiness storage

#### Scenario: Plan a nested-only explicit body

- **WHEN** an explicit Execution body reaches nested suspension but not external parking
- **THEN** layout covers the owned package and any statically required initial nested continuation storage without a wake cell

#### Scenario: Plan an external-park body

- **WHEN** an explicit Execution body can reach external parking
- **THEN** layout includes the fixed endpoint and stable wake-control storage in the same package

#### Scenario: Keep physical layout private

- **WHEN** native and Wasm plan the same logical execution specialization
- **THEN** each returns its exact target Layout and common provenance facts without exposing backend field offsets or a stable ABI

#### Scenario: Reject layout overflow

- **WHEN** the complete package size or alignment cannot be represented for the selected target
- **THEN** target layout reports the canonical layout diagnostic and no initializer contract becomes available

### Requirement: Scalar enum layouts reuse validated representation layouts

Target layout planning SHALL give every valid scalar enum the exact size, alignment, and calling
shape of its declared fixed-width integer representation while retaining the enum's canonical nominal
identity in layout facts. Planning SHALL add no hidden metadata and SHALL leave only the dependent
enum layout unavailable when representation or discriminant validation fails.

#### Scenario: Plan default and explicit layouts

- **WHEN** one enum defaults to `u8` and another explicitly selects `i32`
- **THEN** their physical layouts exactly match `u8` and `i32` respectively on every supported target

#### Scenario: Isolate an invalid enum layout

- **WHEN** one enum has an unsupported representation beside a valid enum
- **THEN** only the invalid enum layout is unavailable and the valid enum layout remains complete

### Requirement: Nominal union layout is a compiler-owned tagged payload plan

Every complete non-generic nominal union SHALL receive a target-aware catalog entry before runtime
reachability, including unused private declarations. Every reachable concrete generic application
SHALL receive one specialized entry, while an open generic declaration SHALL receive no speculative
physical layout. Each available entry SHALL contain an inaccessible variant tag, one payload offset,
and a deterministic fixed carrier payload whose slots unify every variant's logical calling lanes.
The carrier SHALL be aligned and sized for all mapped lanes. Unit variants SHALL add no logical
payload lanes. The plan SHALL separately retain every concrete canonical variant payload layout and
SHALL deterministically derive the maximum materialization size and alignment from those layouts.
The plan SHALL retain canonical parent,
variant, field, ordinal, availability, size, alignment, and padding metadata; source semantics SHALL
expose no numeric tag, stable external ABI, or serialization representation.

#### Scenario: Plan mixed unit and payload variants

- **WHEN** a concrete union contains one unit variant and payload variants with distinct sizes and alignments
- **THEN** the layout contains one tag and one correctly aligned fixed carrier region sufficient for every variant with deterministic padding

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
alignment of those complete variant payload layouts for materialization while stored values use the
compiler-owned fixed carrier mapping rather than one variant's raw field offsets. An address-based
operation on the active payload SHALL materialize its fields at the canonical aggregate offsets; a
Drop hook's mutations SHALL be transferred back to the carrier before structural reclamation. Unit
variants SHALL contribute an empty payload layout and SHALL NOT create source-visible fields.

#### Scenario: Lay out a padded multi-field variant

- **WHEN** one variant contains multiple fields whose target alignments require internal and tail padding
- **THEN** its variant plan records the ordinary declaration-ordered field offsets and address-based operations observe that complete aligned layout after active-variant materialization

### Requirement: Nominal union calling shape is compiler-owned target data

For every reachable nominal-union parameter or result, target planning SHALL publish one
backend-neutral tag-plus-payload calling shape and a complete canonical mapping from every variant's
logical field calling shape into fixed payload slots. Construction, calls, returns, matching, and
cleanup SHALL consume that same mapping. Cleanup requiring canonical field addresses SHALL use the
mapping in both directions rather than interpreting carrier offsets as variant field offsets. An
unavailable variant layout or impossible mapping SHALL make the calling shape unavailable before
MIR or backend emission.

#### Scenario: Plan a nominal union call boundary

- **WHEN** a function accepts and returns a union whose variants have different aggregate field shapes
- **THEN** the plan fixes one tag-plus-payload shape and complete per-variant field mappings for both the parameter and result

### Requirement: Union layout recursion follows nominal aggregate rules

Layout dependency analysis SHALL reject every inline recursive cycle through union and struct fields
and SHALL accept a cycle only when an existing explicit finite indirection breaks storage recursion.

#### Scenario: Reject a mixed struct-union cycle

- **WHEN** a struct stores a union inline and one variant stores the struct inline
- **THEN** layout analysis reports the complete canonical cycle and publishes no partial layout for either declaration
