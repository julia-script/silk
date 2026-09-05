## RENAMED Requirements

- FROM: `### Requirement: Ordinary functions may return one-source lexical views`
- TO: `### Requirement: Ordinary functions return declared lifetime-bearing values`

- FROM: `### Requirement: Returned views remain lexical and non-storable`
- TO: `### Requirement: Returned views preserve validity through ordinary storage`

## MODIFIED Requirements

### Requirement: Slice types make borrowed access explicit

Slice types SHALL accept shared `&'a [T]` and exclusive `&'a mut [T]` forms and their elided forms. Semantic identity SHALL retain the lifetime, canonical element type, and access mode without a fixed source-array length. Lazy or capturing functions SHALL admit lifetime-bearing captures only when their environment bounds preserve source validity and access; new borrowed outcomes remain gated until outcome checking is admitted.

#### Scenario: Declare shared and exclusive slice parameters

- **WHEN** ordinary functions declare `values: &[i32]` and `values: &mut [i32]`
- **THEN** both parameter types resolve with canonical element `i32`, distinct access modes, and no fixed length

#### Scenario: Preserve a slice on a lazy function boundary

- **WHEN** a lazy or capturing function form declares or captures a slice
- **THEN** analysis retains its environment lifetime and rejects execution or escape beyond the borrowed storage's validity

#### Scenario: Reject a slice on a lazy function boundary

- **WHEN** a lazy computation retains a slice beyond its source validity
- **THEN** analysis rejects the escape while preserving its environment lifetime witness

### Requirement: Whole-array borrowing is explicit and call-scoped

An explicit `&array` SHALL create a shared slice and `&mut array` an exclusive slice over initialized stable fixed-array storage. Formation SHALL require sufficient source validity for all retained uses, and exclusive formation SHALL require mutable access. Direct bindings, stable projected places, and materialized temporary owners SHALL follow the same lifetime and loan rules. There SHALL be no implicit fixed-array-to-slice conversion. Shared slice locals, ordinary aggregate storage, generic propagation, and declared ordinary-function results SHALL preserve semantic lifetime arguments; views spanning a missing element SHALL be rejected.

#### Scenario: Borrow two array lengths for one function

- **WHEN** one slice-taking function is called as `fold(&short)` and `fold(&long)` for live fixed arrays with different lengths
- **THEN** both explicit borrows satisfy the same slice parameter type without converting either owner

#### Scenario: Reject implicit array decay

- **WHEN** a fixed array is passed directly to a slice parameter without `&` or `&mut`
- **THEN** the call remains incompatible and reports the missing explicit borrow

#### Scenario: Retain a standalone slice binding

- **WHEN** a binding initializer attempts `let view = &values`
- **THEN** analysis admits the local view and retains its source place and lifetime through every dependent use

#### Scenario: Reject a standalone slice binding

- **WHEN** a standalone slice would span a missing array element or outlive its backing storage
- **THEN** analysis rejects the invalid view while retaining its source place and reason

### Requirement: Slice parameters support compatible call-scoped reborrows

A shared slice parameter SHALL be forwardable only as a shared call-scoped reborrow. An exclusive
slice parameter SHALL support shared or exclusive call-scoped reborrowing; the parent exclusive
access SHALL be suspended for the complete nested call and restored only when that call and every retained child dependent have ended. An
exclusive slice MUST NOT be copied or forwarded as an independent alias.

#### Scenario: Reborrow exclusive storage through a helper

- **WHEN** a function holding `values: &mut [i32]` invokes an ordinary helper with a compatible exclusive reborrow
- **THEN** the helper writes the same backing array, the parent slice is inaccessible during the call, and parent access resumes afterward

#### Scenario: Refuse an access-strengthening reborrow

- **WHEN** a shared slice parameter is used where `&mut [T]` is required
- **THEN** analysis rejects the call without producing an exclusive view

### Requirement: Ordinary functions return declared lifetime-bearing values

An ordinary function SHALL return a shared or exclusive reference, slice, or shared borrowed aggregate when its declared lifetime relationships are satisfied by its body and concrete callers. Explicit relationships SHALL support multiple possible sources and independently named stored lifetimes; shared results SHALL derive only shared permission and exclusive results SHALL require exclusive permission. Structural callable and interface operation contracts SHALL carry those same relationships without requiring an exact implementation body at application. Effect success and failure borrows SHALL remain gated until outcome checking is admitted. References into invalid locals, expired temporaries, or by-value inline parameter storage SHALL be rejected; forwarding external data retained by a by-value wrapper SHALL remain valid.

#### Scenario: Return a shared subview

- **WHEN** an ordinary function takes one shared slice parameter and returns a shared subview of it
- **THEN** the caller receives a lexical view whose origin and maximum lifetime are that parameter's source owner

#### Scenario: Reborrow an exclusive parameter as shared

- **WHEN** an ordinary function takes one exclusive slice parameter and returns a shared view of it
- **THEN** the returned shared view is accepted without granting exclusive access

#### Scenario: Reject exclusive strengthening

- **WHEN** an ordinary function takes only a shared parameter and attempts to return an exclusive view
- **THEN** analysis rejects the result because no exclusive origin exists

#### Scenario: Admit declared multiple possible origins

- **WHEN** a returned view may originate from either of two borrowed parameters whose explicit signature shares one result lifetime
- **THEN** analysis accepts the function and requires every possible source to remain valid for the returned view's required uses

#### Scenario: Return a nominal reference through a pipeline

- **WHEN** `&mut counter |> increment` invokes a function whose exclusive reference result derives from that parameter
- **THEN** the pipeline result retains `counter` as its exact source root and the loan remains active through the result's last use

#### Scenario: Return a captured view from an exact callable section

- **WHEN** a known section captures the declaration's one returned-borrow parameter and a later exact application produces the result
- **THEN** the result retains that capture's loan rather than ending it at application

#### Scenario: Use a structural callable lifetime contract

- **WHEN** a structural callable contract declares its borrowed input/output lifetime relationships without an exact function item or section
- **THEN** analysis instantiates the declared relationships against supplied arguments and preserves the callable environment bound without inspecting a body

#### Scenario: Return stored data independently of wrapper storage

- **WHEN** a getter returns an explicitly named data lifetime from a borrowed holder, while another getter returns a reference to the holder's own index
- **THEN** the data view can survive moving or dropping the wrapper while the index reference prevents invalidating its wrapper place

#### Scenario: Reject multiple possible origins

- **WHEN** a result can derive from two independent borrowed inputs but its signature declares only one input as supporting its lifetime
- **THEN** the body fails its declared relationship rather than manufacturing a merged public source contract

#### Scenario: Do not guess through an opaque callable

- **WHEN** a structural callable has an ambiguous undeclared result lifetime and no deterministic elision default
- **THEN** analysis rejects the incomplete contract without inspecting hidden bodies or captures

### Requirement: Returned views preserve validity through ordinary storage

A returned view SHALL be usable in local bindings, compatible reborrows, shared borrowed aggregate storage, generic payloads, and valid callable or Effect captures. Every nested lifetime SHALL remain visible to compatibility, ownership, and escape checking. Retained uses MUST NOT exceed referent validity. Exclusive stored borrows, dependent user Drop, and borrowed Effect success or failure values SHALL remain explicitly gated until their respective proofs are implemented.

#### Scenario: Use and release a returned local view

- **WHEN** a caller binds a returned view, reads it, and makes no later use of the view
- **THEN** the view's live range ends at its last use and the source owner becomes available under the ordinary borrow rules

#### Scenario: Reject escape from the owner

- **WHEN** control could preserve a returned view after its source owner's lexical scope ends
- **THEN** ownership rejects the escape at the boundary that would outlive the owner

#### Scenario: Store a shared returned view

- **WHEN** source attempts to place a shared returned view in a struct field or array element
- **THEN** analysis accepts the lifetime-bearing payload and retains the view's loans through its containing value's uses

#### Scenario: Reject storing a returned view

- **WHEN** storing a view would permit a later use after its source storage becomes invalid
- **THEN** analysis rejects the escape and identifies the retaining aggregate path
