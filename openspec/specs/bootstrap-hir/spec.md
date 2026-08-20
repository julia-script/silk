# bootstrap-hir Specification

## Purpose
The resolved, typed semantic representation of elaborated function bodies: one integrated
elaboration phase that resolves names, types expressions, validates contracts, and constructs HIR
with canonical identities and exact source provenance, published as immutable fact tables with a
deterministic textual encoder.
## Requirements
### Requirement: HIR carries the complete integer vocabulary

HIR SHALL retain canonical lowercase integer identities, unit/bottom control flow, exact literal magnitude, conversion identity, operation mode, evaluation order, and provenance without host-number or backend-lane approximations.

#### Scenario: Encode a wide integer

- **WHEN** accepted `u64` source contains a value above JavaScript's exact integer range
- **THEN** HIR encodes the exact magnitude and canonical `u64` type

#### Scenario: Encode unit return

- **WHEN** a unit function executes bare `return`
- **THEN** HIR records unit completion with no scalar payload

### Requirement: One integrated elaboration phase constructs HIR

Elaboration SHALL consume the closure-wide declaration index and the containing module's completed
name-resolution scope, then resolve every function body in one integrated phase: local bindings,
unqualified or namespace-qualified declaration references, expression typing, and positional
contract validation together with HIR construction. It MUST NOT recollect declaration headers or
construct import bindings independently. Elaboration SHALL preserve the existing body diagnostics
(`SEM0002`, `SEM0004`, `SEM0006`, `SEM0007`) with their codes, spans, and reasons while adding the
stable name-resolution diagnostics required by imported references. It SHALL return complete ordered
facts and diagnostics rather than throw for source mistakes.

#### Scenario: Elaborate the accepted fixture

- **WHEN** `pub fn main() -> i32 { return 42 }` is elaborated
- **THEN** the result contains one HIR function whose body is a typed `i32` integer-literal return with exact source provenance and no diagnostics

#### Scenario: Elaborate against the published module scope

- **WHEN** a module scope contains a valid selected public function binding used by one body
- **THEN** elaboration resolves that call through the existing binding and does not rebuild the imported module's headers

#### Scenario: Preserve body diagnostics

- **WHEN** a body contains an out-of-range literal, an unknown call target, an unknown parameter reference, and a wrong-arity call across functions
- **THEN** elaboration reports the same stable codes at the same spans as the superseded analysis

### Requirement: HIR is resolved, typed, and canonically identified

Every HIR function body SHALL be an ordered statement sequence: zero or more binding statements
followed by one return statement. Every HIR expression SHALL be a core semantic operation —
integer literal, parameter reference, binding reference, move, or call — carrying its resolved
type and exact source provenance. Calls SHALL reference their target's canonical declaration
identity, parameter references their function-local parameter identity, and binding references
and moves their function-local binding identity. Normalized function contracts (ordered
parameter types and result type) SHALL be published per declaration. An unknown fact SHALL
remain an explicit unavailable state carrying the originating diagnostic's identity where one
exists, and MUST NOT masquerade as a valid empty contract, resolved reference, or concrete type.

#### Scenario: Reference a call target canonically

- **WHEN** `main` returns `answer()` and `answer` is a present unique declaration
- **THEN** the HIR call references `answer`'s canonical identity and carries the resolved `i32` type

#### Scenario: Keep unknown facts explicit

- **WHEN** a body references an unknown function or an unknown parameter
- **THEN** the corresponding HIR expression is an explicit unavailable state carrying the originating diagnostic's identity, and the enclosing contract or type is not defaulted

#### Scenario: Normalize function contracts

- **WHEN** a declaration has two resolved `i32` parameters and a resolved `i32` return
- **THEN** its published contract lists both parameter types in order and the result type, while any unresolved header type keeps the whole contract explicitly unavailable

#### Scenario: Elaborate a binding sequence

- **WHEN** a body spells `let value = identity(42) return value`
- **THEN** the HIR body is one binding statement whose initializer is a typed call followed by one return whose expression is a typed binding reference to that binding

#### Scenario: Keep a damaged statement explicit

- **WHEN** one binding statement's initializer contains an unresolved reference
- **THEN** that initializer is an explicit unavailable expression carrying the originating diagnostic's identity while the statement sequence and the other statements' facts remain intact

### Requirement: Elaboration output is deterministic and encodable

Elaboration over the same input SHALL produce identical facts, HIR, and diagnostics across fresh
processes. The HIR SHALL expose a deterministic textual encoder observing the completed artifact;
identical input SHALL produce byte-identical encodings, gated by committed golden files.

#### Scenario: Repeat elaboration

- **WHEN** equivalent modules are elaborated repeatedly in fresh processes
- **THEN** the functions, HIR bodies, contracts, and diagnostics are identical

#### Scenario: Match the HIR golden encoding

- **WHEN** a committed fixture is elaborated and encoded
- **THEN** the encoding equals the committed golden text byte-for-byte, naming every function, contract, typed expression, and unavailable state

### Requirement: Built-in calls are typed HIR operations

HIR SHALL represent a resolved built-in actor call as a dedicated builtin-call expression
carrying the closed operation name (`Add`, `Subtract`, `Multiply`, `Divide`, or `Remainder`), its
typed argument expressions in order, the resolved `i32` type, and exact source provenance.
Integer-literal expressions SHALL carry signed exact values. An unresolved actor, operation, or
argument SHALL keep the enclosing expression an explicit unavailable state carrying the
originating diagnostic's identity where one exists. The HIR encoder SHALL cover builtin calls and
signed values, gated by committed golden files.

#### Scenario: Elaborate a built-in call

- **WHEN** `pub fn main() -> i32 { return i32.add(40, 2) }` is elaborated
- **THEN** the returned expression is a builtin call with operation `Add`, two typed literal arguments, and type `i32`

#### Scenario: Elaborate a signed literal

- **WHEN** a body returns `-42`
- **THEN** the HIR literal carries the exact value `-42` typed `i32`

#### Scenario: Keep an unknown actor explicit

- **WHEN** a body returns `Math.add(1, 2)`
- **THEN** the HIR expression is an explicit unavailable state carrying the `SEM0009` diagnostic's identity

### Requirement: Conditionals and booleans are typed HIR structure

The semantic type vocabulary SHALL grow to `i32` and `bool`. HIR SHALL represent `true`/`false`
as boolean literal expressions typed `bool`, and a conditional statement as a dedicated
statement carrying its typed condition expression, the taken arm's statement sequence, and the
otherwise arm's (possibly empty) statement sequence, each with exact provenance. Arm bindings
SHALL carry function-unique binding identities. An unavailable condition or a damaged arm SHALL
follow the existing explicit-unavailable rules. The encoder SHALL cover conditionals and boolean
literals, gated by committed golden files.

#### Scenario: Elaborate a conditional body

- **WHEN** `pub fn main() -> i32 { if i32.equals(1, 1) { return 1 } return 0 }` is elaborated
- **THEN** the body is one conditional statement whose condition is a typed `bool` builtin call and whose taken arm returns a literal, followed by the trailing return

#### Scenario: Elaborate an else arm

- **WHEN** a conditional carries both arms with early returns
- **THEN** the HIR conditional carries both statement sequences in order

#### Scenario: Give arm bindings unique identities

- **WHEN** an arm declares `let inner = 1` after two body bindings
- **THEN** the arm binding's identity does not collide with any other binding in the function

### Requirement: Cross-module calls are ordinary canonical HIR calls

A call resolved through a selected-member binding or namespace binding SHALL lower into the same HIR
call operation as a local function call, carrying the imported function's indexed canonical
declaration identity, typed ordered arguments, result type, and exact call-site provenance. The HIR
MUST NOT carry import aliases, module traversal state, runtime namespace objects, or a distinct
cross-module call operation. Missing, conflicting, unknown-member, and inaccessible-member lookups
SHALL remain explicit unavailable HIR expressions carrying their originating diagnostic cause.

Every qualifier form that resolves a call SHALL supply that callee's declared parameter types as the
expected types of the call's arguments, including a qualifier that names a nominal struct or
interface acting as its module's actor. An argument whose meaning depends on the expected type — a
borrow above all, which is well-formed only where a reference or slice is wanted — SHALL therefore
elaborate identically under every spelling of one call. A qualified call and the selected-member
call it duplicates SHALL produce identical lowered MIR.

#### Scenario: Elaborate a selected imported call

- **WHEN** root `main` calls a uniquely selected public `answer()` from module `library/Answer`
- **THEN** HIR contains an ordinary typed call targeting canonical declaration `library/Answer.answer`

#### Scenario: Elaborate a namespace-qualified call

- **WHEN** root imports `library.Answer as Answers` and calls `Answers.answer()`
- **THEN** HIR contains the same canonical call target as the selective form while retaining the qualified call's source span

#### Scenario: Pass a borrow to a nominal actor's operation

- **WHEN** a call qualified by a nominal type passes a shared or exclusive borrow to a parameter declared as a reference
- **THEN** the borrow elaborates as it does through the selected-member spelling and the two calls lower to identical MIR

#### Scenario: Keep an invalid borrow position invalid

- **WHEN** a qualified call passes a borrow to a parameter that declares an owned type
- **THEN** the invalid-borrow-position diagnostic is still reported

#### Scenario: Elaborate a private local call

- **WHEN** a public function calls a unique private helper in its own module
- **THEN** HIR resolves the helper's canonical local identity and preserves its private visibility only as declaration metadata

#### Scenario: Keep an inaccessible imported call unavailable

- **WHEN** a qualified call names a private function in another module
- **THEN** HIR contains an unavailable expression caused by the inaccessible-member diagnostic and no call target

#### Scenario: Encode cross-module HIR deterministically

- **WHEN** equivalent cyclic or acyclic closures are elaborated repeatedly in fresh processes
- **THEN** every module's HIR encoding names identical canonical call targets and is byte-identical

### Requirement: Surface operators erase into canonical HIR operations

A resolved prefix, infix, or equality expression SHALL produce the same typed HIR builtin-call
operation and ordered argument expressions as its canonical qualified actor-call form. A resolved
pipeline SHALL produce canonical unary callable application over its elaborated left and right
expressions. HIR MUST NOT retain a surface operator token, precedence node, pipeline node, implicit
namespace object, or distinct pipeline-call kind. Statically erasable sections MAY become direct
canonical calls, while stored or ownership-bearing sections SHALL retain canonical callable
construction and application. Every resulting expression SHALL retain complete source provenance,
and unavailable facts SHALL produce unavailable HIR with their originating cause.

#### Scenario: Erase infix addition

- **WHEN** a body returns `40 + 2`
- **THEN** HIR contains `BuiltinCall Add` with two typed literal arguments and the infix expression span

#### Scenario: Erase prefix negation

- **WHEN** a body returns `-value`
- **THEN** HIR contains the canonical trapping `Negate` builtin operation over the resolved `i32` value

#### Scenario: Erase a direct section pipeline

- **WHEN** a body returns `2 |> i32.add(3)` and the section need not escape
- **THEN** HIR may contain the same `BuiltinCall Add` arguments as `i32.add(2, 3)` while retaining callable facts and pipeline provenance

#### Scenario: Retain a stored callable application

- **WHEN** a body stores `i32.add(3)` and later pipes `2` into that binding
- **THEN** HIR retains canonical callable construction and application rather than inventing a surface pipeline operation

#### Scenario: Encode nested operator HIR deterministically

- **WHEN** equivalent grouped and precedence-driven operator and callable programs are elaborated repeatedly
- **THEN** their resolved operation nesting and encodings remain deterministic with exact source provenance

### Requirement: HIR represents callable values canonically

HIR SHALL represent named function values, automatic sections, callable types and modes, ordered
capture environments, direct or indirect application, and invocation access without backend layout
or surface-syntax lookup. It SHALL retain every ordered remaining leading parameter, every captured
trailing argument's original parameter ordinal, and source evaluation order across successive
direct section stages. Borrowed and owned captures SHALL retain their canonical ownership roots and
dependencies.

#### Scenario: Retain an owned section environment

- **WHEN** a section captures `move token` and crosses a function boundary
- **THEN** HIR carries one canonical take-once environment with the token's ownership transfer

#### Scenario: Retain three-stage application

- **WHEN** `combine(3)(2)(1)` reaches HIR
- **THEN** section construction captures `3` then `2` once and final application supplies `1` plus those positional captures

### Requirement: Struct construction is canonical typed HIR

Elaboration SHALL lower each valid struct literal to one typed HIR construction carrying the
canonical nominal type and one initializer per canonical field in declaration order. Each
initializer SHALL retain its own typed HIR expression and source provenance. HIR MUST NOT retain
source-order lookup decisions or recalculate field completeness.

#### Scenario: Elaborate a reordered literal

- **WHEN** semantic facts accept a literal whose source fields are reordered
- **THEN** HIR contains one nominal construction with typed initializers in canonical declaration order

#### Scenario: Keep invalid construction unavailable

- **WHEN** construction authority, completeness, or a field initializer is unavailable
- **THEN** HIR retains an unavailable expression with the originating cause rather than a partial aggregate

### Requirement: Field reads are canonical typed HIR projections

Elaboration SHALL lower every valid field read to a typed HIR projection carrying its subject
expression, subject nominal type, canonical field identity, result type, access mode, and exact
source span. Nested projections SHALL remain nested in source order. This slice's available access
mode SHALL be a non-consuming read of a Copy scalar field; a requested partial move SHALL remain
unavailable for ownership checking.

#### Scenario: Elaborate a scalar field read

- **WHEN** a valid expression reads `pair.left`
- **THEN** HIR contains a projection from canonical `Pair` through field `left` with its declared scalar result type

#### Scenario: Preserve a partial-move request

- **WHEN** source requests `move outer.inner`
- **THEN** HIR retains the projection and consuming access request with exact provenance for ownership to reject

### Requirement: Nominal values cross ordinary HIR call boundaries

HIR function contracts, parameters, bindings, calls, and returns SHALL retain canonical nominal
types alongside built-in types. A constructed or whole-moved aggregate SHALL be usable as an
ordinary argument or result without a struct-specific call kind or backend representation in HIR.

#### Scenario: Call a public factory

- **WHEN** another module calls `Token.make` and the function returns `Token`
- **THEN** the call and result carry the same canonical nominal type as the defining struct declaration

### Requirement: Arrays are canonical logical HIR values

HIR contracts, parameters, bindings, calls, and results SHALL carry canonical array types containing
their logical element type and length. A complete literal SHALL lower to one typed construction with
element expressions in ascending index order and exact source evaluation provenance.

#### Scenario: Elaborate a complete literal

- **WHEN** semantic facts accept `[first(), second()]` as `Array<i32, 2>`
- **THEN** HIR retains left-to-right initializer evaluation and one canonical two-element construction

### Requirement: Checked indexing is typed HIR place projection

HIR SHALL represent indexing with its subject expression, canonical array type, index expression,
element result type, access mode, bounds mode, and exact span. Mixed index and field projection chains
SHALL remain nested in source order. A requested non-Copy element move SHALL remain explicit for
ownership to reject.

#### Scenario: Elaborate a dynamic indexed field read

- **WHEN** source reads `pairs[index].left`
- **THEN** HIR contains a checked index place followed by the canonical `Pair.left` Copy read

### Requirement: HIR represents typed mutation explicitly

HIR SHALL distinguish immutable and mutable bindings and SHALL represent each accepted assignment as
one typed write to an ordered binding/field/index place with exact root identity, selector types,
right-hand value, replacement mode, and provenance. It MUST NOT desugar a write into a fabricated
setter call or a partial aggregate value.

#### Scenario: Elaborate an indexed write

- **WHEN** semantic facts accept `values[index] = next`
- **THEN** HIR contains one checked typed write rooted at the mutable array owner

### Requirement: HIR control structure is an acyclic region graph

HIR SHALL represent `while` as one structured loop region containing its condition and body regions,
and SHALL represent `break` and `continue` as lexical region outcomes targeting the canonical
enclosing loop. Child, sequencing, and continuation relationships SHALL form a DAG; repetition is a
property of the loop region and MUST NOT appear as a cyclic HIR edge.

#### Scenario: Elaborate a nested loop DAG

- **WHEN** a function contains nested loops, conditionals, `break`, and `continue`
- **THEN** HIR retains canonical nested regions and lexical outcomes in an acyclic deterministic traversal order

### Requirement: HIR represents canonical union conversion explicitly

HIR SHALL carry normalized union types as canonical nominal member sets and represent each accepted
injection or widening as one typed conversion around its source expression. The conversion SHALL
carry the exact source type, target union, canonical total member mapping, access mode, and
provenance. It MUST NOT encode numeric runtime tags, backend storage, pattern narrowing, or cyclic
control edges.

#### Scenario: Elaborate a nominal injection

- **WHEN** a `Token` expression enters a declared `Token | End` return context
- **THEN** HIR contains one conversion from precise `Token` to the canonical two-member union

#### Scenario: Elaborate union widening inside a loop

- **WHEN** a mutable `Token | End` binding is assigned into a `Token | End | Fault` destination inside a loop
- **THEN** the write source contains one canonical widening operation and the surrounding HIR region graph remains acyclic

### Requirement: HIR represents matching as an acyclic typed region

HIR SHALL represent one match as a scrutinee evaluated once, its logical access mode and type, and
source-ordered arm regions. Each executable arm SHALL carry its canonical member or universal
coverage, narrowed payload, pattern bindings, optional typed guard, result expression, cleanup
boundary, and join result type. Child, guard, arm, cleanup, and continuation relationships SHALL
remain acyclic and MUST NOT contain physical tags, backend blocks, branch depths, or reconstructed
cyclic control.

#### Scenario: Elaborate a guarded union match

- **WHEN** a shared match has a guarded `Token` arm, an unguarded `Token` arm, and an `End` arm
- **THEN** HIR retains three ordered arm regions over canonical members and one acyclic result join

#### Scenario: Elaborate consuming destructuring

- **WHEN** a consuming arm binds one field and acknowledges omitted fields
- **THEN** HIR carries the complete narrowed payload, bound field access, omitted-field cleanup boundary, and arm result provenance

### Requirement: HIR is generic-aware before specialization

HIR SHALL retain canonical type parameters in generic declarations and explicit generic-call
operations carrying normalized type arguments or an unavailable specialization cause. HIR MUST NOT
clone a declaration body per concrete call, and its deterministic encoding SHALL preserve the link
from every call to its generic declaration and substitution.

#### Scenario: Keep one generic body
- **WHEN** one generic function is called with `i32` and `Token`
- **THEN** HIR contains one checked declaration body and two calls with distinct concrete substitutions

### Requirement: HIR retains lexical slice semantics explicitly

HIR SHALL represent each available slice type, borrow or reborrow, loan identity, access mode,
backing-place provenance, runtime length projection, and borrowed indexed place without encoding a
raw address. Each borrow SHALL carry a named, parameter, pattern, or compiler-owned temporary root
plus the complete ordered field and checked-index selector path. Expression and place traversal
SHALL preserve source evaluation order and exact spans, including hidden temporary expressions and
runtime selector expressions exactly once. Unavailable slice facts MUST NOT become typed HIR
operations.

#### Scenario: Retain a shared whole-array borrow

- **WHEN** semantic analysis accepts `fold(&values)`
- **THEN** HIR records a shared slice formation tied to the array root and call region before the call argument

#### Scenario: Retain exclusive indexed replacement order

- **WHEN** an exclusive slice assignment has a dynamic index and an effectful replacement expression
- **THEN** HIR orders source-root resolution, index evaluation, runtime bounds validation, replacement evaluation, old-value cleanup, and committed write exactly once

#### Scenario: Omit an unavailable borrow operation

- **WHEN** borrow analysis lacks a stable source root or compatible slice destination
- **THEN** HIR preserves the diagnostic cause through surrounding unavailable facts and emits no executable borrow node

#### Scenario: Retain an indexed inner-array borrow

- **WHEN** HIR lowers `&mut matrix[index]`
- **THEN** it records `matrix` as the root and the checked runtime array selector without copying the inner array

### Requirement: HIR retains exact usize operations

HIR SHALL retain exact `usize` literal magnitude, canonical operand and result types, unsigned
operator identity, source evaluation order, and provenance. HIR MUST NOT contain a selected LLVM
integer type, Wasm value type, host-number approximation, or backend instruction.

#### Scenario: Inspect a large literal operation

- **WHEN** an accepted native function adds two `usize` literals above the 32-bit range
- **THEN** HIR encoding shows both exact values and one canonical checked unsigned addition

### Requirement: HIR retains explicit flow and typed-failure semantics

HIR SHALL retain flow construction, capture access, one-layer run, failure origin, propagation, and
exact-member catch with normalized contracts and exact provenance. It MUST NOT contain exception,
unwinding, LLVM, Wasm, or runtime type-lookup vocabulary.

#### Scenario: Inspect a recovered flow

- **WHEN** a statically known handler recovers one nominal member
- **THEN** HIR shows the protected and handler targets, selected canonical member, residual row, and one run

### Requirement: HIR retains Effect allocation and Drop semantics

HIR SHALL retain effect construction, capture mode, one-layer execution, retry boundaries, provider
capture or per-run acquisition, typed failure, validated allocation, raw-buffer initializedness,
explicit drop, and automatic Drop. It MUST NOT contain named scopes, allocator implementation kinds,
LLVM types, Wasm values, or dynamic finalizer records.

Each Effect construction SHALL retain its hidden source-site identity and ordered capture fields.
Calls and returns MUST preserve that identity rather than reducing the value to an Effect outcome.

#### Scenario: Inspect Vector growth rollback

- **WHEN** a generic Vector append may fail while moving initialized elements to a replacement buffer
- **THEN** HIR retains allocation, prefix ownership, commit, rollback, and cleanup provenance without a collection-shaped compiler intrinsic

### Requirement: HIR retains allocation and cleanup semantics without policy

HIR SHALL represent validated and repeated layout formation, general allocator capability dispatch,
typed allocation success or `OutOfMemoryError`, self-contained allocation ownership, unsafe RawBuffer and
Slot operations, initialization transitions, restricted Drop declarations and calls, explicit drop,
and automatic cleanup with canonical types and source provenance. HIR MUST NOT encode allocator
implementation kinds, provider-dependent result lifetimes, named lifetime scopes, dynamic finalizer
registries, host addresses, or backend heap policy.

#### Scenario: Elaborate a raw construction guard

- **WHEN** unsafe source allocates repeated storage and initializes a runtime prefix under a Drop guard
- **THEN** HIR retains one typed buffer identity, checked slot projections, prefix updates, hook identity, and typed failure branch in source order

#### Scenario: Keep SystemAllocator ordinary

- **WHEN** a call resolves through a `SystemAllocator` conformance witness
- **THEN** HIR records the general capability dispatch and concrete witness identity without a system-allocator operation tag

### Requirement: HIR carries canonical floats

HIR SHALL retain selected float width, correctly rounded constant bits, operation or conversion identity, evaluation order, and provenance without backend instructions or uncontrolled host coercion.

#### Scenario: Inspect an f32 literal

- **WHEN** a decimal literal is contextually typed `f32`
- **THEN** HIR encoding carries its canonical binary32 bits and source span

### Requirement: HIR carries static data identities and views

HIR SHALL retain decoded bytes, UTF-8 validity, `usize` length, canonical static identity, logical immutable view, and provenance without target placement or owning String behavior.

#### Scenario: Elaborate static UTF-8

- **WHEN** a valid text literal is accepted
- **THEN** HIR encodes exact bytes and one immutable static view

### Requirement: HIR separates intrinsic, source, and service operations

HIR SHALL represent an explicit intrinsic call with its catalog identity and concrete contract, an
ordinary source call with its canonical declaration identity, and a service operation with its
service and witness obligations. HIR MUST NOT encode standard-library actor names, provider kinds,
or wrapper-specific operation tags as primitive behavior.

#### Scenario: Elaborate a generic numeric wrapper

- **WHEN** a source numeric wrapper calls an interface operation mapped to a concrete intrinsic
- **THEN** HIR retains the source call and conformance before specialization and the explicit intrinsic at its primitive boundary

#### Scenario: Elaborate a source service call

- **WHEN** source calls an operation on a declared service
- **THEN** HIR records general service dispatch without a service-specific operation tag

### Requirement: HIR preserves first-class string identity

HIR SHALL represent text literals and all subsequently typed string expressions with canonical
`string` type identity, exact storage provenance, and any lexical loan required by borrowed backing
storage. It MUST NOT encode `string` as a shared `u8` slice or infer byte indexing from its physical
representation.

#### Scenario: Carry a borrowed owned-string view

- **WHEN** elaboration accepts a stdlib view operation over a shared `String` borrow
- **THEN** HIR records a `string` result tied to that borrow rather than an unrelated slice or owner

#### Scenario: Keep literal and runtime views type-identical

- **WHEN** one function accepts both a static text literal and a validated runtime UTF-8 view
- **THEN** HIR gives both arguments canonical `string` type while retaining their distinct storage provenance

### Requirement: Generic HIR carries symbolic row contracts and proof evidence

Generic HIR SHALL retain symbolic row expressions, lifted member terms,
member-well-formedness obligations, callable constraints, substitutions, and `Assumed` evidence.
Requirement-binding HIR SHALL store provider-selection access separately from expression capture
access and SHALL identify the exact solved wanted without requiring concrete capability, role,
provider match, or witness fields before specialization.

A specialized branded binding SHALL contain concrete `RequirementSelection` evidence. HIR encoding,
equality, keys, copying, and dependency analysis SHALL traverse symbolic rows and evidence
deterministically.

#### Scenario: Represent a generic wrapper binding

- **WHEN** a generic wrapper calls a binding intrinsic under a definitionally equivalent declared constraint
- **THEN** HIR contains the symbolic result row and assumed proof without choosing a concrete row member

#### Scenario: Separate selection and capture access

- **WHEN** an exclusive provider satisfies a stored shared requirement
- **THEN** HIR records selected stored access as shared and provider expression capture access as exclusive

#### Scenario: Reject symbolic proof from concrete HIR consumers

- **WHEN** a row-dependent HIR consumer is given an unupgraded assumed proof
- **THEN** the required concrete specialized bundle cannot be constructed

### Requirement: Executable HIR requires a proven return contract

HIR construction MAY retain typed or explicitly unavailable body structure for inspection, but a
function body SHALL be executable only when semantic analysis has proven every reachable return and
fallthrough path against the resolved result contract. Source mistakes SHALL remain semantic
diagnostics and the function MUST be unavailable to reachability and target-dependent phases.

#### Scenario: Keep an invalid body out of executable HIR

- **WHEN** a declaration returns a value incompatible with its resolved result type
- **THEN** its semantic facts retain the source diagnostic, any retained HIR return is explicitly unavailable, and target-dependent phases cannot consume the body

### Requirement: HIR retains closed finite Effect alternatives

HIR SHALL represent an admitted Effect join as one closed finite composite that names every exact
construction alternative, its normalized public contract, capture access, and ownership facts. The
representation and its textual encoding SHALL be deterministic and SHALL NOT erase the alternatives
to a universal runtime Effect identity.

#### Scenario: Encode two construction alternatives

- **WHEN** control flow joins two compatible Effects constructed at distinct source sites
- **THEN** HIR records both exact alternatives in canonical order under one normalized Effect contract

#### Scenario: Retain the selected capture contract

- **WHEN** alternatives capture different values with compatible run access and ownership
- **THEN** HIR preserves enough information to construct, run, and clean only the selected alternative
