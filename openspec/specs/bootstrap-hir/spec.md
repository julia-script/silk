# bootstrap-hir Specification

## Purpose
The resolved, typed semantic representation of elaborated function bodies: one integrated
elaboration phase that resolves names, types expressions, validates contracts, and constructs HIR
with canonical identities and exact source provenance, published as immutable fact tables with a
deterministic textual encoder.
## Requirements
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

- **WHEN** `pub fn main() -> I32 { return 42 }` is elaborated
- **THEN** the result contains one HIR function whose body is a typed `I32` integer-literal return with exact source provenance and no diagnostics

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
- **THEN** the HIR call references `answer`'s canonical identity and carries the resolved `I32` type

#### Scenario: Keep unknown facts explicit

- **WHEN** a body references an unknown function or an unknown parameter
- **THEN** the corresponding HIR expression is an explicit unavailable state carrying the originating diagnostic's identity, and the enclosing contract or type is not defaulted

#### Scenario: Normalize function contracts

- **WHEN** a declaration has two resolved `I32` parameters and a resolved `I32` return
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
typed argument expressions in order, the resolved `I32` type, and exact source provenance.
Integer-literal expressions SHALL carry signed exact values. An unresolved actor, operation, or
argument SHALL keep the enclosing expression an explicit unavailable state carrying the
originating diagnostic's identity where one exists. The HIR encoder SHALL cover builtin calls and
signed values, gated by committed golden files.

#### Scenario: Elaborate a built-in call

- **WHEN** `pub fn main() -> I32 { return I32.add(40, 2) }` is elaborated
- **THEN** the returned expression is a builtin call with operation `Add`, two typed literal arguments, and type `I32`

#### Scenario: Elaborate a signed literal

- **WHEN** a body returns `-42`
- **THEN** the HIR literal carries the exact value `-42` typed `I32`

#### Scenario: Keep an unknown actor explicit

- **WHEN** a body returns `Math.add(1, 2)`
- **THEN** the HIR expression is an explicit unavailable state carrying the `SEM0009` diagnostic's identity

### Requirement: Conditionals and booleans are typed HIR structure

The semantic type vocabulary SHALL grow to `I32` and `Bool`. HIR SHALL represent `true`/`false`
as boolean literal expressions typed `Bool`, and a conditional statement as a dedicated
statement carrying its typed condition expression, the taken arm's statement sequence, and the
otherwise arm's (possibly empty) statement sequence, each with exact provenance. Arm bindings
SHALL carry function-unique binding identities. An unavailable condition or a damaged arm SHALL
follow the existing explicit-unavailable rules. The encoder SHALL cover conditionals and boolean
literals, gated by committed golden files.

#### Scenario: Elaborate a conditional body

- **WHEN** `pub fn main() -> I32 { if I32.equals(1, 1) { return 1 } return 0 }` is elaborated
- **THEN** the body is one conditional statement whose condition is a typed `Bool` builtin call and whose taken arm returns a literal, followed by the trailing return

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

#### Scenario: Elaborate a selected imported call

- **WHEN** root `main` calls a uniquely selected public `answer()` from module `library/Answer`
- **THEN** HIR contains an ordinary typed call targeting canonical declaration `library/Answer.answer`

#### Scenario: Elaborate a namespace-qualified call

- **WHEN** root imports `library.Answer as Answers` and calls `Answers.answer()`
- **THEN** HIR contains the same canonical call target as the selective form while retaining the qualified call's source span

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
pipeline SHALL produce the same ordinary builtin or declaration call with its left expression
inserted as argument zero. HIR MUST NOT retain a surface operator token, precedence node, pipeline
node, implicit namespace object, or distinct operator-call kind. The resulting operation SHALL
retain the complete surface expression span, and unavailable operator or pipeline facts SHALL
produce an unavailable HIR expression carrying their originating cause. Deterministic HIR encoding
SHALL therefore be independent of whether equivalent behavior was authored with operator,
pipeline, or complete qualified-call syntax except for source provenance.

#### Scenario: Erase infix addition

- **WHEN** a body returns `40 + 2`
- **THEN** HIR contains `BuiltinCall Add` with two typed literal arguments and the infix expression span

#### Scenario: Erase prefix negation

- **WHEN** a body returns `-value`
- **THEN** HIR contains the canonical trapping `Negate` builtin operation over the resolved `I32` value

#### Scenario: Erase a builtin pipeline

- **WHEN** a body returns `2 |> I32.add(3)`
- **THEN** HIR contains the same `BuiltinCall Add` arguments as `I32.add(2, 3)` and no pipeline-specific operation

#### Scenario: Erase an imported pipeline

- **WHEN** a body pipes a value into a resolved public namespace-qualified function
- **THEN** HIR contains one ordinary canonical declaration call with the inserted argument first

#### Scenario: Encode nested operator HIR deterministically

- **WHEN** equivalent grouped and precedence-driven operator programs are elaborated repeatedly
- **THEN** their resolved operation nesting and encodings remain deterministic with exact source provenance


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

- **WHEN** semantic facts accept `[first(), second()]` as `Array<I32, 2>`
- **THEN** HIR retains left-to-right initializer evaluation and one canonical two-element construction

### Requirement: Checked indexing is typed HIR place projection

HIR SHALL represent indexing with its subject expression, canonical array type, index expression,
element result type, access mode, bounds mode, and exact span. Mixed index and field projection chains
SHALL remain nested in source order. A requested non-Copy element move SHALL remain explicit for
ownership to reject.

#### Scenario: Elaborate a dynamic indexed field read

- **WHEN** source reads `pairs[index].left`
- **THEN** HIR contains a checked index place followed by the canonical `Pair.left` Copy read
