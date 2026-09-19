## MODIFIED Requirements

### Requirement: One integrated elaboration phase constructs HIR

Elaboration SHALL consume the authored module with its semantic context, the closure-wide
declaration index and the containing module's completed name-resolution scope, then resolve every
function body in one integrated phase: local bindings, unqualified or namespace-qualified declaration
references, expression typing, and positional contract validation together with typed TIR
construction. It MUST NOT recollect declaration headers, construct import bindings independently, or
read source or syntax. Elaboration SHALL preserve the existing body diagnostics (`SEM0002`,
`SEM0004`, `SEM0006`, `SEM0007`) with their codes, spans, and reasons while adding the stable
name-resolution diagnostics required by imported references; spans are resolved from authored
anchors through the current presentation. It SHALL return complete ordered facts and diagnostics
rather than throw for source mistakes.

#### Scenario: Elaborate the accepted fixture

- **WHEN** `pub fn main() -> i32 { return 42 }` is elaborated
- **THEN** the result contains one TIR function whose body is a typed `i32` integer-literal return with exact authored provenance and no diagnostics

#### Scenario: Elaborate against the published module scope

- **WHEN** a module scope contains a valid selected public function binding used by one body
- **THEN** elaboration resolves that call through the existing binding and does not rebuild the imported module's headers

#### Scenario: Preserve body diagnostics

- **WHEN** a body contains an out-of-range literal, an unknown call target, an unknown parameter reference, and a wrong-arity call across functions
- **THEN** elaboration reports the same stable codes at the same spans as the superseded analysis

## ADDED Requirements

### Requirement: A checked artifact is identified by its owner and semantic request

Each typed body SHALL belong to exactly one artifact identified by its authored owner, its request
(`Check`, or `Specialize` with a canonical application of type arguments, static arguments, selected
evidence and contract row), and for hidden or generated bodies the identity of the artifact that
produced them. Static value provenance MUST NOT participate in identity. A generic body applied to
type arguments with nothing static to select SHALL remain an instance of its checked artifact and
MUST NOT be re-checked. A reference that crosses an artifact boundary SHALL name the artifact and the
artifact-local node.

#### Scenario: Distinct applications are distinct artifacts

- **WHEN** `f<T, static N>` is checked and applied as `f<i32, 4>` and `f<i32, 8>`
- **THEN** three artifacts exist with distinct identities, and an anonymous callable inside `f` yields a distinct hidden artifact under each of them

#### Scenario: Equal static arguments share one artifact

- **WHEN** two call sites apply the same declaration to equal static values written at different positions
- **THEN** both select the same artifact

### Requirement: Reuse separates request identity from validity

A request identity SHALL only locate a candidate artifact. Reuse SHALL additionally require the
candidate's recorded validity to hold against the current index: the owner's canonical semantic
signature, its authored body fingerprint, its scope signature, and every recorded observation
including failed lookups, callee signatures, conformance answers and application outcomes. A valid
candidate SHALL be returned unchanged; no part of it may be copied or rewritten for reuse.

#### Scenario: A reused body is the same object

- **WHEN** a module is re-analyzed after an edit that changes only the position of a declaration
- **THEN** the declaration's artifact is returned by identity and its diagnostics are published at the new position

#### Scenario: A repaired miss invalidates

- **WHEN** a body recorded a failed lookup and the missing member is later declared
- **THEN** the candidate fails validation and the body is rebuilt

### Requirement: Every semantic decision has one authoritative representation

Information that lowering or static evaluation needs to execute a node SHALL be on that node: the
resolved operation and its target identity, type, operands, region, transfer, access, call type
arguments, the selected application, and every implicit conversion as an explicit node. A large
payload SHALL be stored once in a table and referenced from the node. Occurrences, scopes, lifetime
evidence, provenance and diagnostics SHALL be supplementary tables that no consumer reads in order to
execute. No decision may be represented both on a node and in a table.

#### Scenario: A call is self-describing

- **WHEN** lowering reads a call node
- **THEN** its target identity, type arguments and evidence reference are available from the node without consulting an occurrence or resolution index

### Requirement: Static text provenance uses value coordinates

Provenance of a static text or bytes value SHALL be expressed in byte offsets of decoded values, as
ordered segments referring either to a literal by authored anchor or to a static parameter by
ordinal. Slicing and concatenation SHALL restrict and rebase segments. A shared outcome or residual
artifact SHALL refer to its parameters only. At a call, parameter references SHALL be substituted
with the argument's provenance. Offsets into source spelling MUST NOT appear in any artifact; they
SHALL be derived at publication from the literal's presented spelling.

#### Scenario: An escape spans more source than value

- **WHEN** a diagnostic names one decoded byte of `"a\x41z"` produced by the escape `\x41`
- **THEN** the published span covers the four source bytes of the escape

#### Scenario: A substring survives helper calls

- **WHEN** a static text is sliced inside one helper and sliced again inside another before it is rejected
- **THEN** the published span covers exactly the source spelling of the surviving bytes in the caller's literal

#### Scenario: Shared results report per call site

- **WHEN** two calls select the same residual artifact whose body rejects part of its static argument
- **THEN** one diagnostic is published for each call, each inside that call's own argument, including when the two arguments spell the same value differently

### Requirement: Artifacts are built privately and published immutable

Elaboration SHALL construct a body through a builder private to that build. Static evaluation during
construction SHALL consume finished typed nodes, an environment of static bindings and local values,
and an evaluation session, and MUST NOT require the artifact under construction. A call to another
static function SHALL request that function's own artifact. The outcome of an application SHALL be
recorded against that application's artifact identity outside any artifact. An artifact SHALL become
reusable only after validation and freezing, and MUST NOT be modified afterwards; a failed or
interrupted build SHALL publish nothing.

#### Scenario: Evaluation does not mutate a published body

- **WHEN** one static function is applied to two different static arguments
- **THEN** two outcomes are recorded under two artifact identities and the function's checked artifact is unchanged

### Requirement: Occurrences are a supplementary authored index

Each artifact SHALL publish its body-internal semantic occurrences as records holding their own
authored anchor, role, semantic target, the import binding they spend when applicable, and an optional
reference to a primary node. An authored reference SHALL produce exactly one occurrence however many
nodes its desugaring produces, and a reference that produces no executable node SHALL still produce
its occurrence.

#### Scenario: A method call is one occurrence

- **WHEN** `value.map(f)` desugars into a receiver borrow and a call
- **THEN** one occurrence is published at `map` referring to the call node

#### Scenario: A type annotation navigates

- **WHEN** a binding is annotated `let x: Option<i32> = …`
- **THEN** an occurrence is published at `Option` with its declaration as target and no node
