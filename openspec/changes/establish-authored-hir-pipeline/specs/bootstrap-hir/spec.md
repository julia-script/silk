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
ordinal. Slicing and concatenation SHALL restrict and rebase segments. Provenance shared between call
sites SHALL parameterize everything that depends on the caller and SHALL keep literal segments
anchored in the callee's own body, which are the same for every caller. At a call, parameter segments
SHALL be substituted with the argument's provenance and literal segments SHALL be preserved. A
location covering several segments SHALL publish the first as its span and the rest as related
spans, each through the presentation of its anchor's own module. Offsets into source spelling MUST
NOT appear in any artifact or outcome; they SHALL be derived at publication from the literal's
presented spelling.

#### Scenario: An escape spans more source than value

- **WHEN** a diagnostic names one decoded byte of `"a\x41z"` produced by the escape `\x41`
- **THEN** the published span covers the four source bytes of the escape

#### Scenario: A substring survives helper calls

- **WHEN** a static text is sliced inside one helper and sliced again inside another before it is rejected
- **THEN** the published span covers exactly the source spelling of the surviving bytes in the caller's literal

#### Scenario: A helper returns its own literal

- **WHEN** a static helper returns a literal written in its body and a caller rejects part of it
- **THEN** the shared outcome keeps the literal's anchor, and the published span lies inside the helper's literal

#### Scenario: A result mixes a callee literal and an argument

- **WHEN** a helper returns `"prefix:" + value` and a caller rejects a range covering the end of the prefix and the start of its argument
- **THEN** the shared outcome holds one literal segment and one parameter segment, the caller substitutes only the parameter segment, and the diagnostic's span lies in the helper's literal with a related span in the caller's argument

#### Scenario: Shared results report per call site

- **WHEN** two calls select the same residual artifact whose body rejects part of its static argument
- **THEN** one diagnostic is published for each call, each inside that call's own argument, including when the two arguments spell the same value differently

### Requirement: Body identity and evaluation identity are separate

A body request SHALL identify which typed body is needed. An evaluation request SHALL identify one
execution: the body that runs, the canonical type substitution and selected evidence of the call
where the body does not already fix them, the canonical value of every parameter and every captured
local, and the compilation and target context the evaluator can observe. Call-site provenance, the
caller's identity and source positions MUST NOT participate in either identity. Outcomes SHALL be
recorded by evaluation identity outside every artifact, and an evaluation MUST NOT require a new
typed body. Re-entering an artifact under construction SHALL be an availability cycle; re-entering a
body under evaluation with a different evaluation identity SHALL be ordinary recursion bounded by the
evaluation budgets; re-entering the same evaluation identity SHALL be rejected as non-terminating.

#### Scenario: Two evaluations share one checked body

- **WHEN** `static fn next(value: i32) -> i32 { return value + 1 }` is evaluated as `next(1)` and `next(2)`
- **THEN** one artifact exists for `next`, two outcomes `2` and `3` are recorded under two evaluation identities, and the artifact is unchanged

#### Scenario: Recursion is not an availability cycle

- **WHEN** a static function calls itself with a smaller argument until a base case
- **THEN** evaluation completes within its budgets and no availability cycle is reported

### Requirement: Artifacts are built privately and published immutable

Elaboration SHALL construct a body through a builder private to that build. Static evaluation SHALL
receive a finished typed node, a read-only view resolving the local definitions, evidence and causes
that nodes reference, an environment of values, and the evaluation session. During construction that
view SHALL expose only completed rows, MUST NOT require the surrounding artifact to be finished, and
MUST NOT permit writing. A call to another static function SHALL request that function's own
published artifact before evaluating it. An artifact SHALL become reusable only after validation and
freezing and MUST NOT be modified afterwards.

#### Scenario: Evaluation does not mutate a published body

- **WHEN** one static function is applied to two different arguments
- **THEN** its checked artifact is byte-identical before and after both evaluations

#### Scenario: A reference resolves during construction

- **WHEN** a `static if` condition calls an interface operation whose evidence row was appended earlier in the same build
- **THEN** the evaluator reads that evidence through the view while the enclosing body is still unfinished

### Requirement: Completed rejections are published and aborted requests are not

A construction that completes with source errors SHALL publish an immutable artifact carrying its
diagnostics, with damaged nodes unavailable and healthy structure preserved, and that artifact MUST
NOT be executed. An evaluation that completes with a semantic rejection — a compile error, a phase
violation, an exhausted deterministic budget or a non-terminating evaluation — SHALL be recorded as
an outcome. A cancelled, interrupted or internally failed construction or evaluation SHALL publish
no artifact, no partial artifact and no outcome. Whether a completed diagnostic-bearing result may
be reused SHALL be an explicit validity policy; this change keeps the existing policy, under which
it is reusable subject to ordinary validation and an owner with parser recovery damage only while
its authored content is byte-identical.

#### Scenario: A rejected body remains inspectable

- **WHEN** a body calls an unknown function beside a valid statement
- **THEN** an artifact is published with the diagnostic, the valid statement's nodes, and an unavailable node for the call, and executable admission rejects it

#### Scenario: An interrupted build leaves nothing

- **WHEN** construction is interrupted before it finishes
- **THEN** no artifact and no outcome exist for the request, and asking again builds it from the start

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
