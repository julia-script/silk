## Purpose

Define an explicit, deterministic compile-time execution phase that specializes ordinary Silk
functions into target-neutral runtime programs without macros, hidden phase inference, or a second
observable memory model.

## ADDED Requirements

### Requirement: Static function and binding modes are explicit

A `static fn` SHALL execute only during static evaluation, SHALL accept only static arguments, and
SHALL produce one static result or one static diagnostic. It MUST NOT have a runtime calling shape,
runtime instance, callable representation, or backend body. An ordinary function parameter prefixed
by `static` SHALL be a static specialization input and MUST NOT occupy a runtime parameter lane. A
local `let static` binding SHALL evaluate its initializer statically and retain the resulting value
for later static use. An ordinary local binding SHALL remain runtime even when initialized by a
literal.

A literal supplied directly where a static parameter, condition, initializer, or compile-error
message is required SHALL satisfy that static context without a call-site `static` prefix. Any
non-literal expression supplied to a static context SHALL be accepted only when all of its
dependencies and operations are available to static evaluation.

#### Scenario: Specialize an ordinary function with one static parameter

- **WHEN** an ordinary function accepts `static template: string` and one runtime argument and a caller supplies a text literal plus a runtime value
- **THEN** the literal participates in the specialization identity, the runtime value remains a runtime parameter, and the emitted calling shape contains no lane for `template`

#### Scenario: Retain a value with an explicit static local

- **WHEN** a body initializes `let static template` from a statically evaluable expression and later passes it to a static parameter
- **THEN** the binding is accepted and the call receives the retained static value

#### Scenario: Refuse an ordinary local at a static boundary

- **WHEN** an ordinary `let template` binding is passed to a static parameter even though its initializer was a literal
- **THEN** analysis reports that the argument is runtime and identifies the static parameter that requires a compile-time value

#### Scenario: Omit a static function from runtime artifacts

- **WHEN** a reachable specialization calls a static function and consumes its result while generating runtime code
- **THEN** every backend receives only the generated runtime operations and no function, symbol, table entry, or calling lane for the static function

### Requirement: Static evaluation produces one residual runtime program

`static if` SHALL require a statically evaluated `bool` condition. Both arms SHALL be parsed with
ordinary recovery, but only the selected arm SHALL undergo name resolution, type elaboration,
ownership-producing residualization, and call discovery for that specialization. The unselected arm
MUST NOT contribute types, Effects, requirements, ownership obligations, target availability,
runtime reachability, or backend operations. A `static if` without an `else` SHALL contribute no
operation when its condition is false.

The selected arm MAY contain ordinary runtime operations and runtime values. Such operations SHALL
be retained rather than executed by the static evaluator. Static selection SHALL be controlled only
by explicitly static constructs; an ordinary runtime `return`, branch, or loop MUST NOT decide
which later source is statically elaborated. `static if` SHALL be permitted only in executable
statement or expression positions and MUST NOT conditionally introduce declarations.

#### Scenario: Retain runtime work from a selected static arm

- **WHEN** a target-selected arm logs through an ordinary runtime operation and returns one runtime value
- **THEN** specialization retains the log and return in the residual program without executing either during compilation

#### Scenario: Ignore an invalid name in an inactive arm

- **WHEN** the unselected arm of a syntactically valid `static if` refers to a declaration unavailable for the selected target
- **THEN** that reference receives no name or type diagnostic and contributes no semantic or runtime fact

#### Scenario: Preserve syntax diagnostics in every arm

- **WHEN** an unselected arm contains malformed syntax
- **THEN** parsing reports the ordinary syntax diagnostic because static selection does not suppress parsing or recovery

#### Scenario: Refuse a conditional declaration

- **WHEN** source places `static if` where a module or block declaration is expected
- **THEN** parsing rejects the form without creating a target-dependent declaration surface

#### Scenario: Analyze returns after specialization

- **WHEN** an ordinary function returning `i32` has one selected arm that returns `32` and another unselected arm with a different return path
- **THEN** return analysis judges only the selected residual control flow for that specialization

### Requirement: Static functions use value semantics without static borrowing

Every static value SHALL be finite, deterministic, identity-free, and freely reusable by static
evaluation. Reuse MUST NOT establish or imply the runtime `Copy` property for its declared type and
MUST NOT affect interface selection that depends on runtime `Copy`. Static evaluation SHALL admit
scalars, scalar enums, static text, and recursively pure aggregate values whose members are
admissible and whose types carry no runtime resource, borrow, opaque identity, callable execution,
Effect, service, unsafe pointer, or observable cleanup behavior.

A static function MAY use ordinary conditions, loops, and complete `let mut` binding replacement.
Replacement SHALL install a new complete static value and SHALL NOT expose an address, reference,
exclusive loan, in-place aggregate mutation, destructor, or allocator identity. A static function
MAY call another static function but MUST NOT invoke an ordinary runtime function, construct or run
an Effect, access a runtime binding, perform unsafe or external operations, or retain compiler
storage in a residual value.

The ordinary `silk.static_text` source actor SHALL expose static functions for UTF-8 byte length,
byte indexing, and boundary-checked byte slicing. Those wrappers MAY use sealed static-only
intrinsics as their minimal primitive boundary, but the compiler MUST NOT recognize the source
actor or its declarations by spelling. Static text operations SHALL return ordinary admitted
values and SHALL NOT expose compiler storage, addresses, or host strings.

#### Scenario: Reuse a static value whose runtime type is affine

- **WHEN** a static function reads the same admissible static value more than once and its declared runtime type does not implement `Copy`
- **THEN** every static read succeeds while runtime conformance queries still report that the type is not `Copy`

#### Scenario: Parse with accumulator replacement

- **WHEN** a static function loops over static text and repeatedly replaces one `let mut` parse state with the complete result of a static step function
- **THEN** evaluation produces the final value without creating a borrow, mutable alias, cleanup obligation, or observable allocation identity

#### Scenario: Inspect static UTF-8 bytes through ordinary source

- **WHEN** a static function imports `silk.static_text` and asks for the byte length, one byte, and a scalar-boundary slice of a text literal
- **THEN** the source wrappers produce deterministic static `usize`, `u8`, and `string` values while no text-inspection intrinsic reaches runtime HIR

#### Scenario: Reject runtime work in a static function

- **WHEN** a static function calls an ordinary function, reads a runtime local, invokes an Effect, or attempts to borrow static storage
- **THEN** analysis reports the phase violation at that operation and produces no static result

#### Scenario: Embed an admissible static value into runtime code

- **WHEN** a mixed function supplies a runtime-representable, cleanup-free static value to an ordinary runtime operation
- **THEN** residualization embeds that value directly and does not add a runtime parameter or reference to compiler storage

### Requirement: Compile error terminates only the selected specialization

`compileError(message)` SHALL be an inherently compile-time expression that requires a statically
evaluated `string`, terminates the current specialization with a deliberate compile-error
diagnostic, and produces no residual runtime path. It SHALL act as `never` for the selected static
expression or statement path, so that path has no return-value or fallthrough obligation. A
`compileError` in an unselected static arm or an uncalled static function MUST NOT execute. Runtime
control flow MUST NOT decide whether `compileError` executes.

#### Scenario: Exempt a compile-error arm from its expected result

- **WHEN** one selected expression arm produces the expected value and the alternative contains `compileError`
- **THEN** the compile-error arm requires no value of the expected type and a specialization selecting it fails with the compile-error diagnostic

#### Scenario: Discard residual runtime work before a compile error

- **WHEN** a selected static arm retains an ordinary runtime operation and later reaches `compileError`
- **THEN** compilation fails and discards the incomplete residual program without executing or emitting that runtime operation

#### Scenario: Ignore an unselected compile error

- **WHEN** a `static if` condition does not select an arm containing `compileError`
- **THEN** specialization succeeds without evaluating or reporting that compile error

### Requirement: Static evaluation is reachable, finite, and reproducible

Static functions and mixed bodies SHALL be evaluated for one concrete target and complete static
application when demanded by a constant initializer or the executable-specialization worklist.
Merely loading or indexing an uncalled declaration MUST NOT execute its static body. Equal source,
target, generic arguments, evidence, and static argument values SHALL produce the same static result,
diagnostics, residual program, and specialization identity across fresh processes.

The evaluator SHALL enforce deterministic recursion, work, retained-value, and residual-growth
limits. Exceeding a limit SHALL report a dedicated evaluation-limit diagnostic distinct from
`compileError`, name the exhausted resource, and produce no partial static value or residual
program.

#### Scenario: Leave an uncalled static function unevaluated

- **WHEN** a loaded module declares a static function containing `compileError` but no reachable constant initializer or specialization calls it
- **THEN** the declaration remains indexable without reporting the compile error

#### Scenario: Evaluate one reachable concrete application

- **WHEN** executable discovery reaches the same mixed function with two different static argument values
- **THEN** the compiler evaluates and records two distinct deterministic residual specializations

#### Scenario: Report a resource limit separately

- **WHEN** static recursion or looping exceeds its deterministic evaluation budget
- **THEN** compilation reports an evaluation-limit diagnostic rather than presenting the failure as a source-requested compile error

### Requirement: Static diagnostics retain the specialization trace

Every `compileError`, phase violation, and evaluation-limit diagnostic SHALL identify the primary
source operation and retain an ordered trace of static function calls, static arguments in a stable
presentation, selected `static if` arms, and the concrete target. A diagnostic originating while
processing static text SHALL retain the source literal and applicable byte offset. Diagnostics MUST
NOT expose compiler addresses, cache identities, host stack frames, or backend details.

#### Scenario: Trace a nested target failure

- **WHEN** a reachable mixed function selects a target arm, calls a static helper, and that helper reaches `compileError`
- **THEN** the diagnostic identifies the compile-error expression and presents the selected target, arm, helper call, and responsible static arguments in source order

#### Scenario: Repeat a static diagnostic

- **WHEN** two fresh processes evaluate the same failing specialization
- **THEN** their diagnostic codes, semantic details, related spans, and static trace encodings are identical

### Requirement: Target information is static and closed

The selected compilation target SHALL contribute one closed profile value to the static environment
before any reachable specialization is evaluated. Ordinary source in the standard library SHALL
map that profile through zero-argument static functions to nominal scalar enums and SHALL derive
primitive target facts as statically evaluated constants. Source target checks SHALL compare those
enum values rather than string spellings. Target information MUST NOT be readable at runtime,
changed by source, inferred from the host when an explicit target was selected, or recomputed by an
execution engine.

#### Scenario: Select code by target architecture

- **WHEN** a mixed function calls the standard-library static architecture query and compares its result with the `Wasm32` enum member
- **THEN** a WebAssembly compilation selects the wasm arm and every native compilation selects the other arm before residual HIR is produced

#### Scenario: Keep the selected target out of runtime state

- **WHEN** runtime code is emitted from a target-specialized function
- **THEN** the artifact contains the selected residual operations but no target-profile parameter, runtime target probe, or static target query
