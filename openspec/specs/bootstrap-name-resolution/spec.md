# bootstrap-name-resolution Specification

## Purpose

Define one deterministic closure-wide binding and lookup model that turns explicit imports and
top-level declarations into canonical cross-module references without runtime module behavior.

## Requirements

### Requirement: Imports create explicit module-scope bindings

Each source module SHALL publish one immutable module scope built from its local top-level
declarations and unconditional imports. `import compiler.Syntax` SHALL bind the namespace name
`Syntax`; `as Tree` SHALL replace that default with `Tree`; a selective list SHALL bind each named
public member under its declared name or explicit member alias; and a hybrid import SHALL create
both its namespace binding and selected-member bindings. A final contextual path segment SHALL
supply the default namespace name only when that segment is an ordinary identifier. An import whose
final segment is reserved SHALL create bindings only through an explicit ordinary namespace alias
or selected-member list. Each binding SHALL retain its import syntax, local spelling, binding kind,
canonical target module, and resolved member identity when applicable. A source module MAY name a
canonical target module in at most one import declaration. Distribution catalog namespace metadata
MUST NOT itself create a module-scope binding.

#### Scenario: Bind a default namespace

- **WHEN** a module imports `compiler.Syntax`
- **THEN** its scope contains one namespace binding named `Syntax` targeting canonical module `compiler/Syntax`

#### Scenario: Bind a reserved module under an explicit alias

- **WHEN** a module imports `silk.effect as Effect`
- **THEN** its scope contains one ordinary namespace binding named `Effect` targeting canonical module `silk/effect`

#### Scenario: Bind selected and aliased members

- **WHEN** a module imports `compiler.Syntax { Node, parse, encode as encodeSyntax }`
- **THEN** its scope binds the three public members as `Node`, `parse`, and `encodeSyntax` without binding a `Syntax` namespace

#### Scenario: Select from a reserved final segment

- **WHEN** a module imports `silk.effect { map }`
- **THEN** its scope binds public member `map` without creating an implicit namespace named `effect`

#### Scenario: Bind a hybrid import

- **WHEN** a module imports `compiler.Syntax as Tree { Node, parse }`
- **THEN** its scope binds namespace `Tree` and selected members `Node` and `parse` from the same canonical target module

#### Scenario: Reject a repeated target module

- **WHEN** one source module contains two import declarations resolving to the same canonical target
- **THEN** the later import is an explicit invalid import with a stable diagnostic and does not create a second set of bindings

#### Scenario: Keep catalog namespaces out of scope

- **WHEN** a catalog advertises preferred namespace `Effect` for `silk/effect` and the source has no corresponding import
- **THEN** `Effect.map` does not resolve as a module operation namespace

### Requirement: Import aliases are explicit changes

The default namespace name SHALL be the imported module's final canonical path segment. An explicit
namespace or member alias MUST differ from the name it replaces. Missing names and aliases after
parser recovery SHALL remain unavailable without fabricated bindings or a duplicate semantic
diagnostic.

#### Scenario: Reject a redundant namespace alias

- **WHEN** `compiler.Syntax` is imported as `Syntax`
- **THEN** the alias is rejected as redundant at its exact span and the import retains an explicit invalid-alias outcome

#### Scenario: Reject a redundant member alias

- **WHEN** member `parse` is selected as `parse`
- **THEN** the member alias is rejected as redundant at its exact span rather than creating a second spelling of the same binding

#### Scenario: Preserve recovered alias syntax

- **WHEN** an `as` keyword has no following identifier after parser recovery
- **THEN** the parser-owned missing-name state remains queryable and name resolution creates no alias binding or cascading unknown-name diagnostic

### Requirement: One flat top-level namespace forbids collisions

Local top-level declarations, namespace imports, selected-member imports, and the bootstrap's
compiler-known `i32` and `bool` actor namespaces SHALL occupy one flat module namespace. The
compiler-known actors are intrinsic language bindings rather than an implicit import or prelude. If
two otherwise valid bindings claim the same local spelling, resolution SHALL report the complete
collision deterministically and SHALL NOT select a winner using declaration kind, import order, or
source order. Unqualified and qualified lookups through a conflicting binding SHALL remain
explicitly unavailable with the collision diagnostic as their cause.

#### Scenario: Collide a selected member with a local declaration

- **WHEN** a module declares local function `parse` and selectively imports another public `parse`
- **THEN** the scope reports both binding sources as a collision and an unqualified `parse()` call does not resolve to either declaration

#### Scenario: Collide namespace and selected names

- **WHEN** two imports would bind the same local spelling as a namespace and a selected member
- **THEN** both candidate bindings remain visible in the conflict fact and no declaration-kind priority chooses one

#### Scenario: Protect an intrinsic actor name

- **WHEN** a local declaration or import attempts to bind `i32` or `bool`
- **THEN** the module scope reports a collision with the intrinsic actor binding rather than shadowing compiler-known operations

#### Scenario: Repeat collision analysis deterministically

- **WHEN** equivalent closures are supplied or traversed in different orders
- **THEN** their ordered module scopes, conflicts, lookup outcomes, and diagnostics are identical

### Requirement: Visibility governs cross-module member access

A declaration SHALL be visible within its defining module regardless of whether it is public. Only
an explicitly public declaration SHALL resolve through a namespace or selective import. Selecting
or qualifying an unknown member SHALL produce an unknown-member outcome; naming a private member
from another module SHALL produce a distinct inaccessible-member outcome retaining the private
declaration candidate and diagnostic cause. Imports MUST NOT re-export members or activate methods,
operators, overloads, conformances, or runtime initialization.

#### Scenario: Call a private local function

- **WHEN** a module-local body calls a unique private function declared in the same module
- **THEN** the call resolves to that function's canonical declaration identity

#### Scenario: Reject a selected private function

- **WHEN** an import selects a private function from another module
- **THEN** the selected binding is inaccessible with its stable diagnostic and no callable member binding is created

#### Scenario: Reject a qualified private function

- **WHEN** `Tree.hidden()` names a private function through a valid namespace import
- **THEN** lookup reports the inaccessible canonical declaration without producing a resolved call

#### Scenario: Keep imports behavior-neutral

- **WHEN** a module imports another actor module
- **THEN** only its explicit namespace and selected-member bindings enter scope and no unlisted behavior or runtime action appears

### Requirement: Cross-module lookup is cycle-safe and canonical

Name resolution SHALL run after every reachable module header has a canonical identity and complete
declared contract, independently of module traversal order. An unqualified call SHALL resolve only
through a unique local or selectively imported declaration binding; a qualified call SHALL begin
with a unique namespace binding and resolve one public member of that canonical module. Import
cycles SHALL remain valid, and mutually importing modules SHALL resolve calls to each other's public
functions when their explicit headers are complete. Every lookup outcome SHALL retain exact source
provenance and be deterministic across fresh processes.

#### Scenario: Resolve a selected call

- **WHEN** a module selectively imports public function `parse` and calls `parse()`
- **THEN** the reference resolves to `parse`'s canonical declaration identity in the imported module

#### Scenario: Resolve a namespace-qualified call

- **WHEN** a module imports `compiler.Syntax as Tree` and calls `Tree.parse()`
- **THEN** the namespace and member lookups resolve to canonical module `compiler/Syntax` and its public `parse` declaration

#### Scenario: Resolve mutual calls across an import cycle

- **WHEN** two mutually importing modules expose explicitly contracted public functions that call one another
- **THEN** both references resolve canonically and the import cycle produces no error by itself

#### Scenario: Keep an unknown qualifier explicit

- **WHEN** a qualified call begins with no visible namespace binding
- **THEN** the lookup remains explicitly missing at the qualifier span and no member candidate is invented

### Requirement: Declared nominal types resolve through explicit module scopes

Type positions in struct fields and function contracts SHALL resolve built-in scalar names, local
nominal declarations, selected nominal imports, and namespace-qualified public nominal members.
The lookup SHALL use the same immutable module scope, collision rules, canonical declaration
identities, and visibility outcomes as value-level declaration resolution while requiring the
resolved member to be a type declaration. It MUST NOT search filenames, infer imports, prefer a
declaration kind, or reinterpret a function as a type.

#### Scenario: Resolve a local nominal type

- **WHEN** a field or function contract names a unique local struct
- **THEN** the type lookup resolves that struct's canonical identity

#### Scenario: Resolve a selected nominal type

- **WHEN** a module imports `syntax.Tree { Node }` and a field names `Node`
- **THEN** the type lookup resolves the selected public struct's canonical identity

#### Scenario: Resolve a qualified nominal type

- **WHEN** a module imports `syntax.Tree as Tree` and a field names `Tree.Node`
- **THEN** the type lookup resolves the public struct through the namespace alias

#### Scenario: Refuse a function in type position

- **WHEN** a field type path resolves to a function declaration
- **THEN** the type remains unavailable with a kind-mismatch diagnostic and no alternate candidate

### Requirement: Nominal type visibility follows declaration boundaries

A nominal type SHALL be externally resolvable only when its struct declaration is public. A
qualified or selected lookup of a private struct SHALL retain the inaccessible candidate and exact
use-site provenance while producing no available type. Public declarations that expose nominal
types SHALL be validated after type lookup against the defining modules' visibility facts.

#### Scenario: Import a public struct

- **WHEN** a module selects a public struct from another module
- **THEN** the selected binding and every valid type use identify the same canonical nominal type

#### Scenario: Refuse a private struct

- **WHEN** a module selects or qualifies a private struct from another module
- **THEN** lookup retains its inaccessible identity and one visibility diagnostic without resolving a usable type

#### Scenario: Preserve unrelated lookups

- **WHEN** one nominal type lookup is inaccessible or conflicting
- **THEN** unrelated local and imported type lookups in the same closure remain available

### Requirement: Array element types resolve canonically

Type resolution SHALL recursively resolve an array's element type through the same local, selected,
and namespace-qualified module scope used by ordinary contracts. It SHALL canonicalize the decimal
length without treating `Array` as a source declaration or importing an element type implicitly.

#### Scenario: Resolve an imported element type

- **WHEN** a contract names `Array<Model.Token, 8>` through a valid namespace alias
- **THEN** the array element resolves to the defining canonical `Token` identity and length eight

#### Scenario: Retain an unavailable nested element

- **WHEN** a nested array names a private or unknown external element type
- **THEN** the array type remains explicitly unavailable with the original lookup cause

### Requirement: Constants resolve through explicit value scopes

Constant references SHALL use the existing local declaration, selective import, namespace alias,
visibility, collision, and cycle rules. Only public constants SHALL resolve across a module
boundary, and a constant in type or callable position SHALL produce a kind mismatch rather than an
alternate lookup.

#### Scenario: Resolve local, selected, and qualified constants

- **WHEN** source uses one local constant, selectively imports a public constant, and qualifies another through a namespace alias
- **THEN** all uses resolve to their exact canonical constant declarations

#### Scenario: Refuse a private imported constant

- **WHEN** another module selects or qualifies a private constant
- **THEN** lookup retains the inaccessible candidate and reports the existing visibility outcome without exposing a usable value

### Requirement: Intrinsic operations resolve only through the sealed namespace

`Intrinsic` SHALL be one compiler-sealed namespace binding. Compiler-provided callable operations
MUST resolve only as qualified members of that namespace and MUST NOT occupy independent actor
bindings such as `i32`, `Allocator`, or `StandardStreams`. Source code SHALL NOT declare, import,
alias, or shadow the reserved `Intrinsic` binding.

#### Scenario: Resolve a qualified scalar intrinsic

- **WHEN** source names `Intrinsic.i32Add`
- **THEN** resolution selects the canonical intrinsic operation rather than a source declaration

#### Scenario: Keep a scalar actor ordinary

- **WHEN** source names `i32.add` after the standard-library wrapper is in scope
- **THEN** resolution selects the source declaration and not the concrete intrinsic directly

#### Scenario: Reject shadowing Intrinsic

- **WHEN** a declaration or import attempts to bind the name `Intrinsic`
- **THEN** the module scope reports a deterministic collision with the sealed namespace

### Requirement: Service names resolve as source declarations

A service identity and its operation names SHALL resolve from the declaring source module with the
same visibility, import, collision, and canonical-identity rules as other declarations. The
compiler MUST NOT synthesize a service binding because a requirement row contains its spelling.

#### Scenario: Import a service explicitly

- **WHEN** a module imports Logger from its canonical source module
- **THEN** Logger requirements and qualified operations resolve to that source declaration

### Requirement: Qualified enum members resolve through canonical enum identity

Enum type names SHALL resolve through ordinary explicit module scopes and visibility boundaries.
After the qualifier resolves to an enum type, `EnumName.Member` SHALL resolve the member within that
enum's canonical member set rather than through module lookup or standard-library actor spelling.
Unknown, inaccessible, and wrong-enum members SHALL remain distinct deterministic resolution states.

#### Scenario: Resolve a visible imported enum member

- **WHEN** a module imports a public enum and refers to one of its qualified members
- **THEN** resolution records the imported enum identity and that member's canonical identity

#### Scenario: Reject an unknown enum member

- **WHEN** a qualified member path names no member declared by its resolved enum
- **THEN** resolution reports the dedicated unknown-enum-member diagnostic at the member span

#### Scenario: Reject a member through the wrong enum

- **WHEN** a member selected from one canonical enum is required to belong to another enum identity
- **THEN** analysis reports the dedicated wrong-enum-member diagnostic at the member path

#### Scenario: Reject unqualified member construction

- **WHEN** a bare identifier has the same spelling as an enum member but no ordinary binding declares it
- **THEN** resolution reports the ordinary unresolved-name state rather than searching visible enum member sets

### Requirement: Variants resolve through an instantiated nominal union

Name resolution SHALL first resolve a constructor qualifier through ordinary module scopes to one
canonical union declaration and bind any contiguous explicit parent-argument prefix. It SHALL then
resolve the variant only within that declaration; named-field inference SHALL complete the parent
application before the canonical selection becomes available. Pattern qualifiers SHALL resolve one
complete applied parent without scrutinee- or expected-type inference. A bare variant name SHALL NOT
search visible union declarations, and a same-spelled variant from another union SHALL remain a
distinct identity. Cross-module access SHALL enforce parent-union and complete-variant construction
authority under the ordinary nominal declaration rules.

#### Scenario: Resolve one applied variant

- **WHEN** `Result<i32, Problem>.Failure` is selected from a visible generic `Result<A, E>` declaration
- **THEN** resolution records the applied parent arguments and the canonical `Failure` identity owned by `Result`

#### Scenario: Complete a zero-prefix constructor qualifier

- **WHEN** `Option.Some { value: 42 }` resolves `Option<T>` and supplies no explicit parent arguments
- **THEN** resolution selects `Some` from the canonical declaration and records the applied `Option<i32>` only after field inference completes

#### Scenario: Reject a variant through the wrong parent

- **WHEN** two unions declare `Failure` and source selects the first union while requiring the second union's variant
- **THEN** analysis reports the canonical parent mismatch instead of resolving by spelling

#### Scenario: Keep unqualified variants out of ordinary lookup

- **WHEN** source refers to `Failure` without an ordinary binding or parent qualifier
- **THEN** resolution reports the ordinary unresolved-name state and does not search union variant sets

#### Scenario: Refuse pattern inference from the scrutinee

- **WHEN** a pattern spells `Option.Some { value }` against a scrutinee of type `Option<i32>`
- **THEN** resolution reports the incomplete pattern qualifier and requires `Option<i32>.Some`

### Requirement: Alias names resolve to their erased target type

A type position that names a local alias, a selected alias import, or a namespace-qualified public
alias SHALL resolve to the alias target's canonical type. The lookup SHALL use the same immutable
module scope, collision rules, and visibility outcomes as nominal type resolution. An alias name
SHALL NOT resolve in value position. Resolution of a use SHALL NOT depend on whether the alias is
declared before or after the use, or in another module of the closure.

#### Scenario: Resolve a local alias

- **WHEN** a function contract names a unique local alias whose target is `i32 | Token`
- **THEN** the type lookup resolves the normalized union `i32 | Token`

#### Scenario: Resolve a qualified alias

- **WHEN** a module imports `net as Net` and a failure row names `Net.FetchError`
- **THEN** the type lookup resolves the public alias's erased target through the namespace alias

#### Scenario: Refuse an alias in value position

- **WHEN** an expression names a type alias as a value
- **THEN** analysis reports the ordinary unknown-value diagnostic and does not treat the alias as a constructor

### Requirement: Nominal qualifiers resolve associated members

A qualified path `Owner.member` whose qualifier resolves to a nominal declaration SHALL resolve
`member` through that declaration's associated-member set: its intrinsic items (union variants,
enum members, generated operations, declared contract operations) and its inherent members. The
lookup SHALL follow the canonical declaration through selected-import aliases and transparent type
aliases, SHALL apply ordinary visibility so a private inherent member is inaccessible outside its
declaring module, and SHALL report an unknown member when no associated item exists. A declared
associated member SHALL take precedence over any other projection the qualifier might otherwise
offer for the same spelling. The module basename MUST NOT participate in resolving a declared
associated member.

#### Scenario: Resolve a member through a selected type import

- **WHEN** a module imports `silk.option { Option }` and calls `Option.some(2)`
- **THEN** the call resolves to the inherent member `some` of the canonical `Option` declaration

#### Scenario: Resolve a member when the file name differs

- **WHEN** `widgets.silk` declares `pub struct Gadget` with `impl Gadget { pub fn make() -> Self }` and an importer calls `Gadget.make()`
- **THEN** the call resolves to the inherent member without any basename comparison

#### Scenario: Resolve a member through a type alias

- **WHEN** a module declares `type Maybe<T> = Option<T>` and calls `Maybe.some(2)`
- **THEN** the call resolves to `Option`'s inherent member `some`

#### Scenario: Refuse a private member across modules

- **WHEN** `impl Counter { fn secret() -> i32 }` is private and another module calls `Counter.secret()`
- **THEN** resolution reports the member as inaccessible with the private declaration as the candidate

#### Scenario: Declared member outranks any other projection

- **WHEN** a module whose basename matches `Counter` declares both a root `pub fn make` and `impl Counter { pub fn make() -> Self }`
- **THEN** `Counter.make()` resolves to the inherent member and never to the root function

#### Scenario: Resolve service and interface members through one path

- **WHEN** `Logger.inMemoryProvider()` names an inherent member of a service and `HashKey.describe()` names an inherent member of an interface
- **THEN** both resolve through the associated-member set with identical lookup outcomes and diagnostics shapes

### Requirement: Root declarations never attach to a nominal type

A top-level function SHALL remain a module declaration reachable only unqualified, through a
selected import, or through a namespace import. Neither the name nor the type of its first parameter
SHALL attach it to a nominal type, and a selective import MUST NOT select an inherent member as if
it were a root declaration.

#### Scenario: A root function with a self parameter stays free

- **WHEN** a module declares `fn transform(self: Counter) -> Counter` at the top level
- **THEN** `transform(counter)` resolves and `Counter.transform` reports an unknown member

#### Scenario: Refuse to import a member selectively

- **WHEN** a module writes `import silk.option { map }` after `map` became an inherent member of `Option`
- **THEN** the import reports an unknown member of `silk/option` with the inherent member as a related candidate

### Requirement: The module basename has no semantic role in qualified lookup

A qualifier that resolves to a nominal declaration SHALL expose only that declaration's associated
items. A qualifier that resolves to a module namespace SHALL expose only that module's public root
declarations. Name resolution MUST NOT compare a declaration name with its module's basename, MUST
NOT project root declarations through a type, and MUST NOT project inherent members through a
namespace. An unknown member under a nominal qualifier SHALL report an unknown associated member;
an unknown member under a namespace SHALL report an unknown module member.

#### Scenario: A basename match exposes nothing

- **WHEN** `counter.silk` declares `pub struct Counter` and a root `pub fn increment` with no `impl Counter`
- **THEN** `Counter.increment(...)` reports an unknown associated member and `import counter { increment }` resolves the root function

#### Scenario: A namespace does not expose members

- **WHEN** a module imports `silk.option as OptionModule` and calls `OptionModule.map(...)`
- **THEN** resolution reports an unknown module member because `map` is an inherent member of `Option`, not a root declaration

#### Scenario: Type import is not a namespace

- **WHEN** a module imports `silk.vector { Vector }` and `vector.silk` also declares a root `pub fn debugDump`
- **THEN** `Vector.debugDump()` reports an unknown associated member and `Vector.append(...)` resolves the inherent member

#### Scenario: Completion and resolution agree

- **WHEN** completion is requested after `Counter.` in the basename-matching module above
- **THEN** no root function is offered
