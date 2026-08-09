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
both its namespace binding and selected-member bindings. Each binding SHALL retain its import
syntax, local spelling, binding kind, canonical target module, and resolved member identity when
applicable. A source module MAY name a canonical target module in at most one import declaration.

#### Scenario: Bind a default namespace

- **WHEN** a module imports `compiler.Syntax`
- **THEN** its scope contains one namespace binding named `Syntax` targeting canonical module `compiler/Syntax`

#### Scenario: Bind selected and aliased members

- **WHEN** a module imports `compiler.Syntax { Node, parse, encode as encodeSyntax }`
- **THEN** its scope binds the three public members as `Node`, `parse`, and `encodeSyntax` without binding a `Syntax` namespace

#### Scenario: Bind a hybrid import

- **WHEN** a module imports `compiler.Syntax as Tree { Node, parse }`
- **THEN** its scope binds namespace `Tree` and selected members `Node` and `parse` from the same canonical target module

#### Scenario: Reject a repeated target module

- **WHEN** one source module contains two import declarations resolving to the same canonical target
- **THEN** the later import is an explicit invalid import with a stable diagnostic and does not create a second set of bindings

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
