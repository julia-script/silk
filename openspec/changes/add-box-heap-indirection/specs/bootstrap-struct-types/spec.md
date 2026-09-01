## MODIFIED Requirements

### Requirement: Inline struct dependencies are finite

Resolved nominal fields SHALL form a deterministic type-dependency graph. A field SHALL retain every
nominal type it references as a reported dependency. Cycle detection SHALL use the narrower graph of
_inline reach_: the nominals whose layout a field's layout actually requires.

Inline reach SHALL be defined as follows. A field reaches a nominal inline when the nominal appears
in the field's type outside every indirecting position. A type argument of a compiler-owned
indirection whose layout is independent of that argument SHALL NOT be an inline position. A type
argument of a declared generic SHALL be an inline position exactly when that generic reaches its own
corresponding type parameter inline, computed as a monotone fixed point over the declarations and
therefore independent of declaration and module order.

Acyclic inline dependencies SHALL be available regardless of source or module order. Any direct or
mutual cycle consisting only of inline struct fields SHALL make every participating struct's layout
unavailable with one stable, canonically attributed diagnostic cycle. A cycle that passes through at
least one indirecting position SHALL be available, because the participating layouts are finite. The
compiler MUST NOT guess a size, silently add indirection, or reject an import cycle that contains no
inline type cycle.

#### Scenario: Resolve an acyclic nested struct

- **WHEN** `Span` contains two `Position` fields and `Position` contains scalar fields
- **THEN** both nominal types and their dependency edge are available independently of declaration order

#### Scenario: Reject direct inline recursion

- **WHEN** `Node` declares a field of type `Node`
- **THEN** `Node` retains its nominal identity and fields but its dependency and layout state identify the direct recursive cycle as unavailable

#### Scenario: Reject mutual inline recursion across modules

- **WHEN** `syntax.Expression` contains `statement.Statement` and `statement.Statement` contains `syntax.Expression`
- **THEN** both nominal identities remain queryable and one canonical cycle makes both layouts unavailable

#### Scenario: Ignore a harmless module cycle

- **WHEN** two cyclically importing modules declare structs whose field dependency graph is acyclic
- **THEN** their struct facts and layouts remain available without a diagnostic attributed solely to the import cycle

#### Scenario: Accept a cycle that passes through explicit indirection

- **WHEN** a struct declares a field whose type reaches the struct itself only behind an explicit heap indirection
- **THEN** the struct's dependency state is available, its layout is finite, and no cycle diagnostic is produced

#### Scenario: Reject a cycle through a generic that inlines its parameter

- **WHEN** `Wrapper` holds its type parameter as an inline field and `Node` declares a field of type `Wrapper<Node>`
- **THEN** the cycle remains inline throughout and `Node` is rejected exactly as a direct self-reference is

#### Scenario: Accept a cycle through a generic that indirects its parameter

- **WHEN** a generic reaches its type parameter only through an indirecting position and a struct declares a field of that generic applied to itself
- **THEN** the struct is available, without the compiler recognizing the generic by name

#### Scenario: Report a reference behind indirection as a dependency

- **WHEN** a field's type mentions a nominal only inside an indirecting position
- **THEN** the field still reports that nominal as a dependency while contributing no cycle edge
