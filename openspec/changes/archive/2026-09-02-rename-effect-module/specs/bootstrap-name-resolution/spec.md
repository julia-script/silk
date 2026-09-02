## MODIFIED Requirements

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

## REMOVED Requirements

### Requirement: A module's own bindings shadow the standard-library prelude

**Reason**: Catalog membership and preferred namespace metadata are distribution and tooling facts,
not semantic scope. An implicit catalog namespace contradicts explicit imports and makes an
import-bearing completion meaningless.

**Migration**: Add ordinary explicit imports for every standard-library namespace used by source.
For the Effect operation namespace, use `import silk.effect as Effect`; the closed `Effect<...>` type
syntax still requires no import.
