## MODIFIED Requirements

### Requirement: First function declaration fact

Semantic analysis SHALL consume the authored module and publish one ordered function fact for every
selected function declaration in authored order. Header collection SHALL be owned by the
declaration-index phase, and semantic analysis SHALL consume its collected headers rather than
re-deriving them. Each function fact SHALL expose its declaration, returned-expression fact, and
return compatibility together. Every declaration SHALL have a deterministic source-local identity
whose ordinal matches selected authored declaration order, its authored owner identity, its canonical
identity state from the declaration index, public visibility, exact concrete parameter count,
declared-name state, return-type-reference state, and authored anchors resolvable to current spans
through the module presentation. Name lookup SHALL distinguish exactly one match, no match, and
multiple matches without discarding any collected declaration.

#### Scenario: Collect the accepted declaration

- **WHEN** the accepted fixture `pub fn main() -> i32 { return 42 }` is analyzed
- **THEN** one public function fact named `main` is available at ordinal zero with zero parameters, a canonical identity naming its module and `main`, and anchors for its declaration and name that resolve to their current spans

#### Scenario: Count and collect typed parameters

- **WHEN** a function has two complete typed parameters
- **THEN** its declaration fact reports parameter count two and publishes two ordered parameter declaration facts

#### Scenario: Collect two declarations in order

- **WHEN** parsed `answer` and `main` functions appear in that source order
- **THEN** two function facts are published with ordinals zero and one and lookup resolves each present unique name

#### Scenario: Preserve a missing declaration name

- **WHEN** a parsed function contains a missing identifier before its parameter list
- **THEN** its function fact remains available with an unavailable name and an unidentified canonical state, no name lookup entry is invented, and no semantic diagnostic duplicates the parser's missing-token diagnostic

#### Scenario: Keep duplicate declarations explicit

- **WHEN** two declarations have the same present name
- **THEN** both function facts remain in source order, the later declaration's canonical state is a caused duplicate of the first, lookup reports multiple matches, and one `SEM0003` diagnostic identifies the later duplicate name

### Requirement: Function-local parameter declaration facts

Every function fact SHALL publish one ordered parameter declaration fact for every concrete
parameter declaration. A parameter identity SHALL combine its owning function identity with its
zero-based concrete parameter ordinal. Each fact SHALL expose its name state, declared-type state,
and authored anchors for the parameter, its name and its type. The exact spelling `i32` SHALL resolve
to the bootstrap type, an unknown present type SHALL produce `SEM0001`, and missing or damaged
authored names or types SHALL remain unavailable without duplicating parser diagnostics.

#### Scenario: Collect one typed parameter

- **WHEN** `identity` declares `value: i32`
- **THEN** its first parameter has ordinal zero, a present name `value`, a resolved `i32` type, and anchors resolving to the exact parameter, name, and type spans

#### Scenario: Keep parameter identities function-local

- **WHEN** two functions each declare a first parameter named `value`
- **THEN** both parameters have ordinal zero under different owning function identities and do not conflict

#### Scenario: Diagnose an unknown parameter type

- **WHEN** a present parameter type spells `Mystery`
- **THEN** that parameter type is unresolved and one `SEM0001` diagnostic identifies its exact type span

#### Scenario: Preserve damaged parameter syntax

- **WHEN** parser recovery inserts a parameter name or type
- **THEN** the parameter fact remains ordered with the affected state unavailable and no duplicate semantic diagnostic

### Requirement: First parameter reference fact

Every present bare-identifier expression SHALL resolve against the parameters of its enclosing
function and the binding statements that precede it in that function's body, using the authored
lexical binding recorded by local lowering. Its reference fact SHALL be `Resolved` with the exact
parameter or binding identity and the reference anchor when exactly one declaration matches,
`Missing` when none matches, `Ambiguous` with all matching identities when multiple match, or
unavailable when local lowering recorded a missing or invalid name. A resolved reference SHALL use
its parameter's resolved declared type or its binding's inferred type; all other reference or type
states SHALL keep the expression type unavailable. A binding SHALL NOT be referenced before its own
statement completes.

#### Scenario: Resolve a returned parameter

- **WHEN** `identity(value: i32) -> i32` returns `value`
- **THEN** the returned expression resolves to parameter zero, has type `i32`, and the function return is compatible

#### Scenario: Resolve a parameter used as an argument

- **WHEN** a function passes its parameter `value` as a call argument
- **THEN** that argument's identifier reference resolves to the enclosing function's exact parameter declaration independently of the call target

#### Scenario: Preserve an ambiguous reference

- **WHEN** a bare identifier matches duplicate parameters in its enclosing function
- **THEN** the reference exposes every match, selects none, and its expression type remains unavailable

#### Scenario: Preserve parser ownership for a missing reference

- **WHEN** local lowering records a missing identifier name
- **THEN** the reference and type are unavailable without a semantic diagnostic

#### Scenario: Resolve a returned binding

- **WHEN** a body spells `let value = 42 return value`
- **THEN** the returned expression resolves to that binding with its inferred `i32` type

#### Scenario: Reject a use before the binding

- **WHEN** a body references a name whose `let` statement appears later
- **THEN** the reference is `Missing` at that span rather than resolving forward

### Requirement: Ordered call argument facts

Every call expression SHALL publish one ordered argument fact for every concrete argument. Each
argument fact SHALL have a zero-based ordinal, retain the argument's authored anchor, and expose
the existing integer or local-parameter-reference expression fact and type state. Missing or damaged
authored arguments SHALL remain unavailable without creating a semantic argument or duplicating
parser diagnostics.

#### Scenario: Collect a literal argument

- **WHEN** `main` returns `identity(42)`
- **THEN** the call has one argument fact at ordinal zero with exact value `42`, type `i32`, and an anchor resolving to the literal span

#### Scenario: Collect a parameter-reference argument

- **WHEN** a function calls `identity(value)` using its resolved local parameter
- **THEN** the call's first argument retains that parameter reference and its available `i32` type

#### Scenario: Preserve argument source order

- **WHEN** a call contains two concrete arguments
- **THEN** its two argument facts have ordinals zero and one matching concrete list order

#### Scenario: Preserve parser ownership for a damaged argument

- **WHEN** an argument is missing or retained in an authored error region
- **THEN** no semantic argument is invented and the parser diagnostic remains the owning error
