## ADDED Requirements

### Requirement: Function-local parameter declaration facts
Every function fact SHALL publish one ordered parameter declaration fact for every concrete
parameter declaration. A parameter identity SHALL combine its owning function identity with its
zero-based concrete parameter ordinal. Each fact SHALL expose its name state, declared-type state,
and exact syntax provenance. The exact spelling `I32` SHALL resolve to the bootstrap type, an unknown
present type SHALL produce `SEM0001`, and missing or damaged syntax SHALL remain unavailable without
duplicating parser diagnostics.

#### Scenario: Collect one typed parameter
- **WHEN** `identity` declares `value: I32`
- **THEN** its first parameter has ordinal zero, a present name `value`, a resolved `I32` type, and provenance to the exact parameter, name, and type syntax

#### Scenario: Keep parameter identities function-local
- **WHEN** two functions each declare a first parameter named `value`
- **THEN** both parameters have ordinal zero under different owning function identities and do not conflict

#### Scenario: Diagnose an unknown parameter type
- **WHEN** a present parameter type spells `Mystery`
- **THEN** that parameter type is unresolved and one `SEM0001` diagnostic identifies its exact type span

#### Scenario: Preserve damaged parameter syntax
- **WHEN** parser recovery inserts a parameter name or type
- **THEN** the parameter fact remains ordered with the affected state unavailable and no duplicate semantic diagnostic

### Requirement: Function-local parameter lookup
Parameter lookup SHALL consider only the complete parameter collection of the enclosing function
and SHALL distinguish exactly one match, no match, and multiple matches. Every later present
duplicate parameter name SHALL produce one `SEM0005` diagnostic at the later declaration while all
matching parameter identities remain available in source order. Parameters in other functions and
top-level function declarations MUST NOT participate in this lookup.

#### Scenario: Resolve one local parameter name
- **WHEN** a function declares exactly one present parameter named `value`
- **THEN** lookup for `value` in that function resolves to its exact parameter identity

#### Scenario: Do not see another function's parameter
- **WHEN** only a different function declares a parameter named `value`
- **THEN** lookup for `value` in the current function reports no match

#### Scenario: Preserve duplicate parameters
- **WHEN** one function declares two parameters named `value`
- **THEN** lookup is ambiguous, both identities remain in order, and `SEM0005` identifies the second name

### Requirement: First parameter reference fact
Every present bare-identifier expression SHALL resolve against the parameters of its enclosing
function. Its reference fact SHALL be `Resolved` with the exact parameter identity and reference
syntax when exactly one declaration matches, `Missing` when none matches, `Ambiguous` with all
matching identities when multiple match, or unavailable when parser recovery did not supply usable
syntax. A resolved reference SHALL use its parameter's resolved declared type; all other reference
or type states SHALL keep the expression type unavailable.

#### Scenario: Resolve a returned parameter
- **WHEN** `identity(value: I32) -> I32` returns `value`
- **THEN** the returned expression resolves to parameter zero, has type `I32`, and the function return is compatible

#### Scenario: Resolve a parameter used as an argument
- **WHEN** a function passes its parameter `value` as a call argument
- **THEN** that argument's identifier reference resolves to the enclosing function's exact parameter declaration independently of the call target

#### Scenario: Preserve an ambiguous reference
- **WHEN** a bare identifier matches duplicate parameters in its enclosing function
- **THEN** the reference exposes every match, selects none, and its expression type remains unavailable

#### Scenario: Preserve parser ownership for a missing reference
- **WHEN** parser recovery inserts the identifier expression's token
- **THEN** the reference and type are unavailable without a semantic diagnostic

### Requirement: Unknown parameter reference diagnostic
A present bare identifier with no matching local parameter SHALL retain a `Missing` reference fact
and produce one `SEM0006` diagnostic at the exact reference span. Duplicate declarations SHALL rely
on declaration-owned `SEM0005` diagnostics without adding a second ambiguity diagnostic at the
reference. Diagnostics SHALL remain deterministic and phase-separated with existing lexical,
parser, and semantic diagnostics.

#### Scenario: Diagnose an unknown value name
- **WHEN** a function returns `missing` without declaring a parameter named `missing`
- **THEN** the reference is missing and one `SEM0006` diagnostic identifies the exact identifier span

#### Scenario: Avoid duplicate ambiguity diagnostics
- **WHEN** a reference matches duplicate parameter declarations
- **THEN** only the later declarations carry `SEM0005` and no reference-site ambiguity diagnostic is added

#### Scenario: Repeat parameter analysis
- **WHEN** equivalent parameter declarations and references are analyzed repeatedly in fresh processes
- **THEN** parameter identities, lookup outcomes, reference facts, types, compatibility, and diagnostic ordering are identical
