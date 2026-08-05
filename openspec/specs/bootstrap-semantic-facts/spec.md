# Bootstrap Semantic Facts Specification

## Purpose

Give the first parsed Silk function deterministic declaration, type, value, and compatibility
meaning while keeping incomplete syntax explicit and deferring semantic intermediate representations.
## Requirements
### Requirement: First function declaration fact

Semantic analysis SHALL retain the parse result and publish one ordered function fact for every
direct function declaration in the source-file tree. Header collection SHALL be owned by the
declaration-index phase, and semantic analysis SHALL consume its collected headers rather than
re-deriving them. Each function fact SHALL expose its declaration, returned-expression fact, and
return compatibility together. Every declaration SHALL have a deterministic source-local identity
whose ordinal matches concrete declaration order, its canonical identity state from the
declaration index, public visibility, exact concrete parameter count, declared-name state,
return-type-reference state, and exact syntax provenance. Name lookup SHALL distinguish exactly
one match, no match, and multiple matches without discarding any collected declaration.

#### Scenario: Collect the accepted declaration

- **WHEN** the accepted fixture `pub fn main() -> I32 { return 42 }` is analyzed
- **THEN** one public function fact named `main` is available at ordinal zero with zero parameters, a canonical identity naming its module and `main`, and provenance to its original function and name syntax

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

### Requirement: Call result typing remains independent of its argument contract
Top-level call-name resolution and target-return-type facts SHALL remain independent of the
positional argument contract. A uniquely resolved call SHALL retain its target result type even when
its argument contract has the wrong arity, while the caller's return compatibility SHALL continue
to compare only the returned expression type with the caller's declared return type.

#### Scenario: Retain a result type across wrong arity
- **WHEN** an `I32` function is called with the wrong number of otherwise available arguments
- **THEN** the call expression type and caller return compatibility remain available while the separate call contract is an arity mismatch

#### Scenario: Withhold a contract without changing name resolution
- **WHEN** a mapped argument type is unavailable
- **THEN** the call target and result type remain independently resolved while the call contract is unavailable

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
function and the binding statements that precede it in that function's body. Its reference fact
SHALL be `Resolved` with the exact parameter or binding identity and reference syntax when
exactly one declaration matches, `Missing` when none matches, `Ambiguous` with all matching
identities when multiple match, or unavailable when parser recovery did not supply usable
syntax. A resolved reference SHALL use its parameter's resolved declared type or its binding's
inferred type; all other reference or type states SHALL keep the expression type unavailable.
A binding SHALL NOT be referenced before its own statement completes.

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

#### Scenario: Resolve a returned binding

- **WHEN** a body spells `let value = 42 return value`
- **THEN** the returned expression resolves to that binding with its inferred `I32` type

#### Scenario: Reject a use before the binding

- **WHEN** a body references a name whose `let` statement appears later
- **THEN** the reference is `Missing` at that span rather than resolving forward

### Requirement: Unknown parameter reference diagnostic
A present bare identifier with no matching local parameter or preceding binding SHALL retain a
`Missing` reference fact and produce one `SEM0006` diagnostic at the exact reference span.
Duplicate declarations SHALL rely on declaration-owned `SEM0005` diagnostics without adding a
second ambiguity diagnostic at the reference. Diagnostics SHALL remain deterministic and
phase-separated with existing lexical, parser, and semantic diagnostics.

#### Scenario: Diagnose an unknown value name
- **WHEN** a function returns `missing` without declaring a parameter named `missing`
- **THEN** the reference is missing and one `SEM0006` diagnostic identifies the exact identifier span

#### Scenario: Avoid duplicate ambiguity diagnostics
- **WHEN** a reference matches duplicate parameter declarations
- **THEN** only the later declarations carry `SEM0005` and no reference-site ambiguity diagnostic is added

#### Scenario: Repeat parameter analysis
- **WHEN** equivalent parameter declarations and references are analyzed repeatedly in fresh processes
- **THEN** parameter identities, lookup outcomes, reference facts, types, compatibility, and diagnostic ordering are identical

### Requirement: Ordered call argument facts
Every call expression SHALL publish one ordered argument fact for every concrete argument. Each
argument fact SHALL have a zero-based ordinal, retain exact argument syntax provenance, and expose
the existing integer or local-parameter-reference expression fact and type state. Missing or damaged
argument syntax SHALL remain unavailable without creating a semantic argument or duplicating parser
diagnostics.

#### Scenario: Collect a literal argument
- **WHEN** `main` returns `identity(42)`
- **THEN** the call has one argument fact at ordinal zero with exact value `42`, type `I32`, and provenance to the literal syntax

#### Scenario: Collect a parameter-reference argument
- **WHEN** a function calls `identity(value)` using its resolved local parameter
- **THEN** the call's first argument retains that parameter reference and its available `I32` type

#### Scenario: Preserve argument source order
- **WHEN** a call contains two concrete arguments
- **THEN** its two argument facts have ordinals zero and one matching concrete list order

#### Scenario: Preserve parser ownership for a damaged argument
- **WHEN** argument syntax is missing or retained in an error region
- **THEN** no semantic argument is invented and the parser diagnostic remains the owning error

### Requirement: Analyze nested expression facts recursively
Every concrete call used as an argument SHALL produce a recursive call-expression fact with its
own exact syntax provenance, target-resolution state, ordered argument facts, positional contract,
and result-type state. Analysis SHALL resolve and type nested expressions from their leaves outward
while retaining concrete source order and stable identities at every depth. A missing, ambiguous,
damaged, or type-unavailable inner expression SHALL make only its dependent outer contract or type
unavailable and MUST NOT invent a target, binding, value, or duplicate parser-owned diagnostic.

#### Scenario: Analyze one nested identity call
- **WHEN** `main` returns `identity(identity(42))` and both calls resolve uniquely
- **THEN** the outer argument contains a nested call fact whose literal argument, positional contract, result type, call-site span, and target identity are all available

#### Scenario: Preserve nested sibling order
- **WHEN** a call has two nested call arguments in concrete source order
- **THEN** both outer argument ordinals and every nested argument ordinal remain deterministic and match their respective concrete lists

#### Scenario: Propagate an unavailable inner target
- **WHEN** an inner call target is missing or ambiguous
- **THEN** the inner resolution and provenance remain visible while its result type and the dependent outer contract are unavailable without selecting a target or inventing a binding

#### Scenario: Keep inner and outer diagnostics phase-owned
- **WHEN** malformed inner syntax already has a parser diagnostic or a uniquely resolved inner call has the wrong arity
- **THEN** analysis preserves the parser-owned error or emits the applicable inner semantic diagnostic exactly once without adding a speculative outer mismatch diagnostic

#### Scenario: Repeat nested analysis
- **WHEN** an equivalent nested program is analyzed repeatedly in fresh processes
- **THEN** every nested identity, resolution state, contract, type, provenance item, and diagnostic appears in the same order

### Requirement: First positional call contract
A call whose function reference resolves uniquely SHALL map argument ordinal `n` to target parameter
ordinal `n`. Its call-contract fact SHALL be `Compatible` only when argument count equals parameter
count and every mapped argument and parameter type is available and equal. It SHALL be
`ArityMismatch` when the counts differ and `Unavailable` when the target is missing, ambiguous, or
syntax-unavailable or when any mapped type is unresolved or unavailable. Every mapped pair SHALL
retain the exact argument and target-parameter identities and syntax provenance.

#### Scenario: Bind one compatible argument
- **WHEN** `identity(value: I32)` is called as `identity(42)`
- **THEN** argument zero maps to parameter zero and the call contract is compatible

#### Scenario: Bind two arguments positionally
- **WHEN** a uniquely resolved two-parameter function is called with two available `I32` arguments
- **THEN** each argument maps to the parameter with the same ordinal and the call contract is compatible

#### Scenario: Preserve too few arguments
- **WHEN** a two-parameter target is called with one argument
- **THEN** the call contract is an arity mismatch with expected count two and actual count one

#### Scenario: Preserve too many arguments
- **WHEN** a one-parameter target is called with two arguments
- **THEN** the call contract is an arity mismatch with expected count one and actual count two

#### Scenario: Withhold a contract for an unavailable type
- **WHEN** a mapped parameter or argument type is unresolved or unavailable
- **THEN** the mapping remains visible but the call contract is unavailable

#### Scenario: Withhold a contract for an unresolved call
- **WHEN** top-level call resolution is missing, ambiguous, or syntax-unavailable
- **THEN** no target parameters are selected and the call contract is unavailable

### Requirement: Wrong call arity diagnostic
Every uniquely resolved call with a different argument and parameter count SHALL produce one
`SEM0007` diagnostic at the complete call span. Its reason data SHALL retain the target declaration
identity and expected and actual counts. Type-unavailable and unresolved calls SHALL not add an
arity or type diagnostic, and the existing return-type compatibility fact SHALL remain independent
from this call-contract fact.

#### Scenario: Diagnose too few arguments
- **WHEN** a one-parameter function is called with zero arguments
- **THEN** `SEM0007` covers the call and reports expected one and actual zero

#### Scenario: Diagnose too many arguments
- **WHEN** a zero-parameter function is called with one argument
- **THEN** `SEM0007` covers the call and reports expected zero and actual one

#### Scenario: Avoid cascading diagnostics
- **WHEN** a call target or mapped type is unavailable
- **THEN** the call contract is unavailable without adding `SEM0007` or a speculative type-mismatch diagnostic

#### Scenario: Repeat call-contract analysis
- **WHEN** equivalent calls and declarations are analyzed repeatedly in fresh processes
- **THEN** argument ordinals, mappings, compatibility states, reason data, and diagnostics are identical

### Requirement: Built-in I32 type fact
Semantic analysis SHALL resolve the exact return-type spelling `I32` for every function to the
bootstrap signed 32-bit integer type. Any other present identifier spelling SHALL remain an explicit
unresolved type and produce one `SEM0001` semantic diagnostic at that identifier's source span.
Missing or syntax-damaged return-type syntax SHALL remain unavailable without duplicating its parser
diagnostic.

#### Scenario: Resolve bootstrap return types independently
- **WHEN** two functions each declare `I32`
- **THEN** each function fact carries its own resolved built-in signed 32-bit return type and syntax provenance

#### Scenario: Diagnose one unknown return type
- **WHEN** one of two functions declares `Mystery`
- **THEN** only that function's return-type fact is unresolved and one `SEM0001` diagnostic identifies `Mystery`

#### Scenario: Do not guess a damaged return type
- **WHEN** parser recovery leaves one function's return-type identifier missing or inside an error region
- **THEN** that function's return-type fact is unavailable without changing the other function's facts or repeating the syntax diagnostic

### Requirement: Exact decimal I32 value fact
Semantic analysis SHALL interpret each present decimal-integer return expression as an exact
non-negative integer and publish its `I32` value when it is at most `2147483647`. A larger value
SHALL remain explicitly unavailable and produce one `SEM0002` diagnostic covering the complete
literal span. Analysis MUST NOT lose precision because of the host numeric representation.

#### Scenario: Analyze two integer values independently
- **WHEN** two functions return `42` and `0`
- **THEN** their ordered function facts contain the exact available `I32` values `42` and `0`

#### Scenario: Accept the positive I32 boundary
- **WHEN** one returned literal is `2147483647`
- **THEN** that function's expression has the exact available `I32` value `2147483647`

#### Scenario: Diagnose one out-of-range integer
- **WHEN** one returned literal is `2147483648`
- **THEN** only that function's value fact is unavailable and one `SEM0002` diagnostic covers the entire literal

#### Scenario: Preserve a missing integer expression
- **WHEN** parser recovery inserts one function's decimal-integer token
- **THEN** that function's value and expression-type facts are unavailable without affecting other function facts or adding a duplicate semantic diagnostic

### Requirement: First return compatibility fact
Every function fact SHALL publish return compatibility as `Compatible` only when that function's
declared return type and returned expression type are both available and equal. An integer expression
uses its existing `I32` type. A uniquely resolved call expression SHALL use its target declaration's
resolved return type even for forward or self references. Compatibility SHALL be `Unavailable` when
the caller type, expression type, or call target is unresolved, missing, ambiguous, or invalid, and
one function's compatibility MUST NOT overwrite another function's facts.

#### Scenario: Check an integer return
- **WHEN** a function declares `I32` and returns an available `I32` integer
- **THEN** that function reports `Compatible`

#### Scenario: Check a resolved call return
- **WHEN** `answer` declares `I32` and `main` declares `I32` and returns a uniquely resolved call to `answer`
- **THEN** the call expression has type `I32` and `main` reports `Compatible`

#### Scenario: Withhold compatibility for an unknown call
- **WHEN** a function returns a call whose reference is missing
- **THEN** the call expression type and caller return compatibility are `Unavailable`

#### Scenario: Withhold compatibility for an ambiguous call
- **WHEN** a function returns a call whose reference is ambiguous
- **THEN** the call expression type and caller return compatibility are `Unavailable` without selecting a declaration

#### Scenario: Withhold compatibility for an unresolved callee type
- **WHEN** a call resolves uniquely to a declaration whose return type is unresolved or unavailable
- **THEN** the call reference remains resolved but its expression type and caller return compatibility are `Unavailable`

### Requirement: Semantic diagnostics are deterministic data

The elaboration result SHALL expose semantic diagnostics as a separate readonly collection while
retaining lexical and parser diagnostics through its syntax artifact. Every semantic diagnostic
SHALL be a unified `Diagnostic` value whose originating phase is semantic analysis, containing a
stable code, severity, concise message, reason data, and source-owned primary span. A semantic
diagnostic produced because a fact is unavailable SHALL carry the originating diagnostic's
identity as its cause. Present duplicate names after the first occurrence SHALL produce `SEM0003`
at each later name span. Within the result, diagnostics SHALL be ordered by primary span and
stable code, and semantic source mistakes SHALL return complete ordered facts and diagnostics
rather than throw or fail an Effect. The semantic fact shapes and their explicit-unavailability
idiom SHALL be published by the elaboration phase; the single-pass analysis monolith is
superseded.

#### Scenario: Repeat multi-function semantic analysis

- **WHEN** equivalent malformed multi-function parse results are elaborated repeatedly in fresh processes
- **THEN** their declaration identities, fact order, lookup outcomes, source provenance, and semantic diagnostics are identical

#### Scenario: Keep diagnostic phases separate

- **WHEN** one source contains parser recovery, a duplicate present name, and an unknown present return-type identifier
- **THEN** lexical, parser, and semantic diagnostics remain in their owning collections, each identifying its originating phase, and semantic diagnostics are ordered by their exact primary spans

#### Scenario: Diagnose every later duplicate

- **WHEN** three declarations share the same present name
- **THEN** the second and third names each produce one `SEM0003` diagnostic while the first remains the original declaration

#### Scenario: Unavailability links to its cause

- **WHEN** a call target is unresolved and its argument-contract facts become unavailable as a result
- **THEN** any diagnostic reported on those dependent facts carries the unresolved-target diagnostic's identity as its cause, and no duplicate diagnostic restates the unresolved target

### Requirement: First top-level call reference fact
Semantic analysis SHALL resolve every present call callee against all collected
top-level declarations without depending on declaration order. A call-reference fact SHALL be
`Resolved` with the exact target declaration identity and callee syntax when exactly one declaration
matches, `Missing` when none matches, `Ambiguous` when multiple declarations match, or unavailable
when parser recovery did not supply a usable callee. Forward and self references SHALL resolve by
the same rules because this phase records relationships and does not execute functions.

#### Scenario: Resolve a call to an earlier declaration
- **WHEN** `answer` is declared before `main` and `main` returns `answer()`
- **THEN** the call reference resolves to `answer`'s exact declaration identity and preserves the call-site identifier span

#### Scenario: Resolve a forward call
- **WHEN** `main` returns `answer()` and `answer` is declared later in the same source
- **THEN** the call resolves to the later declaration independently of source ordering

#### Scenario: Resolve a self reference as data
- **WHEN** a function returns a call to its own unique name
- **THEN** the reference resolves to that declaration without evaluating the call or deciding recursion policy

#### Scenario: Preserve an ambiguous target
- **WHEN** a call name matches multiple duplicate declarations
- **THEN** the call reference is ambiguous, exposes all matching declaration identities in source order, and does not select one target

### Requirement: Unknown call target diagnostic
A present call name with no matching declaration SHALL produce one `SEM0004` semantic diagnostic at
the callee identifier span and retain a `Missing` reference fact. An ambiguous call SHALL rely on the
existing `SEM0003` duplicate-declaration diagnostics and MUST NOT add a second ambiguity diagnostic
at the call site. Missing or damaged callee syntax SHALL not duplicate parser diagnostics.

#### Scenario: Diagnose an unknown function
- **WHEN** `main` returns `missing()` and no declaration is named `missing`
- **THEN** the call reference is missing and one `SEM0004` diagnostic identifies the exact `missing` span

#### Scenario: Avoid duplicate ambiguity diagnostics
- **WHEN** a call targets a name already diagnosed as duplicated
- **THEN** the call remains ambiguous and the semantic collection contains the declaration-owned `SEM0003` diagnostics without an additional call-site ambiguity diagnostic

#### Scenario: Preserve parser ownership for a missing callee
- **WHEN** parser recovery inserts the call's identifier
- **THEN** the call reference is unavailable and no `SEM0004` diagnostic is emitted

### Requirement: Local binding facts

Every complete binding statement SHALL publish one binding fact: its bound name, declaration
span, ordinal among the function's statements, and inferred type. The inferred type SHALL be its
initializer expression's type; an unavailable initializer type SHALL keep the binding's type
explicitly unavailable carrying the originating diagnostic's identity where one exists, and MUST
NOT default to `I32`. Name resolution SHALL remain flat and non-shadowing: a binding whose name
repeats an enclosing parameter or an earlier binding in the same function SHALL produce one
`SEM0008` diagnostic at the rebinding span carrying the original span as a related span, and
references to that name SHALL keep resolving to the original declaration.

#### Scenario: Infer a binding type from its initializer

- **WHEN** a body spells `let value = identity(42)` and `identity` resolves with an `I32` result
- **THEN** the binding fact carries inferred type `I32` and its exact declaration span

#### Scenario: Keep a damaged initializer explicit

- **WHEN** a binding's initializer contains an unresolved reference
- **THEN** the binding's inferred type is explicitly unavailable carrying the originating diagnostic's identity

#### Scenario: Reject shadowing a parameter

- **WHEN** `identity(value: I32)` declares `let value = 42`
- **THEN** one `SEM0008` diagnostic marks the rebinding with the parameter's span as a related span, and later `value` references still resolve to the parameter

#### Scenario: Reject rebinding a binding

- **WHEN** a body declares `let value = 1` and later `let value = 2`
- **THEN** the second binding carries one `SEM0008` diagnostic and references after it still resolve to the first binding

### Requirement: Move expressions are consuming references

A present `move <name>` expression SHALL resolve its name exactly like a bare-identifier
reference and SHALL carry the resolved declaration's type. The move itself SHALL remain an
explicit semantic fact distinct from an ordinary read so the ownership phase can treat it as a
consuming use. An unresolved moved name SHALL follow the same `Missing` fact and `SEM0006`
diagnostic as a bare identifier.

#### Scenario: Resolve a moved binding

- **WHEN** a body spells `let value = 42 return move value`
- **THEN** the returned expression is a move fact resolving to the binding with type `I32`

#### Scenario: Diagnose an unknown moved name

- **WHEN** a body moves a name with no matching parameter or binding
- **THEN** the move's reference fact is `Missing` with one `SEM0006` diagnostic at the name's span

### Requirement: Signed integer literal facts

A present integer literal with a directly applied minus sign SHALL produce a signed exact value
fact typed `I32` in its context. Literals SHALL be range-checked against the full signed `I32`
range: values above `2147483647` or below `-2147483648` SHALL keep the existing `SEM0002`
out-of-range diagnostic and an explicit out-of-range fact, and `-2147483648` itself SHALL be a
valid exact value.

#### Scenario: Analyze a negative literal

- **WHEN** a body returns `-42`
- **THEN** the integer fact carries the exact value `-42` typed `I32` with no diagnostics

#### Scenario: Accept the signed minimum

- **WHEN** a body returns `-2147483648`
- **THEN** the fact carries that exact value rather than an out-of-range state

#### Scenario: Reject one below the signed minimum

- **WHEN** a body returns `-2147483649`
- **THEN** the fact is out-of-range with one `SEM0002` diagnostic at the literal's span

### Requirement: Compiler-known actor operations resolve without source declarations

Qualified calls SHALL resolve against the compiler-known built-in actor table rather than source
declarations: the `I32` actor SHALL expose the ordinary trapping arithmetic operations `add`,
`subtract`, `multiply`, `divide`, and `remainder`, each accepting two `I32` arguments and
producing `I32`. Built-in operations MUST NOT appear in the declaration index, MUST NOT be
callable by bare name, and their argument facts SHALL follow the same recursive analysis and
arity checking as user calls, with a wrong arity keeping the expression unavailable. A qualified
call naming an unknown actor SHALL produce one `SEM0009` diagnostic, and a known actor with an
unknown operation SHALL produce one `SEM0010` diagnostic, each at the exact offending span with
the expression kept explicitly unavailable.

#### Scenario: Resolve a built-in arithmetic call

- **WHEN** a body returns `I32.add(40, 2)`
- **THEN** the call fact resolves to the built-in operation, both argument facts are exact values, the expression type is `I32`, and no diagnostics are produced

#### Scenario: Diagnose an unknown actor

- **WHEN** a body returns `Math.add(1, 2)`
- **THEN** one `SEM0009` diagnostic marks the actor identifier and the expression is explicitly unavailable

#### Scenario: Diagnose an unknown operation

- **WHEN** a body returns `I32.frobnicate(1, 2)`
- **THEN** one `SEM0010` diagnostic marks the operation identifier and the expression is explicitly unavailable

#### Scenario: Keep bare built-in names unresolved

- **WHEN** a body returns `add(1, 2)` with no such source declaration
- **THEN** the call keeps the existing unknown-function diagnostic rather than resolving to the built-in actor

### Requirement: Bool values and comparisons elaborate with exact types

`true` and `false` SHALL produce exact boolean value facts typed `Bool`. The built-in `I32`
actor SHALL additionally expose the comparison operations `equals`, `notEquals`, `lessThan`,
`lessOrEqual`, `greaterThan`, and `greaterOrEqual` — two `I32` arguments producing `Bool` — and
a built-in `Bool` actor SHALL expose `not` with one `Bool` argument producing `Bool`. Built-in
operations SHALL carry per-operation contracts (parameter types, result type, arity) and
resolution SHALL keep the existing `SEM0009`/`SEM0010` diagnostics for unknown actors and
operations.

#### Scenario: Elaborate a comparison

- **WHEN** a body returns `I32.lessThan(1, 2)`
- **THEN** the call resolves to the built-in comparison with expression type `Bool` and no diagnostics

#### Scenario: Elaborate boolean negation

- **WHEN** a body returns `Bool.not(true)`
- **THEN** the call resolves with one boolean argument and expression type `Bool`

### Requirement: Conditions and arguments are type-checked

The condition of a conditional statement SHALL elaborate to type `Bool`; a present condition of
any other available type SHALL produce one `SEM0011` diagnostic at the condition's span, with no
truthiness or coercion, and the conditional's arms still elaborated. A call argument mapped to a
parameter of a known different type — user or built-in — SHALL produce one `SEM0012` diagnostic
at the argument's span and keep the call expression explicitly unavailable.

#### Scenario: Reject an integer condition

- **WHEN** a body spells `if 1 { return 1 } return 0`
- **THEN** one `SEM0011` diagnostic marks the condition span and the arm's facts remain published

#### Scenario: Reject a boolean argument to arithmetic

- **WHEN** a body returns `I32.add(true, 1)`
- **THEN** one `SEM0012` diagnostic marks the first argument and the call expression is explicitly unavailable

#### Scenario: Reject a mistyped user call argument

- **WHEN** `identity(value: Bool)` is called with `identity(42)`
- **THEN** one `SEM0012` diagnostic marks the argument and the call expression is explicitly unavailable

#### Scenario: Check the return statement against a Bool contract

- **WHEN** `pub fn flag() -> Bool { return true }` and `pub fn broken() -> Bool { return 1 }` are elaborated
- **THEN** `flag`'s return compatibility is compatible and `broken`'s is unavailable

