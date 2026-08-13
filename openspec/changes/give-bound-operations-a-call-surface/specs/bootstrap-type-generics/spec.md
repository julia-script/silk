## ADDED Requirements

### Requirement: A bound's operations are callable through the bound's own name

A generic body SHALL be able to call every operation its parameter's bound declares, spelled
`Bound.operation(args)`, whether or not an operator spells that operation's name. The call SHALL
take the contract the interface declares for that operation with the interface's own type parameter
replaced by the bounded parameter, SHALL be checked once over that parameter before any concrete
argument exists, and MUST NOT accept type arguments of its own.

#### Scenario: Call an operation no operator spells

- **WHEN** a body bounded by an interface declaring `mix(left: T, right: T) -> T` evaluates `Mixer.mix(left, right)`
- **THEN** the call is checked over the bounded parameter and evaluates through the type argument's witness

#### Scenario: Keep a bound operation at its declared result

- **WHEN** a bound declares `ranksBelow(left: T, right: T) -> bool` and the body calls it
- **THEN** the call results in `bool` rather than the bounded parameter

#### Scenario: Refuse an operation the bound does not declare

- **WHEN** a body names an operation the bound's interface never declares
- **THEN** the receiver reports that the interface has no such operation

### Requirement: A bound receiver resolves to the bound before the interface's module

A receiver naming an interface SHALL resolve to that interface's operation whenever the declaration
being elaborated bounds one of its type parameters by that interface and the bound's recorded
contract declares the named member; the bound SHALL be matched by the interface's canonical
identity rather than by the spelling the bound was written with. Every other member of that
interface's module, and every body no such bound belongs to, SHALL keep resolving to the public
function of the declaring module exactly as before. A receiver naming an interface that bounds more
than one of the declaration's type parameters answers to no single parameter and SHALL be reported
rather than resolved to either.

#### Scenario: Prefer the bound over a same-named module function

- **WHEN** a body bounded by `Integer` calls `Integer.add(value, value)` and `silk/numeric` also declares a public `add`
- **THEN** the call selects the bound's operation over the bounded parameter, not the module function

#### Scenario: Keep the module function where no bound claims the name

- **WHEN** an unbounded body calls `Integer.add(40, 2)`
- **THEN** the call resolves to `silk/numeric`'s public `add` exactly as it did before

#### Scenario: Report a receiver naming two bounded parameters

- **WHEN** one declaration bounds two type parameters by one interface and the body calls that interface's operation
- **THEN** the call is reported as ambiguous across those type parameters and resolves to neither

### Requirement: A bound operation selects its implementation from the witness

Lowering a bound operation call SHALL read the conformance the specialization admitted for the
concrete type argument and use the compiler-known operation that conformance maps for the named
operation, rather than any operation fixed before the type argument was known. Two specializations
of one body whose witnesses map one operation to different compiler-known operations SHALL lower to
those different operations, and every such reachable operation SHALL be reported to target
availability under the specialization that reaches it.

#### Scenario: Reach two witnesses from one body

- **WHEN** one bound operation is mapped to a wrapping operation for `i32` and a saturating operation for `u8`, and one generic body is specialized at both
- **THEN** each specialization lowers to the operation its own witness maps, and neither reuses the other's

#### Scenario: Keep operator-spelled operations unchanged

- **WHEN** a body reaches a bound operation through the operator that spells it
- **THEN** the operator keeps the width-neutral lowering it already had and consults no conformance
