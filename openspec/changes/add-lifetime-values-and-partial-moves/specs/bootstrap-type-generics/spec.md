## MODIFIED Requirements

### Requirement: Declarations bind canonical type parameters

Struct and function declarations SHALL accept ordered lifetime parameters, ordinary type parameters, failure-row
parameters, requirement-row parameters, callable representation parameters, and Effect
representation parameters. Every parameter identity SHALL be local to its declaration and distinct
from nominal types and parameters with the same spelling elsewhere. A parameter SHALL be available
only in positions admitted by its kind, and duplicate or unbound parameters MUST produce
deterministic diagnostics.

#### Scenario: Bind one generic struct parameter

- **WHEN** `pub struct Box<T> { pub value: T }` is analyzed
- **THEN** the field type refers to the canonical `T` parameter owned by `Box`, not to a nominal type named `T`

#### Scenario: Bind a representation parameter

- **WHEN** `pub struct Mapper<A, B, F: fn(A) -> B> { transform: F }` is analyzed
- **THEN** `F` is canonical to `Mapper` and can appear only as a represented callable value

#### Scenario: Reject a duplicate parameter

- **WHEN** a declaration introduces `<T, T>`
- **THEN** analysis reports the second parameter as a deterministic duplicate without fabricating another identity

#### Scenario: Separate lifetime and type binders

- **WHEN** a declaration introduces <'a, T> and another declaration independently uses those spellings
- **THEN** their binder-relative identities remain distinct by declaration and kind; alpha-renaming changes presentation only

### Requirement: Generic applications are explicit canonical types

Applying a generic nominal declaration SHALL produce a canonical type identified by the declaration
plus normalized ordered arguments. Ordinary required type arguments SHALL remain explicit; lifetime arguments SHALL follow deterministic declaration elision in signatures and use-driven inference in local annotations. Named struct construction MAY instead supply a contiguous explicit prefix of
ordinary value arguments and SHALL infer its omitted ordinary suffix from all supplied fields;
construction MAY also infer concrete representation arguments from corresponding field
initializers. Applying arguments to a non-generic declaration, supplying the wrong kind, leaving a
parameter uninferred, or producing conflicting field constraints MUST remain explicit semantic
failures. Expected result types and later uses MUST NOT infer ordinary construction type arguments; inferred lifetime regions SHALL account for all relevant retained uses and cleanup.

#### Scenario: Reuse one applied type identity

- **WHEN** independent declarations refer to `Box<Token>`
- **THEN** both references resolve to the same canonical applied type identity

#### Scenario: Infer a construction representation

- **WHEN** `Mapper` construction supplies a named function for field `F`
- **THEN** the complete applied type includes that exact representation argument

#### Scenario: Infer an ordinary construction suffix

- **WHEN** `Pair<A, B>` construction writes `Pair<i32> { first: 1, second: true }`
- **THEN** the complete nominal type is `Pair<i32, bool>` using only the supplied fields

#### Scenario: Reject the wrong arity

- **WHEN** `Pair<i32>` appears in a required type position for a declaration with two parameters
- **THEN** analysis reports the expected and actual argument counts and produces no available applied type

#### Scenario: Reject conflicting construction evidence

- **WHEN** two supplied fields imply distinct arguments for the same omitted parameter
- **THEN** inference retains both field origins, reports the conflict, and produces no applied nominal type

#### Scenario: Preserve nested generic lifetimes

- **WHEN** identity forwards Option<&'a T> or a shared borrowed holder nested in a union and fixed array
- **THEN** substitution preserves all lifetime arguments and retained obligations independently of concrete source owners

### Requirement: Generic bodies are checked once

The compiler SHALL elaborate and check each generic body once over its canonical type and lifetime parameters, declared bounds, implied well-formedness obligations, and semantic configuration.
Concrete specialization MUST substitute the verified generic facts and MUST NOT enable undeclared
operations through concrete duck typing or type-directed source branching. A type parameter SHALL
carry compiler-owned Copy evidence only when its declaration has an explicit `Copy` bound, and that
symbolic evidence SHALL propagate through nested generic calls.

#### Scenario: Propagate Copy evidence through a generic call

- **WHEN** `outer<T: Copy>` calls `inner<T>` whose parameter is also bounded by `Copy`
- **THEN** constraint solving forwards the caller's symbolic evidence and accepts the call without concrete specialization

#### Scenario: Reject an unbounded structural guess

- **WHEN** an unconstrained type parameter is used where `Copy` is required
- **THEN** generic checking rejects the use even if one later specialization would contain only Copy fields

#### Scenario: Preserve a generic whole-value move

- **WHEN** `identity<T>(value: T)` returns `move value`
- **THEN** ownership checks that transfer once over `T` and every concrete specialization reuses the proof

#### Scenario: Reject undeclared concrete behavior

- **WHEN** an unconstrained generic body calls an operation unavailable for its type parameter
- **THEN** the declaration is rejected before any concrete specialization can make the call appear valid

#### Scenario: Reuse a checked lifetime-generic declaration

- **WHEN** an additional call instantiates the same unchanged generic declaration in the same semantic context
- **THEN** only the new call's obligations are instantiated and the existing generic semantic body check remains reusable

### Requirement: Runtime specialization is finite and monomorphic

Runtime instance discovery SHALL key each generic function by its canonical declaration and
normalized runtime-relevant concrete type arguments with semantic lifetime arguments erased, record the key before following dependencies, and require every
recursive generic call to preserve its current runtime-relevant type arguments. MIR and LLVM emission MUST receive
only concrete monomorphic instances and MUST NOT require runtime generic dictionaries or
type descriptors.

#### Scenario: Discover two concrete instances

- **WHEN** the entry reaches `identity<i32>` and `identity<Token>`
- **THEN** discovery records exactly two deterministic instance keys and lowering produces two concrete MIR functions

#### Scenario: Terminate ordinary generic recursion

- **WHEN** `walk<T>` recursively calls `walk<T>`
- **THEN** discovery reuses the already recorded instance key rather than expanding a new instance

#### Scenario: Reject polymorphic recursion

- **WHEN** a recursive generic call changes its current runtime-relevant type arguments
- **THEN** analysis rejects the call before instance discovery can expand indefinitely

#### Scenario: Erase different caller lifetimes

- **WHEN** two calls differ only in source owners or inferred regions
- **THEN** they share runtime instance keys, layout identities, and backend symbols without lifetime tokens, reference counting, or borrowing allocation
