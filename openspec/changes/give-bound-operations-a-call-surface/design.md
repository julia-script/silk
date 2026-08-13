## Context

`accept-multi-operation-interface-bounds` left one gap deliberately and recorded why: an interface
is not an actor, so `Bound.operation(value)` resolves to a public function of the module declaring
the interface rather than to the bound's operation. It also recorded the constraint any answer must
satisfy — unlike an operator, a bound operation must consult the witness's selected operation at
specialization rather than reuse the width-neutral scalar lowering an operator already has.

The spelling is settled: `Bound.operation(args)`, qualified through the bound's own name, parallel
to a service operation. What follows is how that spelling resolves and how it reaches the engines.

## Goals / Non-Goals

Goals:

- Every operation a bound declares is callable in the generic body, at the contract the interface
  declares for it, checked once over the canonical parameter.
- The call reaches the witness's implementation for the specialized type argument.
- A collision with a module-level function of the same name resolves by a stated rule, and a
  receiver that answers to no single parameter is reported.

Non-Goals: `HashKey` and `Order` themselves, witness admissibility (#129), a bound operation as a
first-class value, type arguments on a bound operation, and any runtime representation change.

## Decisions

### The bound takes the spelling, and only the names it declares

Resolution of a two-segment receiver already asks what the qualifier names. When it names an
interface, the new question comes first: does the declaration being elaborated bound one of its
type parameters by that interface, and does that bound's recorded contract declare this member? Only
both answers together take the spelling. A member the bound does not declare, and a body with no
such bound, fall through to exactly the resolution they had before — which is what keeps
`Integer.add(40, 2)` resolving to `silk/numeric`'s public `add` outside a bounded body, and keeps
every other member of an interface's module reachable inside one.

Inside a bounded body the bound wins. `Integer.add(left, right)` in a body bounded by `Integer`
names the bound's operation, not the module function it shadows. This is the resolution the whole
change exists to produce: qualifying through the bound's name and getting the declaring module's
function instead is the defect being fixed, so preferring the module function there would leave the
bound's operation with no spelling at all. The module function stays reachable through its module.

The match is on the bound's recorded canonical capability, not on the spelling the bound was
written with, so an interface imported under an alias is the same bound under either name.

### A receiver that names two parameters is reported, not guessed

One declaration may bound two of its parameters by one interface. The receiver is the bound's name,
so `Bound.operation(...)` then names no single parameter, and each parameter has its own witness.
The call is reported — `SEM0097`, naming the parameters it is ambiguous across — rather than
resolved to the first parameter. Nothing here forecloses a receiver that names the parameter
instead; it forecloses only guessing.

### The contract is the interface's own, over the bounded parameter

The interface writes its contract over its own type parameter. A bound applies that interface to one
parameter of the bounded declaration, so the operation's contract in the body is the declared one
with the interface's parameter substituted — `mix(left: T, right: T) -> T` over the bound's `T`,
`ranksBelow(left: T, right: T) -> bool` keeping its `bool`. That is the same contract the conformance
check already holds every witness to, which is what lets the body be checked once, over the
canonical parameter, before any concrete argument exists.

A bound operation carries no type arguments of its own: the only type the call varies over is the
bounded parameter, and that one is supplied by the specialization of the declaration the body
belongs to.

### The call records the question; specialization answers it

An operator on a bound-typed operand carries its compiler-known operation from elaboration because
the specialized operand type alone selects the instruction: `Add` over `u8` and `Add` over `i32` are
one width-neutral lowering. A bound operation has no such property. Two providers of one interface
may map one operation to two unrelated instructions — `Mixer.mix` is `Intrinsic.i32WrappingAdd` for
`i32` and `Intrinsic.u8SaturatingAdd` for `u8` — and neither is the other's width-neutral form.

So the HIR node records the question rather than an answer: which operation, of which interface,
over which bounded parameter. `BoundOperationCall` carries no operation code and no intrinsic
identity. Lowering, which is where the substitution exists, reads the conformance the specialization
admitted, takes the compiler-known operation it maps, and continues as the ordinary builtin call it
now is. Two instances of one body therefore lower to two instructions from one node.

Recording the question rather than an answer is also what keeps the node honest under #129: when
witness admissibility widens, the node does not change — only what lowering finds at the end of the
conformance does.

### The reachable-intrinsic report reads the same witness

Target availability is answered from the intrinsics reachable instances retain. A bound operation
names no intrinsic until its type argument is known, so its identity is read per instance, through
that instance's substitution, from the same conformance lowering will read. Two instances of one
body contribute two identities, which is exactly what the availability report has to see.
