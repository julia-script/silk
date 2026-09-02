## Context

Interfaces are monomorphization-time only: they create no effect requirement, no provider slot, and
no runtime dispatch. That property is preserved here in full. The change is about which mapping
targets a conformance may name, and what specialization does with the one it names — not about what
representation reaches the engines.

Two facts about the existing language decide almost everything below.

1. **An interface operation never consumes its operands.** Ownership checks a builtin call's
   arguments non-consumingly (`Ownership.checkExpression`, the `BuiltinCall` case), which is why
   `slice[i] < slice[j]` is accepted inside a body bounded by `Order`.
2. **No nominal type is `Copy`.** `Ownership.categoryOf` classifies every nominal as `MoveOnly`, so
   a user struct is move-only whether or not it owns a resource.

Together these rule out the obvious design — a witness whose signature is literally the contract's,
taking operands by value. Such a witness could only be called by moving the operands out of places
the operator merely reads, which ownership already forbids, and which for a resource-owning element
would mean the callee dropping a value it does not own.

## Goals / Non-Goals

Goals:

- One user nominal type may conform to one interface by mapping each operation to one of its own
  functions.
- The witness is checked for complete coverage exactly as an `Intrinsic.*` one is; the #103 coverage
  check applies unchanged.
- A generic bounded by the interface specializes at the user type and the call reaches the mapped
  function.
- `Integer` and every existing `Intrinsic.*` witness keep working, source untouched.

Non-Goals: blanket or conditional conformances, more than one interface per conformance, any runtime
representation change or dynamic dispatch, and #118's call surface for operations no operator spells.

## Decisions

### A witness target is one intrinsic or one function of the provider's own actor

The mapping keeps its two-segment shape. `Intrinsic.<operation>` selects the sealed operation as
before. `<Provider>.<function>` names a function declared in the provider type's own module, the
same actor-qualified spelling a service conformance already uses (`impl FileSystem for OsFileSystem
{ readFile: OsFileSystem.readFile }`). No third form is admitted, so a conformance can never name a
function belonging to some other type.

### A source witness observes its operands through a shared borrow

For a contract operation `fn op(a: T, b: T) -> R`, the mapped function is declared
`fn name(a: &T, b: &T) -> R` — every contract parameter received by shared borrow, the result by
value. This is forced, not chosen: the operator that reaches the witness does not consume its
operands, and no nominal provider is `Copy`, so a by-value parameter could not be supplied without
either a move the ownership checker rejects or a duplicate the callee would later drop.

The contract itself keeps declaring value operands, because that is what the intrinsic witnesses
take and `Order`/`Integer` must keep working unchanged. The borrow is the source witness's calling
convention, not a change to what the interface declares.

Beyond the operand form, the source witness is checked exactly as the intrinsic one: same arity,
same substituted parameter and result types, ordinary function kind, no type parameters, no failure
row, and no requirement row.

### Specialization redirects the operator; it does not reinterpret the contract

An operator inside a generic body cannot know its operand type, so it still elaborates against the
compiler-known operation of a stand-in actor. It now also records the bound interface operation it
spells — capability, provider parameter, and operation name — on the `BuiltinCall` it produces.

At specialization the recorded provider is substituted. When the resulting type has a conformance
mapping that operation to one of its own functions, lowering emits a shared borrow of each operand
local and an ordinary static call; instance discovery walks the same conformance so the witness is
reachable, since no ordinary call names it. When the mapping names an intrinsic — every scalar —
nothing is recorded to redirect to and the compiler-known operation lowers as it always has.

### A borrow-only place read observes without claiming

Lowering a redirected operator over a move-only element reads the operand place into a temporary and
borrows it. MIR forbids reading a non-`Copy` value out of a place without a paired consume, and
rightly so — but it already licenses one exception: a read whose value feeds a shared `Match` and
nothing else. The same license is extended to a read whose value is never accessed as an owner and
is borrowed shared. Such a value cannot be moved, dropped, or written through, so it is observable
only through a shared reference and no release can notice it.

The alternative — projecting a loan through a slice element selector, so the witness receives a
pointer to the element itself rather than to a temporary — needs runtime index arithmetic in three
backends and a source syntax the language deliberately rejects today ("a slice borrow requires a
direct stable array binding or slice parameter"). It buys nothing here: a comparison reads, and a
read of a copy it cannot move out of is indistinguishable from a read of the original.

## Risks / Trade-offs

The temporary is a shallow copy of the operand for the duration of the call. It is sound because the
witness can only read through `&`, but it does mean a witness cannot observe operand identity — two
distinct elements that are bitwise equal are indistinguishable to it. No interface operation in
scope here can observe identity, and #118 is where any operation that wanted to would be defined.

## Migration

None. No existing source changes: every shipped conformance names an intrinsic and keeps doing so.
