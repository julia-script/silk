# Define function contracts, services, and failure propagation

Type: grilling
Status: resolved

## Question

How do return types, failure rows, requirement rows, nominal service contracts, explicit conformance,
service provision, propagation, handling, and compile-time elaboration compose into one precise
function-call model without runtime dependency-tag lookup?

## Answer

Ordinary `fn` functions execute directly and have empty failure and requirement rows. Invoking a
`flow fn` instead constructs a lazy typed flow value without entering its body. Its contract still
contains parameter types and access modes, success type, failure row, and access-qualified
requirement row. `run` evaluates exactly one flow layer: success produces the declared value, an
unhandled typed failure aborts that execution and propagates through the running composition, and
an unsatisfied service requirement remains in the flow's compile-time contract. A flow is not
necessarily a heap allocation or interpreter object; issue 08 defines when composition is erased or
requires a runtime environment.

Externally visible and directly or mutually recursive functions must declare their complete
contracts. Private non-recursive functions may infer success, failure, and requirement rows from
their bodies without consulting callers or later statements. A declared contract is a checked upper
bound: a body may not fail with an undeclared type or use an undeclared capability. Tooling should
infer and insert contracts mechanically. Provably unused declared row members receive a diagnostic,
initially a warning because recursive and generic bodies can make exact minimality difficult to
prove.

Typed failures are abortive and non-resumable. The sole originating operation consumes an owned
nominal value and fails with it. Its type enters the enclosing failure row, and the failure
expression has success type `Never`. There is no throwable class hierarchy, implicit conversion,
or general exception mechanism. A call that fails transfers ownership of its failure payload to the
nearest matching handler or to its caller, running automatic typed-infallible cleanup for every
ownership scope exited along the way. A handler may explicitly invoke an operation again, but it
cannot resume the failed invocation.

A handler may protect one call, an expression, or a block. It selects exact nominal members of the
protected failure row. An unguarded member branch removes that member from the residual row; a
guarded branch does not, because its guard may reject. Multiple guarded branches likewise do not
prove coverage without a final unguarded branch. A universal branch removes all remaining members,
and duplicate or unreachable unguarded branches are invalid. If the protected row is `E`, the
completely handled members are `H`, and handler branches may fail with `B`, the outward failure row
is the normalized `(E − H) | B`.

The matching branch owns its failure payload and uses the ordinary consuming, shared-borrowing, or
exclusive-borrowing match rules. Recovering cleans up an unconsumed payload at branch exit;
re-failing consumes and transfers it again. Unmatched members continue propagating without being
copied. Failure payloads obey the same named-scope escape check as successful return values, so a
payload tied to a scope being exited must be handled there or converted into data valid in an
ancestor scope. Compiler diagnostics should normally retain stable source identifiers and byte
spans rather than borrowed slices of temporary source storage.

The success type of a handler expression is the normalized union of the protected expression's
success type and every reachable recovery branch's success type. A branch that fails, returns, or
otherwise has type `Never` contributes no member. Handlers never silently coerce different values
to a shared interface or base type.

The failure channel carries one owned value per abortive exit and is fail-fast. Accumulating several
compiler diagnostics is ordinary program logic: a pass returns an owned report or collection and
may later fail with a nominal aggregate such as `DiagnosticBatch`. The language does not give the
failure channel a hidden accumulation policy.

Traps are separate from typed failures. Bounds violations, ordinary integer overflow, impossible
compiler states, and violated unsafe contracts terminate the bootstrap process. Typed handlers
cannot intercept them, and the bootstrap runtime does not promise stack unwinding or automatic
cleanup after a trap. A condition that callers should recover from must use a typed failure or an
ordinary checked return value instead.

Service capabilities are nominal interfaces. A requirement-row entry is keyed by the pair of a
capability type and a nominal service role, and records either shared or exclusive access for that
pair. Omitting a role selects the built-in `DefaultRole`. Combining rows retains the strongest mode
for duplicate capability-role pairs: exclusive dominates shared. A function needing no capability,
or only shared access, remains substitutable where a stronger requirement is permitted; the reverse
is invalid. A lexical environment contains at most one current implementation of each pair, while
distinct statically known roles may provide the same capability simultaneously. Roles are nominal
compile-time markers rather than strings or runtime keys. Dynamically selected or unbounded service
sets remain explicit router, pool, or collection values rather than environment entries.

Each service operation declares how it borrows the implementation. Calling a capability-and-role-
qualified operation uses that pair's lexically current implementation implicitly and contributes
the operation's access mode under the same key to the enclosing requirement row. An unqualified
role selects `DefaultRole`; a function may quantify over a nominal role type when its choice belongs
to the caller. Service operations may not consume the current implementation. Calling an actor
function on a specific concrete implementation remains an ordinary explicit data-first call and
does not use the service environment. The exact source syntax for roles, capability calls, and
access modifiers remains issue 08's responsibility.

Only the module defining a nominal implementation type may declare its conformance to a service
capability, consistently with the conformance rule from issue 02. The declaration maps every
interface operation to an existing actor-module function. A mapped function may have a smaller
failure row, a smaller requirement row, or weaker access needs than the interface operation, but
never stronger ones. An implementation-specific dependency must be declared by the interface
operation, consumed while constructing owned implementation state, or handled inside the
implementation. It cannot appear as a hidden ambient requirement at dispatch time.

Service provision specializes a flow value. `Capability.provide` satisfies one currently missing
capability-role pair with an existing implementation and removes that entry from the flow's
requirement row. Replacing an already supplied pair still requires explicit override. Provision may
move an implementation into the specialized flow, which then owns and automatically cleans it up,
or capture a borrow whose owner must outlive the specialized flow. The same owner may back several
shared roles, but ordinary borrowing rejects overlapping exclusive aliases. A shared provider can
satisfy only shared requirements, while an exclusive provider can satisfy either through temporary
reborrows. Because specialization is a value transformation, callers can branch an open flow and
provide different implementations before supplying affine inputs.

`Capability.provideWith` accepts an acquisition flow and acquires a fresh implementation for every
execution of the specialized flow. Acquisition failures and requirements compose mechanically with
the target flow while the provided capability-role entry is removed. Nested providers acquire in
wrapper order; a provider is not visible during its own construction. On acquisition failure or
body exit, every successfully owned implementation is cleaned up infallibly in reverse acquisition
order without replacing the original exit. There is no implicit memoization, bootstrap `Layer`
graph, global container, dependency solver, or service registry.

Function and flow values retain their complete success, failure, and requirement contracts.
Referencing an open `flow fn` captures nothing. Supplying arguments or providers creates a closed or
specialized flow with a compiler-shaped environment. Borrowed captures constrain its lifetime;
moved captures are owned and cleaned up with it. The strongest access needed by captured state
determines execution access: shared captures permit repeated shared runs, exclusive mutable state
requires exclusive runs, and an owned value consumed by the body makes that closed flow take-only.
There is no independent reusable-versus-single-shot effect category. General-purpose owned closures
remain deferred as described by issue 02.

Reusable higher-order functions may quantify over one failure row and one access-qualified
requirement row, or more when their signatures require it. These contract-row parameters are
inferred from callback values, may appear only in function-contract positions, and are concretized
during finite monomorphization. They are not runtime values or general row-level programming. The
compiler owns normalization, union, and subtraction of row expressions. This narrowly amends issue
02's type-only generic rule: ordinary user generics remain type parameters, while function
contracts additionally admit failure-row and requirement-row parameters so callbacks preserve
their complete contracts.

Service requirements lower to individual hidden service slots in canonical capability-and-role
identity order. Each slot is a non-owning pair of an opaque implementation pointer and a compiler-
shaped conformance witness table. Capability operations dispatch through statically known witness-
table offsets. Roles have no runtime lookup representation: the selected slot is fixed by the call
site. There is no string, type-tag, heterogeneous-container, or registry lookup. Pure functions
receive no hidden slots; running an open flow receives only its unresolved slots, while `provide`
or `provideWith` substitutes one slot in the elaborated composition. Service-bearing flow bodies
compile once rather than being monomorphized for every provider combination; later optimization may
devirtualize a witness call whose implementation is statically known.

Typed failures lower to explicit discriminated success-or-failure returns and ordinary branches,
not C++ exceptions, `setjmp`, `longjmp`, or platform unwinding. A caller branches on the result,
continues with the success value, or runs compiler-emitted cleanup and forwards the owned failure.
Handlers branch again on the nominal failure member. Functions with empty failure rows return their
success values normally. LLVM remains free to realize the private lowered result through registers,
an out pointer, or another target-appropriate ABI form, and optimization may eliminate redundant
tags and branches. Canonical nominal identity determines discriminants; they have no promised public
ABI or serialization representation.

Finally, unresolved rows may not silently cross the native executable boundary. A typed host
adapter constructs the platform implementations approved by issue 07, specializes and runs the
user entry flow, exhaustively handles its remaining typed failures, converts the result to the
platform exit convention, and cleans up its providers. The generated machine entry itself has empty
failure and requirement rows. The exact platform capabilities, diagnostic presentation, and exit
codes remain for issues 07 and 09.

All syntax above is semantic notation only. Issue 08 owns concrete spelling for contracts,
failures, handlers, service declarations, provision, override, access modes, and contract-row
parameters.
