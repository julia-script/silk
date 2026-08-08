## Context

The current parser gives pipelines a dedicated `PipelineTarget` limited to a qualified path and
optional later arguments. Elaboration inserts the left expression into argument zero and then
erases the pipeline into a direct call. `Type.Type` has no callable member, call syntax accepts only
named or qualified callees, and HIR/MIR have calls but no callable values or environments. `run` is
parsed as a primary/prefix expression, so it stops before a following pipeline.

The compiler already has useful precedents: Effect construction sites have hidden nominal
identities and compiler-shaped capture environments; ownership derives shared, exclusive, or taking
access; generics specialize to finite monomorphic instances; MIR remains a backend-neutral DAG; and
target-aware layout is complete before either backend. Callable values should reuse those
properties without becoming Effects or requiring a universal runtime object.

## Goals / Non-Goals

**Goals:**

- Make the right side of `|>` an ordinary unary callable expression.
- Support named function values and one automatic data-first section for every multi-parameter
  function.
- Support Copy, borrowed, exclusive, and owned section captures with deterministic cleanup.
- Make shared, exclusive, and consuming invocation visible in callable contracts.
- Compose callable values through user functions and the bootstrap Effect combinators.
- Preserve static specialization and allow non-escaping sections to erase completely.
- Make `run` encompass a following Effect pipeline without parentheses.

**Non-Goals:**

- Lambda or anonymous-function syntax.
- General currying, placeholders, or omitting any parameter other than parameter zero.
- Overloads, default arguments, variadic functions, or runtime method lookup.
- Heterogeneous collections of erased callable environments.
- Bodyless APIs that return an owned callable whose environment size and identity are unknown.
- A universal heap-allocated closure box, garbage-collected closure runtime, or callable interpreter.
- Non-effect logging or a debug tracing intrinsic.

## Decisions

### 1. Give every callable construction a hidden concrete identity

A named function item and each automatic section construction site have a compiler-known concrete
identity in addition to their public callable contract. A section identity determines its ordered
capture environment, layout inputs, cleanup, and invocation thunk. Local inference preserves that
identity, and generic higher-order calls specialize over it exactly as they specialize over other
concrete types.

This follows the Effect construction-site model and avoids erasing differently shaped environments
into a single unsized value. A callable parameter may be lowered as a specialized parameter or as a
call-scoped code-and-environment view when an abstract boundary requires it; neither choice gives
the callee ownership beyond the declared `fn`, `mut fn`, or `once fn` contract. Owned erased returns
and heterogeneous callable storage remain out of bootstrap because they would require an explicit
box, caller-provided storage protocol, or existential layout.

Alternative: make `fn(A) -> B` a universally boxed runtime value. Rejected because it would make
ordinary composition allocate, introduce an allocator requirement into pure section construction,
and hide layout and cleanup choices from the compiler.

### 2. Use one structural callable contract with three invocation modes

Canonical callable types contain ordered parameter types, result type, and invocation mode:

```text
fn(A) -> B          shared reusable
mut fn(A) -> B      exclusive reusable
once fn(A) -> B     consuming / at most once
```

The modes form an invocation-guarantee ordering: a shared reusable callable can satisfy `mut fn` or
`once fn`; an exclusive reusable callable can satisfy `once fn`; a weaker callable cannot satisfy a
stronger reuse promise. This is not Copy subtyping. Moving a callable value and borrowing its
environment remain ordinary ownership operations.

Concrete expressions infer their mode. Source spells the mode only in parameter or other abstract
callable contracts, so ordinary function declarations do not acquire modifiers. A named function
item has no capture environment and is shared reusable even if each invocation consumes its newly
supplied arguments.

Alternative: keep invocation mode compiler-private. Rejected because bodyless and generic
higher-order contracts must state whether they may call a callback repeatedly, mutate it, or consume
it.

Alternative: expose only reusable and once. Rejected because an exclusive borrowed or mutating
capture can be invoked repeatedly in sequence but cannot safely be invoked through shared access.

### 3. Make leading-argument sections automatic and intentionally narrow

For a function of arity `N >= 2`, a call with exactly `N - 1` arguments binds parameters `1..N-1`
and produces a unary callable for parameter zero. A call with `N` arguments invokes normally. Every
other arity is an error. A unary function is referenced by its name; `unary()` is not a redundant
section form. A zero-argument function retains its ordinary `zero()` call.

```silk
combine(a, b, c)  // complete call
combine(b, c)     // section awaiting a
combine(c)        // arity error
```

Section resolution happens only after resolving the canonical target and arity. This avoids syntax
markers and gives builtins, imported functions, and user functions one rule. It also preserves a
clear diagnostic for deeper under-application.

Alternative: `dual` on parameter zero. Rejected because the data-first position is fixed globally,
all APIs need the same composition ability, and the marker would add declaration ceremony without
choosing anything.

Alternative: placeholder sections such as `add(_, 2)`. Deferred with lambda syntax and arbitrary
partial application; it solves a larger problem than Effect combinators require.

### 4. Treat a section as an ownership-checked environment value

The ordered trailing arguments are evaluated once at section construction and become environment
slots:

- Copy inputs snapshot into reusable slots.
- Shared borrows retain shared loans and dependencies.
- Exclusive borrows retain exclusive loans and require exclusive invocation access.
- Moved affine inputs transfer cleanup into the environment. If invocation transfers such a slot
  onward, the callable is take-once.
- Dropping an uncalled or partly live environment ends loans and drops every still-owned slot once.

Invocation mode derives from how the generated invocation uses its environment, not merely from
whether the environment owns data. An environment may own a value and repeatedly borrow it if the
bound parameter contract permits that; a trailing parameter accepted by ownership transfer makes
the corresponding section slot consumable.

The ownership pass records slot state and dependency edges. A consuming call marks transferred
slots unavailable before entering the target. Typed Effect failure follows the target's normal
cleanup plan; a trap retains the bootstrap rule that no unwinding or cleanup is promised.

Alternative: permit only Copy captures. Rejected because it excludes resource adapters, owned
configuration, mutable services, and the higher-order cases most relevant to Silk.

### 5. Parse calls as postfix application and pipelines as low-precedence application

Call syntax becomes a repeated postfix over a callable-producing primary/projection expression, so
`I32.add(2)(3)`, `operation(value)`, and `(choose(flag))(value)` have one concrete form. Qualified
function references are resolved as values when they are not immediately invoked.

The pipeline grammar retains left associativity but replaces `PipelineTarget` with a right operand
parsed at the callable-expression precedence above pipeline. Its semantics are deliberately:

```text
let piped = evaluate(left)
let operation = evaluate(right)
invoke(operation, piped)
```

This is unary callable application with a pipeline-specific left-first evaluation promise. It is
not textual rewriting to `right(left)`, which could otherwise evaluate a complex callee before the
left expression under ordinary call order. HIR retains only callable construction/application plus
source provenance; it has no pipeline operation.

Alternative: keep qualified pipeline targets and merely allow them to escape as functions.
Rejected because callable bindings and callable-producing expressions would remain second-class.

### 6. Complete generic inference at full section application

Section formation infers generic parameters available from bound trailing arguments. A parameter
that remains unresolved but appears in the omitted leading parameter is retained in the section's
callable type and resolved when the leading value is applied. Inference still does not use expected
return types or unrelated later uses. Once complete, instance discovery receives the same canonical
type arguments it would receive for a direct full call.

Hidden callable identity participates in instance keys so two construction sites or two concrete
capture substitutions do not collide. Discovery records an instance before following callable
targets and capture layouts, preserving finite recursion checks.

Alternative: require explicit type arguments whenever section formation cannot finish inference.
Rejected because `Effect.map(genericMapper)` and data-first generic helpers would become needlessly
verbose even though the leading application supplies the missing evidence.

### 7. Reuse Effect capture access rather than special-casing callbacks

Effect combinators are ordinary higher-order operations with callable parameters. Their section
forms arise automatically:

```silk
succeed(2) |> Effect.map(I32.add(2))
```

When a combinator stores a callback, the resulting hidden Effect instance includes that callback
environment. Its run access is the strongest access required by the input Effect and callback. A
take-once callback therefore makes the composed Effect take-once; an exclusive callback makes runs
exclusive. Retry uses the already settled repeatability rule and rejects any take-once component.

`map` never flattens a returned Effect. Effectful logging passed to `map` therefore becomes a nested
success value; callers use the appropriate `tap` or `flatMap` contract to execute it. Logging itself
remains an Effect and continues to propagate its Logger requirement.

Alternative: add pipeline-aware overloads inside each Effect combinator. Rejected because it would
repeat the current special syntax and prevent user-authored combinators from composing equally.

### 8. Give `run` the complete following expression

`run` moves out of tight prefix parsing and owns a complete following expression until the current
comma, closing delimiter, block delimiter, or statement boundary. Newlines are trivia and do not
terminate it.

```silk
run attempt |> Effect.retry(2)       // run(attempt |> retry(2))
(run attempt) |> I32.add(1)          // apply add after execution
```

Nested `run run effect` remains right-associated and executes one layer per keyword. Parser recovery
inserts one missing operand at the same enclosing boundaries and does not consume the following
statement or declaration.

Alternative: retain tight prefix precedence and rely on parentheses. Rejected because the common
Effect composition form becomes noisier and contradicts the expression extent users already read
after `return`.

### 9. Keep logical callable semantics backend-agnostic and layout target-aware

HIR and ownership describe callable identity, signature, captures, invocation mode, and dependency
edges. Instance discovery makes every reachable callable concrete. Target layout then plans each
environment and any call-scoped code/environment view. MIR carries typed construction, application,
and cleanup in the existing control DAG.

LLVM and Wasm may erase immediate sections into direct calls or choose different concrete calling
shapes for escaping local callables. Both consume the same logical MIR and compiler-selected layout;
neither may identify builtin closure kinds or reconstruct capture semantics. This preserves the
project rule that backends lower the compiler's DAG rather than rebuilding semantic structure.

## Risks / Trade-offs

- [Automatic `N - 1` calls can hide a missing leading argument] → The expression has callable type,
  so incompatible result contexts produce a focused diagnostic naming the automatic section and
  omitted parameter zero. Other under-application remains an immediate arity error.
- [Owned sections introduce more affine values] → Reuse the Effect environment state machine and
  show capture slots and call mode in diagnostics and Labs.
- [Generic inference across two application sites is more complex] → Limit deferred inference to
  parameters evidenced by the omitted leading argument and snapshot the complete substitution in
  semantic facts before HIR.
- [Callable abstraction can accidentally require dynamic allocation] → Keep hidden identities
  monomorphic and explicitly defer unknown-sized owned erased returns and heterogeneous storage.
- [A backend may prematurely choose a universal closure ABI] → Make logical callable identity and
  target layout authoritative before MIR; test direct-erased and stored-environment cases across
  evaluator, LLVM, and Wasm.
- [`run` precedence changes existing parse trees] → Add formatter, recovery, semantic, and golden
  coverage for grouped and ungrouped forms and migrate examples in one breaking change.

## Migration Plan

1. Introduce callable syntax and canonical types while retaining diagnostics for the old
   qualified-target pipeline implementation behind tests.
2. Add semantic/HIR callable construction and application, then switch pipeline elaboration to the
   new path and remove inserted-argument facts.
3. Add ownership, instance, layout, MIR, evaluator, and backend support for concrete environments.
4. Convert Effect combinators and all examples to ordinary callable contracts.
5. Change `run` precedence, update formatting and syntax recovery, and regenerate affected goldens.
6. Add unified Labs presets and three-engine parity/determinism gates before removing the legacy
   `PipelineTarget` representation completely.

Rollback before release is a source-and-artifact revert because the project is pre-stable and the
change is intentionally breaking; no compatibility mode or persisted user artifact migration is
required.
