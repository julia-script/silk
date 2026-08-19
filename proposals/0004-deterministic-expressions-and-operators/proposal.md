# SLP-0004: Deterministic expressions and ordinary operator dispatch

SLP: 0004
Status: Draft
Revision: 7
Author: Julia Ortiz
Created: 2026-08-18
Updated: 2026-08-19
Discussion: —
Review record: —
Depends on: SLP-0002, SLP-0003
Split from: —
Split into: —
Supersedes: —
Superseded by: —
Revisit when: —
Resolution: —
OpenSpec handoff: —

## Summary

Provisional direction: Silk expressions evaluate deterministically from left to right and exactly
once. Eager surface operators select ordinary statically known operations without implicit
conversion or runtime dispatch. An interface operation may opt into one existing operator
explicitly, after which ordinary conformance and specialization rules apply. Only forms that change
whether or where evaluation occurs—such as short-circuiting, pipeline application, place
replacement, `move`, and `run`—receive direct language semantics.

## Problem and evidence

Silk already parses calls, projections, indexing, prefix and infix operators, pipelines, borrows,
moves, `run`, conditional and match expressions, and assignment. Their evaluation, ownership, and
diagnostic rules are spread across bootstrap specifications and compiler paths. The current operator
surface partly resolves concrete scalar modules by compiler-known identity while generic arithmetic
and ordering already use interface witnesses selected during specialization.

Without one model, a programmer cannot tell whether `left + right` is syntax for an ordinary
operation, compiler-only arithmetic, interface selection, or potential user overload; nor can they
predict when the right operand, assignment replacement, cleanup, or trap occurs.

## Driving examples: current and desired

### Case: Evaluate a composed expression once in observable order

#### Intent

Compute with two observations while preserving source order and the selected integer semantics.

#### Current Silk

```silk
fn observe(trace: &mut [i32], index: usize, value: i32) -> i32 {
  trace[index] = value
  return value
}

pub fn main() -> i32 {
  let mut trace = [0, 0]
  let result = observe(&mut trace, 0, 20) / observe(&mut trace, 1, 5)
  return result + trace[0] + trace[1]
}
```

Current machinery evaluates this deterministically, but the contract is distributed across call,
operator, slice, mutation, and trap artifacts.

#### Desired Silk

```silk
fn observe(trace: &mut [i32], index: usize, value: i32) -> i32 {
  trace[index] = value
  return value
}

pub fn main() -> i32 {
  let mut trace = [0, 0]
  let result = observe(&mut trace, 0, 20) / observe(&mut trace, 1, 5)
  return result + trace[0] + trace[1]
}
```

The source need not change. Its rules must state that the left call completes before the right call,
each call executes once, division uses exact `i32` semantics, and every later addition follows the
published precedence and associativity table.

#### Observable result

The program returns `29`; `trace` records `[20, 5]`; a zero right operand would trap only after both
operand calls completed.

#### Boundary case

```silk
fn invalid(left: i32, right: i64) -> i64 {
  return left + right
}
```

The operands are already typed differently. Operator syntax performs no widening.

### Case: Give library mathematics explicit operator participation

#### Intent

Let ordinary library types support familiar heterogeneous mathematics without becoming
compiler-built-in and without making operation names magical.

#### Current Silk

Concrete user values cannot consistently select an interface witness through operator syntax.
Inside a generic body, the compiler instead guesses an interface operation from names such as
`multiply`; direct concrete source does not use the same rule. The reliable workaround is a named
actor call:

```silk
let scaled = Vector.scale(move vector, 2.0)
```

#### Desired Silk

```silk
interface Multiply<Left, Right, Output> {
  operator * fn multiply(left: Left, right: Right) -> Output
}

impl Multiply<Vector, f64, Vector> for Vector {
  multiply: Vector.scale
}

let scaled = move vector * 2.0
```

The operator marker is explicit declaration syntax. After eligibility is established, the existing
ordinary interface and static conformance machinery selects `Vector.scale`.

#### Observable result

`Vector * f64 -> Vector`, `Vector * Vector -> f64`, `Matrix * Vector -> Vector`, and `Matrix *
Matrix -> Matrix` may coexist when their precise operand contracts do not overlap. No runtime
dictionary, service slot, or type tag is created.

#### Boundary case

Two operations accepting the same precise operand types cannot be selected by their differing
result types. Zero matches is an applicability error; multiple matches is an ambiguity error.

### Case: Execute and consume conditionally through Boolean operators

#### Intent

Use the ordinary meaning of short-circuiting without a second purity system.

#### Current Silk

```silk
effect fn decide() -> bool { return true }

effect fn choose(gate: bool) -> bool {
  return gate && run decide()
}
```

The parser represents the intended conditional `run`, but semantic analysis rejects it with
`SEM0096`. A `move` nested in the same position is rejected for the same reason, while mutation and
other observable calls are already allowed.

#### Desired Silk

The source above is valid. The right operand is an ordinary conditional branch. A move there
consumes its owner only on the path where the operand executes; Effect failures and requirements
remain part of the enclosing static contract.

#### Observable result

`decide()` is constructed and run only when `gate` is true. A skipped affine operand remains owned
and is cleaned on its continuing path; an executed move transfers exactly one ownership obligation.

#### Boundary case

A later use is invalid when any path reaching it has moved the owner. Short-circuiting does not make
unhandled Effect failures or requirements legal at an ordinary execution boundary.

### Case: Replace storage without exposing partial initialization

#### Intent

Make mutation order predictable for dynamic places and affine values.

#### Current Silk

```silk
fn replacement() -> i32 { return 42 }

fn write(values: &mut [i32], index: usize) {
  values[index] = replacement()
}
```

Current lowering validates the indexed destination before calling `replacement`, then commits one
complete replacement.

#### Desired Silk

The source remains unchanged. The ordering becomes an explicit language contract: validate the
complete destination, evaluate the right side once, clean the displaced value when necessary, and
commit the new complete value. `Intrinsic.replace(place, value)` performs the corresponding atomic
swap and returns the displaced value.

#### Observable result

An out-of-bounds `index` traps before `replacement()` begins. A right-side structured exit does not
commit. A successful affine replacement cleans the displaced value once and leaves one initialized
owner.

#### Boundary case

`destination = move destination` remains `OWN0004`; assignment is not an ownership-renaming or
self-move operation. Compound-assignment spellings remain unsupported.

### Case: Convert numbers explicitly

#### Intent

Make range loss, rounding, and representation access visible at the call site.

#### Current Silk

```silk
let wide = i32.toI64(value)
let checked = i64.checkedToU8(other)
```

#### Desired Silk

The named actor-function model remains unchanged. Silk adds no cast expression and operators insert
no conversion. `toX` states a trapping conversion, `checkedToX` returns `Option` where provided, and
`toBits`/`fromBits` remain representation access rather than numeric conversion.

#### Observable result

Range, truncation, and rounding follow the selected named operation. Direct and piped actor calls
have identical behavior.

#### Boundary case

Already-typed `i32` and `i64` operands do not combine because their particular runtime values happen
to fit. `f64.toU64` and `f64.toBits` are not interchangeable.

## Goals and non-goals

### Goals

- Define source expression categories and deterministic evaluation order.
- Define the closed operator spelling, precedence, and associativity table.
- Separate eager named-operation dispatch from short-circuit and place semantics.
- Define assignment commit, cleanup, trap, and failure boundaries.
- Define explicit conversion boundaries and the absence of truthiness or hidden allocation.
- Give invalid operands, discarded results, invalid places, and traps predictable diagnostics.

### Non-goals

- Redefine function calls, callable sections, pipelines, or control-flow result joining already
  covered by the function reference.
- Select the complete generic and interface specialization model.
- Define constant evaluation, macro syntax, unsafe operations, or target ABI.
- Catalogue every standard-library numeric, text, or collection operation.

## Current language model

Parsing uses fixed precedence metadata. Operator elaboration selects compiler-known scalar or string
operations, except short-circuit boolean operators, which lower as conditional control flow.
Generic integer addition and ordering can instead select interface witnesses. Assignment checks a
complete destination place before evaluating its replacement and commits one complete value.

## Proposed language model

Every expression publishes one precise type, evaluates its eager children left to right exactly
once, and either produces one value, propagates a structured failure, or traps. Surface sugar may
select an ordinary operation but cannot introduce conversion, truthiness, allocation, retry, or
hidden ownership transfer. User-defined operators require an explicit interface-operation marker
and reuse ordinary static conformance selection. Conditional evaluation and writable-place
replacement remain explicit language semantics.

## Worked language experience

The programmer-facing reference begins with one general rule: eager children evaluate once from
left to right. Conditional and deferred syntax then names its exception locally. Postfix
projection, indexing, and calls compose before prefix and infix operations; parentheses change
grouping only; assignment and bootstrap `if` remain statements; and a completed non-unit expression
must be used or explicitly dropped.

Concrete scalar operators keep their published checked integer or conservative IEEE behavior.
Library types opt into an existing operator explicitly through an interface operation and may use
heterogeneous operand and result types. Short-circuit right operands follow ordinary branch
ownership and Effect rules. Assignment validates its place before the replacement and commits one
complete value. Numeric conversion remains a named actor operation.

## Semantic sketch

- Primary expressions produce values before postfix projections, indexing, calls, or pipeline
  application consume them.
- Eager child expressions evaluate left to right exactly once.
- Grouping changes parsing, not evaluation count or ownership.
- Prefix and eager infix operators require statically selected compatible operations.
- An interface operation may opt into one closed operator spelling explicitly; names alone have no
  operator meaning, selection uses operand types only, and ambiguity is rejected.
- `&&` and `||` evaluate their right operand only when required; `run` and `move` there follow
  ordinary path-local rules rather than a special purity restriction.
- Assignment validates its destination before evaluating the replacement, cleans the displaced
  value when required, then commits one complete value atomically.
- `Intrinsic.replace` performs the same place-aware swap while returning the displaced value.
- Integer arithmetic operators trap on overflow and invalid division; checked, wrapping, and
  saturating behavior remain named operations.
- Floating arithmetic follows the selected width's IEEE behavior.
- Conversion of an already-typed value is always an explicit named operation; Silk has no cast
  expression.

## Compiler–standard library boundary

### Compiler necessity

Parsing, evaluation order, conditional evaluation, place resolution, ownership transfer, and trap
provenance cannot be expressed by an ordinary eager library call.

### Smallest target-neutral primitive

The compiler needs expression structure, the `operator` eligibility marker on an interface
operation, static operation selection, short-circuit branching, writable-place replacement, and the
existing sealed target-neutral arithmetic primitives. `Intrinsic.replace` is the narrow sealed
primitive retaining place identity. No standard-library actor or interface is recognized by source
spelling.

### Standard-library construction

Named checked, wrapping, saturating, converting, comparing, and collection operations remain
ordinary Silk functions and interfaces over sealed intrinsics where machine behavior is necessary.
Vector, Matrix, and other mathematical types may remain ordinary library types whose conformances
map operator-marked interface operations to actor functions.

### Privilege audit

The compiler checks operator eligibility once at declaration and then reuses the same interface,
conformance, coherence, and specialization machinery as a named operation. It does not privilege
`add`, `multiply`, or another function name. Short-circuiting and assignment cannot be reduced to
eager calls because they change whether an expression evaluates or which storage is replaced. An
ordinary wrapper cannot currently preserve place identity, which is why replacement remains in the
sealed `Intrinsic` namespace.

## Whole-language interaction map

| Surface | Disposition | Analysis |
| --- | --- | --- |
| Syntax and names | Affected | Expression forms, precedence, associativity, and operator spellings are fixed. |
| Types and abstraction | Affected | Exact operand types and explicitly eligible interface conformances determine result types; expected results do not select overloads. |
| Execution contracts | Affected | Eagerness, conditionally executed `run`, complete Effect channels, failures, and traps compose without a short-circuit purity tier. |
| Ownership and resources | Affected | `move`, borrow, assignment commit, replacement cleanup, and skipped operands are observable. |
| Runtime and targets | Affected | Integer traps, float behavior, and target-width integers must agree across engines. |
| Compiler | Affected | Parser, elaboration, HIR, MIR, evaluation, and backends must share one expression model. |
| Standard library | Affected | Ordinary named operations and explicitly operator-marked interfaces expose policy above minimal intrinsics. |
| Tooling and diagnostics | Affected | Formatting, hover, facts, invalid operands, overload ambiguity, and trap provenance require exact spans. |
| Learning and use | Affected | One precedence table and one evaluation rule must explain ordinary expressions locally. |

## Scope cohesion

The central thesis is one deterministic expression model: syntax controls evaluation shape while
ordinary operations supply eager behavior. Pipeline details remain in the callable reference, and
explicit result discard remains SLP-0002; this Draft cross-references rather than re-decides them.

## Complexity and subtraction budget

The model spends complexity only where syntax changes evaluation or storage. It rejects truthiness,
implicit numeric conversion, hidden allocation, general runtime operator lookup, and operator-only
backend paths.

## Surface displacement

The Draft replaces name-based generic operator selection with explicit interface declarations,
extends the same static selection to concrete user types, and removes the short-circuit purity walk.
It retains concrete scalar semantics and the greedy complete-expression `run` grammar. It does not
preserve an implementation shortcut merely because tests encode it.

## Drawbacks and risks

- User-defined eager operator participation may make APIs less explicit or create surprising
  generic constraints if the qualifying interface is not obvious.
- Strict checked integer operators make unchecked numerical code verbose when wrapping is intended.
- Left-to-right evaluation prevents backends from reordering observable computations.
- A broad expression page can accidentally duplicate control-flow, ownership, or value-type rules.

## Alternatives and prior art

### Status quo

Keep compiler-known scalar dispatch and distributed evaluation rules. This minimizes implementation
change but leaves generic and concrete operator behavior conceptually split.

### Smaller primitive or library solution

Remove most operator syntax and require named functions. This is expressible but gives up familiar
precedence and does not solve conditional evaluation or assignment.

### Strongest competing language model

Permit broad coercion, truthiness, dynamic operator lookup, and unspecified operand order. This can
reduce explicit source but makes types, ownership, traps, and costs context-dependent.

## Falsifiers and acceptance blockers

- A direct user-defined operator model that requires runtime dispatch or compiler-known interface
  identity
  would violate the static ordinary-operation thesis.
- Assignment must not displace the old value before destination validation and right-side
  completion; traps remain fatal rather than promising post-trap cleanup.
- Operator selection based on expected result type would make expression meaning context-dependent
  and blocks acceptance.

## Open realization questions

- Assign stable codes for operator applicability and operator ambiguity.
- Decide the smallest OpenSpec change partition that preserves one conceptual handoff while keeping
  parser/interface work, short-circuit ownership, and place replacement independently auditable.

## Future directions

Pattern-conditioned `if`, compound assignment, custom literal protocols, constant evaluation, and
unsafe arithmetic remain possible later directions unless review deliberately pulls them into this
thesis.

## OpenSpec realization map

After direction acceptance, reconcile these independently auditable slices:

| Slice | Required reconciliation |
| --- | --- |
| Expression grammar | Preserve the precedence table and greedy complete-expression `run` boundary. |
| Operator interfaces | Add explicit operator eligibility syntax; remove method-name guessing; use one static path for generic and concrete operands. |
| Short-circuit control | Delete `SEM0096` and the impurity walk; apply normal path-local Effect and ownership analysis to the right branch. |
| Assignment | Preserve destination-before-right evaluation, complete replacement, cleanup ordering, and `OWN0004`. |
| Place swap | Rename stale `Place.replace` specification text to the sealed `Intrinsic.replace` primitive unless first-class places are introduced. |
| Numeric behavior | Preserve integer traps, float behavior, exact operand typing, and named conversion APIs across engines. |
| Diagnostics and tooling | Add operator applicability/ambiguity diagnostics and expose selected interface operations without runtime-dispatch language. |

Known stale artifacts include `bootstrap-operator-semantics` and `SEM0096`, HIR comments describing
a pure right operand, multi-operation interface changes that map operator meaning from method names,
the `numeric` and `order` source interfaces that rely on those names, and one
`bootstrap-semantic-facts` requirement spelling `Place.replace`.

## Revision and decision record

| Revision | Date | Change or decision |
| --- | --- | --- |
| 1 | 2026-08-18 | Initial Draft with deterministic evaluation and ordinary static operator dispatch as the provisional thesis. |
| 2 | 2026-08-19 | Drafted the programmer-facing expression shape and deterministic evaluation rules; retained operator and short-circuit seams as explicit open questions. |
| 3 | 2026-08-19 | Confirmed the expression foundation and drafted the closed precedence, concrete scalar operator, numeric trap, comparison, Boolean, and bitwise rules. |
| 4 | 2026-08-19 | Confirmed the scalar operator batch and selected explicit operator-marked interface operations with heterogeneous operands, result types, and static overload resolution. |
| 5 | 2026-08-19 | Confirmed explicit operator participation and drafted short-circuit right operands as ordinary conditional branches without a separate purity restriction. |
| 6 | 2026-08-19 | Confirmed ordinary short-circuit branches and drafted assignment ordering, atomic place replacement, compound-assignment absence, and explicit numeric conversion. |
| 7 | 2026-08-19 | Confirmed assignment and conversion, expanded every central driving case, refreshed the privilege audit, and recorded the exact OpenSpec/compiler reconciliation frontier. |
