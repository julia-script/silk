# SLP-0001: Module-Level Static Value Composition

SLP: 0001
Status: Draft
Revision: 2
Author: Julia Ortiz
Created: 2026-08-18
Updated: 2026-08-18
Discussion: —
Review record: —
Depends on: —
Split from: —
Split into: —
Supersedes: —
Superseded by: —
Revisit when: Static schema composition becomes a stabilization priority or a concrete `Schema<T>` representation exists.
Resolution: —
OpenSpec handoff: —

## Summary

Explore immutable module-level values constructed by side-effect-free Silk expressions during
compilation, without introducing runtime module initialization. The motivating case is a reusable
`Schema<User>` assembled through ordinary generic interface selection and data-first composition.
The exact syntax, eligible expression subset, representation, and compiler boundary are deliberately
unresolved in this parked Draft.

## Problem and evidence

Schema libraries commonly assemble one reusable description from smaller operations. Rebuilding the
same description through a runtime factory obscures that the result is immutable program metadata,
while hand-encoding it as a type would lose the ordinary value-level API and pipeline composition.

Silk currently permits top-level constants only when they have explicit primitive types and one
literal initializer. Aggregate constants, computed initializers, type inference, and effectful
initialization are unavailable. Modules are intentionally inert: imports run no code and there is no
runtime global-initialization order. The first blocked operation is therefore the call to
`Schema.of<User>()` in a module-level constant initializer.

## Driving examples: current and desired

### Case: Publish one composed schema for a nominal type

#### Intent

Construct one immutable `Schema<User>` from ordinary schema combinators and publish it for reuse by
other modules without runtime initialization or a type-level schema language.

#### Current Silk

```silk
// Current constants reject this initializer because it is a computed aggregate expression.
pub const UserSchema: Schema<User> = Schema.of<User>()
```

The available workaround is an ordinary runtime function, which constructs or returns the schema
when called:

```silk
pub fn userSchema() -> Schema<User> {
  return Schema.of<User>()
    |> Schema.check(userCheck)
    |> Schema.annotate(userAnnotation)
}
```

#### Desired Silk

The syntax is illustrative and not a proposal decision:

```silk
interface SchemaOf {
  fn schema() -> Schema<Self>
}

impl SchemaOf for User {
  schema: User.schema
}

pub const UserSchema: Schema<User> =
  Schema.of<User>()
    |> Schema.check(userCheck)
    |> Schema.annotate(userAnnotation)
```

#### Observable result

Another module imports `UserSchema` as an ordinary immutable `Schema<User>` value. Compilation
selects the `SchemaOf for User` conformance and validates the composition. Importing the module runs no
code and creates no observable initialization order.

Whether the resulting value is embedded, inlined, shared as static data, or represented by another
target-neutral mechanism is unresolved.

#### Boundary case

An initializer that performs runtime I/O, requires a service, executes an Effect, mutates global
state, or creates a resource needing runtime cleanup must remain invalid:

```silk
// Illustrative invalid boundary.
pub const UserSchema: Schema<User> = run loadSchemaFromDisk()
```

## Goals and non-goals

### Goals

- Preserve ordinary value-level schema APIs, interface selection, and pipeline composition.
- Keep modules inert and constant evaluation order-independent.
- Make invalid static composition a compile-time diagnostic.
- Identify the smallest capability beyond primitive literal constants.

### Non-goals

- Finalize syntax during the initial stabilization pass.
- Introduce runtime module initializers, mutable globals, import-time execution, or hidden services.
- Turn schemas into types or add general type-level programming merely for this case.
- Commit to a general-purpose `comptime` language before concrete examples require it.

## Current language model

Top-level `const` declarations have explicit primitive contracts and literal initializers. Their uses
lower as immediate values or static text without runtime storage, initialization, allocation, or
cleanup. Existing module direction permits side-effect-free compile-time constants in principle,
rejects cyclic constant evaluation, and forbids runtime-owned global values and import-time code.

## Proposed language model

Provisional thesis: some ordinary Silk expressions may construct immutable aggregate values during
compilation, and a module-level constant may publish the resulting ordinary value without becoming
a runtime initializer.

This is not yet a semantic proposal. In particular, the Draft does not decide:

- whether eligibility is an expression property, a `const fn` contract, or an explicit evaluation
  boundary;
- whether interface witness selection and monomorphized generic calls are sufficient for
  `Schema.of<User>()`;
- which aggregate, allocation, callable, string, union, or recursive values may survive as constants;
- whether validation failures are constant-evaluation diagnostics or typed values;
- whether each use copies, references, or otherwise materializes the result; or
- whether the motivating `Schema<T>` representation contains runtime callables that make true
  compile-time materialization the wrong model.

## Worked language experience

Parked. The next revision should begin with a concrete `Schema<T>` data representation and at least
one consumer, then compare the static constant with the runtime-function workaround.

## Semantic sketch

Unknown beyond these provisional invariants:

- evaluating a module constant performs no runtime or import-time action;
- declaration order does not affect the result;
- cyclic evaluation is rejected with the complete declaration path; and
- effects, unresolved requirements, mutable globals, and runtime resource acquisition do not cross
  the static boundary.

## Compiler–standard library boundary

### Compiler necessity

Ordinary Silk cannot currently evaluate a computed aggregate initializer or preserve its result as
a module constant.

### Smallest target-neutral primitive

Unknown. The leading candidate is a target-neutral constant evaluator for a deliberately bounded
ordinary-Silk subset, but that may be too broad if schema values contain runtime callables.

### Standard-library construction

Schema representation, checks, annotations, composition policy, and public combinators must remain
ordinary Silk. The compiler must not recognize `Schema`, `SchemaOf`, `check`, or `annotate` by name.

### Privilege audit

Open. A future revision must compare computed aggregate constants against a smaller static-data
constructor primitive and against retaining an ordinary runtime factory function.

## Whole-language interaction map

| Surface | Disposition | Analysis |
| --- | --- | --- |
| Syntax and names | Unknown | `const` may be sufficient; exact syntax is intentionally illustrative. |
| Types and abstraction | Affected | Generic calls and interface witnesses appear in the driving initializer. |
| Execution contracts | Affected | Effects, failures, and requirements need a static-boundary rule. |
| Ownership and resources | Unknown | Aggregate ownership, allocation, copying, sharing, and cleanup are unresolved. |
| Runtime and targets | Unknown | Materialization and native/Wasm parity may decide feasibility. |
| Compiler | Affected | Current constant analysis accepts primitive literals only. |
| Standard library | Affected | Schema remains an ordinary library value and API. |
| Tooling and diagnostics | Unknown | Evaluation traces, cycles, and invalid operations need source diagnostics. |
| Learning and use | Unknown | The model must be explainable without implying arbitrary compile-time execution. |

## Scope cohesion

The current scope is one question: may an immutable module constant be constructed through ordinary
side-effect-free Silk composition? General compile-time programming, reflection, code generation,
and runtime globals are separate directions unless the driving case proves they are inseparable.

## Complexity and subtraction budget

The feature is not justified if it requires a second general-purpose interpreter, broad reflection,
hidden runtime initialization, or a parallel type-level schema language merely to avoid calling a
factory function.

## Surface displacement

If adopted, computed constants would displace some zero-argument runtime factories for immutable
program metadata. They must not displace explicit runtime construction when values depend on
services, runtime input, resources, or target state.

## Drawbacks and risks

- “Compile time” can grow into an unrestricted second execution model.
- Large or callable-bearing aggregates may have unclear target representation and cost.
- Cross-module constant dependencies introduce cycle and diagnostic complexity.
- Users may assume every pure-looking function is statically evaluable.

## Alternatives and prior art

### Status quo

Expose `fn userSchema() -> Schema<User>` and construct the value at runtime, relying on optimization
when repeated construction is undesirable.

### Smaller primitive or library solution

Permit only literal aggregate/static-data constants, leaving computed composition as runtime code.

### Strongest competing language model

Introduce an explicit general `comptime` evaluation language or macro system. This is more powerful
than the motivating case currently justifies.

## Falsifiers and acceptance blockers

- A realistic `Schema<T>` necessarily owns runtime-only closures, services, or resources.
- The same user experience can be obtained predictably through ordinary functions and optimization.
- Native and Wasm cannot materialize the same observable ordinary value without target-specific
  semantics.
- No narrow eligibility rule separates useful composition from an unrestricted second language.

## Open realization questions

- What is the concrete value representation of `Schema<T>`?
- Which calls are eligible in a constant initializer, and how is eligibility communicated?
- May static construction allocate compiler-owned memory that becomes target static data?
- How do callable fields, recursion, strings, annotations, and user validation cross the boundary?
- What diagnostic explains the first runtime-only operation in an otherwise static pipeline?

## Future directions

Compile-time reflection, derived schemas, macros, generated declarations, mutable static storage, and
runtime lazy initialization are explicitly outside this initial Draft.

## OpenSpec realization map

None while the Draft is parked.

## Revision and decision record

| Revision | Date | Change or decision |
| --- | --- | --- |
| 1 | 2026-08-18 | Parked the module-level static composition goal and illustrative schema case without selecting syntax or semantics. |
| 2 | 2026-08-19 | Reconciled the illustrative schema interface with SLP-0006's implicit `Self` provider model. Static composition semantics remain parked. |
