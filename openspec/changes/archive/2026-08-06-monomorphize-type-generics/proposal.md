## Why

The accepted algorithmic slice still fixes every data shape at declaration time, while the next
memory boundary requires one `Vector<T>`, `Slice<T>`, `RawPointer<T>`, and `Slot<T>` implementation
to work for many element types. Type-only monomorphized generics are therefore the first focused
dependency: without them, every later memory abstraction would either be compiler magic or be
duplicated per element type.

## What Changes

- Add angle-bracket type parameters to nominal struct and function declarations, generic type
  applications, inferred call specialization from supplied arguments, and explicit specialization
  such as `identity<Token>(value)` and `Vector.empty<Token>()`.
- Keep generic inference intentionally one-way: a call either supplies the complete ordered type-
  argument list or infers all arguments from supplied call values; expected return types and later
  uses may not contribute. Reject missing, conflicting, excess, or unconstrained type arguments
  with deterministic diagnostics.
- Check each generic body once over canonical type parameters using the existing compiler-owned
  Copy and cleanup properties. Do not re-check a specialization through concrete duck typing or
  permit type-directed source branching.
- Extend canonical type identity, HIR, ownership facts, cleanup plans, runtime instance keys, and
  target-layout discovery with normalized concrete type arguments. Generic declarations remain in
  generic-aware HIR; every reachable MIR function and runtime type is concrete and monomorphic.
- Discover specializations from the entry worklist, record an instance before following its
  dependencies, and require recursive generic calls to preserve their normalized type arguments so
  instance discovery stays finite and deterministic.
- Realize multiple concrete instances through logical evaluation, native LLVM, and direct
  WebAssembly without adding runtime type descriptors, erased generic containers, backend-selected
  layouts, or polymorphic function values.
- Extend deterministic encoders, differential fixtures, analysis queries, and the unified `/labs`
  workbench with generic declaration, specialization, instance, layout, ownership, MIR, evaluation,
  and backend provenance.
- Preserve the reserved template boundary: `<...>` after a recognized declaration name, type, or
  callee is generic syntax; JSX-like `<Tag...>` and `<>...</>` remain reserved only where the parser
  expects a primary expression.

## Capabilities

### New Capabilities

- `bootstrap-type-generics`: Defines type-only generic declarations and applications, argument-led
  inference, explicit specialization, generic-body checking, finite monomorphization, and the
  absence of runtime generic representation.

### Modified Capabilities

- `bootstrap-syntax`: Parses and recovers angle-bracket type parameter lists, generic type
  applications, and explicit call specialization without consuming reserved template starts.
- `silk-source-formatting`: Formats generic declarations and applications idempotently while
  preserving comparison expressions, template reservations, and damaged syntax.
- `bootstrap-semantic-facts`: Publishes canonical type-parameter binding, substitution, inference,
  specialization, and diagnostic facts.
- `bootstrap-hir`: Retains canonical type parameters and generic calls in typed HIR while keeping
  every specialization traceable to its declaration and arguments.
- `bootstrap-ownership`: Checks a generic body once using compiler-owned type properties and
  substitutes its target-neutral ownership and cleanup proof into concrete instances.
- `bootstrap-instances`: Replaces empty instance argument lists with normalized concrete type
  arguments and performs finite deterministic specialization discovery.
- `bootstrap-target-layout`: Plans layouts only for reachable concrete generic instances and reuses
  one canonical specialization identity across compiler consumers.
- `bootstrap-mir`: Lowers only verified monomorphic instances while preserving the originating
  generic declaration and concrete type-argument provenance.
- `bootstrap-evaluation`: Executes concrete generic struct and function instances without an
  interpreter-owned generic representation.
- `bootstrap-backend`: Emits deterministic native and WebAssembly definitions for concrete
  specializations using the compiler-selected layout and symbol identity.
- `bootstrap-compiler-driver`: Adds multi-specialization, invalid-inference, recursion, and
  fresh-process generic programs to the differential corpus.
- `bootstrap-analysis-facade`: Exposes generic declarations, substitutions, discovered instances,
  layouts, and cross-phase specialization provenance from the immutable snapshot.
- `bootstrap-syntax-inspector`: Adds coordinated generic syntax, semantic, instance, ownership,
  layout, MIR, evaluation, and backend inspection to the existing `/labs` workbench.
- `language-codemirror`: Highlights generic declaration and application punctuation consistently
  with comparison and reserved-template contexts.
- `language-textmate`: Tokenizes the accepted generic surface consistently in TextMate and generated
  VS Code grammars.

## Impact

This change affects parser disambiguation and formatting; canonical types and semantic facts; HIR,
ownership, instance discovery, target layout, MIR, evaluation, native LLVM and direct WebAssembly
emission; deterministic symbols and encoders; the driver corpus; analysis facade; and unified labs.
`InstanceKey` and related encodings change incompatibly because type-argument lists become
meaningful. No interface constraints, failure- or requirement-row parameters, natural-number
generics beyond existing fixed arrays, higher-kinded types, compile-time type branching, generic
runtime dictionaries, or memory-allocation feature is introduced in this change.
