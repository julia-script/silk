## Context

See `proposal.md` for motivation. The current compiler has a closed semantic type vocabulary,
non-generic declaration facts and HIR functions, and `InstanceKey.typeArguments` permanently set to
an empty array. Instance discovery follows canonical call targets, layout plans concrete nominal
types before MIR, and every evaluator/backend consumer already receives monomorphic target-aware
MIR. That existing phase boundary matches the Wayfinder model and should be deepened rather than
replaced.

The next memory sequence depends on reusable `Slice<T>`, `Slot<T>`, `RawPointer<T>`, and `Vector<T>`
actors, but this change deliberately ends before borrowing or allocation. It must provide enough
generic structure for those actors without introducing their runtime behavior early.

## Goals / Non-Goals

**Goals:**

- Represent canonical declaration-owned type parameters and applied nominal types without strings
  as semantic identity.
- Check generic declarations once and instantiate only reachable concrete uses.
- Preserve generic provenance across HIR, ownership, instance discovery, layout, MIR, evaluation,
  emission, deterministic artifacts, and inspection.
- Keep MIR and every runtime consumer concrete, monomorphic, backend-neutral, and target-aware.
- Make the accepted angle syntax deterministic beside comparisons and the reserved template
  boundary.

**Non-Goals:**

- Interface or type-set constraint syntax, contract-row parameters, const generics beyond fixed
  arrays, higher-kinded types, specialization by implementation, compile-time reflection, or
  type-directed source branching.
- Partial explicit type-argument lists, expected-result inference, default type arguments, generic
  entry points, or polymorphic function values.
- Slices, pointers, slots, allocation, drop hooks, vectors, or bulk memory operations.

## Decisions

### Extend canonical types with parameters and applied nominal arguments

Add a declaration-owned type-parameter identity containing the canonical owner and source ordinal.
Extend nominal type identity with an ordered immutable argument list rather than introducing a
second unrelated applied-type graph. Keys and encodings recurse through those arguments, so
`Box<I32>` and `Box<Token>` are distinct while repeated `Box<I32>` uses intern identically.

An open parameter is valid in generic-aware declaration and HIR types but invalid in layout, MIR,
evaluation, and backend types. This keeps the phase boundary mechanically verifiable.

Alternative: represent parameters and substitutions as source names. Rejected because imports,
shadowing, alpha-renaming, and nested declarations would make equality textual and fragile.

Alternative: clone a nominal declaration for each application during elaboration. Rejected because
it would check bodies repeatedly, obscure source identity, and allow concrete behavior to influence
generic validity.

### Parse generic calls only in the complete call-specialization shape

Declaration parameter lists and type-position applications are contextually unambiguous. In an
expression, parse `<Type, ...>` as explicit specialization only after a qualified callee and only
when its closing `>` participates in the immediately following call postfix. Comparison parsing
otherwise retains the existing precedence rules. The parser records recovery locally when the
generic list begins but is damaged.

This accepts `Vector.empty<Token>()` and leaves primary-expression `<Tag...>` / `<>...</>` starts to
the reserved template path. It also gives formatting one canonical spelling without adding Zig-like
sigils or a second `::<>` syntax.

Alternative: resolve angle ambiguity from name lookup. Rejected because parsing and formatting must
remain useful for incomplete or semantically invalid source.

Alternative: require `::<T>`. Deferred as the fallback already discussed; the contextual grammar is
smaller and the committed comparison corpus will expose whether it is inadequate.

### Require all explicit arguments or infer all arguments

A call with angle arguments supplies exactly the declaration's complete ordered list. A call with no
angle arguments gathers equations solely from supplied value arguments against parameter types,
recursing through fixed arrays and applied nominal structure. Every occurrence of one parameter must
resolve to the same canonical type. After substitution, the existing argument-compatibility rules
run normally.

Expected returns, assignment destinations, and later uses never feed inference. A parameter absent
from value-parameter positions therefore requires explicit specialization. This makes inference
local, deterministic, and easy to explain while supporting ordinary `identity(value)` calls.

Alternative: partial explicit arguments plus inferred suffixes. Rejected because it adds placeholders
or positional omissions without enabling the planned memory actors.

Alternative: bidirectional expected-type inference. Rejected because it makes later context alter
call identity and diagnostic ownership.

### Check open parameters conservatively once

Declaration elaboration adds canonical parameters to its type environment and constructs one HIR
body. Without a declared constraint surface in this slice, an open parameter is treated as
move-only and potentially cleanup-bearing: whole moves, storage, calls, and returns are valid, while
operations requiring Copy or nominal/interface behavior are unavailable. Concrete instantiation
cannot retroactively validate rejected operations.

Ownership records symbolic cleanup/property facts over open types. Substitution resolves those
facts for a concrete instance, allowing `Container<I32>` to become Copy when its complete concrete
shape is Copy without allowing the generic body to copy unconstrained `T`.

Alternative: re-run elaboration and ownership for every concrete type. Rejected because it is
template instantiation rather than checked generics and permits accidental duck typing.

### Carry substitutions on HIR calls and discovered instances

Generic declarations gain ordered parameter identities. HIR call operations retain the canonical
target and an ordered type-argument vector that may still mention the caller's parameters. Instance
discovery begins with a concrete entry, substitutes the current instance arguments into each
reachable call, and keys the target by canonical declaration plus normalized concrete arguments.

The worklist records a key before scanning its body. A recursive call is valid only when its
post-substitution arguments equal the current recursive declaration's arguments. This preserves the
existing finite recursion algorithm and rejects polymorphic expansion before it reaches the
worklist.

`InstanceKey.typeArguments` becomes canonical semantic types rather than display strings; its text
key and encoders use `Type.key`. `Instance` retains the generic HIR declaration plus the concrete
substitution instead of cloning source HIR.

Alternative: use symbol text as the worklist key. Rejected because symbols are an emission product,
not semantic identity.

### Substitute completely before layout and MIR

Concrete instance discovery feeds a specialization view that recursively substitutes contracts,
locals, struct fields, fixed arrays, calls, and cleanup facts. Layout planning creates entries only
for concrete reachable applied nominal types and derives their fields from the generic declaration
under that substitution. Open generic declarations remain inspectable but have no physical layout.

MIR lowering accepts an instance plus substitution and rejects any residual parameter. MIR call
targets become concrete instance keys rather than declaration-only identities. Deterministic symbols
derive from the canonical declaration and encoded concrete argument list. Evaluation, LLVM, and
WebAssembly therefore receive no generic operation to interpret.

Alternative: preserve generic MIR and let each backend specialize. Rejected because it duplicates
reachability, layout, symbol, cleanup, and diagnostic decisions and violates the compiler-owned
target-aware boundary.

### Make generic provenance a coordinated inspector dimension

Syntax, semantic facts, HIR calls, ownership, instances, layouts, MIR functions, traces, and emitted
symbols retain canonical links through the snapshot and analysis facade. `/labs` adds these facts to
its existing coordinated panes and selection model. No browser-side specialization or standalone
generic inspector is introduced.

## Risks / Trade-offs

- [Angle parsing accepts an unintended specialization-shaped comparison] → Gate expression generic
  parsing on a complete type list plus following call postfix, retain comparison regression fixtures,
  and keep `::<T>` as a future syntax escape only if real source demonstrates ambiguity.
- [Canonical type recursion becomes inconsistent across actors] → Centralize parameter, substitution,
  key, equality, normalization, and encoding operations in the `Type` actor and require every phase
  to consume those identities.
- [Substitution leaks an open parameter into runtime phases] → Add explicit concrete-type predicates
  and fail verification before layout, MIR completion, evaluation, or emission.
- [Recursive specializations expand without bound] → Reject argument-changing recursive generic
  calls during generic checking and record every concrete instance before following dependencies.
- [Concrete Copy behavior accidentally changes generic validity] → Check open parameters
  conservatively once and permit only symbolic compiler-owned property substitution, never source
  re-elaboration.
- [Instance/symbol changes destabilize deterministic artifacts] → Sort by canonical declaration and
  recursive type keys, update committed encodings intentionally, and gate fresh-process output.
- [The change grows into the memory roadmap] → Acceptance uses generic structs and functions only;
  all borrowed and allocated types remain explicit non-goals and separate OpenSpec changes.

## Migration Plan

1. Extend syntax and canonical type/declaration facts while retaining unavailable recovery states.
2. Add generic semantic binding, complete-or-inferred call substitutions, and generic-aware HIR.
3. Generalize ownership and instance discovery, replacing permanently empty instance arguments.
4. Specialize concrete layouts and MIR from discovered substitutions.
5. Extend evaluation, native LLVM, direct WebAssembly, facade, `/labs`, formatting, highlighting,
   deterministic encoders, and differential fixtures.
6. Update affected goldens and document the resulting focused memory dependency chain in the project
   roadmap when the change archives.

The project is unreleased; rollback is a normal revert of this change and its coordinated golden
updates. No compatibility adapter for old empty-argument instance encodings is retained.
