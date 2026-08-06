# Define the bootstrap type system and value model

Type: grilling
Status: resolved
Blocked by: 01

## Question

What exact bootstrap type system supports nominal structs, implicit-discriminant runtime unions,
minimal monomorphized generics, functions, pointers or references, arrays and slices, recursive
compiler data, and exhaustive matching while remaining coherent with the intended mature language?
The answer must also choose a coherent behavior-extension model: qualified/data-first functions,
associated methods, interface conformance, and whether any form of retroactive implementation is
allowed.

## Answer

The bootstrap language has no general subtyping, inheritance, structural conformance, user-defined
method lookup, or overload sets. Struct declarations create nominal types; `type` declarations are
transparent aliases. All bootstrap aggregates are named structs, including one-field nominal
wrappers and zero-sized singleton marker types. Anonymous tuples and records are deferred until
self-hosting provides evidence that their convenience outweighs the additional aggregate form.

Value unions are unordered, duplicate-free structural sets such as `A | B`. Each member is a
nominal type, and the active member's type is the only case identity; users do not define a second
tag name. Union order and nesting normalize away, transparent aliases with the same members denote
the same type, and compiler-known intersection and difference operations provide finite set
algebra. `Never` is the empty union and uninhabited bottom type. General conditional types,
reflection over types, and user-defined type-level computation are excluded. Absence is explicit:
`Option<T>` is the union `Some<T> | None`, using a nominal `Some<T>` wrapper so nested options do
not collapse.

Union injection and widening are contextual rather than inference-changing conversions. A nominal
`A` may be used where an immediate expected type is a union containing `A`, and an existing union
may widen to an expected union containing all of its members. A binding with no declared or other
immediate expected type retains the precise type of its initializer; a later use cannot
retroactively widen that binding. Narrowing occurs only through pattern analysis such as `match`.
Moving a non-Copy value into an owned union consumes that value.

At runtime, a bootstrap union uses a compiler-owned discriminant plus storage sized and aligned for
its largest member. Canonical nominal type identity, not alias spelling order, determines the
internal member order. Numeric tag values are not observable and the layout has no stable external
ABI or serialization promise. Niche optimization is deferred with automatic layout optimization.

The complete scalar set is `Bool`, `Unit`, `Never`, `U8`, `U16`, `U32`, `U64`, `Usize`, `I8`,
`I16`, `I32`, `I64`, `Isize`, `F32`, and `F64`. `Usize` and `Isize` follow the target pointer width;
stable data formats use fixed-width types. Numeric literals receive a type from their immediate
context and are range-checked, but existing numeric values never convert implicitly. Ordinary
integer overflow traps in every build mode; checked, wrapping, and saturating operations are
explicit. `F32` and `F64` map directly to conservative LLVM floating semantics with explicit
conversions and no implicit fast-math. Ordinary floating comparisons follow IEEE behavior;
total-order and bitwise comparisons are named operations. Wider integers and arbitrary-precision
numbers are deferred.

Only `Bool` controls ordinary conditionals, loops, and guards. There is no truthiness. An `if` may
instead use an explicit shared or exclusive refutable pattern, bind the successful member within
that branch, and refine an unguarded `else` branch by structural union subtraction. A guarded
pattern does not subtract its member because the guard may reject a value of that type.

Behavior belongs to qualified, data-first actor-module functions, optionally used through
first-argument piping. Nominal values have no user-defined instance methods. Interfaces appear only
as generic constraints and service requirements, not as storable existential values or open-ended
heterogeneous collections. Only the module defining a nominal type may declare its conformances,
and a conformance is a witness mapping from interface operations to existing actor functions; it
does not define hidden behavior or add concrete methods. Third-party combinations use nominal
adapter types.

Ordinary user-defined generics accept type parameters only and are monomorphized. Function
contracts additionally admit narrowly scoped failure-row and requirement-row parameters so
higher-order functions can preserve callback contracts without erasure; those parameters are
inferred at calls, may appear only in contract positions, and are concretized during finite
monomorphization. `[T; N]` is the sole bootstrap type form with a natural-number parameter;
arbitrary const generics, higher-kinded types, and general row-level programming are deferred. Call
arguments may determine all generic arguments, but expected return types or later uses may not. A
call supplies either the complete ordered type-argument list or no explicit type arguments; mixed
explicit/inferred argument lists are not part of the bootstrap language. Each
generic body is checked once against explicit interface constraints, compiler properties, optional
finite type-set constraints such as `T in U32 | U64`, and any declared contract-row parameters;
concrete instantiation does not enable duck typing or compile-time type branching. Copyability and
automatic cleanup are compiler-verified type properties rather than interfaces. Recursive generic
calls must preserve their type and contract-row arguments.

Function value types include parameter access modes, return type, failure row, and requirement row.
A function with smaller failure or requirement rows may be used where larger rows are expected, but
the reverse is invalid. Generic functions must be concretely instantiated before becoming values.
Named and non-capturing functions are storable. As resolved by issue 08, invoking or specializing a
`flow fn` may also create a storable typed flow value with a compiler-shaped environment containing
its supplied inputs and providers. That environment may borrow or own captures; ordinary lifetime
and affine checks determine whether the flow may escape and whether it supports shared, exclusive,
or consuming execution. This narrowly admits owned environments for flow values. General-purpose
capturing closures remain non-escaping callbacks with explicit shared or exclusive capture lists;
general move closures, arbitrary owned closure objects, and polymorphic function values are still
deferred.

Every parameter and struct field has an explicit type. Initialized local bindings and the result,
failure, and requirement contracts of non-recursive functions may be inferred from their local
declaration body; inference never consults callers or later statements. Directly or mutually
recursive functions are legal but must declare their complete contracts, and the language server
offers a code action to infer and insert those contracts for a recursive group. The same action can
make any inferred function contract explicit.

`[T; N]` is a non-allocating inline aggregate whose ownership, copyability, and cleanup derive
from `T`; its length is part of its type and it never decays to a pointer. A slice is a shared or
exclusive lexical borrow of contiguous elements rather than an owned container. Compiler-recognized
index and range place projections create element and slice borrows without permitting ordinary
functions to return them. Default indexing and slicing check bounds and trap on failure. Recoverable
access uses a non-escaping callback or copies a copyable element; unchecked access is unsafe.

Structs preserve physical declaration order in the bootstrap language while target data layout
chooses padding and alignment. Logical field and cleanup order remain declaration order. Exact
external formats use explicit encoders. A later explicit automatic-layout mode may reorder physical
storage without changing logical ownership order, but it is not part of bootstrap. Direct inline
recursive layout is invalid; recursive ownership uses explicit `Box<T>` indirection, while cyclic
and cross-linked compiler structures should use owned collections plus stable copyable IDs. `Box<T>`
access uses an explicit shared or exclusive place projection and never implicitly dereferences or
converts to another type.

Only the module defining a nominal struct may assemble it with a raw struct literal. Other modules
construct values through ordinary public actor functions exported by that module. Literal fields
are labeled and complete: every field appears exactly once, although source order need not match
declaration order. A field projection is a place. Copy fields may be read by copy, shared or
exclusive borrows may project fields, and moving an individual field out of a non-Copy struct is
excluded during bootstrap; ownership moves the whole value.

`RawPointer<T>` is a copyable, non-owning, non-null default-address-space pointer invariant in `T`.
Safe code may hold, pass, copy, and compare raw pointers, and may obtain one from a current borrow
without extending that borrow's lifetime. Address interpretation, arithmetic, dereferencing,
slice construction, integer conversion, and pointee reinterpretation require an unsafe boundary.
Integer-pointer conversions make no portable round-trip or dereferenceability guarantee; ordinary
traversal uses provenance-preserving pointer operations. Nullable addresses use
`Option<RawPointer<T>>`. Address-space parameters and implicit untyped pointers are deferred; LLVM
address spaces remain ordinary compiler data during bootstrap.

Matches may consume, share, or exclusively borrow their scrutinee, with that mode applying to every
nested binding. Consuming destructuring consumes the whole aggregate and cleans up omitted fields;
borrowed destructuring cannot move fields out. A match is exhaustive when it enumerates all remaining
members or uses an explicit universal `_` branch. Guards run in source order, never count toward
exhaustiveness, and cannot follow an already exhaustive member branch. The result type of a match is
the normalized union of reachable branch result types, with `Never` contributing no member.

Safe values are fully initialized when they come into existence. Bindings, structs, arrays, and
owned allocations cannot expose partial or uninitialized state. Low-level construction may later
use an explicit unsafe `MaybeUninitialized<T>` abstraction, which is not interchangeable with `T`.
All syntax in this answer is illustrative; concrete spelling remains the responsibility of the
bootstrap syntax prototype.

## Amendment — 2026-08-05

The data-slice planning session made contextual union widening and nominal struct construction
boundaries explicit. It also confirmed that concrete struct and union layout is selected by the
compiler's target-aware layout phase described by issue 06, not independently by a backend.
