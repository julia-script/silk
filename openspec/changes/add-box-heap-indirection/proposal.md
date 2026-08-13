## Why

Silk has no heap indirection, so no struct can name itself. `SyntaxTree`, `Hir.Expression`,
`Type.Type`, and `Mir` are all recursive trees, and a self-hosted compiler cannot hold one of them
today. `pub struct Expr { left: Expr }` is rejected with `SEM0020`, and that rejection is correct:
`bootstrap-struct-types` forbids the compiler to "silently add indirection". What is missing is a
way for a program to ask for indirection explicitly. This is the self-hosting gate (#19).

Two claims in the issue that shaped its plan were measured against `main` (30310de) and are false or
incomplete, so this proposal replaces the mechanism rather than restating it.

- **The issue claims intrinsic nominals already break the cycle.** They do not.
  `Type.nominals` (`Type.ts:694-713`) descends into a nominal's type *arguments*, so
  `RawBuffer<Node>` yields `[RawBuffer, Node]` and `Node` survives the `byKey.has` filter at
  `DeclarationIndex.ts:2790`. Measured on `main`: `struct Node { next: RawBuffer<Node> }` produces
  `SEM0020` with `dependency: Unavailable`. The cycle check needs a real change.
- **The issue's `Box` shape cannot carry the `Drop` hook it also requires.**
  `Ownership.cleanupPlan` returns `NoCleanup` unless `byCanonical` yields a `StructDeclaration`
  (`Ownership.ts:1673-1680`) and reaches `HookCleanup` only through a `SourceConformanceWitness`
  (`Ownership.ts:1703-1706`). An intrinsic nominal has no struct declaration and no source `impl`,
  so requirement 1 and requirement 4 of the issue are not simultaneously satisfiable as written.

Admitting the recursion without a working runtime cleanup path is the trap the issue itself names:
cleanup plans are statically unrolled to constant byte offsets (`WasmBackend.ts:1189-1204`), so the
plan releases the top level and silently leaks everything below it. That leak was reproduced: a
3-level tree built from a `Box` with its `Drop` hook removed compiles with **zero diagnostics**,
returns the right answer, and traces **6 acquires against 2 releases**.

## What Changes

- **`Box<T>` is ordinary standard-library Silk source**, not a compiler-intrinsic nominal:
  a struct over `RawBuffer<T>` with an `impl<T> Drop for Box<T>` hook, built the way `Vector<T>`
  already is. The compiler learns no new type and gains no new cleanup plan node, and both backends
  are untouched. See `design.md` for the rejected intrinsic alternative and the trade.
- **Struct cycle detection distinguishes *inline* reach from *mentioned* reach.** A field's
  neighbours become the nominals its layout actually requires: descent stops at `RawBuffer<T>` and
  `Slot<T>`, whose layouts are fixed and element-independent (`Layout.ts:644-651`), and descent into
  a user generic's type argument happens only when that parameter is itself reached inline, computed
  as a monotone fixed point over the declarations. `struct Node { next: Box<Node> }` is accepted;
  `struct Node { next: Node }` and `struct Node { next: Pair<Node> }` still fail with `SEM0020`.
- **`bootstrap-struct-types` is amended** so its "cycle consisting only of inline struct fields"
  clause names explicit indirection as the sanctioned escape and defines "inline" precisely. The
  clause forbidding the compiler to silently add indirection is kept verbatim.
- **The leak becomes a pinned acceptance test** rather than an unwritten assumption: a 3-level tree
  asserts acquire count equals release count on the evaluator, the Wasm backend, and the native
  LLVM backend.

`Box<T>` is `MoveOnly` for every `T` with no ownership change — `Ownership.categoryOf`
(`Ownership.ts:258-281`) has no nominal branch, so every nominal already falls through to
`MoveOnly`. `Box.make` allocates through `Allocator` and fails with `OutOfMemory`; the drop path
stays requirement-free and failure-free as the hook rules demand (`DeclarationIndex.ts:3450-3459`),
because releasing needs neither.

## Capabilities

### Modified Capabilities

- `bootstrap-struct-types`: define the inline-reach dependency graph, name explicit indirection as
  the sanctioned way through a cycle, and keep rejecting cycles that are inline throughout.
- `bootstrap-target-layout`: pin that a struct reaching itself only through indirection lays out,
  and that indirected element layout is not a layout dependency of its holder.
- `bootstrap-ownership`: state that a value reachable only through indirection is released by its
  holder's `Drop` hook at runtime, never by the statically unrolled plan.
- `bootstrap-silk-stdlib`: ship `Box<T>` as canonical ordinary Silk source with no compiler
  privilege.

## Impact

Affects struct dependency analysis and its two cycle-detection sites in `DeclarationIndex`, the
standard-library manifest and its generated source table, and acceptance tests. It adds no compiler
intrinsic, no cleanup plan node, no backend code, and no change to `Ownership.cleanupPlan`.

Out of scope, unchanged: inline recursion stays rejected; no `Rc`, `Arc`, or shared ownership; no
cyclic graphs — `Box` gives trees only; no arena allocator (#22). A deep `Box` chain exhausts the
stack, which is accepted behaviour, and it must not leak.
