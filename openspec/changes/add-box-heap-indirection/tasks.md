## 1. Inline-Reach Dependency Graph

- [ ] 1.1 Add the inline-reach walk and its monotone per-parameter fixed point over struct
      declarations: descent stops at `RawBuffer<T>` and `Slot<T>`, and enters a user generic's type
      argument only when that parameter is reached inline by the generic's own fields.
- [ ] 1.2 Use the walk for SCC neighbour extraction at `DeclarationIndex.ts:2784-2791`, replacing
      the `Type.nominals` call.
- [ ] 1.3 Use the same walk and the same fixed point for the `selfEdge` test at
      `DeclarationIndex.ts:3685-3693`, so both cycle sites agree.
- [ ] 1.4 Leave `Type.nominals` and the reported `dependency` list at
      `DeclarationIndex.ts:3706-3714` unchanged, so a field still names every type it references.
- [ ] 1.5 Confirm the fixed point is order-independent and produces byte-identical facts across
      fresh processes, matching the determinism the declaration index already promises.

## 2. Box in Canonical Silk Source

- [ ] 2.1 Add the canonical `silk/box` module: `Vacant`, `Occupied<T>` over `RawBuffer<T>`, and
      `pub struct Box<T> { storage: Vacant | Occupied<T> }`.
- [ ] 2.2 Add `Box.make<T>(value: T) -> Box<T> ! OutOfMemory ? &mut Allocator`, allocating one
      element through `Allocator` and writing the value into slot zero.
- [ ] 2.3 Add the borrow and consume accessors (`get`, `getMut`, `into`) so a boxed value is
      reachable without unsafe code at the call site.
- [ ] 2.4 Add `impl<T> Drop for Box<T>`: move the storage out with `Intrinsic.replace`, match the
      union, and release the occupied case through `Slot.dropValue` before the buffer releases.
      No failures and no requirements, per `DeclarationIndex.ts:3450-3459`.
- [ ] 2.5 Register the module and its prelude alias in the standard-library manifest and regenerate
      the compiler-shipped source table.

## 3. No Compiler Privilege

- [ ] 3.1 Confirm no new intrinsic, no `Type.intrinsicNominals` entry, no `Layout` branch, and no
      `CleanupPlan` node were added, and that neither backend changed.
- [ ] 3.2 Confirm `Box<T>` is `MoveOnly` for every `T` through the existing fall-through in
      `Ownership.categoryOf` (`Ownership.ts:258-281`), with no ownership change.
- [ ] 3.3 Confirm `Ownership.cleanupPlan`'s `seen` guard (`Ownership.ts:1671-1672`) is never reached
      on a `Box`-mediated cycle, so no owner silently becomes `NoCleanup`.

## 4. Acceptance

- [ ] 4.1 Accept `struct Expr { left: Box<Expr> }` with no diagnostic, and accept a mutual cycle
      that passes through `Box` across two modules.
- [ ] 4.2 Keep rejecting `struct Node { next: Node }`, `struct Node { next: Pair<Node> }` where
      `Pair<T> { value: T }`, and `struct Node { anchor: [Node; 0] }` with `SEM0020`.
- [ ] 4.3 Add the leak test: a 3-level `Box` tree asserting acquire count equals release count on
      the evaluator, with the same value from the Wasm backend and the native LLVM backend. Write it
      so it fails when the `Drop` hook is removed — the unhooked tree traces 6 acquires against
      2 releases while still compiling clean and returning the right answer.
- [ ] 4.4 Pin that a polymorphically recursive `Box` shape is rejected with `SEM0053` by the
      existing finite-discovery check rather than diverging.
- [ ] 4.5 Pin that a deep `Box` chain exhausts the stack without leaking.
- [ ] 4.6 Run the full compiler suite and report any test whose expected diagnostics change.

## 5. Specification

- [ ] 5.1 Amend `bootstrap-struct-types` to define inline reach and name explicit indirection as the
      sanctioned way through a cycle, keeping the clause that forbids the compiler to add
      indirection silently.
- [ ] 5.2 Amend `bootstrap-target-layout` to pin that indirected element layout is not a layout
      dependency of its holder.
- [ ] 5.3 Amend `bootstrap-ownership` to state that an indirected value is released by its holder's
      `Drop` hook at runtime, never by the statically unrolled plan.
- [ ] 5.4 Add the `bootstrap-silk-stdlib` requirement for `Box<T>` as ordinary source with no
      compiler privilege.
