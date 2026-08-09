## Context

See `proposal.md` for motivation. The reduced two-Vector aggregate returns correctly through
ordinary calls, Effect runners, and in-place recursive cleanup. The failure appears only in the
full VM control-flow shape. LLVM preallocates byte-addressable storage for every root that may be
borrowed, but `materializedAddressRoots` is a TypeScript set populated while emitting blocks. Once
an exclusive borrow is encountered in any emitted branch, every later emitted call reloads that
root from address storage—even on runtime paths that never executed the materializing branch. The
valid VM path therefore replaces the empty diagnostic Vector with uninitialized lanes before
returning it. LLDB observes the invalid tag in `Vector<VmDiagnostic>.drop`.

## Goals / Non-Goals

**Goals:**

- Keep native address-root state valid independently on every runtime path.
- Preserve ordinary SSA/mutable-root values before a borrow and callee mutations after an actual
  exclusive call.
- Retain compiler-planned lane layouts, ownership, and cleanup without collection-specific logic.

**Non-Goals:**

- Changing Silk borrowing rules, MIR control flow, aggregate return conventions, or Effect outcome
  layout.
- General structural-union `Slot.copy`, nested dynamic reference places, named constants, or shared
  Vector reads.
- Rewriting the completed stack VM back to separate vectors as part of the compiler repair.

## Decisions

### D1: Characterize ordinary transport before the branch-sensitive failure

The focused matrix retains a small two-owner return fixture that proves aggregate transport itself
is sound, plus the original stack VM transformed to separate step and diagnostic vectors. The VM
fixture is the regression because it has an address-taken diagnostic root whose exclusive borrow is
reachable in malformed branches but untaken by the valid sentinel path.

Changing the flattened aggregate ABI was rejected: the small fixture, evaluator, and Wasm all show
that the selected result and parameter lanes are correct. Changing Effect outcome payload slicing
was rejected because the outcome shape is positional here and the corrupted root is already visible
in native address reload state.

### D2: Give every known address root valid private storage from function entry

LLVM continues to allocate one byte-addressable frame slot per address root. It initializes every
planned lane in that slot to the representation's zero value at function entry. This makes broad
post-call reloads memory-safe even before a root's defining operation has executed; a definition
then immediately synchronizes the real complete value into the slot.

A compile-time materialized set was rejected because emission order cannot represent runtime path
state. Per-root runtime initialization flags were rejected because they add a conditional branch at
every call while conveying no semantic value: the storage is compiler-private and address roots are
already a pay-for-use set.

### D3: Synchronize address roots at definition and reload them after calls

After an operation defines a local that is statically known to become an address root, LLVM writes
all its lanes to both ordinary mutable storage and the byte-addressable slot. `BeginLoan` still
materializes immediately so intervening ordinary mutations are captured. Calls reload every known
address root from valid storage, removing the path-insensitive materialization guard; actual
exclusive callees therefore write back exactly as before.

This keeps the repair local to native lowering. It does not inspect Vector, union tags, generic
arguments, or cleanup plans, and it preserves the existing direct Wasm frame model.

### D4: Test both untaken and taken exclusive paths

The regression must prove the original untaken diagnostic branch no longer corrupts cleanup. A
smaller fixture also takes an exclusive-borrow branch and observes the callee's mutation so zero
initialization cannot accidentally mask writeback. Evaluator cleanup traces remain balanced, and
native plus Wasm execution must agree.

## Risks / Trade-offs

- **[Risk] Entry initialization adds stores for address-taken roots.** → Limit it to roots already
  selected by native borrow/capture analysis; ordinary scalar and non-borrowed aggregate functions
  emit no extra work.
- **[Risk] Definition synchronization misses a root update without a destination.** → Keep
  `BeginLoan` materialization and existing `WritePlace`/post-call synchronization; add focused tests
  for definition, taken mutation, untaken branch, and later cleanup.
- **[Risk] Zero lanes accidentally become an observable source value.** → MIR ownership and
  definition rules remain authoritative; zero storage is private and is overwritten when the root
  is defined. Tests inspect semantic outputs, not the private seed.
- **[Risk] The repair changes deterministic native artifacts.** → Update only the focused expected
  artifacts if required and retain fresh-process byte equality.

## Migration Plan

This is an internal native correctness repair with no source migration. Land the focused regression
and lowering fix together. Rollback restores the prior path-insensitive behavior and regression
failure; no persisted format or public API depends on the repair.
