## Context

Silk has references (`&T`, `&mut T`) and slices with lexical loans, `RawBuffer<T>` as an affine
owner over one `Allocation`, and no address type. The layout layer already has an `Address` calling
scalar (`CallingShape.AddressScalar { element, bits }`), `NativeType` maps it to an LLVM pointer,
and `WasmLanes` treats borrow addresses as `i32`. Every borrowed root of any type already has
authoritative storage in the native backend: `NativeFunction.discoverRoots` collects every
`BeginLoan` root, gives it a byte alloca, every write and definition also stores to that storage,
and `NativeCall.callSynchronous` and the `OsCall` arm reload every address root after the call. The
direct-Wasm backend reloads only roots reachable through borrow-address lanes
(`WasmLanes.carriesBorrowAddress`). The evaluator models references as `ReferenceValue { frame,
cell, selectors, indexes }`, slices as `SliceValue` with a base and length, and raw-buffer-backed
slices by an allocation `ticket` rather than a frame cell; `BootstrapPlace.selectStored` and
`placeAccess` resolve places, and `blockedStep({ _tag: 'Trap' })` is the as-data violation.

Builtin scalars and shared borrows are Copy through `ConformanceProof.copyProof` and
`Layout.entry.copy`, not through `impl Copy`. Intrinsic entries have fixed `semanticParameters`
and no type-class bounds; `RawBuffer.read`'s Copy rule is the source wrapper's `<T: Copy>` bound
plus `isCopy` in `MirVerification`. `TypeCompatibility` has a `ReferenceAccess` arm for
exclusive-to-shared reborrow consumed by `HirLowering`. `Star` is a token; `Grammar.typeStarts`
decides whether `<` after a callee opens a generic argument list. `Type.isReference` has about
thirty call sites.

The user chose the `*const T` / `*mut T` syntax over a nominal `Pointer<T>` type.

## Goals / Non-Goals

**Goals:**

- One new `Type` variant with the smallest primitive set that makes pointer-taking C functions
  usable and testable end to end.
- Engine parity for every primitive on programs without foreign calls, so the evaluator keeps
  covering pointer-using Silk code.
- Reuse the existing address-root storage and reload machinery unchanged.

**Non-Goals:**

- Provenance or aliasing analysis for pointers; validity is the unsafe caller's obligation.
- Casts, comparison, function pointers, C-layout records, safe libc wrappers.

## Decisions

### `Type.Pointer` is a new variant; primitives live in a `Pointer` intrinsic actor

```ts
{ _tag: 'PointerType', mutable: boolean, pointee: Type }
```

The variant joins `fold`, `key`, `encode`, `substitute`, surface encoding, the inspectors, and
every predicate chain that today asks `isReference` or `isSlice` (type inference, cleanup planning,
`copyProof`, layout, execution affinity, presentation, completion). It is Copy through `copyProof`
and `Layout.entry.copy` the way builtin scalars are, needing no `impl Copy`. The primitive set is
one `Intrinsic` actor, `Pointer`, with all-target operations, each unsafe one carrying its
`invariant` and an arm in the admission switch; `silk/pointer.silk` is the documented public API
over it, per the minimal-intrinsic rule.

`offset` and `offsetMut` are two primitives because an intrinsic signature cannot be polymorphic
over mutability; this follows `RawBuffer.view` / `viewMut`.

The nominal alternative (`silk/core Pointer<T>` over an opaque representation, like `OsHandle`)
would avoid the variant but leave `*const T` unspellable and make every pointer API a method call;
the user chose the syntax.

### The Copy rule for `read` and `write` lives in the source wrappers and MIR verification

The catalog cannot express a Copy bound, so `pub unsafe fn read<T: Copy>(pointer: *const T) -> T`
and `write<T: Copy>` in `silk/pointer.silk` reject a move-only pointee at the call, exactly as
`RawBuffer.read` does, and `MirVerification` applies `isCopy` to the pointee of every pointer read
and write operation as the backstop for direct intrinsic use.

### Formation is a safe address-of over the borrow's lane

`fromRef` / `fromMutRef` / `fromSlice` / `fromMutSlice` take the borrow's address lane and return it
as the pointer lane. They are safe because forming an address has no memory effect; the borrow's
loan ends where it would anyway. Ownership treats the call as an ordinary read of the borrow and
records no loan on the pointer. This is Rust's `&x as *const T` rule.

### Observability reuses the address-root storage and post-call reload

Because a pointer is formed from a borrow, its root is already an address root with authoritative
storage, every write to the root already stores to that storage, and every synchronous call already
reloads every address root afterwards. The extern change's `ForeignCall` arm performs the same
reload. Nothing new is computed in MIR; the extern design's reload requirement is what makes a C
write visible. The direct-Wasm backend reloads only borrow-reachable roots, so its reachability
gains pointer lanes to keep parity when a Silk callee writes through a `*mut` parameter.

The alternative considered first, a per-function set of pointer-formed roots carried on MIR, was
dropped after review: the native backend already materializes every borrowed root and reloads all
of them after every call.

### Evaluator pointer values

`PointerValue` is null, `{ frame, cell, selectors, indexes, offset }` for a pointer formed from a
frame-backed borrow, or `{ ticket, offset }` for one formed from a raw-buffer-backed view.
`fromRef` copies the `ReferenceValue` fields; `fromSlice` copies the `SliceValue` base or ticket as
the offset; `offset`/`offsetMut` add elements; `read` and `write` resolve the cell through
`selectStored`/`placeAccess` or the ticket path and produce the `Trap` blocked step when the frame
is gone or the pointer is null. The evaluator therefore detects dangling pointers that native code
cannot; the spec states this as permitted, not required, behavior for other surfaces.

### Foreign ABI admission adds one arm

`CAbi.admit` accepts `PointerType` without examining the pointee; `classify` maps it to the LLVM
pointer type and includes mutability in the signature key so `*const u8` and `*mut u8`
redeclarations disagree. When `add-export-c-functions` has landed, export thunks forward the
pointer lane unchanged.

### `*mut T` to `*const T` is an immediate-boundary conversion

Added beside the `ReferenceAccess` arm in `TypeCompatibility` and consumed at the same `HirLowering`
site, applied only at immediate expected-type boundaries. Nothing else converts.

### Parsing

`Parser/Type.ts` gains a prefix arm on `Star`: expect `ConstKeyword` or `MutKeyword`, then
`parseType`. A bare `*` reports missing mutability and continues with the pointee. `Star` joins
`Grammar.typeStarts` so `identity<*const i32>(x)` opens a generic argument list instead of a
comparison; `x.*` referent projection is `Dot`-prefixed and unaffected. The formatter prints
`*const T` / `*mut T`.

## Risks / Trade-offs

- [Evaluator dangling detection diverges from native] → Documented as evaluator-only detection of
  an unsafe-contract violation; parity is required only for well-defined programs.
- [Copy bound on read/write blocks moving values through raw memory] → Deliberate for V1; a
  `RawBuffer`-style slot protocol over pointers is a later change if needed.
- [Thirty-odd predicate sites are easy to miss and do not all fail typecheck] → Task 1.3 lists the
  known groups; the surface round-trip, inference, cleanup, and inspector tests cover behavior.
- [`Star` in `typeStarts` changes generic-argument disambiguation] → Only `<` followed by `*` is
  affected, which no expression grammar produces today; a fixture covers `a < *b` staying an
  expression outside call-argument position.
- [Wasm reload reachability grows] → Only lanes typed as pointers are added; unrelated roots are
  untouched.
