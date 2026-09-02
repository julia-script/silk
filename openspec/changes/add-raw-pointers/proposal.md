## Why

After `add-extern-c-functions`, Silk can call native scalar functions but cannot pass or receive an
address, which rules out nearly every useful libc and platform API. `RawBuffer<T>` is not a
substitute: it is an affine owner of one allocation with initializedness obligations, not a C
address. References carry lexical loans foreign code cannot honor. Silk needs a value that is
exactly a machine address: Copy, nullable, un-owned, un-borrowed, and unsafe to dereference. This
change adds `*const T` and `*mut T` as a first-class type, admits them into the foreign ABI, and
proves them against pointer-taking C functions. It depends on `add-extern-c-functions` and, for
thunk forwarding, on `add-export-c-functions`.

## What Changes

- Add the types `*const T` and `*mut T` for any concrete pointee `T`, including opaque struct
  types. A raw pointer is Copy, may be null, owns nothing, borrows nothing, and carries no
  validity or lifetime guarantee. `*mut T` converts to `*const T` at immediate expected-type
  boundaries; no other implicit conversion exists.
- Add a sealed primitive set under `Intrinsic` and a source module `silk/pointer` exposing
  `Pointer`: `null<T>()`, `isNull`, `fromRef` / `fromMutRef` from `&T` / `&mut T`, `fromSlice` /
  `fromMutSlice` from `&[T]` / `&mut [T]`, and the unsafe `offset` / `offsetMut` (element-count
  arithmetic on `*const` / `*mut`), `read` and `write`. The `Pointer` source API bounds `read` and
  `write` to Copy `T`, and MIR verification enforces the same rule. Forming a pointer is safe and
  ends no loan early; dereferencing is unsafe.
- Native code writing through a pointer formed from a Silk place is observable by subsequent Silk
  reads of that place: a borrowed root already has authoritative memory storage and is reloaded
  after every call, and a foreign call reloads it the same way.
- The evaluator models a pointer as a logical address into its frames, executes the intrinsics with
  engine parity, and treats a dereference of a dead frame or null as an unsafe-contract violation.
- The foreign-ABI admission relation admits `*const T` and `*mut T` for any pointee, lowering to
  the C pointer class; extern and export signatures may use them.
- Pointers lower to one address lane of target pointer width on LLVM and Wasm and join layout,
  MIR, ownership, surfaces, formatting, and tooling as a new type variant.

## Non-goals

- **No struct layout promise.** Pointing at a Silk struct is allowed; C reading its fields is not
  defined until C-layout records exist.
- **No pointer-to-integer casts, pointer comparison beyond null, or function pointers.**
- **No safe wrappers over libc.** The corpus proves the mechanism; stdlib adoption is later.
- **No change to `RawBuffer`, references, or slices** beyond pointer formation from them.

## Capabilities

### New Capabilities

- `bootstrap-raw-pointers`: the pointer types, their value semantics, the primitive set and its
  safety split, formation from places, the observability rule for foreign writes, evaluator
  behavior, and the `silk/pointer` source API.

### Modified Capabilities

- `bootstrap-syntax`: pointer type syntax parses losslessly and recovers locally.
- `silk-source-formatting`: pointer types have one canonical layout.
- `bootstrap-foreign-functions`: raw pointers join the admitted V1 subset for imports and exports.
- `bootstrap-ownership`: raw pointers are Copy, carry no cleanup, and form without extending or
  ending a loan.
- `bootstrap-mir`: MIR carries the pointer type and the pointer operations.
- `bootstrap-backend`: both backends realize pointers as one address lane and materialize
  pointer-formed roots in memory around foreign calls.
- `bootstrap-instances`: pointer-bearing instance keys include the canonical pointee and
  mutability.

## Impact

- **Parser and formatter.** `PointerType` node from `*` then `const` or `mut` then a type in
  `Parser/Type.ts`, with `Star` added to the type-start table so generic arguments such as
  `identity<*const i32>` parse; formatter layout.
- **Type layer.** New `Type.Pointer` variant threaded through `fold`, `key`, `encode`, `substitute`,
  the predicate chains (`Type.isReference` has about thirty call sites, including type inference,
  cleanup planning, the Copy proof, layout, execution affinity, presentation, and completion),
  `ModuleSurface`, `DeclarationCollection`, `CallResolution`, inspectors, and `SyntaxTree`.
- **Layout and MIR.** Pointer-width scalar in `Layout`, `Mir.Type` variant, new MIR operations for
  the intrinsics, verification (including the Copy rule for read and write) and encoding.
- **Ownership.** Copy property; pointer formation reads the borrow without a loan.
- **Evaluator.** `PointerValue`; intrinsic execution; dangling and null detection.
- **Backends.** `NativeType` address lane, `NativeOperation` arms reusing the existing
  address-root storage and post-call reload; `WasmLanes`/`WasmBackend` arms and pointer lanes in
  the Wasm reload reachability.
- **Intrinsic catalog.** New `Pointer` actor with all-target operations; `CAbi.classify` pointer arm.
- **Stdlib.** `silk/pointer.silk` with documented wrappers and doctests; manifest entry.
- **Tests.** Second native corpus: `malloc`, `free`, `memcmp`, `memcpy`, `strlen`, `read`, `write`,
  `close`, plus a C fixture that mutates through a pointer.
- **Docs.** `values-and-types.md` pointer section, FFI section update, glossary, stdlib reference.
