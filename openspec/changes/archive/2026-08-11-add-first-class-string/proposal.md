## Why

Silk currently decodes text literals as valid UTF-8 but gives them the same semantic type as an
immutable byte slice, so their textual identity disappears as soon as they flow through bindings,
calls, MIR, or tooling. The language needs a first-class text view that preserves UTF-8 semantics
without moving ownership, allocation, Unicode policy, or collection behavior into the compiler.

## What Changes

- **BREAKING**: Introduce lowercase `string` as a compiler-known immutable, lexically borrowed,
  valid-UTF-8 value type distinct from `&[u8]`.
- **BREAKING**: Give text literals type `string`; byte-string literals remain immutable byte views.
- Preserve `string` identity through semantic facts, HIR, MIR, target planning, evaluation,
  backends, calling shapes, and debug metadata instead of lowering it early to a byte slice.
- Keep the physical representation abstract. Native and Wasm targets may initially realize a
  `string` as an address plus target-sized byte length, but source cannot observe that ABI.
- Provide explicit byte length, UTF-8 byte viewing, scalar traversal, and exact equality. Direct
  indexing, generic `length`, and implicit Unicode normalization are not supported.
- Add minimal string intrinsics for unchecked formation, immutable bytes, byte length, and exact
  equality. Declare them portable across the current evaluator, native, and Wasm execution targets,
  and keep target availability visible in the auditable intrinsic inventory. Safe stdlib validation
  returns a typed invalid-UTF-8 value.
- Add ordinary stdlib `String` ownership over valid UTF-8 storage. `String` can copy a `string` and
  expose an allocation-free lexical `string` view, but the compiler does not recognize the stdlib
  declaration by module or type spelling.
- **BREAKING**: Migrate semantic-text standard-library boundaries to `string`: complete logging
  messages, normalized path construction and resolution, path text accessors, and native
  filesystem roots. Keep byte slices at explicit UTF-8 conversion, binary I/O, and native ABI
  boundaries.
- Keep every conversion explicit: no implicit allocation and no implicit coercion between
  `string`, `String`, and byte views.

## Capabilities

### New Capabilities

- `bootstrap-string`: The lowercase `string` semantic type, its invariants and operations, and the
  ordinary owning stdlib `String` abstraction built over the minimal compiler boundary.

### Modified Capabilities

- `bootstrap-static-text`: Text literals become first-class `string` values while byte literals
  retain their existing byte-view behavior.
- `bootstrap-hir`: HIR retains the distinct `string` type, storage provenance, and lexical loan.
- `bootstrap-mir`: MIR represents and verifies string formation, views, calls, and explicit
  conversions without treating strings as ordinary slices.
- `bootstrap-target-layout`: Target planning selects an abstract string calling shape and current
  target realization without publishing a source ABI.
- `bootstrap-evaluation`: Evaluation preserves string identity, exact equality, UTF-8 access, and
  the unchecked-construction unsafe contract.
- `bootstrap-backend`: Native and Wasm emission preserve string behavior, provenance, deterministic
  artifacts, and textual debug presentation.
- `bootstrap-intrinsic-boundary`: The sealed intrinsic catalog gains only the target-neutral
  primitives required to form and inspect a string view.
- `bootstrap-silk-stdlib`: Shipped source gains the ordinary `String` owner, safe UTF-8 validation,
  explicit view/copy functions, and Unicode-facing operations.
- `bootstrap-operator-semantics`: String equality compares exact Unicode scalar/UTF-8 sequences
  without implicit normalization.

## Impact

The change affects lexer/elaboration type assignment, the compiler type vocabulary, ownership and
borrow analysis, HIR and MIR, target layout and calling shapes, evaluation, LLVM and Wasm emission,
debug information, analysis snapshots, and compiler fixtures. It adds shipped Silk stdlib source
for owned strings and UTF-8 validation. Existing semantic-text APIs now accept `string` directly;
source that intentionally crosses into bytes, such as file contents, standard streams, or native
OS intrinsics, must call the explicit UTF-8 byte-view operation instead.
