## Context

See [proposal.md](proposal.md) for motivation. The current frontend already distinguishes
`TextLiteral` from `ByteStringLiteral`, and `StaticText.Data` retains `Text | Bytes`, exact decoded
bytes, and UTF-8 validity. That distinction is lost at expression typing: both categories become a
shared `u8` slice, HIR `StaticTextLiteral` carries `Type.Slice`, and MIR `StaticView` exposes the
same address-and-length shape for both.

Silk already has the supporting mechanisms this change should reuse: lexical shared loans, static
data identities, target-selected slice-like calling lanes, a sealed auditable `Intrinsic`
namespace, ordinary source-defined `Bytes` and `Vector<T>` owners, and debug/release backend
profiles. The design must respect minimal compiler privilege: the compiler may understand the
language type `string`, but it may not identify or specialize the source declaration
`silk/string.String` by spelling.

## Goals / Non-Goals

**Goals:**

- Preserve a canonical text identity from source through every compiler and runtime phase.
- Reuse existing lexical-loan and static-data machinery without making `string` an alias for a
  slice.
- Give current native and Wasm targets one deterministic realization while retaining freedom for
  later targets to choose an equivalent representation.
- Implement validation, ownership, growth, scalar decoding, and normalization policy in shipped
  Silk source over a minimal intrinsic seam.
- Keep the public general-purpose text vocabulary to `string` and `String`; traversal cursors or
  Unicode result records are operations/supporting data, not additional string storage types.

**Non-Goals:**

- Expose a stable address-and-length ABI, storage identity, interning, or pointer equality.
- Add direct indexing, an ambiguous character or length unit, implicit allocation, implicit
  conversion, implicit normalization, locale policy, or platform/C string variants.
- Make `String` copy-on-write, reference-counted, target-native, or compiler-known.
- Complete grapheme segmentation, locale-sensitive comparison, or every normalization form in the
  first implementation; the API boundary must allow those ordinary stdlib features later.

## Decisions

### `string` is a non-scalar compiler type with its own canonical identity

Add source spelling `string` to the closed semantic type vocabulary without adding it to the
numeric/boolean `Scalar` catalog. Type identity, keys, comparison, substitution, concreteness,
presentation, declaration resolution, compatibility, and analysis serialization gain an explicit
string case. This avoids accidentally admitting arithmetic, scalar layout, or indexing merely
because the TypeScript representation is a string literal.

HIR text literals change from `Type.Slice` to canonical `string`. Static literal storage continues
to use the existing deterministic `StaticText.Data`, while byte literals keep the slice path.
Expressions returned by checked/unchecked construction and by `String.view` have the same semantic
type but retain runtime-backing provenance and loans.

_Alternative considered:_ represent text as nominal `silk/string.String` or recognize that
declaration by spelling. Rejected because it couples compiler correctness to a replaceable stdlib
actor and violates the sealed intrinsic boundary.

_Alternative considered:_ brand `&[u8]` only in tooling. Rejected because the brand would be lost by
ordinary type compatibility, calls, storage, and generic substitution; safe code could also create
the brand without proving UTF-8.

### A string is a lexical immutable view, not an owner or a reference wrapper

The logical value carries valid UTF-8, byte length, readable storage provenance, and a lifetime.
Static literals use program-lifetime provenance. Runtime formation borrows the complete source byte
view, and `String.view` borrows the complete initialized storage of a shared `String` owner. Loan
analysis applies the existing rule used for returned slices: the view may flow through calls and
returns only while its backing borrow remains live, and all structured exits end the loan exactly
once.

`string` is already the view value, so source never spells `&string`. Taking a second generic
reference to a string adds no useful capability and would create the type proliferation this design
is intended to avoid; declaration analysis rejects reference and slice forms whose element/target
is `string` where they would duplicate the view abstraction.

_Alternative considered:_ one `String` type with static, borrowed, and owned storage variants.
Rejected because it requires compiler-known ownership tags, copy-on-write, or representation-aware
Drop and turns an allocation-free literal into an owning-policy decision.

### MIR and layout retain a separate logical string shape

MIR gains a `String` type case and explicit operations for static string views, unchecked formation,
UTF-8 bytes, byte length, and exact equality. These operations carry provenance and loans rather
than being rewritten to ordinary slice operations. MIR verification rejects type confusion,
mutable byte exposure, missing loan ends, and unchecked formation without unsafe authorization;
it does not revalidate caller-proven UTF-8.

The layout plan gains a string representation/calling shape separate from slice representation.
For all current native and Wasm targets it selects an immutable address-provenance lane followed by
a target-sized byte-length lane, matching the efficient existing static-view realization. Type and
shape encodings continue to say `string`, not `Slice<u8>`. Source operations expose neither lane,
so a future backend may select an equivalent target-native representation after updating target
planning and parity tests.

_Alternative considered:_ permanently specify `{ pointer, usize }`. Rejected because it would make
the bootstrap lowering a public ABI and prevent a future target-native text handle.

### Four sealed intrinsic operations form the complete compiler seam

The `Intrinsic` catalog adds:

- unsafe unchecked formation from a shared live `u8` slice to `string`;
- safe immutable UTF-8 byte viewing from `string` to a shared live `u8` slice;
- safe encoded byte length;
- safe exact equality for `==` and `!=`.

The first operation establishes a new invariant and therefore requires an unsafe boundary. The
other three cannot invalidate text or ownership. Equality belongs in the sealed set because the
existing operator table resolves concrete operators to compiler-known actors; adding recognition
of a stdlib equality declaration would be a larger and less principled privilege. Its semantics are
only exact byte comparison of valid UTF-8, so it adds no Unicode or locale policy.

Each operation declares the complete current execution-target set in the intrinsic catalog. The
shared availability pass therefore admits the portable string operations for evaluator, native,
and Wasm pipelines and rejects a missing target during backend preparation before target layout and
MIR lowering. The deterministic inventory records the normalized target list alongside signature,
safety, admission, and consumer metadata.

Validation, copying, appending, capacity, scalar decoding, normalization, and grapheme policy are
excluded from the catalog. The intrinsic inventory fixture remains the auditable source of exact
signatures, unsafe classification, admission phase, and stdlib consumer.

_Alternative considered:_ lower equality to a compiler-generated byte loop. Rejected because that
hides a reusable source-callable compiler operation outside `Intrinsic` and duplicates backend and
evaluation behavior.

### The stdlib owns all validation and storage policy

Add canonical `silk/string` source defining private nominal `String` storage over `Bytes`, a typed
`InvalidUtf8` value carrying the first invalid byte offset, and actor operations including:

- validation of `&[u8]` into a borrowing result;
- effectful copy from `string` using the ordinary `Allocator` requirement and `OutOfMemory` failure;
- allocation-free `view(&String) -> string`;
- explicit UTF-8 bytes and byte length;
- construction and append operations that accept only valid `string` inputs;
- scalar traversal through a cursor/step API whose byte offset is explicit.

The safe validator walks the complete input with ordinary `u8`/`usize` operations, rejects
overlong encodings, surrogate code points, truncated sequences, and values above U+10FFFF, and
calls unchecked formation exactly once after success. A failed validation returns ordinary typed
result data and never publishes a partial view. The borrowing success inside the result remains
tied to the input slice; ownership analysis is extended if necessary to retain loans nested in
generic nominal result fields.

`String` keeps its byte owner private. Safe constructors and append operations can only introduce
validated text, so `String.view` may call unchecked formation internally after borrowing its
initialized bytes. Growth and rollback reuse `Bytes`/`Vector` and their existing allocator and Drop
contracts; there is no collection-shaped compiler operation.

_Alternative considered:_ validate in the intrinsic and return a result. Rejected because UTF-8
diagnostic detail, validation strategy, and safe wrapper shape are reusable library policy rather
than representation primitives.

### Semantic stdlib text boundaries use string

The standard library classifies every existing `&[u8]` boundary by meaning rather than preserving
the pre-string API mechanically. Complete log messages, normalized paths, path components, and
native filesystem roots are text and therefore accept or return `string`. Their implementations
borrow UTF-8 bytes only at private storage loops, binary stream writes, directory-entry decoding,
or raw OS intrinsic calls. Directory entries arriving from a native provider are validated before
they become path text through the explicitly named `Path.joinUtf8` provider interop operation;
ordinary path construction, joining, resolution, and inspection remain string-shaped.

Byte collections, whole-file contents, standard streams, and the sealed OS ABI remain byte-based:
their semantics permit arbitrary octets or require a physical encoding. The explicit
`string.utf8Bytes` operation marks each text-to-binary crossing. This keeps `string` useful at
domain boundaries without pretending that every byte buffer is text.

_Alternative considered:_ preserve all old byte-slice signatures and require callers to convert
string literals. Rejected because logging and paths only used bytes as a substitute for the text
type that did not yet exist, making the resulting API needlessly representation-shaped.

### Literal and conversion behavior is deliberately non-contextual

Every text literal has type `string`; every byte literal keeps its immutable byte-view type.
Expected-type elaboration does not turn either literal into the other or allocate `String`.
Conversions use named stdlib operations, including `String.copy`, `String.view`, safe UTF-8
validation, unchecked formation inside `unsafe`, and immutable UTF-8 bytes. Existing programs that
passed a text literal to `&[u8]` migrate by explicitly requesting its UTF-8 byte view.

This rule keeps effects and allocation visible and avoids a general coercion system solely for
strings.

### Unicode identity is exact; higher policy is versioned stdlib behavior

Valid UTF-8 has one encoding for each scalar sequence, so exact byte equality implements exact
scalar-sequence equality. Neither literals nor equality normalize or case-fold. Scalar traversal
validates no bytes because `string` already carries that invariant. Normalization, grapheme
segmentation, case mapping, and locale-sensitive comparison are explicit stdlib algorithms with
their own data versions and tests; adding or updating them cannot change `string` type identity,
layout, or ordinary equality.

### Tooling renders semantic text, not byte-shaped storage

Type presentation, semantic snapshots, HIR/MIR encoders, evaluator traces, and debugger-facing
metadata retain `string`. Values render as quoted escaped Unicode text with byte length available
as secondary detail. A byte slice renders as binary/numeric data even when its current contents are
valid UTF-8. Native debug builds emit the strongest supported UTF-8 string description in LLVM
metadata while preserving the selected physical lanes; release builds remain metadata-free.

The source-defined `String` owner is not special-cased. A later general debugger-presentation
facility may teach tools to render that nominal owner as text, and the canonical stdlib can opt in
without changing this compiler type boundary.

## Risks / Trade-offs

- [Nested borrowing result exceeds current ownership coverage] → Add focused ownership fixtures for
  `Result<string, InvalidUtf8>` and preserve the input loan through generic nominal fields before
  exposing the safe validator.
- [String accidentally reuses slice admission paths] → Give semantic, HIR, MIR, layout, and encoded
  snapshots distinct string cases and add negative indexing/mutation fixtures.
- [Unsafe construction leaks invalid UTF-8 into safe code] → Keep the primitive sealed and unsafe,
  make the stdlib validator the canonical safe entry, and test every malformed UTF-8 class.
- [Two physically equal calling shapes become interchangeable] → Verify logical type identity in
  MIR and layout in addition to lane equality.
- [Equality or traversal grows compiler Unicode policy] → Limit compiler equality to exact bytes of
  already valid input and keep every Unicode table/algorithm in stdlib source.
- [A string intrinsic is implemented by only some execution targets] → Declare and inventory the
  complete portable target set, test availability for evaluator/native/Wasm, and reject unavailable
  targets during backend preparation before target layout and MIR lowering.
- [Debuggers cannot express a native UTF-8 view directly] → Preserve canonical string metadata and
  deterministic physical components; use the strongest pinned LLVM representation without making
  debugger limitations part of source semantics.
- [The `string`/`String` casing distinction confuses users] → Document the invariant consistently:
  lowercase is a borrowed immutable language value; uppercase is the allocating stdlib owner.

## Migration Plan

1. Add the semantic string identity and analysis encodings while retaining existing byte literals.
2. Split text-literal HIR/MIR/layout/evaluation paths from static byte views and update snapshots.
3. Add the four intrinsic operations, verification, evaluator behavior, and native/Wasm parity.
4. Add lexical runtime formation and loan propagation, including nested validated results.
5. Ship `silk/string` with validation, owned storage, explicit views/copies, and scalar traversal.
6. Change text literal typing to `string`, migrate semantic stdlib boundaries to accept text, and
   add explicit UTF-8 byte views only at genuinely binary or native ABI crossings; then enable the
   breaking diagnostics for implicit use as `&[u8]`.
7. Add string-aware inspection and debug metadata, run the full release-candidate verification,
   and land the change atomically because the project is pre-release.

Rollback is a source revert of the change and migrated fixtures; there is no compatibility bridge,
serialized user format, or published ABI to preserve during the alpha stage.
