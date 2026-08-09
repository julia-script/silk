## Context

See proposal.md — Why. Static literals already elaborate as shared `u8` slices, lower into canonical
static data plus address/length lanes, and evaluate as `StaticViewValue`. Slice length accepts both
runtime and static views, but evaluator place selection accepts only `SliceValue`, so a valid
`SliceElementSelector` reaches a representation mismatch.

## Goals / Non-Goals

**Goals:**

- Preserve the existing allocation-free static representation while making it fully slice-readable.
- Align verifier, evaluator, LLVM, and Wasm behavior at the indexed-read boundary.
- Turn CRC-32 into the end-to-end regression for real committed bytes.

**Non-Goals:**

- Add mutable static storage, pointer identity guarantees, or an owning String.
- Generalize raw pointer arithmetic or add a separate byte-literal collection type.
- Change runtime slice indexing semantics.

## Decisions

### D1: Keep static literals typed as shared `u8` slices

No new source or HIR type is introduced. The existing static-data identity is a storage provenance
detail; source indexing continues to use the ordinary slice syntax and bounds contract. A nominal
`StaticBytes` type would duplicate slice APIs and prematurely expose placement policy.

### D2: Make slice-element reads polymorphic over the two valid evaluator view values

Evaluator `ReadPlace` handles `SliceElementSelector` by inspecting the root's semantic slice type and
then selecting from either:

- `SliceValue`, which resolves through a frame cell and base offset; or
- `StaticViewValue`, which resolves directly through its immutable decoded bytes.

Both paths perform the same `usize` bounds check and produce the same scalar `u8` value. Static views
never receive frame/cell identity because they are compiler-owned and immutable.

### D3: Verify from semantic type and selector shape, not evaluator tags

MIR verification continues to operate before runtime values exist. It accepts a slice-element
selector only when the root local has compatible shared-slice type, the selected element is `u8`,
the index is `usize`, and the dominating check/provenance facts are present. Static-data identity
and length must agree with the program table.

### D4: Reuse backend slice-address lowering

LLVM and Wasm already materialize static data as address/length lanes. Indexed reads should flow
through the same checked slice-address path rather than adding a literal-specific load operation.
Target tests pin valid reads, bounds traps, and absence of runtime allocation.

### D5: Make CRC-32 the integration proof

CRC-32 changes its parameter from `[u8; 4]` to a shared byte view and passes
`b"\x99\x13\x1d\x00"`. Its loop, polynomial, and expected checksum remain unchanged, isolating this
change to representation and indexed access.

## Risks / Trade-offs

- [Fixing only the evaluator hides a verifier/backend disagreement] → Add invalid-MIR and both-backend
  bounds tests before changing the algorithm status contract.
- [Static and runtime slice paths drift again] → Share selector validation and bounds helpers where
  their representation-independent inputs match.
- [Text literals accidentally gain scalar-value indexing semantics] → The contract is byte indexing;
  UTF-8 scalar/grapheme behavior remains for the future String design.
