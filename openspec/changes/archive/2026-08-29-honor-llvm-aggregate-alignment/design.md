## Context

See `proposal.md` for motivation. `DataLayout` currently stores primitive and pointer rules as immutable public records but recognizes aggregate components only by routing them through the primitive parser and discarding the result. `Type.layoutOf` computes structure alignment from fields alone. The original byte string is already retained, so exact rendering is independent of the parsed representation.

## Goals / Non-Goals

**Goals:**

- Represent the single effective aggregate rule in the same immutable, queryable style as other data-layout alignment rules.
- Decode LLVM's zero ABI spelling into an explicit one-byte `Alignment` value without weakening `Alignment` invariants.
- Keep structure layout logic local to the existing recursive layout operation.

**Non-Goals:**

- Rework duplicate resolution for primitive or pointer rules; related issues own those semantics.
- Change vector layout or add synthesized primitive alignment defaults.
- Normalize or regenerate data-layout strings from parsed fields.

## Decisions

### Store one effective aggregate specification

Add a public `AggregateSpec` record with ABI and preferred `Alignment` values, and an `aggregate` field on `DataLayout`. Initialize it to the LLVM default equivalent of `a:0:64`, then replace it as each aggregate component is parsed so the last occurrence wins.

This is preferable to storing all entries because aggregate lookup has no key and LLVM assigns the last occurrence authority. It is preferable to reusing `PrimitiveSpec` because aggregate rules have no bit width.

### Parse zero ABI spelling without admitting zero alignments

Use an aggregate-specific parser. ABI bits of zero decode to an explicit frozen one-byte alignment; they do not use `Alignment.defaultAlignment`, whose `byteUnits` are unspecified. Both encoded fields must fit LLVM's unsigned 16-bit alignment field. An omitted preferred field inherits that effective ABI alignment. An explicitly present preferred field uses the ordinary nonzero power-of-two parser and must compare greater than or equal to the effective ABI alignment.

This preserves the invariant that every `Alignment` is usable in modulo and padding calculations. Representing zero inside `Alignment` would spread sentinel handling into unrelated consumers.

Expected parser rejections use a private synchronous parse failure that carries the rejected
component to the public Effect boundary. `DataLayout.parse` translates it to `LlvmError` with an
`InvalidInput` reason; only an unexpected throw is classified as `WrappedFailure`.

### Apply the aggregate minimum to unpacked structures

For each unpacked anonymous or named structure, initialize the maximum alignment from the aggregate ABI alignment, then raise it for stronger fields and use it for final tail padding. An empty structure can therefore have nonzero ABI alignment while its zero offset and allocation size remain zero. Packed structures continue to use one-byte field and aggregate alignment.

Arrays and vectors continue returning their child-based layout directly. The parsed aggregate preferred alignment is observable metadata only; LLVM ABI allocation uses the ABI value.

### Preserve rendering through the original bytes

Keep `DataLayout.render` returning the retained input unchanged. Parsed duplicate resolution therefore affects queries without altering byte-exact round trips.

## Risks / Trade-offs

- [The default preferred aggregate alignment is observable even when no component was written] → Document and test the pinned `a:0:64` default explicitly.
- [A broad structure-layout edit could accidentally pad empty structures or affect packed structures] → Keep zero-offset padding at zero, branch only on `packed`, and add focused regression tests.
- [Alignment validation can drift from LLVM] → Cover zero ABI, explicit zero preferred, power-of-two, ordering, the 16-bit field boundary, and repeated-rule cases with pinned-oracle expectations.

## Migration Plan

No compatibility layer is needed under the green-field policy. Update the public value, all constructors, layout logic, tests, and delta specification atomically. Reverting the change restores the previous parser and layout behavior if validation exposes an oracle mismatch.
