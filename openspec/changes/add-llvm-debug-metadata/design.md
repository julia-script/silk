## Context

LLVM metadata has a separate identity space from values and types. It mixes structurally uniqued nodes, intentionally distinct nodes, inline nodes, local references, named collections, cycles, and forward references. The pinned builder also changes naming and attachment behavior when stripping is enabled. These concerns build on a complete instruction model but remain isolated from executable semantics.

## Goals / Non-Goals

**Goals:**

- Preserve metadata identity, cycles, numbering, flags, and attachments deterministically.
- Make forward-reference failures explicit before any text or bitcode is returned.
- Make stripped output omit debug state by construction.

**Non-Goals:**

- Interpret DWARF beyond fields and flags supported by the pinned builder.
- Synthesize debug information automatically from function bodies.
- Support metadata node kinds not represented by the pinned baseline.

## Decisions

### Use one Metadata actor with explicit constructors

Metadata owns public handles and sibling constructors for strings, tuples, debug node kinds, named metadata, distinct nodes, and forward references. Node payloads are readonly semantic records; callers never supply raw bitcode metadata codes. Optional metadata uses normal TypeScript absence publicly and a compact sentinel only internally.

### Separate uniqued, distinct, and forward tables

Uniqued nodes use canonical structural keys and return an existing identity. Distinct nodes always allocate a new identity. A forward-reference table stores unresolved or resolved state and the permitted target category. Resolution is a gated one-time mutation. Output first computes reachable metadata and fails if any reachable forward reference is unresolved or invalid.

Alternative considered: replace references in every user when resolved. That is expensive and error-prone for cycles; indirection keeps identity stable.

### Preserve insertion identity and resolve at serialization edges

Metadata identities follow builder insertion order, matching the upstream model. Text and bitcode adapters dereference forward entries while assigning encoded indices and distinguish inline from numbered nodes. Cycles are traversed with explicit visited state rather than unguarded recursion.

### Store attachments by semantic owner

Global attachments live with the global entry. Instruction attachments and debug locations live in the committed function body keyed by semantic instruction handle, not emitted instruction ordinal. Encoding derives final ordinals after excluding arguments and block markers, preventing attachments from drifting when pseudo-entries are present.

### Apply stripping before metadata allocation

The builder's immutable mode is either strip or preserve. In strip mode, debug constructors return a canonical absent result where permitted, names are omitted according to pinned behavior, and attachments are not stored. Encoding does not build then filter a debug graph, so stripped modules cannot retain dangling debug references.

### Keep debug flag types closed

DI and subprogram flags are private-representation value actors with named constructors and combinators. Text and bitcode mappings are exhaustive over supported fields, including zero or default behavior and mutually exclusive enum fields.

## Risks / Trade-offs

- [Metadata graphs can be cyclic and deep] → Use iterative traversal with visited sets and explicit forward resolution.
- [Distinct and uniqued semantics are easy to conflate] → Expose separate constructors and test identity behavior independently from serialization.
- [Stripping can change value numbering] → Build executable numbering independently and compare stripped and preserving semantic fixtures.
- [LLVM debug schemas evolve quickly] → Pin node fields and record codes to the selected upstream and LLVM baseline.

## Migration Plan

Apply after add-llvm-advanced-ir. Introduce metadata identity primitives and forward references first, then debug node families, named metadata, global attachments, instruction locations and attachments, and strip-mode coverage. Existing modules without metadata remain semantically stable.
