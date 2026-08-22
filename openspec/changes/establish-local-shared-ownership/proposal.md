## Why

Silk cannot currently represent several independently owned local handles to one value without
weakening lexical borrowing or losing exactly-once cleanup. This first slice establishes the
semantic identity and ownership category that every later SLP-0002 slice depends on.

Source: [SLP-0002, revision 6](../../../proposals/0002-allocation-backed-local-shared-ownership/proposal.md),
SHA-256 `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`,
realization slice 1 of 6.

## What Changes

- Add one opaque generic local-shared core type with a sealed non-Copy ownership category.
- Publish semantic facts for local shared ownership, explicit strong-handle obligations, and local
  execution affinity without naming a standard-library wrapper.
- Permit moves through ordinary callables, Effects, and frames in one same-thread local execution
  domain while publishing the canonical domain-level affinity fact that later execution and
  parallel-transfer slices will consume. This slice adds no Scheduler policy, transfer syntax, or
  transfer diagnostic.
- Preserve ordinary ownership for the contained `T`; sharing the handle never makes `T` Copy.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-semantic-facts`: publish the opaque local-shared core, ownership role, and execution-affinity facts.
- `bootstrap-ownership`: classify every local-shared core handle as affine and track one obligation per live handle.

## Impact

This changes semantic type facts, ownership classification, specialization, diagnostics, and
inspection encodings. It is a prerequisite for `add-local-shared-control-block-allocation` and
`add-local-shared-lifecycle-operations`; it does not yet add construction, clone, access, cleanup,
or backend execution.
