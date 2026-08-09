## Why

Silk currently parses `run foo()` as an expression but rejects it when it appears as a standalone
statement, even when it produces unit. The resulting `Unexpected token sequence` diagnostic is not
actionable and recovery can misparse the following valid statement as a phantom declaration,
producing an unrelated “Expected `{`” cascade.

## What Changes

- Accept standalone expressions in statement position when their result is compatible with `()` or
  `never`, including the natural effectful-entry sequence `run foo()`.
- Diagnose a non-unit expression statement with its inferred type and actionable alternatives to
  bind, return, or explicitly consume the value.
- Preserve expression statements as first-class CST, semantic-fact, and HIR nodes, and execute them
  in source order across evaluation, native, and direct WebAssembly paths without inventing a
  binding, return, or drop.
- Format expression statements as ordinary block statements and retain their source provenance in
  analysis and inspection surfaces.
- Make unexpected-token diagnostics name the encountered source token or construct and the parser
  context, while keeping malformed statement recovery inside its owning block.
- Suppress parser diagnostics that arise only because recovery from one primary statement error
  synthesized dependent structure.
- **BREAKING**: enrich the structured reason data and user-facing messages for unexpected-token
  diagnostics; consumers that match the current generic diagnostic payload or exact message must
  adopt the contextual form.

## Capabilities

### New Capabilities

- `bootstrap-expression-statements`: Standalone unit or diverging expressions, their semantic and
  ownership rules, first-class HIR representation, execution, formatting, and cross-engine parity.

### Modified Capabilities

- `bootstrap-diagnostics`: Contextual unexpected-token reporting and one-primary-error block
  recovery that does not cascade into phantom declarations or dependent missing-token diagnostics.

## Impact

- Parser block dispatch, statement recovery, CST node kinds, and canonical formatting.
- Semantic facts and diagnostics for statement-result compatibility.
- HIR statement modeling, encoding, verification, analysis queries, ownership and layout traversal,
  MIR lowering, evaluator behavior, and native/Wasm parity coverage.
- Diagnostic structured reasons, presentation snapshots, syntax-inspector output, and tests that
  currently assert the generic `Unexpected token sequence` message.
