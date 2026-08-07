# Flow-to-Effect migration inventory

This inventory records the intentionally breaking public surface discovered before implementation.
Generic “control flow” and `/labs` data-flow terminology is not part of the rename.

## Compiler surface

- Lexical and syntax: `flow`, `FlowKeyword`, parser declaration recovery, formatter keyword output,
  syntax traversal and canonical token encoding.
- Declaration and semantic facts: `functionKind: 'Flow'`, Flow failure-row diagnostics,
  `Type.Flow`, `Type.flow`, `Type.isFlow`, Flow catch recognition, and run/fail validation.
- HIR: `FlowConstruct`, `FlowCatch`, Flow function contracts, capture access, and textual encoding.
- Ownership and instances: delayed Flow capture loans, Flow access categories, catch reachability, and
  generic Flow discovery.
- Target layout and MIR: `FlowOutcome`, `PackFlowOutcome`, `RunFlow`, catch operations, calling
  shapes, verifier text, and deterministic encoding.
- Execution and backends: `FlowOutcomeValue`, evaluator traces/errors, LLVM block names, Wasm
  outcome lowering, and facade-exposed facts.
- Public tests and fixtures: Flow parser, formatter, elaboration, ownership, HIR, layout, evaluator,
  native/Wasm runtime, and determinism fixtures and filenames.

## Tooling and documentation surface

- TextMate, CodeMirror, and generated VS Code keyword grammars.
- Unified `/labs` typed-Flow presets, labels, source snippets, and phase renderers.
- Compiler-facing docs and examples that describe the Silk Flow abstraction.

## Deliberately unchanged terminology

- Ordinary prose about control flow in LLVM and Wasm documentation.
- The `/labs` data-flow projection (`flow-model`) and its `flow` workspace pane identifier.
- LLVM/Wasm upstream APIs whose names use “flow” independently of Silk Effect.
