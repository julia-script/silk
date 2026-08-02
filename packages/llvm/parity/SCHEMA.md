# LLVM parity manifest schema

`manifest.json` is the release ledger for the pinned Zig LLVM sources. `schemaVersion` selects this
schema. `baseline` records the immutable Zig commit, all three SHA-256 source hashes, and the
authoritative LLVM toolchain. `inventory` identifies the mechanically extracted source inventory
and its hash. Each inventory item is covered by exactly one rule selected by `inventoryKind`.

Rules use one of three dispositions:

- `implemented`: the port exposes equivalent supported semantics.
- `intentional-deviation`: the JavaScript/Effect representation differs while the emitted LLVM
  semantics remain compatible.
- `upstream-unsupported`: the pinned source marks the construct TODO, unreachable, panic-only, or
  otherwise outside its supported behavior.

Implemented and intentional-deviation rules require actor, test, and fixture evidence. Unsupported
rules require documentation evidence. Fixture classes are `exact-bytes`, `canonical-semantic`,
`malformed-input`, `boundary`, `determinism`, `llvm-roundtrip`, and `benchmark`. Validation rejects
unknown categories or dispositions, duplicate source IDs, uncovered inventory entries, rules with
no entries, missing actor files or tests, stale hashes, and duplicate rule IDs. Validation also
checks the repeated-sample benchmark record, its six required workload classes, the traced versus
untraced decision, and verifies that production source contains no unmeasured `Effect.fnUntraced`.
