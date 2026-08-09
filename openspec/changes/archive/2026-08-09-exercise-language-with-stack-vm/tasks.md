## 1. Visible stack VM

- [x] 1.1 Add the visible stack-VM example, README, manifest, and categorized findings report with lexer comparison slots
- [x] 1.2 Implement the bounded instruction set, fixed operand stack, invalid-program recovery, and owned trace/diagnostic event stream in ordinary Silk
- [x] 1.3 Provide the allocator explicitly from an effect entry and publish stable observation and fingerprint functions

## 2. Differential evidence

- [x] 2.1 Add the canonical TypeScript reference VM and centralized opcode/diagnostic mappings
- [x] 2.2 Add table-driven valid arithmetic, taken/untaken branch, and bounded-loop corpus cases with exact step comparison
- [x] 2.3 Add malformed opcode, operand, stack, jump, and step-limit cases with exact diagnostic and recovery comparison

## 3. Engine and ownership evidence

- [x] 3.1 Add representative valid and malformed evaluator, native LLVM, and direct WebAssembly fingerprint parity
- [x] 3.2 Sweep every exercised event-stream allocation-failure ordinal and assert typed failure, balanced cleanup, and repeat determinism
- [x] 3.3 Add fresh-process artifact determinism and assert MIR/backends contain no VM-, opcode-, or stack-specific primitive

## 4. Findings and verification

- [x] 4.1 Complete every findings category and explicitly compare repeated or contradicted lexer evidence
- [x] 4.2 Update project and real-program roadmaps from the completed VM evidence
- [x] 4.3 Run focused acceptance tests, `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and strict OpenSpec validation
