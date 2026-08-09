## 1. Concrete language surface

- [x] 1.1 Add the `const` token and lossless `[pub] const name: type = literal` CST grammar with bounded recovery
- [x] 1.2 Add canonical constant formatting and syntax/lexer/formatter regression coverage
- [x] 1.3 Publish constant syntax coherently through syntax inspection and language highlighting

## 2. Declaration and resolution facts

- [x] 2.1 Add canonical constant facts to the flat declaration index with visibility, duplicate, type, literal, and provenance data
- [x] 2.2 Resolve local, selected-import, and qualified constant uses with existing visibility and collision semantics
- [x] 2.3 Add declaration, hover, navigation, and occurrence presentation for constant definitions and references

## 3. Semantic and executable lowering

- [x] 3.1 Validate boolean, integer, `usize`, and floating initializers against explicit primitive types and target ranges
- [x] 3.2 Elaborate accepted constant references into existing typed HIR/MIR immediate values while rejecting invalid value operations
- [x] 3.3 Prove evaluator, native LLVM, and direct WebAssembly parity with no runtime storage, allocation, or cleanup delta
- [x] 3.4 Add fresh-process artifact and backend determinism coverage for local and imported constants

## 4. Real-program evidence

- [x] 4.1 Replace representative repeated literals in the lexer and stack VM with typed constants
- [x] 4.2 Preserve oracle, malformed-input, allocation-failure, cleanup, three-engine, and determinism evidence
- [x] 4.3 Record the resulting findings and update the project and real-program roadmaps

## 5. Verification

- [x] 5.1 Run focused constant and pressure-program tests
- [x] 5.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`
- [x] 5.3 Run strict validation for the change and the complete OpenSpec tree
