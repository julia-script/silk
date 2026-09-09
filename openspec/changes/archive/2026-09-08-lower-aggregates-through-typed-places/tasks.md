## 1. Baseline and representation inventory

- [x] 1.1 Reconfirm the current source/toolchain/WIP identities and capture three uninstrumented cold parser/CLI samples using `benchmarks/selfhost-stages/run.py`; verify all runtime oracles pass and retain all samples without overlapping our builds/tests.
- [x] 1.2 Promote the required external census measurements into an opt-in diagnostic harness without production-build traversal; verify opcode totals reconcile with the immutable LLVM snapshot and instrumented binaries retain their runtime results.
- [x] 1.3 Inventory every consumer of local lane vectors, including generated helper/thunk bodies, and record the concrete type-to-direct/place classification and union view rules in the design; verify the inventory covers all `Mir.Type` variants and every NativeStorage read/write caller before changing their contracts.
- [x] 1.4 Define the planner-owned value-storage view model with role-qualified canonical identity, slot placements, size/alignment, and member mappings; verify outcome and capture-environment identities remain distinct and no backend handles enter deterministic plan encoding.
- [x] 1.5 Plan and verify EffectOutcome storage from existing OutcomeShape tags, carrier types, and logical payload mappings; verify the minimal EffectOutcome regression now has an available storage view, and cover mixed-width/address/floating carriers, zero-payload outcomes, invalid mappings, and overflow on supported native and Wasm targets.
- [x] 1.6 Expose complete represented-composite stored and calling-carrier views with explicit active-alternative mappings while retaining existing layouts; verify differing capture layouts project within their selected extent and unsupported/missing facts fail before backend emission.
- [x] 1.7 Plan bindings between value storage and existing call/suspension transport, preserving actual start-offset padding, signatures, and valid frame/transfer offsets (correcting proven undersized descriptor/environment reservations in planning); verify slot correspondence and incompatible-view conversion with structural tests before any typed-place migration.

## 2. Typed value and place foundation

- [x] 2.1 After tasks 1.4–1.7 pass, define internal lowered-value and typed-place actors that consume the verified storage views, including layout/view identity, storage base, alignment, and projections; verify typechecking plus structural classification tests for scalars, descriptors, structs, arrays, unions, represented values, and zero-sized owners.
- [x] 2.2 Implement canonical aggregate destination allocation and field/array projection using existing layout facts; verify field reads consume the projected address and padded/dynamic-index cases retain their selected offsets and checks.
- [x] 2.3 Implement explicit copy/move/destination construction with conservative storage reuse and overlap handling; verify independent mutable copies, partial-move initialization, and zero-sized ownership through structural tests and distinct cases in the shared corpus.

## 3. Ordinary local lowering

- [x] 3.1 Migrate struct/array construction, scalar field reads, and subaggregate value access to typed places; verify aggregate field access no longer reads unrelated fields or creates a complete cached lane vector.
- [x] 3.2 Migrate mutable aggregate writes, address-taking, and pointer/foreign alias refresh behavior to canonical places; verify post-write reads observe the same backing bytes and no aggregate-wide reload is emitted merely to refresh a cache.
- [x] 3.3 Migrate branch/loop joins and aggregate result destinations, preserving compatible lifetime and initialization; verify joins consume valid predecessor storage and do not retain expired stack addresses.
- [x] 3.4 Migrate union construction, match projection, active payload materialization, and cleanup mutation transfer; verify padded mixed-variant carrier mappings in both directions with existing layout and backend tests.

## 4. Call and return boundaries

- [x] 4.1 Introduce the single explicit planned-lane materialization operation and migrate direct call arguments/results and function parameters/returns; verify caller/callee signatures still match the existing calling shapes and aggregate payloads enter canonical destinations.
- [x] 4.2 Migrate indirect calls, applicable C exports/callbacks, and ordinary generated wrappers; verify planned C classifications with existing independent C fixtures and structural signature assertions, without adding a backend-private ABI.
- [x] 4.3 Audit LLVM block-copy emission and helper capability demand; verify object/import evidence on native and LLVM-to-Wasm targets, including supported libc-none compositions, and ensure undeclared helper requirements are rejected by the existing gate.

## 5. Effects and resource lifetimes

- [x] 5.1 Migrate concrete inline and represented Effect/callable capture construction and access, including borrowed environments; verify capture ownership classification and invoke/drop contracts with structured lowering assertions.
- [x] 5.2 Migrate Effect outcomes and normal/failure propagation while retaining diagnostic ownership and shared completion cleanup; verify selective catch, moved-out diagnostics, and exact-once cleanup using existing tests and distinguishing shared-corpus cases.
- [x] 5.3 Migrate suspension spill/restore, resume thunks, and frame-base projection rebinding; verify persistent payload addresses survive parking and no projection uses the previous stack base.
- [x] 5.4 Migrate cancellation/drop and transfer-only runners; verify success, typed failure, parked cancellation, partial initialization, and borrowed temporary lifetimes through the existing native corpus and intended LLVM-to-Wasm tests.

## 6. Integration and acceptance

- [x] 6.1 Remove obsolete aggregate lane caches, refresh loops, superseded helpers, and backend-owned outcome packing decisions, and update all callers, tests, fixtures, and documentation; verify the consumer inventory is closed and full aggregate lane materialization is confined to explicit call/transport conversions rather than local cache refresh.
- [x] 6.2 Run `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, and `pnpm test` in order, then `pnpm check`; run every native acceptance shard and `pnpm release:candidate` if package contents/exports change. Record exact results and distinguish pre-existing worktree gate failures from regressions.
- [x] 6.3 Run the parser/stdlib fixture corpus and bootstrap AST differential check with the completed compiler; verify grammar/recovery, flat-tree invariants, token ownership, and AST output on the unchanged real input.
- [x] 6.4 Repeat the opt-in census and at least three uninstrumented cold builds each of parser-only and CLI; verify reduced aggregate traffic, retain all timing/CPU/RSS/hash evidence, and investigate regressions or inconclusive timing before claiming an optimization. Include scalar/descriptor compilation controls and representative debug/optimized runtime checks outside the correctness timing suite.
