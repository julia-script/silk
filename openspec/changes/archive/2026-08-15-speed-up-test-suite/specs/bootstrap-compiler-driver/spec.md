# bootstrap-compiler-driver Delta

Fresh-process determinism is consolidated: instead of every feature area re-proving byte-identical
artifacts in spawned child processes, designated canary gates prove fresh-process determinism for
the full artifact surface, and every feature area keeps its determinism evidence through repeated
in-process compilation byte-compared against committed goldens. Per-feature engine-agreement
obligations are discharged through the aggregate differential corpus.

## MODIFIED Requirements

### Requirement: Determinism gates are enforced continuously

The test suite CI runs SHALL enforce the pinned gates: identical compiler, source snapshot,
target, profile, and toolchain inputs produce byte-identical syntax, HIR, and MIR textual
encodings and LLVM bitcode. Fresh-process determinism SHALL be proven by a small designated set of
canary gates that together exercise the full artifact surface — native and WebAssembly release
backends, standard-library imports, generics, stored callables, and conditional conformances —
each compiling its program in at least two spawned compiler processes and byte-comparing every
published artifact. All other determinism evidence SHALL be collected through repeated in-process
compilation compared against committed goldens; feature areas MUST NOT add further fresh-process
determinism gates.

#### Scenario: Gate the four encodings

- **WHEN** the determinism suite runs
- **THEN** syntax, HIR, and MIR encodings and the bitcode digest are all byte-compared against committed goldens and repeated runs

#### Scenario: Canaries prove fresh-process identity

- **WHEN** a canary determinism gate compiles its program in two fresh compiler processes
- **THEN** every published artifact, including those of imported standard-library modules, is byte-identical across the processes

### Requirement: Differential gates execute terminating recursion

The compiler driver corpus SHALL execute representative direct recursion, mutual recursion, generic
same-argument recursion, and recursion over a mutable slice through evaluation, native LLVM, and
direct WebAssembly. Completing programs SHALL agree on results and caller-visible mutations, while
repeated compiler artifacts remain deterministic.

#### Scenario: Compare recursive quicksort engines

- **WHEN** the committed in-place quicksort recursively partitions its mutable slice
- **THEN** evaluation, native execution, and direct WebAssembly produce the same sorted fingerprint

#### Scenario: Preserve monomorphic recursive identity

- **WHEN** a generic recursive function calls itself with its current concrete type arguments
- **THEN** one monomorphic instance is reused while each runtime invocation receives a distinct activation frame

### Requirement: Aggregate differential and determinism gates remain continuous

The driver corpus SHALL include valid, invalid, nested, empty, reordered, cross-module, moved,
projected, and cleanup-bearing aggregate programs. Native execution, WebAssembly execution, and MIR
evaluation SHALL agree where applicable, and repeated compilation SHALL preserve diagnostics, HIR,
layouts, MIR, symbols, IR, WAT, and bitcode exactly.

#### Scenario: Run the aggregate parity corpus

- **WHEN** continuous checks execute the aggregate corpus on supported targets
- **THEN** every valid program agrees across evaluation and available backends and every invalid program preserves its expected phase-owned diagnostics

### Requirement: Control DAG artifacts are deterministic

Repeated compilation SHALL preserve semantic loop facts, HIR regions, ownership fixed points,
cleanup plans, MIR DAG nodes and topological encoding, evaluation traces, symbols, LLVM IR and
bitcode, WAT, and WebAssembly bytes exactly for equivalent inputs.

#### Scenario: Repeat nested-loop compilation

- **WHEN** one nested-loop program is compiled repeatedly for supported targets
- **THEN** every compiler-owned artifact is identical and backend-local control conversion is deterministic

### Requirement: Structural-union artifacts are deterministic

Repeated compilation SHALL preserve source and semantic union facts, normalized identities, HIR,
ownership, instance order, layouts, calling shapes, MIR mappings, traces, symbols, LLVM IR and
bitcode, WAT, and WebAssembly bytes exactly for equivalent inputs.

#### Scenario: Repeat equivalent union compilations

- **WHEN** equivalent union programs compile repeatedly for supported targets
- **THEN** every compiler-owned artifact and backend-private realization is byte-identical

### Requirement: Exhaustive-match artifacts are deterministic

Repeated compilation SHALL preserve match syntax, facts, coverage sets, HIR regions, ownership,
instance order, layouts, MIR, traces, symbols, and backend artifacts exactly for equivalent inputs.

#### Scenario: Repeat a guarded match corpus

- **WHEN** equivalent guarded and nested matches compile repeatedly for supported targets
- **THEN** every compiler-owned artifact and backend-private realization is byte-identical

### Requirement: Composed acceptance artifacts are deterministic

The compiler SHALL retain deterministic source closure, semantic, HIR, ownership, instance, layout,
MIR, evaluation, native, and WebAssembly artifacts for the compiler-shaped acceptance program.

#### Scenario: Repeat the acceptance program

- **WHEN** equivalent acceptance module maps are compiled repeatedly
- **THEN** every compiler-owned encoding, evaluation trace, symbol set, target text, and binary hash agrees exactly

### Requirement: Differential gates cover generic specialization

The compiler driver corpus SHALL include valid inferred and explicit specializations, multiple
instances of one declaration, generic nominal layouts, recursive same-argument calls, invalid
arity and inference, and repeated-compilation determinism. Completing programs SHALL agree across
evaluation, native LLVM, and direct WebAssembly for their selected targets.

#### Scenario: Compare a multi-specialization program
- **WHEN** the corpus compiles and runs one declaration at two concrete argument types
- **THEN** all three engines agree on the result and repeated compilations produce identical artifacts

#### Scenario: Keep invalid inference out of lowering
- **WHEN** a corpus program cannot determine one type argument from supplied arguments
- **THEN** it produces the committed semantic diagnostic and no runtime instance, layout, or MIR function

### Requirement: Effect acceptance covers both outcome branches deterministically

The compiler corpus SHALL execute Effect success, propagation, exact recovery, residual-row rejection,
ownership cleanup, and trap separation through evaluation, native, and Wasm where valid. Equivalent
repeated compilations SHALL preserve semantic facts, layout, MIR, text, and binary artifacts.

#### Scenario: Compare success and recovery across engines

- **WHEN** a canonical fixture is compiled once for its success input and once for its handled failure input
- **THEN** all three engines agree and repeated builds are byte-identical

#### Scenario: Reject an unresolved executable failure

- **WHEN** an ordinary entry attempts to run an Effect with a nonempty residual row
- **THEN** compilation rejects it before MIR emission and creates no executable artifact

### Requirement: Driver acceptance covers Effect and owned allocation vertically

The compiler corpus SHALL cover Effect construction versus execution, capture modes, catch, retry,
provider placement, Layout validation, allocation success and exhaustion, partial initialization,
Vector growth, explicit drop, typed-failure cleanup, and trap separation across evaluator, native,
and Wasm where valid. Repeated runs SHALL preserve every textual and binary artifact
deterministically.

#### Scenario: Compile the owned-token milestone

- **WHEN** a compiler-shaped program tokenizes borrowed runtime bytes into a growable owned Vector and returns it through an Effect
- **THEN** evaluation, native, and Wasm agree on tokens, ownership, allocation failures, cleanup, target layout, and emitted artifacts

### Requirement: Driver acceptance covers first-class callables vertically

The compiler corpus SHALL cover named function values, automatic sections, callable bindings and
returns, generic higher-order functions, Copy and borrowed captures, exclusive mutation, owned
take-once capture, Effect map, flatMap, tap and logging composition, retry rejection, grouped and
ungrouped run, cleanup, and diagnostics across evaluator, native, and Wasm where valid. Repeated
runs SHALL preserve syntax, semantic facts, HIR, ownership, instances, MIR, textual artifacts, and
binary artifacts deterministically.

#### Scenario: Compile the callable Effect milestone

- **WHEN** a canonical program maps and taps an Effect through stored reusable and consuming sections
- **THEN** evaluation, native, and Wasm agree on success, effect nesting, invocation access, ownership, and cleanup

#### Scenario: Reject invalid reuse before emission

- **WHEN** the corpus invokes a take-once section twice or supplies it to a repeatable callback contract
- **THEN** compilation emits the stable ownership or callable-mode diagnostic and no conflicting runtime artifact

#### Scenario: Preserve deterministic callable artifacts

- **WHEN** equivalent callable programs compile repeatedly
- **THEN** generated environment identities, instance ordering, MIR, symbols, and emitted artifacts are byte-identical

### Requirement: Allocation acceptance covers the substrate vertically

The continuous compiler corpus SHALL cover valid and invalid layout formation, role-selected
allocator provision, successful and exhausted allocation, provider access ending before result
cleanup, affine moves, typed buffers and slots, initialization and rollback, restricted-hook
rejection, explicit early drop, every structured exit, trap separation, zero-sized and over-aligned
storage, and post-failure reuse. Evaluator, native, and Wasm SHALL agree on every logical result and
cleanup trace. Repeated runs SHALL keep syntax, facts, ownership, HIR, instances, target layout,
MIR, traces, textual output, and binary artifacts deterministic.

#### Scenario: Compile the construction-guard milestone

- **WHEN** a canonical program allocates runtime-counted move-only slots, initializes a guarded prefix, and exits through success and injected typed failure
- **THEN** all three engines agree on values, `OutOfMemory`, hook order, exact releases, target layout, and emitted artifacts

#### Scenario: Reject unsafe misuse before artifacts

- **WHEN** source accesses a Slot safely, escapes it, consumes its live buffer, duplicates an Allocation, or declares an invalid Drop hook
- **THEN** compilation emits the responsible stable diagnostic and produces no MIR or executable artifact for that program

#### Scenario: Preserve allocation-free stability

- **WHEN** an allocation-free corpus program compiles after the substrate is added
- **THEN** it gains no allocator witness, allocation layout, reclaim ticket, Drop hook, or heap operation solely because the feature exists

### Requirement: Scanner acceptance proves the owned sequence vertically

The driver's continuous gates SHALL include a scanner written in Silk that borrows runtime-sized
source bytes as a slice and returns an owned `Vector<Token>`, growing across at least one
reallocation. The differential harness SHALL verify identical token results across the evaluator,
LLVM native execution, and instantiated Wasm; a failure-ordinal sweep over every allocation the
scanner performs SHALL confirm each injected `OutOfMemory` propagates typed, rolls back partial
initialization, and leaks nothing, with the evaluator and Wasm carrying every ordinal and native
execution carrying representative boundary ordinals including at least the first failure, one
mid-growth failure, and unrestricted completion; and repeated compilation SHALL keep the scanner's
artifacts deterministic.

#### Scenario: Three engines agree on scanned tokens

- **WHEN** the scanner acceptance program tokenizes input long enough to force vector growth
- **THEN** the evaluator, native executable, and Wasm instance produce identical token sequences and exit values

#### Scenario: Exhaustion at every ordinal leaks nothing

- **WHEN** the harness injects allocation failure at each successive allocation ordinal of the scanner run
- **THEN** every evaluator and Wasm run fails with typed `OutOfMemory` or completes and releases every live owner exactly once, and native runs at the boundary ordinals report no leaked allocation

#### Scenario: Scanner artifacts are deterministic

- **WHEN** the scanner acceptance program is compiled repeatedly
- **THEN** every published artifact, including those of imported standard-library modules, is byte-identical

### Requirement: Differential gates pressure pipeline composition

The continuous compiler corpus SHALL compile and execute a deterministic matrix of ordinary value
and Effect pipelines through evaluation, native LLVM, and direct WebAssembly. The matrix SHALL
cover left association and grouping, direct and stored forms, ordinary and effectful entries,
Copy and affine values, automatic and stored callables, `map`, `flatMap`, `tap`, `catch`, `retry`,
`provide`, and `provideWith`, including representative combinations rather than only isolated
operators. Equivalent source shapes SHALL produce equal observable outcomes and cleanup; repeated
analyses SHALL preserve deterministic artifacts.

#### Scenario: Compare pipeline source shapes

- **WHEN** data-first, piped, grouped, and stored programs express the same valid computation
- **THEN** every supported engine returns the same result with the same logical failure and cleanup observations

#### Scenario: Exercise an effectful entry pipeline

- **WHEN** effectful `main` directly runs a mapped and provisioned Effect
- **THEN** compilation reaches every requested backend and runtime execution completes without a compiler exception or generated trap

#### Scenario: Pressure a recognizable affine program

- **WHEN** the Silk lexer maps its owned token and diagnostic result through verification before allocator provision and execution
- **THEN** evaluator, native, and WebAssembly preserve its fingerprint, allocation-failure behavior, and exactly-once cleanup

#### Scenario: Repeat the pipeline matrix

- **WHEN** equivalent pipeline fixtures are analyzed repeatedly
- **THEN** their closure, HIR, ownership, instances, layout, MIR, traces, symbols, and backend artifacts remain identical

### Requirement: Slice acceptance exercises failure boundaries

The compiler corpus SHALL retain deterministic negative cases for implicit decay, immutable
exclusive borrowing, conflicting argument loans, recursive slice storage or return, unsupported
standalone binding, non-Copy extraction, unrepresentable length, and runtime out-of-bounds access.

#### Scenario: Repeat invalid slice compilation

- **WHEN** each invalid slice fixture is compiled repeatedly
- **THEN** it yields the same phase-owned diagnostic or runtime trap without producing a successful conflicting artifact

### Requirement: usize has target-aware differential acceptance

The compiler acceptance surface SHALL compare evaluator, native, and Wasm results for `usize`
programs whose values fit 32 bits, compare evaluator and native results above 32 bits, and require
Wasm target rejection for out-of-range literals before emission. Repeated runs SHALL preserve
identical facts, layouts, MIR, textual artifacts, and binary artifacts for the same target.

#### Scenario: Compare the shared range

- **WHEN** a canonical fixture uses checked `usize` arithmetic entirely within the 32-bit range
- **THEN** evaluator, native execution, and Wasm execution return the same unsigned value

#### Scenario: Compare the native-only range

- **WHEN** a canonical native fixture computes a valid value above `2^32 - 1`
- **THEN** evaluator and native execution agree exactly while the Wasm-targeted counterpart is rejected before MIR

### Requirement: Differential gates enforce static Effect representation normalization

The continuous compiler corpus SHALL compare normalized and explicitly unnormalized synchronous
Effect programs through evaluation, optimized native entry structure, and direct WebAssembly entry
structure. Eligible cases SHALL preserve behavior and SHALL NOT retain foldable constructor calls or
an immediately materialized Effect environment. Ineligible controls SHALL preserve their ordinary
representation and behavior. Structural verdicts SHALL be asserted on entry structure rather than
on exact byte, branch, or timing measurements.

#### Scenario: Gate eligible constructor and run shapes

- **WHEN** direct map, flat-map, generic-provider, stored, and trapping cases compile
- **THEN** evaluator and Wasm behavior agree, native entries do not regress, and eligible direct-Wasm entries omit foldable constructor calls

#### Scenario: Keep an affine capture explicit

- **WHEN** an Effect environment directly captures an affine or exclusive value
- **THEN** the first normalization slice rejects that environment while the allocation-backed corpus preserves ordinary exactly-once Drop behavior
