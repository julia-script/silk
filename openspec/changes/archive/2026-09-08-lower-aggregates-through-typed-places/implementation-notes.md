# Completed implementation — 2026-09-08

Status: all 25 implementation and evidence tasks are complete. Native locals now use direct
descriptors or typed places, including the approved planner prerequisite. Test coverage was
completed in parts as recorded below; the unrelated root formatting failure remains.
The structural traffic reduction is verified, but a causal cold-build speedup is not
established. The initial investigation below is retained as history.

Archived on 2026-09-08 with all 25 tasks complete. This implementation-only change
declared `skip_specs: true`; no delta specifications existed to sync into main specs.

Benchmark workloads, harnesses, and raw reports are retained locally, outside this pull request.
Benchmark paths in this archive identify historical inputs, not files shipped in the repository.

## Fresh baseline

The locally retained raw results (`2026-09-08-typed-places-before.json`)
retain all three fresh-process samples per stage, phase timing, CPU/RSS, load averages,
source identities, and runtime oracles. No builds/tests from this work overlapped the batch;
unrelated host activity was not stopped. Native/Node compilation caches were disabled;
OS page cache was not flushed. No input reads stdin.

| Stage       | Median |        Range | Backend median | Median peak RSS |
| ----------- | -----: | -----------: | -------------: | --------------: |
| Parser only | 35.49s | 34.47–41.04s |         13.86s |       2,241 MiB |
| Full CLI    | 46.32s | 45.51–47.05s |         17.41s |       2,271 MiB |

All six runtime checks and the CLI bootstrap differential check passed. The compiler
diff identity remains `619c7765615f14858049f760b477838424d40e02301311b5dd0f702bd1c00246`;
the CLI bundle identity remains `db49c23cd2a9d78594d86450432ce9743d04540f00808199b6cf594a8ba4b2f3`.
No source, calling convention, or runtime simplification was applied. These measurements
are a fresh baseline, not a regression or improvement attributable to a new implementation.

## Closed consumer inventory

Graph discovery and inbound tracing of NativeStorage.readLocal identified the ordinary
operation consumers. A file-level search also found direct map consumers and helper
payload APIs, which cannot be covered by changing readLocal alone.

| Boundary                                   | Modules requiring migration or audit                                                                |
| ------------------------------------------ | --------------------------------------------------------------------------------------------------- |
| Local map and root storage                 | NativeStorage, NativeFunction, NativeLoweringContext, NativeOperationContext                        |
| Construction, projection, aliasing         | NativePlaceOperation, NativeOwnedPlace, NativeValueOperation, NativePointerOperation                |
| Scalar/descriptor reads and writes         | NativeScalarOperation, NativeAssemblyOperation                                                      |
| Allocation and owned payloads              | NativeMemoryOperation, NativeLocalSharedOperation                                                   |
| Calls, returns, result extraction          | NativeCall, NativeCallOperation, NativeCallable, NativeForeignOperation, NativeResult, NativeReturn |
| Control-flow reads and union discriminants | NativeControl                                                                                       |
| Cleanup and diagnostic contexts            | NativeAggregate, NativeDiagnosticScope                                                              |
| Effects and frames                         | NativeEffectOperation, NativeExecutionOperation, NativeExecutionStorage, NativeSuspension           |

The local representation inventory now has these contracts: NativeStorage alone owns the
tagged local map; ordinary operations project/copy canonical places; direct scalar operations
read bounded values; calls, returns, representation conversions, and generated invocation
helpers explicitly materialize ABI lanes. NativePayload carries lazy selected cleanup
projections, including execution-package components. NativeFrame retains/restores planned
payload places, with pointer slots for rebound EnvironmentBorrow inputs. Aggregate joins
and alias writes do not refresh a full vector. The integration sweep is complete; validation
and the two discovered repairs are recorded below.

The intended classification covers all current Mir.Type categories:

- Builtin scalar tags, Enum, Pointer, Reference, ForeignFunction: direct physical values;
  unit/zero-sized cases retain logical ownership without payload.
- Bottom: no physical value on reachable execution paths.
- String and Slice: bounded direct descriptors, independent of element width.
- Nominal, FixedArray, Union: canonical aggregate/carrier places where payload exists.
- EnvironmentBorrow: borrowed place for the selected concrete underlying representation,
  not a new owned allocation; boundary transport remains its planned reference.
- EffectValue and CallableValue: exact environment or stored represented layout, with
  zero-sized environments handled without erasing logical ownership.
- EffectComposite: requires a complete canonical carrier view, including alternative
  placement, not only total byte size and an ABI lane list.
- EffectOutcome: uses the planner-owned outcome storage view added to close the historical
  prerequisite described below.

## Historical prerequisite: outcome storage was not planned

The design assumes every aggregate can consume an existing canonical addressable layout.
This is false for EffectOutcome, independently of the size of the parser:

- `Layout.ts`'s `layoutType` explicitly returns an unavailable entry for an Effect type
  (around line 1914).
- `Layout.plan`'s `add` visits an Effect's success/failure types and returns without
  creating an addressable Effect entry (around line 3200).
- The same plan separately includes an `OutcomeShape` in the calling-shape collection
  (around lines 3286 and 3904). This specifies lanes and tags, not a canonical storage entry.
- `NativeType.addressLayout` delegates EffectOutcome to `Layout.entry`, which therefore
  returns undefined. `LayoutVerify.laneOffset` also has no OutcomeShape storage view.
- Suspension currently stores outcome transport using `NativeType.packLanes`; that
  backend packing operation is not a planner-owned canonical value-storage contract.

A minimal diagnostic using the existing Analysis facade reproduces the missing layout:

```silk
effect fn value() -> i32 { return 42 }
pub fn main() -> i32 { return run value() }
```

With an aarch64-apple-darwin object profile, runtime none, and explicit retention of main,
the source has no diagnostics. Both resulting EffectOutcome locals have an OutcomeShape
with two calling lanes, while `NativeType.addressLayout(module.layout, type)` is undefined.
The read-only probe is retained at `/tmp/silk-aggregate-census.Pgi6EY/outcome-layout.mjs`.
This does not mean current Effect compilation is broken: the existing implementation uses
separate lane storage. It means the proposed replacement cannot reuse a canonical storage
fact that does not exist.

EffectComposite also warrants admission before conversion: its represented layout can
currently publish an Aggregate with empty fields and only a total size, whereas its calling
shape carries alternative lane information. That is not yet a complete typed carrier view.
No claim is made here that the existing representation produces an incorrect program.

## Historical recommendation (now implemented)

Add a prerequisite to plan and verify physical value-storage views for outcomes and
represented composite carriers: total size/alignment, tag/payload placements, per-lane or
field mappings, and their relationship to existing call/transfer transport. Keep these
facts in compiler target planning, distinguish them from capture-environment storage,
and keep the public/source semantics and current ABI unchanged.

Then use those facts in typed-place allocation, copying, projection, cleanup, and frame
transport. Do not guess sizes in the backend, reinterpret unrelated carrier offsets, or
leave outcomes permanently on the obsolete lane-cache path. This adds a planner prerequisite
to the implementation order and must be reflected in design/tasks before continuing the
representation migration. No artifact scope or task was silently narrowed to bypass it.

## Current implementation evidence

- `ValueStorage` owns outcome/composite views, active overlay locations, deterministic
  encoding, verification, and actual-start transport bindings. Exact captures retain their
  own environment identity; they are not outcome storage or callable descriptors.
- `NativeValue` exhaustively separates aggregate places from primitive descriptors.
  `NativePlace` owns projection, grouped boundary conversion, and overlap-safe byte copies.
  `NativeStorage` has no complete aggregate lane cache or aggregate join/alias refresh.
- Array, reference, slice, pointer, slot, and execution-package movement now uses selected
  storage. Cleanup reads only initialized/live payload fields. Branch joins commit result
  destinations; borrowed capture addresses are reloaded from rebound pointer slots.
- The frame audit exposed pre-existing extent assumptions: BorrowedDependency can carry a
  two-word slice descriptor, and a callable environment is not its two-word invocation view.
  Frame planning/verification now share the concrete payload extent. This intentionally fixes
  affected private frame offsets, while preserving call signatures and transfer packing.
- The locally retained final census (`2026-09-08-typed-places-census.json`)
  reconciles 800,774 LLVM instructions, with 133,739 loads, 145,744 stores, and 760 unused loads.
  Baseline: 802,864 instructions, 246,523 loads, 204,060 stores, 100,437 unused loads.
  Loads fall 45.8%, stores 28.6%, unused loads 99.2%, and allocas 83.9%. Total instructions
  fall only 0.3%: copies, field addresses, and active-union dispatch replace much of the old
  scalar traffic. This is structural evidence, not a timing sample. The post-integration-fix
  census is byte-identical to this report's raw census.
- Root typecheck passed all 18 tasks. Root lint passed. Root format checking reports only
  the pre-existing `.zuse/settings.toml` after formatting task-owned files. Completed test
  coverage and controlled measurement results are recorded below.

## Integration findings

The first broad compiler run passed 2,440 tests and exposed two remaining failures:

- `RuntimeSliceNative` expected the old combined scalar offset expression. The emitted
  code correctly checked the bound, computed the eight-byte element stride, projected
  the selected four-byte field offset, and loaded through that address. Its assertion now
  follows those consumed operands rather than requiring the superseded expression shape.
- `ExternalWakeParking` exposed a non-returning `Shared.withMut` conflict callback being
  treated as a zero-lane result for a two-lane union destination. The narrow existing
  regression reproduced in 6.5 seconds. Non-returning ordinary/callable calls now skip
  nonexistent result materialization; MIR control flow still owns the unreachable
  terminator. The shared join also commits only direct descriptors, without materializing
  an aggregate merely to call a no-op scalar-cache store. Both files pass all 11 tests.

The existing C ABI suite passed all 24 tests. Debug and optimized real CLI binaries each
passed all 113 parser corpus files, including bootstrap AST comparisons and recovery.
Wasm execution-storage conformance passed in both debug and optimized modes, including
the freestanding memory-helper composition. Release-candidate validation passed all 20
tests. All three native acceptance shards passed after the integration fix: 112, 111, and
111 tests, including the shared pre/post-corpus checks repeated in each shard.

## Completed validation

Logs remain under `/tmp/silk-aggregate-census.Pgi6EY`.

| Check                              | Result                                                                                                                            |
| ---------------------------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| `pnpm typecheck`                   | Passed, 18 tasks (`places-typecheck-acceptance.log`).                                                                             |
| `pnpm format:check`                | Failed only on pre-existing `.zuse/settings.toml` (`places-format-verified.log`); unrelated configuration left untouched.         |
| `pnpm lint`                        | Passed (`places-lint-acceptance.log`).                                                                                            |
| Compiler suite                     | 2,442 tests across 210 files passed in the workspace rerun (`places-test-acceptance.log`).                                        |
| Native acceptance                  | All three explicit `SILK_NATIVE_SHARD=k/3` runs passed: 112 / 111 / 111 tests (`places-native-shard-{1,2,3}.log`).                |
| Remaining workspace                | `pnpm test --filter='!@silklang/compiler'` passed all 21 tasks, including cached dependencies (`places-test-workspace-rest.log`). |
| Repository scripts                 | `pnpm test:scripts`: 19 passed (`places-scripts-final.log`).                                                                      |
| Parser/stdlib corpus               | 113 files passed on the final diagnostic binary; debug and optimized CLI corpus checks passed.                                    |
| Wasm execution-storage conformance | Passed both optimization modes, including libc-none support (`places-wasm.log`).                                                  |
| Release candidate                  | Final `pnpm release:candidate`: build passed and 20 tests passed (`places-release-accepted.log`).                                 |
| `pnpm check`                       | Failed at formatting on the same pre-existing `.zuse/settings.toml` (`places-check-final.log`).                                   |
| OpenSpec validation                | `openspec validate lower-aggregates-through-typed-places`: valid.                                                                 |

Full test coverage was completed in parts, not through a successful monolithic `pnpm test`
exit. The initial full run found the two integration failures above. The rerun passed all
2,442 compiler tests, then began repeating the native corpus already covered by the three
successful shards. That redundant leg was deliberately interrupted (exit 130); the remaining
workspace packages were run with the explicit filter shown above. No successful monolithic
`pnpm test` or `pnpm check` exit is claimed, and no native corpus cases were omitted.

## Cold-build conclusion

Two uninstrumented after batches provide six cold samples per stage, with all twelve runtime
oracles and both bootstrap AST checks passing. No builds/tests from this work overlapped
timed samples; source and built-module identities remained unchanged. The combined medians
are 32.61s for the parser and 43.26s for the CLI, versus 35.49s and 46.32s before.

This is not sufficient evidence of an optimization win: host load fell substantially,
process CPU was effectively flat (43.09 → 42.81s and 56.26 → 56.00s), backend time increased
(13.86 → 15.52s and 17.41 → 17.96s), and peak RSS increased about 15%/14%. The confirmation
batch and a separate CPU profile were collected to investigate that inconclusive result.
The profile shows distributed compiler/LLVM construction and encoding work and 6.64s of
whole-process GC samples, not one dominant new place-planning lookup.

The census explains the structural trade-off: aggregate reloads disappear, but field-address
construction and active-union conversion at the unchanged flattened ABI keep total LLVM
instructions almost constant. An indirect aggregate calling convention remains an explicit
separate follow-up, not an unimplemented part of this migration or a promised speedup.
The scalar and string/slice controls passed all six executions at 6.68s/6.44s medians.

The locally retained benchmark report (`benchmarks/selfhost-stages/README.md`)
links every sample, phase/CPU/RSS observation, fingerprint, census, and profile summary.
