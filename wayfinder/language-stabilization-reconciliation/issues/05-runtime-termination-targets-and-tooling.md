# Audit runtime, standard library, targets, termination, and tooling

Type: audit
Status: resolved
Blocked by: 04

## Question

For the confirmed runtime/library layers, distribution, providers, unsafe and target availability,
entry termination/reporting, and tooling rules, what does the repository implement, partially
implement, contradict, or omit?

## Scope

- all 16 rules in `docs/language/runtime-and-standard-library.md`;
- all 16 rules in `docs/language/unsafe-intrinsics-and-targets.md`; and
- all 12 rules in `docs/language/program-termination-and-reporting.md`.

The five entry-shape rules were classified by ticket 01. This audit classifies the remaining 44
rules exactly once.

## Answer

### Audit result

Of 44 scoped rules, 15 are implemented, 21 are partial, 4 are contradicted, and 4 are not
implemented. No rule is unknown.

Target availability is the strongest area: executable-closure selection, pre-lowering target
errors, and pay-for-use backend inventories already implement the confirmed model. The sealed
intrinsic catalog and lexical unsafe block also provide a sound base. The missing unsafe work is one
coherent language extension: ordinary source cannot declare an unsafe callable, use a single-call
acknowledgement, preserve unsafety through partial application, or check safe/unsafe callable
compatibility.

The runtime/stdlib differences mostly come from role conflation. One manifest currently acts as
catalog, source resolver, implicit prelude, and internal inventory; the native adapter also always
includes some host support. The confirmed direction separates these roles, keeps public source
ordinary, makes imports explicit, classifies portable/provider modules, and derives completion and
availability without creating invisible bindings.

Termination is the largest observable mismatch. Ordinary unit `main` is rejected, effect failures
still require `Report`, failure-member ordinals become process statuses, native reports contain only
`Error: <identity>`, and direct Wasm exposes only the ordinal. The evaluator already retains useful
logical traces and cleanup ordering, but the compiler does not yet carry the stable origin/path,
causal recovery context, trap report, or equivalent structured host outcome through every backend.

The focused suite covered 10 files and 114 tests; 113 passed at the default timeout and the sole
timeout (`IntrinsicCatalog`'s full presentation analysis) passed when rerun with a 20-second test
timeout. A second focused run of effect entry, driver, and editor-intelligence tests passed 41
tests.

### Evidence anchors

- Standard-library source and tooling:
  [`manifest.json`](../../../packages/compiler/stdlib/manifest.json),
  [`Stdlib.ts`](../../../packages/compiler/src/Stdlib.ts),
  [`StdlibNamespaceAcceptance.test.ts`](../../../packages/compiler/test/StdlibNamespaceAcceptance.test.ts),
  and [`EditorIntelligence.test.ts`](../../../packages/compiler/test/EditorIntelligence.test.ts).
- Unsafe and intrinsics:
  [`Intrinsic.ts`](../../../packages/compiler/src/Intrinsic.ts),
  [`Parser.ts`](../../../packages/compiler/src/Parser.ts),
  [`StringIntrinsics.test.ts`](../../../packages/compiler/test/StringIntrinsics.test.ts), and
  [`IntrinsicCatalog.test.ts`](../../../packages/compiler/test/IntrinsicCatalog.test.ts).
- Target closure:
  [`IntrinsicAvailability.ts`](../../../packages/compiler/src/IntrinsicAvailability.ts),
  [`Instances.ts`](../../../packages/compiler/src/Instances.ts), and
  [`IntrinsicTargetAvailability.test.ts`](../../../packages/compiler/test/IntrinsicTargetAvailability.test.ts).
- Termination:
  [`Backend.ts`](../../../packages/compiler/src/Backend.ts),
  [`ToolchainPlan.ts`](../../../packages/compiler/src/ToolchainPlan.ts),
  [`BootstrapEvaluation.ts`](../../../packages/compiler/src/BootstrapEvaluation.ts), and
  [`EffectEntry.test.ts`](../../../packages/compiler/test/EffectEntry.test.ts).

### Runtime and standard-library boundary

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| RUNTIME-001 | Partial | Partial | Canonical source, OS providers, intrinsics, and backend support are distinct artifacts, but the manifest does not classify all four layers and the prelude collapses catalog into language scope. |
| STDLIB-001 | Partial | Partial | Shipped actors compile as ordinary `.silk` source and narrow intrinsics carry primitives. Compiler-known library spellings, source exceptions, and implicit bindings still grant nonordinary treatment. |
| STDLIB-002 | Contradicted | Contradicted | Reserved canonical identities exist, but lexical discovery and name-resolution prelude bindings make ordinary stdlib actors available without explicit imports. |
| STDLIB-003 | Partial | Partial | One deterministic source manifest and generated embedding exist. It lacks the complete catalog metadata/integrity contract and is also used as prelude and internal inventory. |
| STDLIB-004 | Partial | Partial | Portable contracts and OS provider files are separated in major facilities, but the catalog has no enforced portable/provider classification or dependency-direction audit. |
| STDLIB-005 | Partial | Partial | Allocation, failures, services, ownership, and imports are generally explicit. Prelude discovery and adapter support still make some dependencies/costs appear ambient, and the promised LSP repair set is incomplete. |
| STDLIB-006 | Implemented | Aligned | Catalog modules use ordinary module resolution and `pub`/private visibility; there is no hidden stdlib-only visibility tier. |
| PROVIDER-001 | Implemented | Aligned | OS providers are ordinary explicit modules; target compatibility derives from reachable intrinsics rather than module annotations or import-time rejection. |
| PROVIDER-002 | Implemented | Aligned | Effect entry discovery requires an empty requirement row and does not synthesize allocator, logger, clock, filesystem, or other service providers. |
| RUNTIME-002 | Partial | Partial | Concrete specialization and executable reachability correctly control intrinsic/backend support. Implicit stdlib discovery means explicit imports do not yet solely control source-closure cost. |
| RUNTIME-003 | Implemented | Aligned | Native helpers and direct-Wasm lowering are compiler-versioned implementation machinery, not an advertised source module or stable public ABI. |
| RUNTIME-004 | Partial | Partial | No facility becomes a source-visible ambient capability, and suspension is pay-for-use. The mandatory native shim still embeds argument/stream support even for a trivial program, so the zero-unrelated-host-cost claim is not met. |
| RUNTIME-005 | Partial | Contradicted | A generated adapter selects and invokes one entry and executes effects once, but it rejects ordinary unit entries, requires `Report`, uses ordinal statuses, and lacks the confirmed report outcome. |
| DIST-001 | Partial | Partial | Generated source and intrinsic inventories are deterministic and matched in-tree, but there is no single verified compiler/catalog/runtime identity or public integrity diagnostic. |
| DIST-002 | Partial | Partial | Dedicated module, entry, intrinsic-target, and toolchain failures exist, but implicit discovery, unavailable-entry backend fallthrough, and incomplete reporting still blur several boundaries. |
| TOOLING-001 | Partial | Partial | Canonical source navigation, hover, docs, code actions, and auto-import foundations exist. Completion still relies on implicit namespace visibility and does not yet provide the complete import/collision and Effect-contract repair workflow. |

### Unsafe code, intrinsics, and targets

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| SAFETY-001 | Partial | Partial | Intrinsics record unsafe invariants and safe operations trap, but no complete source-level unsafe contract model or portable UB/debug-detection boundary exists yet. |
| UNSAFE-001 | Implemented | Aligned | Lexical unsafe blocks authorize unsafe intrinsic calls while ordinary typing, ownership, borrowing, Effect, visibility, and cleanup checks remain active. |
| UNSAFE-002 | Not implemented | Missing | `unsafe fn` is rendered for intrinsic signatures but cannot be declared by ordinary source. |
| UNSAFE-003 | Partial | Partial | `unsafe { ... }` statement regions parse and lower; the `unsafe call(...)` single-invocation form is absent. |
| UNSAFE-004 | Partial | Partial | Unsafe intrinsic effect calls are acknowledged when their lazy Effect is constructed rather than when run. Ordinary unsafe effect declarations do not exist. |
| UNSAFE-005 | Not implemented | Missing | Callable contracts have no source-level unsafety qualifier, compatibility relation, interface check, or override diagnostic. |
| UNSAFE-006 | Implemented | Aligned | Unsafe lexical permission never disables move, borrow, initialization, cleanup, or Effect analysis. |
| UNSAFE-007 | Not implemented | Missing | Source-declared unsafe callable values do not exist, so partial application cannot preserve their unsafety or defer acknowledgement. |
| UNSAFE-008 | Partial | Partial | Ordinary declarations already have visibility and documentation, but they cannot attach an enforceable unsafe contract/qualifier for tooling to present. |
| INTR-001 | Implemented | Aligned | `Intrinsic` has sealed compiler identity in every module and cannot be imported, extended, shadowed, or declared by source. |
| INTR-002 | Implemented | Aligned | Safety is cataloged per intrinsic operation; safe and unsafe calls share the namespace without namespace-wide permission. |
| INTR-003 | Partial | Partial | The deterministic inventory records admission and consumers, but not every entry has completed irreducibility/real-consumer evidence as an enforced release gate. |
| INTR-004 | Implemented | Aligned | Safe wrappers are ordinary Silk source that proves or checks preconditions before calling narrow unsafe intrinsics; wrappers receive no name-based privilege. |
| TARGET-001 | Implemented | Aligned | Availability runs on concretely retained calls after specialization, deduplicates by canonical intrinsic/target, and precedes lowering. |
| TARGET-002 | Implemented | Aligned | Unreachable restricted intrinsics add no host import, helper, adapter, or inventory entry to the emitted artifact. |
| TARGET-003 | Implemented | Aligned | A reachable unsupported intrinsic is a compile-time compatibility error, never a typed failure, service requirement, runtime probe, or fallback. |

### Program termination and reporting

| Rule | Semantics | Diagnostics/reporting | Current evidence and boundary |
| --- | --- | --- | --- |
| TERM-001 | Contradicted | Contradicted | Entry discovery accepts ordinary `main` only when its result is exactly `i32`; explicit `()` is rejected despite being semantically valid elsewhere. |
| TERM-002 | Implemented | Aligned | A unit-success effect entry is constructed/run once, returns zero on success, and emits no automatic success report. |
| TERM-003 | Contradicted | Contradicted | Compiled entries return normalized one-based failure member tags, so member order and identity determine the status instead of every failure using one. |
| TERM-004 | Partial | Missing | Evaluator outcomes retain call events, but native reports contain only an identity and compiled artifacts do not carry the stable failure origin and logical path. |
| TERM-005 | Contradicted | Contradicted | Payload fields are not reflected, but entry discovery still requires the compiler-sealed operation-free `Report` marker, contrary to ceremony-free error values. |
| TERM-006 | Not implemented | Missing | A handler's new failure does not retain the handled error identity/trace as an explicit causal `while handling` chain across engines and reports. |
| TERM-007 | Partial | Partial | Evaluator traces and suspension bookkeeping retain logical calls, but release native/Wasm failure reports do not consume that metadata or guarantee the stable minimum. |
| TERM-008 | Partial | Partial | Defined traps and reasons exist across engines, but there is no shared best-effort fatal-trap report with source origin and logical path. |
| TERM-009 | Implemented | Aligned | Automatic native reporting lives in the private adapter and adds no source Logger, Console, allocator, or other requirement. |
| TERM-010 | Partial | Partial | Native reports an identity while direct Wasm returns only an ordinal; artifact metadata knows identities but no equivalent structured runner outcome carries classification, origin, and path. |
| TERM-011 | Partial | Partial | Propagation and entry tests prove cleanup before failure-tag exposure, including selected payload cleanup. The report pipeline does not yet retain/use the required diagnostic context afterward. |
| TERM-012 | Implemented | Aligned | Source may explicitly recover/provide, perform its own presentation, and return an ordinary `i32`; no naming convention automatically selects custom status policy. |

### Superseded artifacts

| Superseded model | Current artifact evidence |
| --- | --- |
| Standard-library namespaces are an implicit prelude | manifest lexical discovery, name-resolution seed tier, and namespace completion tests |
| The manifest is simultaneously public catalog and compiler inventory | generated stdlib source, namespace aliases, internal actors, and source lookup |
| Unsafe exists only for compiler-owned intrinsics | parser grammar, intrinsic presentation, and absence of source unsafe declarations |
| Effect-entry errors require an operation-free `Report` marker | `Type.reportCapability`, entry discovery, declaration validation, CLI message, tests, and stdlib conformances |
| Failure union ordinals are process statuses | MIR entry tags, backend termination metadata, native switch cases, direct-Wasm return, and tests |
| Native identity text is the complete error report | `ToolchainPlan.shimSource` and effect-entry native assertions |

### Ordered implementation handoffs

1. **Separate catalog, scope, provider classification, and runtime inventory.** Extend the canonical
   catalog with source identity/digest/docs/layer metadata, remove its prelude role, enforce
   portable-to-provider dependency direction, and keep executable support derived from reachable
   intrinsics.
2. **Finish explicit-import tooling.** Make completion discover catalog declarations, insert a
   module-qualified import with collision-aware aliasing, and add explicit Effect failure/
   requirement propagation, recovery, and provision actions.
3. **Add source-declared unsafe callable contracts.** Parse and type `unsafe fn`/`unsafe effect fn`,
   add single-call acknowledgement, preserve the qualifier through values and partial application,
   enforce safer implementation compatibility, and keep all ordinary checks active.
4. **Remove the `Report` eligibility gate.** Delete the capability, conformance exception, entry
   validation, stdlib declarations, tests, docs generation assumptions, and CLI wording; every
   concrete detached owned error remains entry-eligible.
5. **Build one structured termination outcome.** Give unhandled failure, fatal trap, and success
   target-neutral data containing classification, identity/reason, provenance, logical path, and
   causal history; preserve it through suspension, optimization, cleanup, evaluator, LLVM, Wasm,
   native adapter, and embedding runner.
6. **Normalize entry statuses and shapes.** Accept explicit ordinary `()`, map it and effect success
   to zero, map every typed failure to one, and keep custom status only in an ordinary `i32` entry.
7. **Make adapters strictly pay-for-use.** Retain only the minimal entry boundary plus support
   justified by the executable intrinsic/report inventory; a trivial program must not acquire
   standard-stream, command-line, scheduler, allocator, or provider machinery.
8. **Add matched-toolchain integrity verification.** Validate compiler/catalog/intrinsic/runtime
   identities and classify missing source, bad distribution, unsupported target, open entry, and
   operational failure at their owning boundary.

### Next frontier

The final planned frontier is
[06 — explicit Effect suspension](06-effect-suspension.md).
