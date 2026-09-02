# Runtime and standard-library boundary

Silk separates language semantics from the ordinary library source and toolchain runtime support
that ship together. Importing a public actor must not grant compiler privilege or silently acquire
an unrelated platform dependency.

This page records the confirmed language, library, provider, and runtime-layer rules.

## Terminology

- The **language core** is Silk syntax and closed semantics that exist without resolving a source
  declaration.
- A **language binding** is a closed name such as a primitive type spelling or `Intrinsic` whose
  identity is defined by the language rather than an imported declaration.
- A **standard-library module** is canonical public `.silk` source distributed by the toolchain and
  compiled under ordinary source rules.
- A **portable module** exposes a contract and implementation whose reachable behavior is defined on
  every target supporting its required language primitives.
- A **target-provider module** is ordinary source that implements a portable service or abstraction
  using target-restricted primitives.
- **Toolchain runtime support** is the target machinery implementing reachable intrinsic and entry
  contracts. Its guaranteed surface is those contracts, not its raw implementation symbols or ABI.
- The **source closure** is the root module plus modules reached through explicit imports.
- The **executable closure** is the concretely specialized code reachable from the selected entry.
- A **toolchain distribution** is one verified set of compiler, canonical library source, target
  support, and toolchain runtime artifacts.

## Layer boundary

### RUNTIME-001 — Language, public source, target providers, and toolchain runtime support are distinct layers

**Status:** Confirmed

Silk programs interact with four layers:

1. The language core defines syntax, built-in type identities, static semantics, ownership, Effect
   execution, traps, entry adaptation, and the sealed intrinsic catalog.
2. Portable standard-library modules define public values, functions, interfaces, services,
   validation, policy, and safe composition in ordinary Silk source.
3. Target-provider modules are ordinary Silk implementations that explicitly use target-restricted
   intrinsics to satisfy portable contracts.
4. Toolchain runtime support implements reachable intrinsic and machine-entry operations below
   source. The toolchain guarantees the contracts above this layer, not the spelling or ABI of its
   implementation symbols.

```text
program source
    │ imports and calls
    ▼
ordinary portable modules
    │ optionally provided by
    ▼
ordinary target-provider modules
    │ explicitly call
    ▼
sealed Intrinsic contracts ── toolchain target support
```

A toolchain may package all four layers together. Packaging does not collapse their contracts:
standard-library and provider declarations gain no semantic privilege, while raw runtime
implementation symbols do not become guaranteed source API merely because they are present.

**Boundary:** A declaration is not part of the language merely because every distribution ships it,
the compiler uses it for self-hosting, or its source path begins with `silk`. Conversely, a closed
language operation does not become replaceable merely because a standard-library wrapper exposes
it more conveniently.

**Diagnostics:** Errors in public library or provider source use ordinary source diagnostics and
canonical source locations. An unavailable target intrinsic is reported through target
availability before lowering. Missing support required by a matched toolchain is a toolchain-
integrity error, not a source-level missing import or typed failure.

**Current compiler:** Aligned. Canonical standard-library files compile through the ordinary
pipeline, the generated catalog classifies portable and target-provider modules, source scope is
explicit, and intrinsic support is reachable-only.

**Evidence:** [bootstrap library source specification](../../../../openspec/specs/bootstrap-silk-stdlib/spec.md),
[unsafe and intrinsic boundary](unsafe-intrinsics-and-targets.md),
[current library manifest](../../../../packages/compiler/stdlib/manifest.json).

## Ordinary public source

### STDLIB-001 — Public standard-library declarations receive no compiler privilege

**Status:** Confirmed

Every public standard-library value, function, type, interface, service, provider, combinator, and
safe wrapper is ordinary Silk source. It uses the same parsing, name resolution, type checking,
ownership, specialization, reachability, diagnostics, and lowering rules as equivalent user source.

Canonical distribution identity lets imports find the source and lets tooling navigate it. It does
not authorize a compiler phase to recognize a declaration by module, name, signature, manifest
entry, or installation path. When ordinary source cannot express a required primitive, the smallest
operation belongs in sealed `Intrinsic` under its explicit safety and target contract.

**Boundary:** The compiler may embed verified source bytes for distribution, cache their ordinary
analysis, or precompile them without changing semantics. Generated embeddings and caches are derived
artifacts; canonical visible `.silk` files remain the editable source of truth.

The first stable model gives `Result` no dedicated language syntax or propagation rule. Automatic
typed-failure propagation remains the existing `run` behavior for Effect execution. Possible
optional-field omission involving `Option` belongs to a future struct-construction decision and is
not inferred by this rule.

**Diagnostics:** A malformed shipped module reports the same source error as equivalent user code,
at its canonical library location. Toolchain verification may additionally classify this as a
broken distribution, but it cannot silently replace the module with a privileged implementation.

**Current compiler:** Aligned. Shipped actors are canonical `.silk` files with a generated catalog,
ordinary source analysis, explicit imports, and source navigation.

**Evidence:** [canonical source requirements](../../../../openspec/specs/bootstrap-silk-stdlib/spec.md),
[standard-library source guide](../../../../packages/compiler/stdlib/README.md),
[minimal compiler privilege](../../../../AGENTS.md#minimal-compiler-privilege).

### STDLIB-002 — Standard-library APIs require explicit imports

**Status:** Confirmed

Only closed language bindings exist without source imports. Standard-library actors—including
`Option`, `Result`, `Effect`, scalar operation actors, `Vector`, `String`, services, and provider
types—enter a module scope only through explicit imports.

```silk
import silk.option { Option }

pub fn main() -> i32 {
  let value = Option.some<i32>(42)
  drop value
  return 0
}
```

The type spellings `i32` and `Effect<A ! E ? R>` remain language syntax. Importing `silk.i32` or
selecting `Effect` from `silk.effect` creates an ordinary value binding containing actor operations;
it does not define or replace the language type.

Canonical standard-library modules occupy the reserved `silk.*` distribution identity. Project
source cannot declare, replace, or shadow that identity. This reservation lets one import resolve
deterministically to the toolchain's canonical source but grants no semantic privilege to the
declarations it contains.

**Boundary:** There is no implicit standard-library prelude, token-based module discovery, or
fallback lookup after an unknown name. An LSP auto-import inserts ordinary source text. Importing an
ordinary project module named `Option` is unrelated to `silk.option`.

**Diagnostics:** Naming an unimported standard-library actor uses the ordinary unknown-name
diagnostic and may include an auto-import action. Declaring a project module under reserved
`silk.*` reports a reserved-module-identity collision naming the project and toolchain origins.

**Current compiler:** Aligned. Canonical module identities are reserved, while module closure and
name resolution introduce standard-library declarations only through explicit imports.

**Evidence:** [PRELUDE-001](modules-names-and-visibility.md#prelude-001--only-language-bindings-are-implicit),
[module implementation evidence](modules-names-and-visibility.md#implementation-evidence),
[standard-library module resolution](../../../../openspec/specs/bootstrap-silk-stdlib/spec.md).

### STDLIB-003 — One canonical catalog defines the shipped library, not the language

**Status:** Confirmed

The official toolchain ships one deterministic, dependency-complete standard-library source
catalog. The catalog is the authoritative answer to whether a standard module exists in that
toolchain release. It records canonical identity, source location and digest, documentation origin,
and any distribution classification required by tooling.

The catalog is a library/distribution contract, not a language vocabulary. Adding, revising, or
removing an ordinary module changes the standard-library API without adding a parser rule, built-in
type, intrinsic, or compiler-known declaration. Each module's own reference defines its public API;
this language page defines only how those modules participate in Silk semantics.

The first stable model has no selectable standard-library profiles. Every installation of one
official toolchain release exposes the same canonical source catalog on every supported target.
Target-specific source may be present on every installation; only reachable target-restricted
operations determine whether one executable is compatible.

The baseline catalog is expected to cover these general-purpose areas needed by real portable
programs and the self-hosted compiler:

- scalar actors, ordering, checked conversion, parsing, and formatting;
- ordinary `Option`, `Result`, and Effect composition;
- layout, allocation, owned indirection, sequences, bytes, text, hashing, maps, and sets;
- portable logging, byte streams, host input, filesystem, and child-process contracts; and
- ordinary provider modules for the targets the distribution supports.

This category list is an admission boundary, not a promise that every conceivable helper belongs in
the bootstrap API. A new module needs a coherent general-purpose actor and evidence from real
programs; compiler-specific helpers, speculative convenience, and unrelated future domains do not
enter merely to make the catalog look complete.

**Boundary:** A compiler implementation may support the Silk language without shipping the official
standard library, but it is not the complete official Silk toolchain distribution. Conversely, a
module shipped by the official distribution is not implicitly loaded, reachable, or emitted into
every program.

**Diagnostics:** Importing a canonical `silk.*` identity absent from the selected toolchain catalog
reports an unknown standard-library module and names the toolchain release. It does not pretend the
name is an unknown project path. A catalog that promises a missing or digest-mismatched file is a
broken-toolchain diagnostic, not an ordinary source error.

**Current compiler:** Aligned. One deterministic generated catalog records canonical source,
digests, documentation, layers, provider targets, and intrinsic inventories without creating source
scope.

**Evidence:** [current deterministic catalog](../../../../packages/compiler/stdlib/manifest.json),
[canonical packaging requirements](../../../../openspec/specs/bootstrap-silk-stdlib/spec.md).

### STDLIB-004 — Portable modules never select a target provider

**Status:** Confirmed

A portable standard-library module may depend on other portable modules, language operations, and
intrinsics available on every target it promises. It must not import, construct, provide, or name a
target-provider implementation.

```silk,ignore
// silk/filesystem
pub service FileSystem {
  effect fn readFile(path: &Path) -> Bytes ! FileError ? &mut FileSystem
}
```

The portable contract owns general values, typed failures, and reusable helpers. A separate module
may implement it for one host:

```silk,ignore
// silk/os_filesystem
import silk.filesystem { FileSystem }

pub struct OsFileSystem { /* ordinary owned provider state */ }
impl FileSystem for OsFileSystem { /* maps ordinary source operations */ }
```

This dependency direction applies equally to logging, standard streams, standard input, host input,
child processes, clocks, randomness, networking, and future platform capabilities. Portable source
defines the honest common contract; target providers adapt it to narrower host primitives.

**Boundary:** A portable module may document that an application commonly uses a particular
provider, but documentation does not add a dependency or default. If no honest common contract
exists, the lower-level capability remains explicitly target-specific rather than weakening the
portable API to expose native details.

**Diagnostics and audit:** Importing a target-provider module from a module declared portable is a
distribution-policy violation identifying both modules and the dependency edge. A provider may
import its portable contract. Catalog verification rejects a dependency cycle that makes the
portable closure depend transitively on target-provider source.

**Current standard library:** Partially aligned. The manifest now classifies every module as
`portable` or `target-provider`, and filesystem, standard input, host input, and child-process
contracts are separate from their OS providers. Allocation (`silk.allocator`) and standard streams
(`silk.writer`) are still classified as portable while also containing their
process-backed providers, so those two module boundaries remain to be reconciled with this rule.

**Evidence:** [portable/provider separation](../../../../openspec/specs/bootstrap-silk-stdlib/spec.md),
[requirements and services](requirements-and-services.md),
[target availability](unsafe-intrinsics-and-targets.md#target-availability).

### STDLIB-005 — Library costs and execution contracts remain explicit

**Status:** Confirmed

Standard-library operations express allocation, typed failure, service requirements, ownership,
mutation, and cleanup through their ordinary Silk signatures. Importing a module performs no runtime
initialization and acquires no allocator, provider, registry entry, thread, global state, or host
resource.

```silk,ignore
import silk.allocator { Allocator, OutOfMemoryError }
import silk.vector { Vector }

effect fn copyValues(values: &[i32]) -> Vector<i32> ! OutOfMemoryError ? &mut Allocator {
  let mut result = Vector.make<i32>()
  // append operations state their allocation contract
  return move result
}
```

Allocation-free construction remains allocation-free. Operations that may grow owned storage state
`! OutOfMemoryError ? &mut Allocator` or another honest contract. Static strings and static bytes need no
hidden heap owner. Service use remains in the requirement row until explicitly provided.

**Boundary:** The compiler may optimize a proven allocation, provider selection, or cleanup without
changing observable behavior. It may not infer an ambient allocator, catch an undeclared failure,
run a module initializer, or inject a singleton merely because the operation belongs to the standard
library.

**Diagnostics:** A call missing an allocator or other service receives the ordinary unresolved-
requirement diagnostic. Ignoring a failure, moving an owner twice, or violating a borrow uses the
ordinary language diagnostic. No “standard library exception” suppresses those errors.

The LSP should automate the mechanical repair while preserving explicit source. From a missing
failure or requirement diagnostic it may offer to propagate the precise residual type into the
enclosing declaration, add required imports, or generate a local recovery or provision scaffold.
Hover and inlay information may show which call introduced each channel. An action previews and
writes an ordinary signature or expression; the compiler does not silently infer a larger public
contract or pretend the source already contains the edit.

**Current standard library:** Aligned for source dependencies. Allocation, services, failures, and
standard-library names remain explicit in source; entry adaptation is defined separately.

**Evidence:** [Effect contracts](effect-contracts.md),
[requirements and services](requirements-and-services.md),
[ownership and cleanup](ownership-and-borrowing.md).

### STDLIB-006 — Cataloged modules use ordinary module visibility

**Status:** Confirmed

Before Silk has a native package and re-export model, every canonical module in the shipped catalog
may be imported explicitly. Its `pub` declarations are visible across modules and its private
declarations are not. There is no second hidden “standard-library internal” visibility level.

Implementation details that must not become callable from user source stay private within their
module. A support module that must expose declarations to several canonical modules is necessarily
an importable low-level module in this first model; documentation may discourage direct use, but the
compiler does not pretend its `pub` declarations are private only to outsiders.

**Boundary:** A future package system may add package-private modules, explicit public exports, or
re-exported facades. That model must apply uniformly to ordinary packages and the standard library.
Until then, a catalog flag cannot silently override the language's confirmed `pub` meaning.

**Diagnostics:** Imports and member access use the ordinary module and visibility diagnostics. The
compiler does not report an “internal standard library API” error for a declaration that is
otherwise public and cataloged. Tooling may show a non-blocking stability or low-level API notice.

**Current standard library:** Aligned. Catalog-wide module resolution preserves ordinary `pub`
visibility, and no catalog namespace becomes visible without an explicit import.

**Evidence:** [ordinary visibility](modules-names-and-visibility.md),
[current source catalog](../../../../packages/compiler/stdlib/manifest.json),
[deferred re-exports](modules-names-and-visibility.md#export-001--imports-do-not-re-export-declarations).

## Target providers and entry closure

### PROVIDER-001 — Target providers are ordinary explicit modules

**Status:** Confirmed

A target provider is an ordinary source module whose operations implement one or more portable
services through sealed target-restricted intrinsics. It follows ordinary imports, visibility,
conformance, ownership, Effect, unsafe, specialization, and cleanup rules.

Importing and type-checking a target-provider module is valid on every target. Compatibility is
checked only when a provider operation and its restricted intrinsic enter the selected executable
closure. Provider names, module paths, and conformances do not create module-level target semantics.

```silk,ignore
import silk.os_filesystem { OsFileSystem }

pub fn main() -> i32 {
  // Importing the provider actor is valid even when this target cannot execute its OS calls.
  return 0
}
```

An executable that never reaches a provider operation pays no target/runtime cost. An executable
that reaches an unsupported OS intrinsic receives the ordinary intrinsic target-availability error.

**Boundary:** A provider cannot extend an intrinsic's target set, install a runtime fallback, or
claim portability through a source annotation. A pure in-source provider may satisfy the same
portable service on every target without using any target intrinsic.

**Diagnostics:** Source errors use ordinary diagnostics. A reachable unsupported primitive names
the intrinsic and target, not merely the provider module. Tooling may show the provider's inferred
target compatibility without rejecting an unreachable import.

**Current compiler:** Largely aligned through separate OS modules and reachable intrinsic
availability. Provider-module target summaries remain a tooling derivation rather than a stable
source feature.

**Evidence:** [TARGET-001](unsafe-intrinsics-and-targets.md#target-001--intrinsic-availability-is-checked-only-for-the-selected-executable-closure),
[canonical provider modules](../../../../openspec/specs/bootstrap-silk-stdlib/spec.md),
[current OS provider source](../../../../packages/compiler/stdlib/silk/os_filesystem.silk).

### PROVIDER-002 — Entry points receive no implicit service providers

**Status:** Confirmed

After ordinary composition, an effectful `main` must have an empty requirement row. The compiler and
runtime do not automatically provide an allocator, logger, filesystem, clock, host input, standard
stream, or other service merely because an official implementation ships with the toolchain.

```silk,ignore
import silk.effect { Effect }
import silk.logger { LogError, LogLevel, Logger }

effect fn program() -> () ! LogError ? &mut Logger {
  return run Logger.log(LogLevel.Info, "ready")
}

pub effect fn main() ! LogError {
  let mut logger = Logger.stdoutProvider()
  return run Effect.provideMut<Logger>(program(), &mut logger)
}
```

The example's exact provider construction remains library API; the semantic point is that source
selects and provides it before the entry closes.

**Boundary:** A future proposal may define target-specific default providers that explicit source
can override. Such defaults require a visible selection order, collision rule, lifetime, failure
contract, target contract, and tooling presentation. Shipping a provider today does not anticipate
or activate that behavior. Optional service requirements are a separate future design in the same
area: it must define how absence appears in a requirement row, how source observes or defaults that
absence, and how explicit provision and entry closure behave. An ordinary required service does not
become optional merely because a provider is absent.

**Diagnostics:** An effect entry with remaining requirements is unavailable before backend emission,
and the entry diagnostic lists each unresolved requirement. It does not suggest that the runtime
will fill the row later.

**Current compiler:** Aligned with empty requirement-row entry discovery. Some private native
adapter state supports explicitly selected host providers but does not satisfy a source requirement
by itself.

**Evidence:** [ENTRY-004](program-entry.md#entry-004--effect-entry-requirements-must-be-resolved),
[service provision](requirements-and-services.md),
[host-input explicit-provider requirement](../../../../openspec/specs/bootstrap-host-input/spec.md).

## Runtime support and pay-for-use

### RUNTIME-002 — Source closure and executable closure control separate costs

**Status:** Confirmed

Explicit imports determine the dependency-complete source closure that must parse and type-check.
Concrete specialization and entry reachability determine the executable closure that may emit code,
static data, host imports, adapters, and toolchain runtime support.

Importing an unused module therefore has source-analysis cost but no runtime behavior. An unreachable
function, constant, provider, or intrinsic contributes no executable support solely because its
module was loaded.

```silk,ignore
import silk.os_child_process { OsChildProcess }

pub fn main() -> i32 {
  return 0
}
```

A direct-Wasm build remains valid and contains no process import or support when no provider
operation enters the executable closure.

**Boundary:** Runtime control flow is executable behavior. A target-specific call in a branch whose
condition is runtime data remains reachable and requires target support. Compile-time specialization
may remove a declaration only under the ordinary reachability rules; an optimizer cannot redefine
source compatibility after the fact.

**Diagnostics and audit:** Unreachable support produces no programmer diagnostic. Artifact
verification rejects unrelated host imports, runtime symbols, static tables, or adapters not
justified by the retained executable inventory.

**Current compiler:** Aligned for intrinsic operations. Source analysis follows explicit import
closure, while executable support follows reachable specialized operations.

**Evidence:** [TARGET-002](unsafe-intrinsics-and-targets.md#target-002--unreachable-target-specific-primitives-have-no-artifact-cost),
[explicit module closure](modules-names-and-visibility.md#module-004--compilation-loads-only-the-transitively-reachable-module-closure),
[current intrinsic availability](../../../../packages/compiler/src/IntrinsicAvailability.ts).

### RUNTIME-003 — Toolchain runtime support guarantees contracts, not implementation ABI

**Status:** Confirmed

Each execution target implements the language entry contract and every reachable intrinsic contract
through target instructions, compiler-emitted helpers, a linked support object, or an evaluator.
Those contracts are the supported boundary. The raw symbols, calling conventions, layouts, and
helper structure beneath them may be compiler-versioned and may change whenever lowering, intrinsic
contracts, or target support changes.

Silk does not treat those implementation details as forbidden knowledge or permanently inaccessible
machinery. `extern "C"` ([FFI-001](unsafe-intrinsics-and-targets.md#ffi-001--extern-c-declares-a-native-symbol-under-an-explicit-abi))
is that explicit linking facility: a developer may deliberately name a target symbol that the
selected toolchain happens to expose. Doing so is unsafe, target-specific, toolchain-version-specific,
and outside the compatibility guarantee: the developer must satisfy the real ABI and accept that
another build may rename, replace, inline, or omit it.

The runtime must implement the canonical observable contract exactly. It cannot add hidden
recoverable failures, retain a Silk borrow beyond its declared lifetime, invoke arbitrary source
callbacks, change cleanup ownership, or reinterpret low-level outcomes as public domain values.

**Boundary:** Unsupported does not mean prohibited. The official compiler need not hide raw symbols,
and future low-level interop should be capable of reaching arbitrary target facilities. A stable
embedding API, portable FFI declaration, independently replaceable runtime, or guaranteed dynamic-
linking contract would create a new compatibility boundary and requires its own proposal. The
official compiler invoking one matched helper does not make that helper stable.

**Diagnostics:** A missing or incompatible support symbol the compiler itself requires from its
matched distribution is a toolchain-integrity error before execution. A developer's future explicit
unsupported linkage instead follows that low-level facility's own link and ABI diagnostics; the
toolchain does not imply compatibility merely because a symbol existed in another release.

**Current compiler:** Aligned in direction. Native support is compiler-versioned and direct Wasm
uses its own lowering model; neither is presented as a user-facing runtime library.

**Evidence:** [sealed intrinsic contracts](unsafe-intrinsics-and-targets.md#sealed-intrinsic-boundary),
[current native runtime support](../../../../packages/compiler/src/OsRuntime.ts).

### RUNTIME-004 — Silk has no ambient runtime facilities

**Status:** Confirmed

The language runtime does not imply a garbage collector, scheduler, thread pool, async executor,
reflection registry, dependency container, global allocator, random source, clock, filesystem,
environment, current directory, console, or logger.

Language-owned behavior is limited to the selected program's ordinary execution semantics: values,
calls, ownership and cleanup, Effect construction and execution, typed failure, fatal traps, and the
entry boundary. A program gains additional facilities by importing ordinary APIs and explicitly
constructing or providing their implementations.

`Effect.suspend` transfers one deferred child through the explicit stack-safe execution boundary.
It does not park an unfinished execution or schedule another one. The sealed Execution and Wake
identities provide a separate narrow seam for independently owned activation and external parking.
The ordinary `silk.execution` module exposes safe construction, drive, and park operations.
Schedulers, executors, queues, timers, deferred values, and cancellation policies remain ordinary
source. Programs that cannot reach these sealed operations acquire no scheduler or fiber cost.

This makes an allocation-free, host-independent program genuinely require no heap provider or host
runtime:

```silk
pub fn main() -> i32 {
  return 42
}
```

**Boundary:** Compiler-planned storage for a value or callable representation is part of target
lowering, not evidence of an ambient public allocator. A toolchain adapter may receive machine
process state so an explicitly selected provider can expose it, but ordinary source cannot read that
state without the provider contract. External parking specifies Wake ownership, dormant cleanup,
and target behavior. It does not select an executor. None of these contracts is inferred from
`Effect.suspend`.

**Diagnostics:** Using an unavailable language feature receives its language diagnostic; using an
unprovided service receives a requirement diagnostic. The compiler must not silently initialize a
runtime facility to make either program succeed.

**Current compiler:** Aligned for the current inventory. Entry and intrinsic reachability retain
only explicitly selected facilities, while direct-Wasm and pressure tests keep trivial programs
free of host imports, Scheduler, Fiber, LocalScheduler, Execution, and Wake machinery.

**Evidence:** [explicit requirements](requirements-and-services.md),
[Effect suspension](effect-suspension.md),
[entry requirement closure](program-entry.md#entry-004--effect-entry-requirements-must-be-resolved),
[runtime-tier pressure tests](../../../../packages/compiler/test/LocalSharedPressure.test.ts),
[intrinsic availability tests](../../../../packages/compiler/test/IntrinsicAvailability.test.ts).

### RUNTIME-005 — The compiler-generated adapter is the only mandatory program runtime boundary

**Status:** Confirmed

An executable contains one compiler-generated target adapter that invokes the selected public
zero-parameter `main` according to the language's program-entry rules. It is not a standard-library
function and cannot be imported as a source module. Ordinary `main` compilation does not replace
this adapter from source or create an additional user-visible entry API; a future explicit custom-
entry or freestanding build mode may define a different boundary.

- An ordinary `pub fn main() -> ()` executes eagerly and supplies status zero, while
  `pub fn main() -> i32` supplies its explicit status to the target entry contract.
- A `pub effect fn main()` constructs one Effect and the adapter runs it exactly once.
- Effect success becomes successful target termination.
- A concrete unhandled typed failure is closed at the boundary, reported according to the target's
  defined entry contract, and its owned payload receives ordinary failure cleanup.
- A fatal trap remains abnormal termination and promises no cleanup.

The adapter does not require a `Report` conformance, synthesize service providers, infer command-line
parameters, or expose native ABI values to `main`. Process arguments, environment, current
directory, and streams remain available only through explicitly selected ordinary services.

**Boundary:** Target implementations may use different private machine signatures—for example a
native process entry versus an exported direct-Wasm function—while preserving the same applicable
Silk entry semantics. Exact external embedding ABIs and custom entry points are separate future
contracts.

**Diagnostics:** Invalid `main` shapes and unresolved requirements are rejected before lowering
through the program-entry diagnostics. Missing private adapter support is a broken-toolchain error.
Unhandled typed failures and fatal traps retain their distinct runtime behavior rather than being
reported as source validation errors.

**Implementation:** Entry discovery, generated MIR, evaluator outcomes, and backend termination
contracts share the same entry inventory. The private native adapter is derived from that inventory;
a trivial closed entry links no stream, command-line, scheduler, allocator, or provider machinery.

**Evidence:** [program entry](program-entry.md),
[entry termination specification](../../../../openspec/specs/bootstrap-entry-termination/spec.md),
[typed-failure cleanup](typed-failures.md#fail-006--typed-failure-applies-ordinary-cleanup-and-preserves-diagnostic-context).

## Distribution compatibility and diagnostics

### DIST-001 — Compiler, canonical source, and required runtime support form one matched toolchain

**Status:** Confirmed

Before a native package and dependency system exists, the official compiler, canonical
standard-library source catalog, intrinsic catalog, target support, and required runtime-support artifacts
are one versioned toolchain distribution. Components the official compiler requires are built and
verified together and are not
independently selected, upgraded, overridden, or compatibility-negotiated by a Silk program.

The canonical catalog records enough content identity for reproducible source resolution. The
intrinsic/runtime inventory records enough identity for the compiler to reject a damaged or mixed
installation before source behavior depends on it.

**Boundary:** Lockstep distribution does not make the standard library part of the language or
promise source compatibility between pre-1.0 releases. It is an operational integrity rule. A
future package model may allow an independently versioned public library while required toolchain
support remains matched to the compiler.

**Diagnostics:** Mixed component versions, missing catalog files, digest mismatches, and absent
required runtime artifacts report one deterministic broken-toolchain diagnostic naming the expected
and observed component identities. The compiler does not continue with fallback copies, an older
runtime, or a user module shadowing the missing standard source.

**Current toolchain:** Aligned. [`ToolchainIntegrity`](../../../../packages/compiler/src/ToolchainIntegrity.ts)
publishes one normalized `silk-toolchain-v1` graph covering the compiler, catalog, exact source
bytes, sealed intrinsic inventory, target providers, and per-target runtime support. Its SHA-256
identities exclude absolute paths, timestamps, directory enumeration order, and other checkout
state. The driver validates the compiler/catalog/source/intrinsic set before project resolution,
then validates only providers and runtime support reached by the prepared program before emission.
Compiled artifacts retain the graph identity, and language-tooling inventories expose the same
graph and validation result.

**Evidence:** [standard-library manifest](../../../../packages/compiler/stdlib/manifest.json),
[intrinsic inventory](../../../../packages/compiler/test/fixtures/intrinsic-inventory.json).

### DIST-002 — Missing source, unsupported targets, open entries, and broken toolchains are distinct

**Status:** Confirmed

The toolchain classifies failures at the boundary that owns them:

| Condition                                                                             | Classification                                                                       |
| ------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------ |
| An import names no project or canonical library module                                | source-resolution error                                                              |
| A canonical module exists but its source is invalid                                   | ordinary source diagnostic at the library location, plus broken-distribution context |
| A reachable intrinsic does not support the selected target                            | compile-time target-compatibility error                                              |
| `main` retains a service requirement                                                  | unavailable-entry diagnostic listing the open row                                    |
| A matched runtime-support artifact required by the compiler is absent or incompatible | broken-toolchain error                                                               |
| A supported host operation fails while executing                                      | the operation's declared value, typed failure, or fatal-trap outcome                 |

These classes do not substitute for one another. In particular, target absence is not a typed
failure, a missing provider is not a runtime linker error, and a damaged distribution is not an
unknown user module.

**Boundary:** One source problem may prevent later checks whose prerequisites do not exist. The
compiler suppresses derivative backend or runtime noise rather than reporting every downstream
consequence.

**Diagnostics:** Each error names the responsible module, operation, requirement, target, or
component and points to the earliest actionable source location when one exists. Stable wording and
codes belong to the diagnostic catalog; this table fixes their semantic classification.

**Current compiler:** Aligned at the bootstrap driver boundary. A malformed, unreadable, or
mismatched distribution produces the structured `ToolchainFailed` outcome; unsupported reachable
intrinsics produce `TargetFailed`; missing project imports remain `SourceResolutionFailed`; open or
invalid entries remain `NoEntry`; source diagnostics remain `Rejected`; backend construction and
external tool execution retain their own outcomes. The CLI renders these classes separately and
does not reinterpret a broken installation as a source or backend error.

**Evidence:** [module diagnostics](modules-names-and-visibility.md),
[entry diagnostics](program-entry.md),
[target diagnostics](unsafe-intrinsics-and-targets.md#target-availability),
[CLI reporting](../../../../packages/cli/src/Report.ts).

### TOOLING-001 — Tooling presents library source and derived availability honestly

**Status:** Confirmed

Go-to-definition, hover, completion, signature help, references, documentation, and diagnostics use
the canonical `.silk` declaration for every standard-library and provider API. Generated manifests,
embedded bytes, caches, native shims, and TypeScript compiler code are never shown as the public API
definition when canonical source exists.

Completion discovers public declarations and modules in the selected canonical catalog even when
they are not yet imported. Each candidate identifies its defining module; accepting it inserts the
required explicit import, preserves the preferred module-qualified style, and offers or applies an
alias when the local name would collide. Completion must not make an unavailable declaration appear
in scope without the corresponding visible source edit.

Hover may summarize a provider's derived target compatibility and an operation's allocation,
failure, requirement, ownership, and unsafe contracts. Derived target information is advisory until
the operation becomes reachable in a selected executable.

For an incomplete Effect contract, tooling may offer competing explicit repairs: propagate the
residual failure or requirement into the enclosing signature, insert or scaffold a matching
recovery/provision operation, and add the corresponding imports. The action must use the precise
ordinary types calculated at the failing expression and must show the source edit before applying
it; it never creates an invisible inferred contract.

The generated standard-library reference lists the modules and public declarations actually present
in the selected toolchain catalog. This is the user-facing answer to whether a library facility is
implemented in that release; absent documentation cannot be replaced by an AI-invented API.

**Boundary:** Auto-import completion writes an ordinary import; it is automation, not an implicit
prelude or alternate name-resolution path. Tooling suggestions do not add hidden imports, providers,
target fallbacks, or semantic privilege. A copied equivalent declaration navigates to the copy and
behaves ordinarily rather than redirecting to a canonical actor because its spelling matches.

**Diagnostics:** When source belongs to a damaged toolchain, tooling reports the integrity problem
and may still open the available canonical file. It must not synthesize declarations that let
analysis continue as though the catalog were complete.

**Current tooling:** Aligned for this boundary. Canonical source documentation and navigation exist;
catalog completion materializes explicit collision-aware imports, and Effect repairs write visible
source edits.

**Evidence:** [standard-library documentation guide](../../../../packages/compiler/stdlib/DOCUMENTATION.md),
[editor intelligence tests](../../../../packages/compiler/test/EditorIntelligence.test.ts),
[canonical source requirements](../../../../openspec/specs/bootstrap-silk-stdlib/spec.md).

## Deferred directions

The following are deliberately outside the first stable model:

- native package acquisition, registries, version solving, and explicit re-exports;
- independently selected standard-library or required runtime-support versions;
- target-conditional source and module-level availability annotations;
- compiler-selected or overridable default service providers;
- optional service requirements, including absence, selection, and entry-closure semantics;
- foreign pointers, records, and exports, a stable ABI, dynamic linking, and user-selected runtime
  implementations;
- runtime suspension, concurrency, scheduler/executor selection, async I/O, streams, networking,
  entropy, and wall-clock facilities;
- alternative standard-library profiles or “no-stdlib” project configuration in the official
  toolchain; and
- omitted struct fields, field defaults, and any deliberate integration with ordinary `Option`.
