# SLP-0008: Layered toolchain and ordinary standard library

SLP: 0008
Status: Candidate
Revision: 11
Author: Julia Ortiz
Created: 2026-08-19
Updated: 2026-08-19
Discussion: —
Review record: —
Depends on: SLP-0003, SLP-0004, SLP-0005, SLP-0006, SLP-0007
Split from: —
Split into: —
Supersedes: —
Superseded by: —
Revisit when: —
Resolution: —
OpenSpec handoff: —

## Summary

Provisional thesis: Silk separates four layers that happen to ship together during bootstrap but
have different contracts: the language core, canonical ordinary-Silk library modules, ordinary
target/provider modules, and toolchain runtime support. The language defines syntax, types, static
semantics, execution contracts, and the sealed `Intrinsic` boundary without requiring a particular
public library inventory. Programs explicitly import the library modules they use; executable
reachability includes only their source closure and required runtime support. The toolchain may ship
these layers as one matched distribution without making standard-library declarations compiler
privileged or promising that raw support symbols form a stable runtime ABI.

## Problem and evidence

The bootstrap workload has already produced a broad standard library: scalar actors, Effect and
Result composition, allocation and ownership foundations, vectors, bytes, strings, hashing,
Unicode, logging, filesystem, standard input, host input, child processes, and native providers.
The resolved bootstrap issue correctly bounded that workload, but its phrase “minimum runtime and
standard library” now risks turning everything the compiler happens to need into mandatory language
surface.

Several established rules already point toward a cleaner boundary. Standard-library files compile
as ordinary visible `.silk` source. Public wrappers receive no compiler privilege. Standard-library
actors require explicit imports. Services remain ordinary replaceable contracts, while only sealed
intrinsics cross into compiler/runtime behavior. Target-specific primitives are pay-for-use after
executable reachability.

What remains undefined is the distribution model joining those decisions: which declarations are
language bindings, which modules a conforming toolchain guarantees, whether platform providers are
portable standard library or target adapters, what a program links when it imports but does not use
a facility, and whether compiler, library source, and runtime may version independently.

## Driving examples: current and desired

### Case: Use an ordinary portable collection without acquiring a runtime platform

#### Intent

Build a small Wasm program with owned dynamic storage but no filesystem, process, console, or native
host dependency.

#### Current Silk

The compiler ships `silk.vector` in one generated manifest alongside portable actors, native
providers, Unicode tables, and platform boundaries. Reachability is generally pay-for-use, but the
manifest itself does not state which layer or compatibility contract each module belongs to.

#### Desired Silk

```silk,ignore
import silk.vector as Vector

pub fn main() -> i32 {
  let values = Vector.make<i32>()
  return 0
}
```

#### Observable result

The import resolves to canonical visible Silk source. Only the reachable vector, allocation, and
primitive support enters the executable closure. No filesystem, process, host-input, console,
native-provider, or unrelated Unicode support is linked merely because it ships in the same
toolchain.

#### Boundary case

```silk,ignore
pub fn main() -> i32 {
  let values = Vector.make<i32>()
  return 0
}
```

Without an explicit import, `Vector` is unknown. The compiler does not seed standard-library actors
as a hidden prelude.

### Case: Keep a portable service separate from one target implementation

#### Intent

Write reusable code against a filesystem contract and choose a native or virtual implementation at
the application boundary.

#### Current Silk

Canonical source already separates `silk.filesystem` from `silk.os_filesystem`, and target
availability rejects reachable OS intrinsics on direct Wasm. The overall distribution contract does
not yet say whether the OS provider is part of the portable standard library, a target library, or
toolchain runtime policy.

#### Desired Silk

```silk,ignore
import silk.bytes { Bytes }
import silk.filesystem { FileSystem, Path }

effect fn load(path: &Path) -> Bytes ! FileError ? &mut FileSystem {
  return run FileSystem.readFile(path)
}
```

The reusable module imports only the portable contract. A native executable may separately import
and provide an OS implementation; a browser or test may provide ordinary source of its own.

#### Observable result

Importing or compiling `load` creates no native host dependency. Only selecting a reachable target
provider introduces its target-specific intrinsics and required toolchain support.

#### Boundary case

Selecting a native provider in a direct-Wasm executable reports compile-time target incompatibility.
It does not silently substitute a provider, add host imports, or convert target absence into a
typed filesystem failure.

## Goals and non-goals

### Goals

- Separate language semantics, public library source, target providers, and toolchain runtime support.
- Define which names exist without imports and how canonical shipped modules resolve.
- Preserve ordinary-source semantics and navigation for all public library declarations.
- Define pay-for-use source and runtime closure across portable and target-specific modules.
- Establish the compatibility relationship among compiler, shipped source, and required runtime
  support.
- Bound the minimum stable distribution without canonizing the bootstrap compiler's complete API
  wishlist.

### Non-goals

- Stabilize every existing standard-library actor API in one proposal.
- Design a general package manager, registry, dependency solver, or re-export system.
- Add default service providers before their selection and override rules are designed.
- Expose a public C ABI, FFI, dynamic runtime discovery, or independently replaceable system runtime.
- Add concurrency, networking, serialization, wall-clock time, randomness, streams, or JSX templates.

## Current language model

Closed language bindings include scalar types and literals, `Effect` type syntax and construction,
function and control syntax, ownership operations, and the global sealed `Intrinsic` namespace.
Standard-library actor namespaces are intended to require explicit imports, although current name
resolution still seeds a manifest-derived prelude that conflicts with the confirmed module rules.

Canonical `.silk` library files are the editable source of truth and a generated embedded table is
verified against them. They compile through the ordinary pipeline. One manifest currently lists
portable values and contracts, generic helpers, large generated Unicode data, and native providers
together. Native lowering links a private compiler-versioned support object; direct Wasm remains
import-free unless a future host contract says otherwise.

## Proposed language model

The language core owns only behavior that cannot be expressed as ordinary Silk: grammar, type and
ownership semantics, Effect execution mechanics, entry adaptation, concrete layout/lowering, fatal
traps, and sealed intrinsic contracts. It does not own `Option`, `Result`, `Vector`, `String`,
`Logger`, `FileSystem`, an OS provider, or another declaration by name.

The standard library is a versioned set of canonical ordinary Silk modules under a reserved
distribution identity. Portable modules expose reusable values, functions, interfaces, services,
and policies. Target-provider modules are also ordinary Silk but explicitly choose target-specific
intrinsics. Toolchain runtime support implements reachable intrinsic and machine-entry contracts;
the toolchain guarantees those contracts without promising its raw symbols or ABI.

## Worked language experience

The recommended first stable distribution has one canonical source catalog rather than selectable
profiles. Closed language names and `Intrinsic` are implicit; every ordinary actor requires an
explicit import from the reserved `silk.*` origin. The catalog, generated documentation, and
auto-import index answer which library declarations exist in the selected toolchain release.

Portable modules never select target providers. Provider modules remain ordinary source and may be
imported on any target; only their reachable restricted intrinsics determine executable
compatibility. An effect entry closes every requirement explicitly. Shipping `StdoutLogger` or
`SystemAllocator` does not cause the entry adapter to provide it.

Compiler, canonical library source, intrinsic inventory, target support, and required runtime form
one matched pre-package-system toolchain. Missing imports, unsupported reachable intrinsics, open
entry requirements, operational typed failures, and damaged toolchain components remain distinct
diagnostic classes.

## Semantic sketch

- Language bindings require no source import because they are syntax or closed semantic identities.
- Public reusable actors are ordinary declarations and require explicit imports.
- Canonical shipped source receives stable module identity and ordinary source locations, not
  semantic privilege.
- Import closure controls what is loaded; executable reachability and specialization control what is
  emitted.
- A portable module does not depend on a target provider or runtime-support symbol merely because
  both ship in one distribution.
- A target provider is ordinary source whose reachable intrinsic calls determine compatibility.
- Toolchain runtime support guarantees sealed intrinsic and entry contracts, not raw implementation
  symbols. Future low-level linking may deliberately reach unsupported symbols at the developer's
  own target- and version-specific risk.
- Toolchain packaging may require matched components without turning their private representation
  into a language compatibility promise.
- The official first stable toolchain exposes one target-independent canonical source catalog; it
  has no selectable standard-library profiles or ambient prelude actors.
- Every cataloged module is explicitly importable and its declarations use ordinary `pub` or private
  visibility until a general package and re-export model exists.
- Import closure costs source analysis; executable closure alone incurs emitted code, static data,
  host imports, providers, and toolchain runtime support.
- The runtime supplies no ambient allocator, dependency container, scheduler, host state, console,
  filesystem, clock, randomness, or logger.
- One compiler-generated adapter is the mandatory program runtime boundary; it implements the
  existing ordinary/effect `main` semantics without a `Report` marker or implicit providers.

## Compiler–standard library boundary

### Compiler necessity

Source cannot parse or type-check itself without an initial compiler, lower values without target
layout, execute an Effect entry without a machine adapter, or implement machine and host primitives
that ordinary Silk cannot express. Canonical module resolution also requires the toolchain to locate
the source it distributes.

### Smallest target-neutral primitive

The sealed intrinsic catalog and language-defined entry contract remain the only source-observable
compiler/runtime primitive boundary. Distribution resolution supplies canonical source bytes and
identity but grants no declaration semantics.

### Standard-library construction

Ordinary source builds all public values, errors, interfaces, services, provider types,
combinators, validation, policy, buffering, collections, encoding, and safe wrappers. Target
providers translate narrow intrinsics into portable contracts without being selected by the
compiler by name.

### Privilege audit

No compiler-known actor, hidden prelude module, standard-library-only unsafe permission, automatic
platform provider, public runtime symbol, or manifest-wide linkage is required. Any such behavior
must either reduce to a closed language rule, a sealed intrinsic, ordinary import/provision, or a
separate future proposal.

## Whole-language interaction map

| Surface | Disposition | Analysis |
| --- | --- | --- |
| Syntax and names | Affected | Closed language bindings, explicit imports, reserved distribution identities, and provider module names must be separated. |
| Types and abstraction | Not affected — library declarations use existing rules | Public library types, interfaces, services, and generic functions remain ordinary nominal declarations. |
| Execution contracts | Affected | Entry adaptation, service closure, future default provision, and runtime-support reachability need one boundary. |
| Ownership and resources | Affected | Allocation owners and provider resources remain source-defined while toolchain release primitives preserve their guaranteed contracts. |
| Runtime and targets | Affected | Portable modules, target providers, intrinsic availability, guaranteed runtime support, unsupported low-level linkage, and direct-Wasm imports must be distinguished. |
| Compiler | Affected | Canonical source resolution, closed bindings, entry adaptation, reachability, and intrinsic/runtime linkage participate. |
| Standard library | Affected | One canonical ordinary-source catalog covers the portable bootstrap baseline and target providers without selectable profiles or compiler-known actors. |
| Tooling and diagnostics | Affected | Navigation uses canonical source; diagnostics distinguish missing source, unavailable targets, open entries, operational failures, and damaged components. |
| Learning and use | Affected | Closed language names exist automatically; ordinary APIs are imported; services are provided; runtime support follows reachability. |

## Scope cohesion

This proposal decides one distribution boundary: what the language guarantees versus what a matched
toolchain supplies as ordinary source and private support. Individual collection, text, filesystem,
process, or formatting API designs remain separate standard-library work; a long actor inventory is
evidence about distribution pressure, not one indivisible semantic thesis.

## Complexity and subtraction budget

Prefer one closed language vocabulary, one canonical source-resolution mechanism, ordinary imports,
ordinary service provision, and one sealed runtime primitive boundary. Do not add a hidden prelude,
compiler-known library actors, runtime registries, ambient host objects, or compatibility shims.

## Surface displacement

This direction removes semantic weight from the current aggregate manifest. It may add an explicit
distribution/profile classification and reserves canonical shipped module identities, but it adds
no new value or execution construct.

## Drawbacks and risks

- Explicit imports make tiny programs slightly more verbose.
- A lockstep toolchain bundle is operationally simple but prevents independent library upgrades.
- Splitting portable and target-provider modules expands the visible module inventory.
- One catalog is simple and portable but every toolchain installation carries source for facilities
  that many programs never import.
- Treating every current module as mandatory would freeze bootstrap accidents; making too little
  guaranteed would weaken portability and documentation expectations.

## Alternatives and prior art

### Status quo

Keep one undifferentiated shipped manifest and let implementation reachability decide cost. This is
working code but does not tell programmers which surface is language, portable library, provider,
or required runtime support.

### Smaller primitive or library solution

Document only that `silk.*` modules are ordinary source and leave distribution contents unspecified.
This preserves freedom but gives no portable baseline and makes missing modules depend on a
particular installation.

### Strongest competing language model

Make a rich prelude and platform object intrinsic to every program, with compiler-known `Option`,
`Result`, collections, I/O, allocation, and default services. This simplifies introductory examples
but creates ambient cost, target assumptions, hidden provider policy, and a large privileged
compiler surface.

## Falsifiers and acceptance blockers

- A required public abstraction cannot be expressed with ordinary source plus the sealed intrinsic
  and entry contracts.
- Explicit imports cannot preserve the closed meanings required by language syntax without hidden
  actor resolution.
- Pay-for-use cannot exclude unrelated shipped source or runtime support before target lowering.
- A provider-independent portable contract cannot be separated from its target implementation.
- No small distribution baseline supports useful portable programs across native and direct Wasm.

## Open realization questions

- Whether canonical standard-library source is embedded, installed beside the compiler, or both.
- Which catalog metadata is stored versus derived from canonical imports, docs, and intrinsic use.
- Which existing aggregate modules must split to preserve the portable-to-provider dependency
  direction.

## Future directions

Native packages, re-exports, semantic versioning across third-party libraries, independently updated
standard libraries, default entry providers, optional services, hosted-Wasm contracts, stable
FFI/ABI, dynamic linking, user-selected runtime implementations, runtime suspension and async
execution, and omitted/defaulted struct fields remain future work. Optional services must define
absence, provision, override, and entry-closure behavior instead of silently weakening ordinary
service requirements. The current model gives `Result` no language-specific propagation syntax:
`run` remains the only automatic typed-failure propagation convenience. A later values proposal may
decide whether omitted optional fields integrate deliberately with ordinary `Option` or use a
general field-default model.

The current `Effect.suspend` operation supports deferred Effect construction and stack-safe
trampolining; it is not runtime parking. Future async work may require a narrow compiler/runtime seam
for resumable execution plus ordinary-source scheduler and executor APIs, while preserving zero
scheduler cost for programs that cannot suspend or run concurrently.

## OpenSpec realization map

None until the accepted direction is translated into implementation planning.

## Revision and decision record

| Revision | Date | Change or decision |
| --- | --- | --- |
| 1 | 2026-08-19 | Created the Draft around four distinct layers—language core, portable ordinary source, target-provider source, and private runtime—and left the stable distribution inventory, provider placement, compatibility, and diagnostics open. |
| 2 | 2026-08-19 | Drafted the complete recommended model: explicit imports from one canonical source catalog, ordinary visibility for every cataloged module, portable/provider dependency separation, one compiler-generated entry adapter without implicit providers or `Report`, no ambient runtime, executable pay-for-use, a matched private toolchain, and distinct diagnostic classes. |
| 3 | 2026-08-19 | Reframed the bottom layer as guaranteed toolchain runtime support rather than forbidden private machinery: intrinsic and entry contracts are stable boundaries, while future explicit low-level linkage may reach unsupported target symbols at the developer's own ABI and version risk. |
| 4 | 2026-08-19 | Preserved future `try`-like syntax for ordinary `Result`-like values through a general explicit eligibility contract; control-flow syntax may be language-owned without making `silk.result.Result` compiler-known. |
| 5 | 2026-08-19 | Removed speculative `Result` propagation syntax from this direction: `run` remains the sole automatic typed-failure propagation convenience, while omitted/defaulted struct fields and possible deliberate `Option` integration are deferred to values and construction design. |
| 6 | 2026-08-19 | Confirmed the four-layer language/library/provider/toolchain-support boundary, explicit imports from the reserved `silk.*` origin, and one deterministic target-independent source catalog whose contents are library rather than language vocabulary. |
| 7 | 2026-08-19 | Confirmed one-way portable-to-provider dependencies, explicit library cost and Effect contracts, and ordinary visibility for all cataloged modules; required LSP actions to automate precise contract propagation, imports, and recovery/provision scaffolds without invisible compiler inference. |
| 8 | 2026-08-19 | Confirmed explicit target providers, separate source and executable closure, the toolchain-support boundary, no ambient runtime facilities, and the compiler-generated entry adapter; deferred optional services and distinguished today's deferred `Effect.suspend` from future runtime suspension and scheduler design. |
| 9 | 2026-08-19 | Made catalog-wide import completion a required LSP behavior: candidates identify their defining modules and acceptance inserts an explicit import or collision-resolving alias without changing language name resolution. |
| 10 | 2026-08-19 | Confirmed the matched-toolchain integrity model, distinct diagnostic classifications, and canonical-source tooling contract, completing review of all normative rules in the Draft. |
| 11 | 2026-08-19 | Promoted the fully reviewed direction to Candidate with the author's explicit acceptance. |
