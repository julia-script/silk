## Context

This change implements JUL-120 (F1), the profile and package-configuration part of the Target/source foundation milestone. Its authority is the Native OS Integration Plan, updated 2026-09-04T21:17:22.942Z, especially D-014, D-021, WS-03 and SPEC-01. See proposal.md for motivation.

The current Target actor has four canonical triples and pointer facts. StaticEvaluation maps those triples to ordinals. Project carries artifact settings separately, and ModuleClosure has no package configuration. Residualization already resolves constant initializers through ordinary static helper calls and nested constant callbacks. Reuse that evaluator and its deterministic limits; do not introduce a second expression interpreter.

## Goals / Non-Goals

Goals: specify and implement unconditional source schemas, dependency-aware defaults, typed binding precedence, validation, immutable publication, canonical identity, narrow intrinsic queries, and profile-aware compiler/tooling requests.

Non-goals: conditional declaration/import selection (JUL-121), aggregate layout or complete scalar/pointer ABI (JUL-123), resolving application/runtime roots (JUL-125), physical SDK/sysroot supplies and linking (JUL-126). The profile records logical runtime and link requests without claiming those later capabilities exist. It is not a general build scripting language.

## Decisions

### Source-owned declarations

Introduce `[pub] param name: Type = expression [where predicate]` at unconditional module scope. The initializer and optional predicate use ordinary Silk expression syntax. The predicate must statically produce bool and can call an ordinary static helper or use a block that calls compileError. Within the predicate, the parameter name denotes its final resolved value. A false predicate is a structured configuration validation failure. A parameter without a default uses `[pub] param name: Type [where predicate]`; its value must be supplied externally. Private parameters require a default and cannot receive external bindings.

Parameters are immutable static values at use sites, not variables or callable compiler operations. Introduce a distinct declaration kind so tooling can distinguish schema declarations from constants. Do not encode declarations as compiler-recognized calls to a standard-library helper. Inline predicates ensure externally supplied values are validated too; validation hidden only in a default helper would bypass overrides.

### Package and parameter identities

A resolved package instance has logical identity `name@version`; source provisioning maps it to a source root outside the profile. Two unequal package sources claiming the same logical identity in one graph are diagnosed. A parameter identity is the tuple (package identity, package-relative canonical module identity, declared name). Renamed import aliases and physical checkout locations cannot change it. Dependency bindings address only public declarations using that tuple. Standalone source requests supply an explicit logical root package identity at the application edge; the compiler does not invent identity from an absolute filename.

### Bootstrap and publication

1. Resolve and freeze initial target description plus artifact/build logical inputs. Validate their independent constraints. No parameter values exist yet.
2. Discover unconditional schema declarations and their unconditional imports. Parse loaded files normally. Collect headers before resolving parameter types so forward references are possible. F1 uses the unconditional closure; the selected closure is JUL-121's responsibility.
3. Resolve schema types. Only concrete serializable static types are admitted. A type whose shape depends on a parameter from the schema under construction is a bootstrap cycle, rather than an invitation to mutate a partially published profile.
4. Validate external binding identities, visibility, provenance and type. Group by parameter and tier. Multiple bindings at one tier are conflicts even when values are equal. Project and workspace share one tier; artifact and selected-profile overrides share one higher tier.
5. Resolve final parameter values on demand. Explicit winning bindings suppress evaluation of the replaced default. Default references observe other parameters' final values. Evaluate imported/forward constants and helpers using the existing evaluator under the frozen initial facts and a private dependency resolver. An in-progress dependency reached again produces a cycle trace. Cache keys here include initial logical facts, binding inputs and dependency source identities; they are not final profile keys.
6. After all values resolve, run every declared validation predicate in stable parameter identity order against the complete value map. Validation cannot update values. A validation predicate can read another resolved parameter without creating a value-resolution cycle.
7. Publish a deeply immutable normalized profile only after all stages succeed. Failed bootstrap publishes diagnostics, never a partial profile. Ordinary analysis, static specialization and backend planning then consume the completed profile.

No provisional profile escapes into semantic caches. A public completed profile is never revised to break cycles. Demand-driven default evaluation avoids false cycles in overridden defaults and uncalled helper bodies. JUL-121 must extend dependency availability tracking before adding conditional declarations; this ticket does not emulate conditional selection by eagerly loading its branches.

### Logical domains and encoding

Keep machine description, CPU/features, deployment, libc, artifact form, link policy, code/relocation model, optimization, safety, threading, sanitizers, unwind, runtime request and package values as separate typed fields. Artifact form is executable image, loadable module, static archive or relocatable object; emission stage (IR, bitcode, assembly, object or final artifact) is a separate request. Runtime selection is default, named logical package composition, or none. Entry policy is a logical default/named/none request; actual roots remain JUL-125.

Use a versioned canonical tagged encoding, not object identity or insertion order. Integers use canonical decimal text with their declared signedness/width; enums include nominal type identity and member; optionals distinguish none and some; arrays retain order; records use sorted declared field identities. Sort and deduplicate feature/sanitizer sets, and sort parameter tuples. Preserve strings exactly, without Unicode normalization. Include all normalized logical choices and the machine-description revision. Keep provenance alongside values but outside semantic identity. Diagnostic presentation uses the current request's origins, so a cached value never carries a previous request's physical paths.

Source/dependency identity remains an additional cache dimension. A changed helper/default invalidates dependent bootstrap results even if target and overrides are unchanged. Equivalent final values can share completed semantic identity after revalidation. Physical paths, SDK/sysroot discovery, output directories, runtime facts, timestamps and secret values cannot enter the canonical encoding.

### Intrinsic boundary and migration

Expose closed static-only fact queries for individual domains: architecture, OS, ABI, object format, endianness, primitive widths/alignment and logical build fields. Return primitive integers/bools/static strings as appropriate; ordinary Silk wrappers convert domain spellings to nominal enums. No whole-profile ordinal is exposed. Schema declarations provide direct static parameter access, avoiding arbitrary string-based privileged library field lookup. Native availability tests should use the narrow fact they require; exact target IDs remain selection and diagnostic identities.

### Inputs and tooling

Project manifests declare a selected profile name, named profiles and lists of typed bindings. Bindings carry explicit package/module/parameter identity and an admitted typed value. Integer transport uses decimal strings to avoid JSON precision loss; enum, optional, array and record transport is tagged. CLI and LSP support a complete profile-input override, named project selection, and a target-triple shorthand. Precedence between request modes is full override, explicitly selected project profile, project default, target shorthand when supplied without a profile, then explicit application-edge host fallback. Combining mutually exclusive request modes is an error; shorthand is not a partial mutation of a named profile.

External values carry provenance classified as literal, translated-public, secret, physical-supply or runtime. Reject the last three before echoing values. Explicit build-tool translation provides a deterministic value and logical translator identity, never an ambient callback. Do not heuristically classify arbitrary strings as secrets; callers must label secret-bearing inputs. Static Silk has no ambient environment or discovery capability.

### Evidence and admitted machine facts

Research pins verified in local checkouts: Zig e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa; Rust c33d8f3b5a50b56466998e8c5ed8a077d2caed84. LLVM interoperability pin: 22.1.8 (packages/llvm/UPSTREAM.md).

Zig Target.zig separates architecture/OS/ABI/object format and Build/Module.zig carries module build inputs; Rust rustc_target/src/spec/mod.rs separates Target and TargetOptions, while rustc_session/src/config/cfg.rs projects known target configuration. Silk adopts separate logical domains, but uses unconditional typed source schemas rather than copying Zig build execution or Rust string cfg flags.

Inspected Zig test/standalone/dep_shared_builtin checks shared builtin/root identity across imports; it does not prove same-target different-package-option isolation. Inspected Rust const-fn-cycle.rs is a check-pass regression for an unused const function, not a failing cycle oracle. const-size_of-cycle.rs rejects a type-level layout cycle; it is only an analogue for cycle diagnostics. Add Silk-specific fixtures for overridden cyclic defaults, imported/forward helpers, validation after override and same-target differing parameters. Neither language supplies a direct analogue for Silk's complete bootstrap contract.

The first fact fixture admits primitive integers, bool storage, f32/f64 and pointer width/alignment only, for the four ticket targets. Compare the compiler description with LLVM 22.1.8 data-layout/object output and independently compiled freestanding C static assertions. No host-header or host-layout fallback is permitted. Record exact Clang version, target flags, source digest and object inspection tool version with the fixture; obtain normative platform references before admitting each fact. No SDK/header version is needed for the header-free primitive fixture. Any later fixture using headers must pin those headers separately. This planning document is not a claim that fixture comparison has run or that aggregate/C ABI conformance is established.

## Risks / Trade-offs

- Bootstrap reuses analysis machinery that currently assumes a complete target environment → separate initial facts/private resolver from final publication and test that failed initialization cannot leak into a subsequent request.
- Nominal enum/record values may expose evaluator gaps → complete the admitted value support through existing static values, never silently restrict configuration to scalar literals.
- Wide consumer migration → inventory every ordinal intrinsic, target enum, cache key and tooling request before editing; delete superseded paths in the same change.
- Incorrect target facts → require pinned independent fixtures before publication; reject incomplete descriptions.

## Migration Plan

Land one coherent breaking change after the required checks. Update source consumers, catalogs, examples and documentation together. No transitional adapter, old-key fallback or dual parameter path is retained. Reverting the entire change is the rollback mechanism.
