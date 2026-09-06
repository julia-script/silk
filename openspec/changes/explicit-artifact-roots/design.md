## Context

CompilationProfile already separates logical artifact, runtime and entry selections. ArtifactKind lacks a durable relocatable object; realization maps executable to conventional main and every other native artifact to foreign-export roots. Source selection already computes active imports/declarations before completion. NativeLinkInput preserves ordered physical inputs. This change joins these foundations without making physical supply discovery part of semantic analysis.

## Goals / Non-Goals

Goals: explicit selected composition and retention, no-runtime forms without main, an ordinary source runtime importing the application, scoped logical requirements, all-origin conflict diagnostics, deterministic inspectable identity, and independently inspected optimized objects on the native matrix.

Non-goals: JUL-126 supplies/discovery, JUL-127 helper/libm migration, JUL-130 hosted startup/reporting migration, additional native symbol forms, arbitrary raw linker arguments, or LTO.

## Decisions

### Composition and binding

The compilation request root is the application root. An artifact composition catalog contains named runtime descriptors with a canonical source module and optional invocation declaration, default runtime candidate names, explicit retention selectors, and artifact requirements. Profile.runtime selects default, named or none. Default resolves zero or one distinct candidate; multiple candidates, missing named descriptors, duplicate descriptor identities and missing source roots are errors with configuration origins. None selects no runtime module and no invocation. A descriptor's optional invocation is a concrete root called by the existing execution adapter; it is independent of the loader symbol. Source compositions normally expose their own foreign exports and omit invocation.

Build defaults describe the existing hosted behavior explicitly: the application source is the runtime root and its invocation is main for a native executable. Non-executable defaults have no runtime. This keeps the existing hosted startup responsibility in JUL-130, while the compiler's generic resolver never inserts a fallback for an absent, invalid or explicitly none composition. A project may replace the defaults entirely. The descriptor and the default/named/none request both enter identity even when they resolve to the same source.

The generic import spelling is `import Intrinsic.application as app` (ordinary selective/public imports also work). Selected closure discovery resolves that sealed binding to the request's exact application module before name resolution. It does not clone source under a second identity. Ordinary visibility, imports, diagnostics, cycles and selection apply. A runtime is an additional analysis root; the application remains an analysis root. Unselected catalog modules are never loaded. A missing application binding in non-artifact analysis diagnoses when the import is active.

Retention selectors contain canonical module plus unambiguous declaration name. They select monomorphic runtime function bodies, including private functions, without exporting them. Invalid/static/generic/ambiguous/missing roles diagnose; no visibility filter is applied. Their modules are analysis roots, then the selected declarations are instance roots. Active foreign exports remain roots; ordinary public declarations do not. Roots are resolved before instance closure and optimization. Empty no-runtime compositions are valid. Duplicate active export definitions diagnose before emission.

### Forms, stages and entry

Native semantic forms are executable, loadable-module, static-archive and object. Emission stage is independently llvm-ir, llvm-bitcode, assembly, object or final. Final object writes the one relocatable object and never invokes a linker or generates an executable startup adapter. No-runtime archives/modules likewise omit the hosted entry adapter. Existing runtime support required by reachable compiler operations is tracked separately and remains in the JUL-127/130 migration inventory.

Profile.entry describes loader policy only: default, none or named symbol. A named symbol may come from a declared physical input. It neither selects a Silk function nor implies a foreign export. Non-final stages retain the policy in logical identity. Final tool plans emit explicit supported target linker entry options; unsupported form/policy combinations diagnose rather than ignore requests. Missing or conflicting build/runtime loader requests retain every origin. LTO is rejected by strict configuration decoding.

### Requirement syntax and scope

A function can have multiple sealed `with` clauses. `with Intrinsic.native(kind: "library", name: "example", linkage: "dynamic")` on a foreign function is declaration-bound; it activates only when that declaration is reachable. This composes with `with Intrinsic.foreign(...)`. A standalone `module with Intrinsic.native(...)` is module-bound and can occur inside module static selection. Artifact-bound requirements are typed composition configuration records. The scope follows the attachment; source cannot spell an artifact scope or a filesystem path.

Kinds are library, framework, startup-object, linker-script, prebuilt-object and prebuilt-archive. Required name is a logical identifier. Optional linkage is static/dynamic (libraries only); minimumDeployment and maximumDeployment are normalized numeric version bounds; alternatives is a nonempty tuple of admitted logical provider identities. Unknown/duplicate fields, malformed identities, inappropriate constraints and invalid placements diagnose at the property/attachment. Neither foreign symbol spelling nor standard-library module spelling contributes requirements.

Collection uses reachable foreign declarations, every selected module (even one with no runtime instance), and artifact configuration. Group by kind/name, collapse exact duplicate facts while retaining all origins, intersect alternatives, combine deployment bounds, and require agreement of hard linkage. Empty intersections and incompatible bounds/profile constraints produce one conflict carrying all contributing origins. Configuration choices must satisfy the merged requirement. Unresolved physical supplies remain visible in a logical plan; final linking requires explicit admitted bindings until JUL-126 provides the resolver. Existing ordered NativeLinkInput arrays are preserved, never set-deduplicated. No new raw linker escape is admitted.

### Identity and retention

An ArtifactPlan actor owns normalized composition, resolved declaration identities, exports, scoped requirement facts and merged constraints, selected module content digests, profile, form/stage, loader request/resolution, codegen settings and compiler distribution identity. Canonical sorted logical sets are independent of input construction order and machine paths; origins remain inspectable but do not enter semantic identity. Ordered physical input identity remains a separate ordered encoding for emission/link cache composition. Changes to default/named/none requests, names, roots, closure bytes, exports and compiler/codegen identity are visible even if today's emitted bytes happen to agree.

Explicit retained native definitions enter llvm.used (appending linkage in llvm.metadata), not llvm.compiler.used. Native object inspection verifies target retention flags and symbols after optimization; external exports remain ordinary export roots. An archive member is not automatically extracted by an unrelated downstream link: the produced archive retains the definition, and a final artifact requiring extraction must declare its physical link policy. This avoids conflating compiler retention, section GC and archive extraction.

## Prior art and authority

Studied local Zig revision e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa: Build/Step/Compile.zig Entry separates default/disabled/named entry, Module.zig attaches link choices to modules, start.zig imports root and owns platform startup choices. entry_point/build.zig proves entry affects cache/output; global_linkage/build.zig links two archives. Silk uses explicit selected module identities and a sealed application seam; it does not copy Zig's compiler-selected standard-library startup or infer exports from application spellings.

Studied local Rust revision c33d8f3b5a50b56466998e8c5ed8a077d2caed84: link_attrs.rs validates kind-specific native modifiers; link.rs distinguishes archive extraction from GC and preserves ordered native operations. c-link-to-rust-staticlib/rmake.rs independently links C, and lto-linkage-used-attr/rmake.rs covers symbols lost under LTO. Silk adopts separate logical constraints and physical order; Rust crate-wide attributes are not an exact analogue of declaration reachability. LTO remains rejected rather than claiming coverage from Rust's test.

Correctness authority is LLVM 22.1.8 LangRef llvm.used/linkage/sections plus pinned AAPCS64, Apple ARM64 and x86-64 psABI authorities and Darwin/GNU tools in supplies.json. llvm.used, rather than compiler.used, is required because retention includes assembler/linker stages. Initial fixtures use monomorphic i32 exports and private scalar functions, explicit objects/archives and small separate C consumers. No new aggregate or variadic ABI is admitted. Missing supplies fail the designated conformance runner. Debug and optimized Darwin ARM64 and GNU/Linux ARM64/x86-64 require real object/archive/link inspection.

## Risks / Trade-offs

Application binding must preserve one module identity and selected source closure; a resolver alias that duplicates the module would be incorrect. Retention must survive LLVM optimization and object lowering, not merely instance discovery. Requirements must be collected after reachability while module facts remain available even for empty modules. Physical supply completeness is deliberately not inferred from a logical name; unresolved final-link requirements diagnose. Existing hosted adapter/helper policy remains explicitly scoped to its later owner and is never silently selected for an explicit no-runtime or custom source composition.

## Validation Strategy

Shared analysis snapshots cover root/selection/requirement activation, codes and spans, conflict origin sets and identity distinctions. MIR/LLVM/optimized object/archive/export inspection proves retention and form. A separate C consumer runs only to distinguish exported behavior. Pinned native conformance covers all three targets and both optimization modes. Run typecheck, format:check, lint, test, check and release:candidate in order; record actual failures and baseline status before stack submission.
