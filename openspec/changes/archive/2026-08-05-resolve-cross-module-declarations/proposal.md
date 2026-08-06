## Why

The compiler already loads a deterministic module closure and assigns declaration identities across
it, but each body is still elaborated against declarations rebuilt from its own source file. Imports
therefore discover files without creating usable namespace or member bindings, so a Silk program
cannot call even a public function from another module. Cross-module resolution is the next required
foundation for operator desugaring, nominal data, and a compiler organized into actor modules.

## What Changes

- Replace the provisional single-identifier import with the accepted path, namespace-alias,
  selective-member, member-alias, and hybrid forms, preserving every token and recovery state in
  concrete syntax.
- Interpret dotted source import paths as exact case-sensitive, extensionless canonical module
  identities and retain the current root-driven, deterministic, cycle-tolerant closure loading.
- Make top-level functions private by default with explicit `pub`, and publish visibility in the
  closure-wide declaration index.
- Add one immutable closure-wide name-resolution artifact that builds each module's flat top-level
  scope from local declarations and explicit import bindings, diagnoses invalid aliases,
  inaccessible or unknown members, duplicate imports, and binding collisions without choosing a
  winner by import or declaration order.
- Elaborate unqualified and namespace-qualified calls through that shared resolution artifact so
  HIR carries canonical cross-module declaration identities and mutually importing modules can call
  each other's explicitly contracted public functions.
- Follow resolved cross-module HIR calls during instance discovery, lowering, interpretation, LLVM
  emission, and WebAssembly emission without creating module-level runtime initialization or
  per-module codegen units.
- Expose import bindings and lookup outcomes through the analysis facade and inspector labs,
  including damaged, missing, private, conflicting, and cyclic cases.
- **BREAKING**: replace phase-local elaboration entry points and the provisional import/declaration
  fact shapes with closure-aware inputs and visibility- and binding-aware results. No compatibility
  layer will preserve the unreleased compiler API.

## Capabilities

### New Capabilities

- `bootstrap-name-resolution`: Closure-wide module scopes, explicit import bindings, visibility,
  deterministic lookup outcomes, collision diagnostics, and cycle-safe declaration resolution.

### Modified Capabilities

- `bootstrap-syntax`: Parse the accepted import forms and private-by-default function declarations
  losslessly with bounded recovery.
- `bootstrap-module-closure`: Resolve multi-segment import paths to exact canonical logical module
  identities while preserving deterministic closure and cycle facts.
- `bootstrap-declaration-index`: Publish public and private function headers once for the whole
  closure before any body resolves.
- `bootstrap-hir`: Resolve local, selectively imported, and namespace-qualified calls through the
  closure-wide index and binding scopes.
- `bootstrap-instances`: Discover reachable functions transitively across canonical module
  boundaries.
- `bootstrap-analysis-facade`: Make module scopes, import bindings, and cross-module lookup facts
  supported snapshot queries.
- `bootstrap-syntax-inspector`: Inspect resolved import bindings, visibility, cross-module HIR
  references, and cross-module instance discovery through facade-only labs.

## Impact

- Compiler actors affected: tokenization and parsing, `ModuleClosure`, `DeclarationIndex`, a new
  `NameResolution` actor, `Elaboration`, `Analysis`, and cross-module fixtures for `Instances`,
  lowering, evaluation, and both backends.
- Docs labs affected: syntax inspection, module closure, declaration index, HIR, instance discovery,
  pipeline overview, and the facade-only import boundary checks.
- Deterministic syntax/HIR encodings and relevant golden files will change; module, semantic, native,
  and WebAssembly differential coverage will grow.
- The compilation request remains an in-memory logical source map. Filesystem discovery, package
  management, re-exports, wildcard or implicit imports, runtime module initialization, structs,
  general type dependency resolution, and operator syntax remain outside this change.
