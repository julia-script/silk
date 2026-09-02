## 1. Lexer, parser, formatter

- [x] 1.1 Add `type` to the complete-identifier keyword table as `TypeKeyword`, and verify a lexer test distinguishes `type` from `typeName`.
- [x] 1.2 Add the `TypeAliasDeclaration` node kind and parse `[pub] type Name [<params>] = <type>` from the declaration-start and `pub`-following tables, using `parseType` for the target, and verify parser fixtures cover a union alias, an applied alias, a missing target with intact recovery, and a retained parameter list.
- [x] 1.3 Render the declaration in `SyntaxFormatter` as `[pub] type Name = <type>` through the existing type layout, and verify an idempotent formatting test with a multi-member union target and attached comments.

## 2. Declaration facts and resolution

- [x] 2.1 Collect an `AliasDeclaration` fact in `DeclarationCollection` with canonical id, visibility, name, retained parameter-list syntax, and the unresolved target type fact, joining the flat top-level namespace, and verify a collision with a same-named struct reports the existing collision diagnostic.
- [x] 2.2 Resolve alias targets lazily and memoized inside the type resolver: when a lookup hits an alias, return its resolved target as the `Resolved` fact; track an in-progress set and resolve to `Unavailable` with a cyclic-alias cause on re-entry. Verify tests for alias-through-alias in both declaration orders and for `type A = B; type B = A` reporting one diagnostic per declaration with the other related.
- [x] 2.3 Force every alias in `DeclarationCompletion` so cycle diagnostics are declaration-owned, reject a retained parameter list with a new diagnostic at the list span, and verify `type Pair<T> = Point<T>` reports the restriction and publishes no type while `type Pair = Point<i32>` resolves.
- [x] 2.4 Apply the field-exposure check to a `pub` alias's erased target and verify `struct Hidden {}` plus `pub type Leaked = Hidden` reports the ordinary exposure diagnostic at the alias.
- [x] 2.5 Verify alias uses in every type position: a struct field typed by an applied alias constructs and projects as `Point<f32>`; a scalar alias parameter accepts a second scalar alias argument; a union alias return type injects a member value; a mismatch diagnostic prints the erased union and not the alias name.

## 3. Failure rows

- [x] 3.1 Flatten a resolved structural-union member into individual members in `semanticFailureRow` and `resolveFailureRow`, dropping `never`, and verify that `-> () ! FetchError` with `type FetchError = HttpError | JsonError` yields a two-member row and `Effect.catch<HttpError>` leaves the residual `JsonError`.
- [x] 3.2 Verify `Effect.catch<FetchError>` on a row `! HttpError | JsonError | Timeout` leaves `Timeout`, identical to `Effect.catch<HttpError | JsonError>`, and that a nominal `union` alias member stays atomic under `catch`, on the evaluator and at least one compiled backend.

## 4. Module surface and cross-module use

- [x] 4.1 Add `Alias` to `ModuleSummary.DeclarationKind` and encode a public alias in `ModuleSurface` as name, visibility, and erased target, and verify an encode/decode round-trip for a public union alias.
- [x] 4.2 Verify surface invalidation: adding a union member to an exported alias invalidates direct dependents; reordering the members leaves the surface equal.
- [x] 4.3 Verify cross-module resolution: a selected import and a namespace-qualified use of a public alias resolve the same canonical type as the defining module, a private alias reports the visibility diagnostic with an inaccessible candidate, and an alias in value position reports the unknown-value diagnostic.

## 5. Tooling

- [x] 5.1 Add `type` to the TextMate and CodeMirror keyword tables and verify the highlighting fixtures.
- [x] 5.2 Add the alias declaration to LSP document structure and hover, and verify the LSP acceptance fixture lists a `pub type` symbol and hovers it with the erased target.
- [x] 5.3 Emit public aliases in docgen module references and the documentation JSON, and verify a generated reference for a module with `pub type` includes it with its erased target.
- [x] 5.4 Add the node kind to the syntax inspector and verify the inspector fixture renders it.

## 6. Documentation and acceptance

- [x] 6.1 Remove type aliases from the gap list in `alpha-status.md`, add the alias form to the typed-failures reference with the `FetchError` example, and verify the docs snippet tests compile the new example.
- [x] 6.2 Regenerate the diagnostic index and stdlib source tables and verify the staleness checks pass.
- [x] 6.3 Run the full gate with `node scripts/turbo.mjs run test` and verify it passes.
