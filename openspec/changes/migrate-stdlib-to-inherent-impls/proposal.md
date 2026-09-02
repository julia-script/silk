## Why

After `add-inherent-impl-members`, two mechanisms answer `Option.map`: the declared member set and
the legacy projection of a module's public root functions through a basename-matching nominal.
Keeping both would recreate the ambiguity the redesign removes. Silk is unreleased, so there is no
compatibility window to honor: this change moves every standard-library operation into its owner's
inherent impl, deletes the filename projection, and rewrites the documentation and style guidance
that formalized it, in one cut.

## What Changes

- **BREAKING** Move every public operation reached today through a filename-matching nominal or a
  zero-data scope actor into `impl Owner { ... }` blocks in that owner's module, across all
  standard-library modules (`Option`, `Result`, `Vector`, `String`, `Bytes`, `Effect`, `Format`,
  `HashMap`, `HashSet`, `Shared`, `Box`, `Slot`, filesystem, clocks, loggers, providers, and the
  rest). Private helpers such as `keepPresent` stay free functions. Primitive modules (`silk.i32`,
  `silk.u8`, ...) are intrinsic namespaces and are untouched.
- **BREAKING** Delete the nominal-module projection: `NameResolution.scopedModule` and every
  caller in call resolution, expression analysis, and completion. A nominal qualifier exposes only
  its associated items; a module namespace exposes only root declarations. The module basename has
  no semantic role.
- **BREAKING** Selective imports of former root operations (`import silk.option { none, some }`)
  stop resolving; every such site in the standard library, tests, fixtures, examples, and
  documentation is rewritten to `Option.none` / `Option.some` or a pipeline section.
- Rewrite `NAME-005`, `STYLE-002`, and `STYLE-003` in the reference: operations intrinsic to one
  nominal type live in its inherent impl with the receiver first so direct, section, and pipeline
  forms share one contract; operations over several peer types remain free functions; a type
  import never doubles as a module namespace.
- Regenerate standard-library documentation, the diagnostic index, and stdlib source tables; retire
  the "scope actor" vocabulary in favor of "owner".
- Update every test fixture and example project that relied on the filename rule to declare impls.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-silk-stdlib`: standard-library operations are inherent members of their owner; the
  scope-actor requirement is restated in those terms.
- `bootstrap-name-resolution`: a nominal qualifier resolves only associated items and the module
  basename has no semantic role; selective imports bind only root declarations.
- `language-server-completion`: qualified completion after a nominal lists associated items only;
  after a namespace, root declarations only.

## Impact

Every standard-library module with a scope actor (about 40 files, roughly 260 public operations
including `Effect`'s effect functions), `NameResolution.ts`, `CallResolution.ts`,
`ExpressionAnalysis.ts`, `Completion.ts`, the `Effect` intrinsic-qualifier lookup, about 94
files with selective imports of former root functions, semantic and editor-intelligence tests that
exercised the projection, docs pages that declare a filename-matching type, generated stdlib docs,
and the style guide. Depends on `add-inherent-impl-members`; independent of
`add-method-call-syntax`, which only adds receiver spelling on top of the migrated members.
