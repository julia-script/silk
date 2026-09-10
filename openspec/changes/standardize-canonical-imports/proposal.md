## Why

The repository already requires importable owners for nonprimitive standard-library operations, but static utilities and raw Linux still expose module-level APIs. Unnecessary aliases also obscure canonical names in compiler source, fixtures, and documentation.

## What Changes

- **BREAKING**: Finish the existing scope-actor migration, including static utilities, target/build facts, and raw Linux operations; update every caller.
- Use selective owner imports and unaliased primitive imports throughout ordinary Silk examples and programs.
- Audit namespace and selected-member aliases in source, embedded programs, comments, and Markdown. Keep only collision cases and explicit alias-language tests or explanations, with their purpose recorded.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

None: this repairs implementation and examples to satisfy the existing nonprimitive scope-actor and primitive-import requirements in `bootstrap-silk-stdlib`.

## Impact

Canonical standard-library sources and generated source catalog, self-hosted compiler, compiler/tooling tests, documentation, and historical examples. Alias grammar remains supported. No compiler privilege or compatibility wrappers are introduced.
