# Compiler-shipped standard-library sources

The files under `silk/` are the canonical standard-library modules embedded by the stage-0
compiler. `Stdlib.generated.ts` is derived build inventory; editors and language-server definition
results point back to these authored source files.

Language-facing standard-library documentation lives in the docs app:

- [standard-library index](../../../apps/docs/content/language/stdlib/README.md)
- [language documentation](../../../apps/docs/content/language/README.md)
- [language glossary](../../../apps/docs/content/language/glossary.md)

Public API reference pages are generated from the `//!` and `///` comments in the Silk sources. See
[DOCUMENTATION.md](./DOCUMENTATION.md) for contributor rules, then regenerate the checked-in docs
with:

```sh
pnpm --filter @silklang/compiler documentation:generate
```

Do not add a parallel language reference here. Compiler-specific source layout and maintenance
instructions belong in this directory; programmer-facing language and standard-library material
belongs in `apps/docs/content/language/`.
