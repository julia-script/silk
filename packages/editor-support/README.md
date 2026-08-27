# `@silklang/editor-support`

Portable editor integration for Silk. This package owns the browser- and editor-facing language
support that can be reused independently of any one application:

- `@silklang/editor-support/CodeMirror` provides lexer-driven CodeMirror highlighting.
- `@silklang/editor-support/TextMate` provides the canonical TextMate grammar and VS Code language
  configuration.
- `@silklang/editor-support/Editor` provides the framework-free CodeMirror editor with diagnostics,
  hover, inlay hints, and UTF-8/UTF-16 position mapping.
- `@silklang/editor-support/Element` and `/register` provide the `<silk-snippet>` custom element.
- `@silklang/editor-support/bundle` resolves to the self-registering browser bundle built for
  generated documentation sites.

The standalone protocol server remains in `@silklang/lsp`; this package consumes its reusable
document projections when adding semantic editor features.

```ts
import * as SilkCodeMirror from '@silklang/editor-support/CodeMirror'

const extension = SilkCodeMirror.extension()
```

To register the custom element:

```ts
import '@silklang/editor-support/register'
```

Then use `<silk-snippet>` in ordinary HTML. Source text remains visible before JavaScript loads;
the element upgrades it to the editor when registered.

The VS Code extension files are generated from `TextMate` with:

```sh
pnpm --filter @silklang/editor-support sync:vscode
```
