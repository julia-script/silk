# `@silk-effect/language`

Editor language support for Silk Effect.

- `@silk-effect/language/CodeMirror` — a CodeMirror 6 extension that highlights Silk by running the
  compiler's bootstrap lexer over the document. No separate grammar; the lexer is the single source
  of truth, and invalid bytes are marked the way the compiler will reject them.
- `@silk-effect/language/TextMate` — the Silk TextMate grammar (`source.silk`) and VS Code language
  configuration, consumed by Shiki for docs code fences and by VS Code-compatible editors.

```ts
import * as SilkCodeMirror from '@silk-effect/language/CodeMirror'

new EditorView({ extensions: [SilkCodeMirror.extension()] })
```

See [docs/README.md](docs/README.md) for details.
