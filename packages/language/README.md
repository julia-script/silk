# `@silk-lang/language`

Editor language support for Silk Effect.

- `@silk-lang/language/CodeMirror` — a CodeMirror 6 extension that highlights Silk by running the
  compiler's bootstrap lexer over the document. No separate grammar; the lexer is the single source
  of truth, and invalid bytes are marked the way the compiler will reject them. Documentation
  markers, CommonMark markup, intra-document links, and fenced Silk examples are layered lazily by
  `@silk-lang/documentation/Highlight`.
- `@silk-lang/language/TextMate` — the Silk TextMate grammar (`source.silk`) and VS Code language
  configuration, including distinct `///` / `//!` scopes, documentation markup, and nested Silk
  fenced-code scopes. It is consumed by Shiki and VS Code-compatible editors.

Both paths classify the compiler's single-line and triple-quoted text/byte literal forms,
modifiers, delimiters, bodies, and escapes. CodeMirror consumes compiler tokens directly; the
TextMate grammar is generated and behaviorally checked against the same literal-form vocabulary.

```ts
import * as SilkCodeMirror from '@silk-lang/language/CodeMirror'

new EditorView({ extensions: [SilkCodeMirror.extension()] })
```

The language documentation is owned by the documentation app. Start with the
[language guide](../../apps/docs/content/language/index.md) or the
[prescriptive reference](../../apps/docs/content/reference/index.md).
