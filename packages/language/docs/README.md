# `@silk-lang/language` documentation

## The Silk language

- **[Tutorial](./tutorial.md)** — start here: from `silk init` to a program that runs.
- **[Language reference](./reference.md)** — the lexical form, the types, the memory and ownership
  rules, and the effect system.
- **[Recursion and the machine stack](./recursion.md)** — what ordinary recursion does and does not
  guarantee, how running out of stack looks on each engine, and the iterative pattern to use
  instead.
- **[Standard library](./stdlib/)** — every module and public declaration, generated from the
  source doc comments.
- **[Diagnostic index](./diagnostics.md)** — every compiler error code and what it means.

The last two pages are generated. Regenerate them with `pnpm --filter @silk-lang/compiler
documentation:generate`; `pnpm --filter @silk-lang/compiler test` fails while either page is
stale, so a new stdlib module or diagnostic code cannot land without its page.

## This package

`@silk-lang/language` provides editor support for Silk: a CodeMirror 6 extension whose
highlighting is driven by the compiler's own bootstrap lexer, and the Silk TextMate grammar used by
Shiki and VS Code-compatible editors.

## Highlighting from the real lexer

The CodeMirror extension does not carry a grammar of its own. It lexes the document with
`@silk-lang/compiler/Lexer` and maps each token kind to a highlight style, so what the editor
colors is exactly what the compiler sees — including invalid bytes:

```silk
/// Returns the answer to everything.
pub fn answer() -> i32 {
    return 42
}

pub fn main() -> i32 {
    let ok = true
    if ok { return answer() } else { return 0 }
    return 0
}
```

Every highlighted span also carries a stable `cm-silk-<category>` class (`keyword`, `boolean`,
`number`, `identifier`, `line-comment`, `doc-comment`, `operator`, `punctuation`, `invalid`), so an
app can restyle tokens with plain CSS on top of whatever CodeMirror highlight style is active.

## TextMate grammar

The `TextMate` module exports the Silk grammar (`source.silk`) and a VS Code language
configuration. The grammar's keyword alternations are built from a keyword table that is
type-exhaustive over the compiler's token kinds, so a keyword change in the compiler fails this
package's typecheck until the grammar follows.
