# Silk Effect

Silk Effect is a strict TypeScript, ESM-only monorepo. It currently contains these packages:

- [`@silk-effect/compiler`](packages/compiler) — byte-oriented source, span, token, diagnostic, and
  bootstrap lexer primitives.
- [`@silk-effect/llvm`](packages/llvm) — Effect-native LLVM IR construction and deterministic text
  and bitcode emission.
- [`@silk-effect/language`](packages/language) — editor support: a lexer-driven CodeMirror 6
  extension and the Silk TextMate grammar (consumed by Shiki and the private
  [Cursor/VS Code extension](packages/vscode)).

## Development

```sh
pnpm install
pnpm dev
pnpm build
pnpm check
pnpm release:candidate
```

`pnpm dev` runs the package compilers in watch mode alongside the documentation app. `pnpm build`
creates a dependency-ordered production build of every workspace package and app.

Effect-returning tests use `it.effect` from `@effect/vitest`; pure tests use ordinary `it` with
`assert`. Package-facing changes require a Changesets entry and a validated packed release
candidate.

## License

[MIT](LICENSE) © 2026 Julia Ortiz
