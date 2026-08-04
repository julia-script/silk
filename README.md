# Silk Effect

Silk Effect is a strict TypeScript, ESM-only monorepo. It currently contains two packages:

- [`@silk-effect/compiler`](packages/compiler) — byte-oriented source, span, token, diagnostic, and
  bootstrap lexer primitives.
- [`@silk-effect/llvm`](packages/llvm) — Effect-native LLVM IR construction and deterministic text
  and bitcode emission.

## Development

```sh
pnpm install
pnpm check
pnpm release:candidate
```

Effect-returning tests use `it.effect` from `@effect/vitest`; pure tests use ordinary `it` with
`assert`. Package-facing changes require a Changesets entry and a validated packed release
candidate.

## License

[MIT](LICENSE) © 2026 Julia Ortiz
