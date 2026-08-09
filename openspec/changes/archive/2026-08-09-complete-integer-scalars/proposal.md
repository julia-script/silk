## Why

Silk's genuine primitive vocabulary is limited to `I32`, `Usize`, and `Bool`, which prevents real byte-oriented and width-conscious algorithms and leaves indices inconsistent. Completing the integer foundation now gives later programs one coherent numeric model instead of accumulating nominal stand-ins.

## What Changes

- **BREAKING** Replace uppercase primitive source spellings with `bool`, `u8`, `u16`, `u32`, `u64`, `usize`, `i8`, `i16`, `i32`, `i64`, and `isize`; remove compatibility aliases.
- Replace nominal Unit source use with `()` and replace `Never` spelling with `never`; support omitted unit results and bare `return`.
- Add exact contextual integer literals, default unconstrained integers to `i32`, and reject ranges before MIR lowering without JavaScript-number loss.
- Add explicit conversions; checked trapping defaults; recoverable checked operations returning `Option<T>`; wrapping and saturating arithmetic; bitwise operations; shifts; and rotates.
- Use `usize` consistently for runtime lengths and indices.
- Require HIR, MIR, layout, evaluator, native LLVM, direct WebAssembly, diagnostics, encoders, hover, and completion parity.

## Capabilities

### New Capabilities

- `bootstrap-integer-scalars`: Complete lowercase integer, unit, bottom, literal, conversion, and integer-operation semantics.

### Modified Capabilities

- `bootstrap-syntax`: Parse lowercase primitive paths, `()`, `never`, bare unit returns, and exact integer forms.
- `bootstrap-hir`: Carry the complete integer vocabulary and exact operations canonically.
- `bootstrap-callable-values`: Expose lowercase primitive actor callables.
- `bootstrap-operator-semantics`: Resolve operators homogeneously for every integer type.
- `bootstrap-usize`: Integrate lowercase target-width `usize` into the complete integer family.
- `bootstrap-fixed-arrays`: Require `usize` dynamic indices.
- `bootstrap-runtime-slices`: Use `usize` lengths and indices.
- `bootstrap-target-layout`: Plan every fixed- and target-width integer plus payload-free unit/bottom.
- `bootstrap-evaluation`: Evaluate exact width-aware integer behavior.
- `bootstrap-mir`: Represent and verify the complete integer operation vocabulary.
- `bootstrap-backend`: Realize identical integer behavior in LLVM and direct WebAssembly.
- `bootstrap-silk-stdlib`: Provide ordinary Silk `Option<T>` source used by recoverable checked operations.
- `language-server-hover`: Render canonical lowercase primitive types.
- `language-server-completion`: Offer the complete lowercase integer vocabulary.

## Impact

This is a repository-wide source migration across compiler phases, fixtures, examples, documentation, standard-library source, editor tooling, goldens, and both backends. Because Silk is unreleased, no uppercase compatibility layer is retained.
