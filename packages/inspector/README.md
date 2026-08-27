# @silk-lang/inspector

Compiler-phase view projections for Silk inspectors.

The package owns the registry of inspector views (tokens, concrete tree, HIR, ownership, MIR,
backend output, …) and the row projections that turn an analysis snapshot into serializable row
models. Rows are pure data — strings, numbers, tones, and module-qualified byte spans — so a
projected view survives structured serialization unchanged, which is what lets the docs `/labs`
workbench render them in React while the language server answers them over JSON-RPC.

Projections consume the compiler exclusively through the `Analysis` facade.
