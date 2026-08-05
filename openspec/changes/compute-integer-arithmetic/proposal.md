## Why

Programs can bind and pass integers but cannot compute with them: no arithmetic exists, and
literals cannot even be negative. The differential harness therefore compares programs whose
only behavior is data plumbing. Arithmetic is the smallest feature that gives interpreter and
native execution something real to agree on — including the trap semantics issue 02 pins:
ordinary integer overflow traps in every build mode, which forces the first compiler-generated
control flow (overflow checks) through MIR, the interpreter, and the backend.

A deliberate consequence of the accepted surface: issue 08 contains no infix operators.
Arithmetic is spelled as qualified, data-first actor operations (`Math.sum(2, 3)`), so this
change introduces qualified-callee parsing and a compiler-known built-in actor — not a
precedence table. That same parsing is what comparisons (next change) and the future runtime
library (issue 07) ride on.

## What Changes

- Lex signed integer literals per issue 02's context-typing rule (a `-` prefixed literal in
  `I32` context; range-checked, no implicit conversions).
- Parse qualified callees: `Actor.operation(arguments)` alongside today's bare-name calls,
  with lossless nodes and recovery.
- Add the compiler-known `I32` actor with the ordinary trapping arithmetic operations
  (`add`, `subtract`, `multiply`, `divide`, `remainder`) as built-in declarations visible to
  the declaration index and elaboration; checked/wrapping/saturating variants stay explicit
  and deferred until a real program needs them.
- Elaborate qualified calls to HIR: built-in operations type-check like user functions
  (two `I32` parameters, `I32` result); unknown actors or operations are semantic diagnostics.
- Lower arithmetic to MIR: either dedicated arithmetic operations or intrinsic calls — design
  decides; overflow and division-by-zero lower to the existing `Trap` terminator via
  compiler-generated branches (division by zero and `I32_MIN / -1` included).
- Interpreter evaluates arithmetic with exact trap parity; backend emits LLVM arithmetic with
  overflow-checked lowering (`sadd.with.overflow`-style intrinsics branching to trap).
- Extend encoders, goldens, corpus (including overflow-trap and divide-by-zero programs), and
  the differential harness; interpreter and native must agree on results **and** on traps.
- Inspector: MIR lab shows the generated overflow-check diamonds; syntax lab shows qualified
  callees; evaluation lab traces arithmetic.

## Capabilities

### Modified Capabilities

- `bootstrap-lexer`: signed literal spelling.
- `bootstrap-syntax`: qualified callees, signed literals.
- `bootstrap-declaration-index`: compiler-known built-in actor declarations.
- `bootstrap-hir`: built-in operation calls, literal context typing.
- `bootstrap-semantic-facts`: unknown-actor/operation diagnostics.
- `bootstrap-mir`: arithmetic lowering with trap-checked control flow.
- `bootstrap-evaluation`: arithmetic and trap parity.
- `bootstrap-backend`: overflow-checked LLVM arithmetic emission.
- `bootstrap-syntax-inspector`: labs over the new forms.

## Impact

First compiler-generated control flow: MIR CFGs stop being straight-line even before `if`
exists, exercising `Branch` in lowering, interpretation, and the backend. First built-in
declarations in the index — the pattern issue 07's runtime actors will reuse. Design open
questions: MIR representation (dedicated ops vs intrinsic calls) and where the built-in
actor's declarations live in the index without a source module.

## Plan References

- [Roadmap — Now: widen the language, slice 1](../../../roadmaps/project.md)
- [Issue 02](../../../wayfinder/bootstrap-language/issues/02-bootstrap-type-system-and-values.md):
  "Numeric literals receive a type from their immediate context and are range-checked …
  Ordinary integer overflow traps in every build mode; checked, wrapping, and saturating
  operations are explicit."
- [Issue 08](../../../wayfinder/bootstrap-language/issues/08-prototype-bootstrap-syntax.md):
  qualified data-first actor operations (`Math.sum(2, 3)`) are the accepted arithmetic
  surface; no infix operators appear in the accepted grammar. Pipe insertion (`|>`) is part
  of that surface but deferred out of this slice.
- [Issue 06](../../../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md):
  determinism gates and the differential interpreter-vs-native harness this change extends to
  trap behavior.
