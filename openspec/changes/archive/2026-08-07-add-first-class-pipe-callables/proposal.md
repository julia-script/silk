## Why

Silk's current pipeline is special call-site sugar: it inserts the left value into a qualified
call, but the resulting partial operation is not an ordinary callable value and therefore cannot
compose through higher-order APIs such as `Effect.map`, `tap`, or user-authored combinators. The
new Effect model makes this limitation immediate, and the current tight-binding `run` grammar also
forces unnecessary parentheses around piped Effects.

## What Changes

- **BREAKING** Replace pipeline-only argument insertion with first-class callable application:
  `value |> operation` evaluates the value once and invokes the unary `operation` with it.
- Make named functions ordinary callable values and give every function with at least two
  parameters an automatic leading-argument section: supplying exactly the trailing parameters
  creates a unary callable awaiting parameter zero. No `dual` declaration marker is introduced.
- Add compiler-derived callable modes with explicit abstract spellings: `fn(A) -> B` for shared
  reusable invocation, `mut fn(A) -> B` for exclusive reusable invocation, and
  `once fn(A) -> B` for consuming invocation.
- Allow sections to capture Copy values, shared borrows, exclusive borrows, and affine owners.
  Their lifetime, cleanup, and invocation mode follow the captured arguments rather than a
  Copy-only restriction.
- Permit callable values and sections wherever an expression of compatible callable type is
  accepted, including Effect combinators and user-defined higher-order functions.
- **BREAKING** Make `run` consume the complete following expression through its enclosing comma,
  delimiter, or statement boundary, so a following pipeline is transformed before the Effect is
  executed. Grouping remains the explicit way to pipe the executed success value.
- Keep semantic logging effectful. This change does not introduce a non-effect debugging or
  tracing escape hatch.

## Capabilities

### New Capabilities

- `bootstrap-callable-values`: First-class named functions, automatic leading-argument sections,
  callable invocation modes, ownership-aware capture environments, and higher-order invocation.

### Modified Capabilities

- `bootstrap-syntax`: Parse callable types, callable-valued callees, general pipeline operands,
  and low-precedence `run` losslessly and recoverably.
- `bootstrap-operator-semantics`: Replace qualified-call argument insertion with unary callable
  application while preserving single evaluation and left-to-right order.
- `bootstrap-type-generics`: Infer generic arguments across sections, pipelines, and higher-order
  callable contracts without runtime dictionaries.
- `bootstrap-semantic-facts`: Publish function references, sections, capture modes, callable
  contracts, applications, and the expanded `run` operand.
- `bootstrap-hir`: Represent callable values, capture environments, and invocation canonically
  while erasing pipeline syntax.
- `bootstrap-ownership`: Track callable captures, borrows, cleanup, and shared, exclusive, or
  consuming invocation with ordinary affine rules.
- `bootstrap-flow-functions`: Make Effect combinators consume real callable values and propagate
  callback invocation modes into Effect repeatability.
- `bootstrap-mir`: Lower monomorphic callable construction and invocation into the backend-neutral
  structured DAG.
- `bootstrap-evaluation`: Execute callable environments and low-precedence `run` with deterministic
  ownership and trace behavior.
- `bootstrap-backend`: Realize callable environments and applications consistently in LLVM and
  WebAssembly without prescribing one universal closure representation.
- `bootstrap-syntax-inspector`: Explain callable types, captures, pipeline application, Effect
  composition, ownership failures, and `run` grouping in the unified `/labs` inspector.
- `bootstrap-compiler-driver`: Add interpreter, LLVM, and WebAssembly parity and determinism gates
  for reusable, exclusive, consuming, generic, and Effect-composed callables.

## Impact

The change touches the parser and formatter; canonical types, declaration and generic analysis;
semantic facts and diagnostics; HIR, ownership, instance discovery, MIR, evaluation, LLVM, and
WebAssembly lowering; Effect combinators; syntax highlighting; and unified Labs presets. Existing
pipeline programs retain their result when the right side denotes the equivalent section, but AST
shape, diagnostics, evaluation-order wording, `run` precedence, and intermediate artifacts change.
No external runtime dependency or mandatory heap-allocated closure representation is introduced.
