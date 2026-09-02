## Why

Silk has first-class named function items, callable sections, and stored callable environments, but
it cannot define a callback body at its use site or close over lexical state. Anonymous callable
expressions complete that model without introducing dynamic closure erasure: every expression keeps
a deterministic static target and a finite ownership-aware environment.

## What Changes

- Add ordinary `fn(parameters) -> Result { ... }` and effectful
  `effect fn(parameters) -> Success ! Failure ? Requirements { ... }` expressions with explicit
  parameter and result contracts.
- Discover lexical captures implicitly in deterministic first-use order and derive the strongest
  valid `fn`, `mut fn`, or `once fn` invocation mode from how those captures are accessed.
- Give each anonymous body a deterministic source-occurrence target and lower it through the
  existing exact callable-environment representation, ownership, cleanup, evaluator, Wasm, and
  native paths.
- Extend formatting and language-server facts so anonymous bodies round-trip canonically, resolve
  names in lexical scope, and present their callable contract and captures without inventing an
  importable declaration name.
- Update the prescriptive language reference while preserving named functions, callable sections,
  bound methods, stored callables, and `effect { ... }` unchanged.
- Exclude explicit anonymous `mut fn`/`once fn` construction, self-recursive and independently
  generic anonymous bodies, declaration modifiers, and overload participation from the first slice.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-syntax`: recognize dedicated lossless ordinary and effectful anonymous callable
  expression nodes with bounded local recovery.
- `bootstrap-semantic-facts`: resolve anonymous parameters, lexical references, explicit result
  contracts, implicit captures, and derived invocation modes.
- `bootstrap-callable-values`: define anonymous callable contracts, contextual compatibility,
  deterministic targets, capture order, substitution, and excluded forms.
- `bootstrap-ownership`: enforce capture access, escape, reuse, consumption, and exactly-once
  cleanup for anonymous callable environments.
- `bootstrap-hir`: represent anonymous targets, bodies, captures, types, and source identities in
  deterministic typed HIR.
- `bootstrap-mir`: lower finite anonymous callable environments and bodies without a universal
  closure ABI while preserving evaluation and cleanup order.
- `bootstrap-evaluation`: execute ordinary and effectful anonymous callable values with the same
  invocation-mode and cleanup semantics as other exact callable environments.
- `bootstrap-flow-functions`: preserve the two delayed boundaries of effectful anonymous callables:
  callable construction, then Effect construction, then body execution at `run`.
- `bootstrap-type-generics`: allow explicit anonymous contracts to reference enclosing binders and
  contribute supplied-argument evidence without expected-result back-inference.
- `bootstrap-backend`: require Wasm and native execution to consume the same verified exact target
  and finite environment rather than introducing a backend-specific closure ABI.
- `silk-source-formatting`: format complete anonymous callable expressions canonically and
  idempotently.
- `language-server-hover`: present an anonymous callable's contract and captures without a synthetic
  declaration name.
- `language-server-completion`: expose the correct lexical scope inside anonymous bodies and the
  applicable expression keywords at anonymous callable sites.

## Impact

The change spans compiler syntax, semantic analysis, HIR, executable discovery, ownership, layout,
MIR, evaluation, Wasm/native execution, formatter and LSP presentation, plus the callable and
ownership reference documentation. It adds no external dependency, compiler-known standard-library
actor, dynamic callable erasure, or heap-only closure ABI.
