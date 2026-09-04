## Context

See `proposal.md` for motivation. Silk already represents first-class callable sections with a
stable `CallableSiteId`, an exact `CallableEnvironmentIdentity`, ordered captures, and ordinary MIR
functions invoked through `MakeCallable` and `ApplyCallable`. Named `effect fn` declarations also
already lower their authored body to a lazy Effect recipe. Anonymous callables need to compose those
mechanisms without becoming importable declarations, structural closures, or backend-specific
runtime objects.

The syntax tree is lossless and recovery-oriented; semantic analysis owns lexical identity and
ownership facts; HIR is the first typed executable representation; MIR must be finite and
monomorphic. LLVM lowering consumes verified MIR rather than rediscovering syntax. Tooling reads
shared semantic facts and must not fabricate declaration names or execute user code.

## Goals / Non-Goals

**Goals:**

- Represent every anonymous body as an exact, deterministic executable target plus a finite ordered
  environment that reuses the existing callable application path.
- Keep capture discovery, ownership acquisition, mode derivation, lowering, cleanup, and tooling on
  one canonical set of facts.
- Preserve the distinct timing boundaries for effectful literals: literal evaluation captures the
  environment, invocation constructs an Effect, and `run` enters the body.
- Reject unsupported forms at a well-defined semantic boundary while retaining their recoverable
  syntax for diagnostics and formatting.

**Non-Goals:**

- Introducing an erased closure ABI, a heap-only representation, dynamic target lookup, or a
  compiler-known library actor.
- Adding capture-list syntax, explicit anonymous `mut fn` or `once fn`, independent anonymous type
  parameters, self binders, overload entries, or declaration modifiers.
- Supporting anonymous bodies nested inside another anonymous body in this first slice. That needs
  a separate design for transitive environment lifting and escape analysis.
- Inferring omitted anonymous parameter, result, failure, or requirement types from context.

## Decisions

### Use a dedicated expression node and share only callable-contract parsing

The parser will recognize `fn(...) -> ... { ... }` and `effect fn(...) -> ... { ... }` as dedicated
anonymous-callable expressions. The result arrow remains mandatory, including for `()`. The
existing parameter-list and callable-tail parsers will be factored only where their grammar is
identical. Declaration-only prefixes and modifiers never enter this expression production.
`effect { ... }` remains the existing Effect expression; the token after `effect` selects the form.

This retains every token and gives recovery bounded delimiters at commas, the result contract, and
the body brace. Desugaring into a synthetic source declaration was rejected because it would leak a
name into declaration lookup and force later phases to reconstruct expression provenance.

### Analyze the body in a nested lexical scope and derive captures once

Semantic analysis will reuse a generalized function-body analyzer with authored parameters as
local bindings and the enclosing executable scope as its lexical parent. Parameters, locals, and
pattern bindings owned by the body are not captures. Every resolved reference to an outer value is
deduplicated by canonical binding/root identity in first lexical occurrence order. Unlike existing
Effect capture facts that expose declaration-ordinal order, the anonymous collector preserves map
insertion order. That one order drives capture ordinals, acquisition, environment fields, lowering,
and reverse cleanup.

The collector aggregates ordinary ownership access: Copy or shared read derives `fn`, any exclusive
or mutating access derives at least `mut fn`, and any consuming path derives `once fn`. Existing
ownership diagnostics resolve incompatible uses. Expected callable types validate the explicit
contract and mode through normal substitution; they do not select capture access, infer missing
annotations, or rewrite the body contract.

Explicit capture lists and explicit mode spellings were rejected because access analysis is already
the source of truth and duplicate syntax could disagree with it. Contextual signature inference was
rejected because Silk's generic call inference is supplied-argument driven and locally explicit
contracts keep bodies deterministic.

### Give each occurrence a private source target, not a source declaration

Each accepted occurrence receives a stable `CallableSiteId` beneath its enclosing executable and a
compiler-private callable target derived from that owner, site, and enclosing specialization. The
target is available to HIR/MIR executable maps but is never inserted into declaration lookup,
module surfaces, imports, overload sets, or user-facing names. Equal capture-free literals at
different sites remain distinct.

HIR will retain the source kind, site, explicit contract, derived mode, authored parameters, typed
body, surrounding substitution, and ordered capture facts. Elaboration publishes each accepted body
in a separate hidden executable catalog before instance discovery while the enclosing expression
references that target for construction. The existing source-declaration function catalog remains
unchanged for tooling and API consumers. A damaged or rejected body remains unavailable and cannot
publish a hidden executable target.

A structural signature key or runtime-generated identity was rejected because either would merge
distinct source occurrences. An importable synthetic declaration was rejected because anonymous
identity is provenance, not a language-level declaration.

### Admit hidden bodies before discovery and lower them as ordinary instances

Elaboration will publish accepted anonymous bodies as hidden `HirFunction`s before instance
discovery. Shared executable-lookup helpers will search both source declarations and the hidden
catalog, while module headers, declaration indexes, surfaces, imports, completion declarations, and
documentation continue to search only source declarations. Instance discovery, residualization,
ownership, and dependency traversal can then specialize and validate anonymous bodies through their
existing paths instead of being bypassed by a lower-only generation queue.

Each hidden function keeps authored parameters at their source ordinals and appends capture
parameters after them. Capture facts retain the corresponding appended `parameterOrdinal`, while
the source-facing `Type.Callable` exposes only authored parameters. This preserves the existing
authored parameter identities and lets the ordinary `ApplyCallable` operand assembler place both
groups by ordinal without a second remapping table. Elaboration remaps free outer binding references
in the body to those hidden capture parameter identities. Every discovered target therefore emits an ordinary monomorphic
`MirFunction`; expression lowering emits the existing exact `MakeCallable`, and invocation continues
through the existing `ApplyCallable` operand assembler. The MIR verifier validates the target,
environment, mode, ownership transfers, and signature from those facts.

Section-specific ownership, layout, dependency-discovery, and executable-origin helpers will be
generalized to operate on any environment-bearing callable. The exact environment layout remains
finite and target-specific. LLVM-native and LLVM-to-Wasm lowering therefore reuse existing callable
execution and cleanup rather than adding a universal dispatch table or a new closure object.

A backend-specific closure lowering and a universal boxed callable representation were rejected
because they would erase environment identity and create divergent ownership behavior.

### Model `effect fn` with two delayed boundaries

Evaluating an `effect fn` literal acquires its free lexical captures into the callable environment.
Invoking that callable evaluates and supplies its authored arguments, then constructs one exact
Effect recipe without running the body. Only `run` executes the body. The generated callable body
will reuse the named effect-function wrapper that produces an `EffectBlock`, including declared
failure and requirement channels.

The returned Effect retains, borrows, or consumes the callable environment according to the same
capture contract, so invocation cannot duplicate affine captures. Its identity derives from the
anonymous target and enclosing specialization under existing recipe identity rules, never from a
JavaScript object. Treating `effect fn` as an eager function returning a coincidentally Effect-shaped
value was rejected because it would collapse the construction and execution boundaries.

### Keep tooling on semantic occurrences and lexical scopes

The formatter prints the dedicated node directly. Semantic occurrence tables expose anonymous
parameters, body locals, captured outer bindings, the explicit contract, derived mode, and capture
summary. Hover presents a source-like anonymous signature without a declaration link or invented
name. Completion uses the nested lexical scope and offers expression starts while excluding
declaration-only modifiers.

Independent parser or LSP reconstructions were rejected because recovery and partial programs would
otherwise produce facts that disagree with analysis.

### Verify each claim at its cheapest falsification tier

Parser and formatter cases live in their existing suites. One shared analyzed source proves the
semantic, HIR, ownership, and MIR claims where practical. Runtime semantics use an independently
pinned shared native-acceptance case, while intended WebAssembly behavior is checked through
LLVM-to-Wasm. Hover and completion receive focused cases in existing files. Global determinism
tests remain the only fresh-process determinism checks.

## Risks / Trade-offs

- [Synthetic targets leak into source declarations or tooling] → Keep target creation in executable
  facts, assert absence from module surfaces, and render source-kind signatures without names.
- [Existing capture collection sorts by declaration ordinal] → Add an explicit first-occurrence
  path for anonymous callables and make the resulting order the only downstream capture order.
- [Anonymous parameters are mistaken for captures] → Establish the body-owned binding set before
  walking references and test parameter/local shadowing.
- [Effectful bodies execute during invocation] → Reuse the named effect-function HIR wrapper and
  test separately that literal construction, invocation, and `run` observe the three stages.
- [Generated body dependencies are omitted] → Teach executable discovery to traverse embedded
  anonymous bodies and add a body call to the representative structural case.
- [Capture order and parameter order become conflated] → Preserve separate ordinals through HIR,
  MIR inputs, invocation assembly, and verifier assertions.
- [Nested bodies are accidentally half-supported] → Parse them losslessly but emit one focused
  semantic diagnostic before executable HIR publication.
- [The vertical change lengthens the compiler suite] → Reuse analysis snapshots and the shared
  native corpus; avoid new test files and redundant backend legs.

## Migration Plan

1. Land and strictly validate the OpenSpec artifacts before compiler implementation.
2. Add syntax and semantic/HIR facts, then make all current consumers exhaustive so no old path can
   silently ignore the new expression.
3. Generalize executable lookup and exact callable-environment consumers so hidden anonymous
   functions pass through existing discovery, residualization, ownership, and MIR application.
4. Add LLVM-native/LLVM-to-Wasm, formatter, LSP, and prescriptive-reference support in the same
   change; regenerate committed artifacts where required.
5. Run repository verification in the mandated order and publish a draft PR.

The repository is green-field, so there is no compatibility mode or staged dual representation.
Rollback is the ordinary revert of the planning and implementation commits; no persisted data or
external migration is involved.
