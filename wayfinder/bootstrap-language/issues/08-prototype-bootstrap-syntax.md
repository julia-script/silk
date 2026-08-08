# Prototype the bootstrap language syntax

Type: prototype
Status: resolved
Blocked by: 01, 02, 03, 04

## Question

What concrete syntax makes the settled bootstrap semantics direct, constrained, readable, and easy
for humans and AI tools to produce—especially function contracts, service provision, allocation,
lifetimes, structs, runtime type unions, generics, matching, modules, and unsafe boundaries?
Ownership and borrowing syntax must make access modes and lexical boundaries intuitive without
assuming familiarity with Rust's notation. Prototype qualified calls, first-class callable values,
automatic leading-argument sections, and unary pipeline application without introducing import-
dependent method lookup.

## Answer

The accepted direction is a keyword-light language with an explicit eager/lazy boundary. An ordinary
`fn` executes directly. An `effect { ... }` expression creates a lazy typed effect whose imperative
body does not execute during construction. An `effect fn` is sugar for an ordinary function whose
entire body is such an effect. The `run` keyword evaluates exactly one effect layer, including inside
another effect body, and `return` is always explicit. A single-statement body may omit braces, but it
may not omit `return`.

```silk
pub effect fn compile(request: own Request) -> Artifact
  ! FileError | ProcessError | OutOfMemory
  ? &FileSystem | &mut Allocator@Scratch
{
  let source = run FileSystem.read(&request.sourcePath)
  let syntax = run Parser.parse(source)
  return run Backend.emit(move syntax)
}

let computation = compile(move request)
return run computation
```

The compact contract rows are part of the accepted surface. `!` introduces the normalized union of
owned abortive failure types. `?` introduces the normalized set of capability requirements, with
`&` for shared access, `&mut` for exclusive access, and `@Role` selecting a nominal compile-time
role. `pub role Scratch` declares such a role. Omitting `@Role` selects `DefaultRole`; roles are
never strings or runtime lookup keys.

Actor operations remain qualified and data-first. Named functions are ordinary values. For every
function of arity `N >= 2`, supplying exactly the trailing `N - 1` arguments constructs an automatic
unary callable awaiting parameter zero; supplying all `N` arguments calls it. Pipelines evaluate
their left value first, then their callable right expression, and invoke the callable once. No
`dual` marker or placeholder is required.

```silk
let direct = Math.sum(2, 3)
let plusThree = Math.sum(3)
let piped = 2 |> plusThree
let disabled = flag |> Bool.not
```

This is ordinary callable construction and application, not import-dependent method lookup or
pipeline-only insertion. `Math.sum(3)(2)` is therefore equivalent to the piped form, and stored or
user-authored callables compose through higher-order APIs. Actor names remain visible in both forms.
Language behavior should prefer ordinary pipeable
actor operations such as `Effect.flatMap`, `Effect.catch`, and `FileSystem.provide` over additional
keywords.

Effect nesting is valid and never flattened implicitly. Returning an effect preserves the nested
layer; `run` evaluates one layer; `Effect.flatten` removes one layer explicitly; and
`Effect.flatMap` composes with the same one-layer flattening rule. The bootstrap Effect actor needs
`map`, `flatMap`, `flatten`, `tap`, `catch`, and `retry`. Their data-first forms produce automatic
sections through the same callable rule.

The explicit expression form isolates eager setup from delayed work:

```silk
fn risky<T>(value: T, selector: I32) -> Effect<T ! Problem> {
  let prepared = normalize(selector)

  return effect {
    if prepared == 0 {
      fail Problem { code: 41 }
    }

    return move value
  }
}
```

`prepared` is eager; the block is lazy. A Copy failure value needs no `move`; `fail move problem` is
used only when ownership actually transfers from a non-Copy binding.

Provision is specialization of an effect value rather than a new lexical block syntax.
`Capability.provide(provider, @Role)` captures an existing implementation and removes that
capability-role entry from the effect's requirement row. Because the open function is itself a value,
callers can branch and specialize it before supplying affine inputs.

```silk
let fsCompilation = Compiler.compile(move diskRequest)
  |> Allocator.provide(&mut scratch, @Scratch)
  |> FileSystem.provide(&fileSystem)

let memoryCompilation = Compiler.compile(move memoryRequest)
  |> Allocator.provide(&mut scratch, @Scratch)
  |> FileSystem.provide(&virtualFileSystem)

let diskArtifact = run fsCompilation
return run memoryCompilation
```

A borrowed provider constrains the specialized effect's lifetime. A moved provider is owned by the
specialized effect and is cleaned when that effect is consumed or dropped. `provide` is not a
per-execution cleanup boundary. `Capability.provideWith(acquisitionEffect, @Role)` is the separate
operation for acquiring a fresh provider on every execution. Its acquisition requirements and
failures compose with the target effect, and successful provider owners clean up in reverse
acquisition order after success or typed failure. Traps abort without a cleanup guarantee;
cancellation and interruption are absent from bootstrap. Acquisition is never implicitly memoized.

Effect reuse is derived from captured ownership rather than represented by separate reusable and
single-shot types. Copy captures are snapshotted at construction. An effect whose body only views
captured state can run repeatedly. One that edits captured state requires exclusive run access and
observes its mutations on later runs. Once execution takes a captured owned value, the closed effect
cannot run again. `Effect.retry` accepts only repeatable effects: it reconstructs execution locals
for each attempt but reuses captures. Diagnostics should identify the consumed capture, even though the
failed operation is a second `run`.

```silk
let reusable = inspect(&payload)
let first = run reusable
let second = run reusable

let once = consume(move payload)
let digest = run once
let again = run once // error: captured payload was taken
```

Typed handlers compose through the Effect actor and ordinary pipelines:

```silk
let recipe = relay(0)
  |> Effect.catch<Problem>(recover)

return run recipe
```

Recursive effect construction is lazy, but laziness alone does not guarantee bounded native stack
usage during execution. Compiler-proven tail-recursive effect calls lower directly to loops. Every
other recursive cycle must cross the ordinary pipeable `Effect.suspend` operation or compilation
fails. A suspended recursive edge lowers through a trampoline and explicit continuation frames.
This makes stack safety a checked property without imposing interpreter overhead on every effect.

```silk
pub effect fn depth(node: &Node) -> U32 {
  if node.isLeaf {
    return 1
  }

  return run depth(&node.child)
    |> Effect.suspend
    |> Effect.map(Math.add(1))
}
```

`run` owns the complete following expression through its enclosing delimiter or statement boundary.
Thus `run effect |> Effect.map(callback)` composes before execution. Parentheses explicitly move
the pipeline outside execution: `(run effect) |> Math.add(1)` transforms the eager success value.
`run` still evaluates exactly one Effect layer.

Statically known non-recursive compositions elaborate directly to MIR and need no generic runtime
Effect objects, vtables, or per-combinator allocation. Tail recursion becomes a loop. A stored or
dynamically selected effect may require a compiler-shaped environment, and suspended non-tail
recursion necessarily requires memory for unfinished continuations, but the compiler may
monomorphize and stack- or owner-allocate those structures. The target is zero abstraction overhead
where semantics permit it, not the impossible promise of zero memory for arbitrary non-tail
recursion.

The rest of the accepted surface follows the same low-keyword direction:

- `pub struct Name { ... }` declares nominal data with explicit `pub` fields.
- `pub impl Copy for Name` requests compiler-sealed Copy conformance and is accepted only when every
  field is Copy and cleanup-free; users cannot supply custom copying behavior.
- Runtime alternatives retain the visually union-like `A | B` spelling, are normalized by the
  compiler, and widen only from an immediate expected union context.
- Generic arguments use angle brackets where inference is insufficient; actor construction remains
  a qualified call rather than special allocation syntax.
- `import path.Module` binds `Module`; `as Alias` appears only when the name is actually changed;
  `{ Member, other as renamed }` selects public members; and `import path.Module as Alias {
  Member }` is the hybrid form that binds both the namespace and selected members.
- Unsafe operations remain qualified actor operations inside the explicit unsafe boundary settled
  by the earlier semantic issues rather than gaining unrelated special call syntax.
- `drop value` explicitly consumes an affine owner. A restricted `impl Drop for Name` hook supplies
  automatic cleanup but is not callable as an ordinary public operation.
- Allocation remains an unsafe qualified actor operation over validated `Layout`; safe
  `RawBuffer<T>`, `Vector<T>`, and other containers are ordinary Silk standard-library actors built
  above that boundary. No syntax names an allocation scope or allocator implementation.

The concrete import forms are therefore:

```silk
import compiler.Syntax
import compiler.Syntax as Tree
import compiler.Syntax { Node, parse, encode as encodeSyntax }
import compiler.Syntax as Tree { Node, parse }
```

Nominal struct construction uses a labeled literal only inside the defining module. External code
uses an ordinary public actor function such as `Token.make`; field access remains `value.field`.
Literals must name every field exactly once, while `..` in a pattern explicitly acknowledges
omitted fields.

```silk
pub struct Token {
  pub kind: U32
  lexeme: String
}

pub fn make(kind: U32, lexeme: own String) -> Token {
  return Token { kind: kind, lexeme: move lexeme }
}
```

Matching states the scrutinee access mode explicitly for affine values. `match move value`
consumes it, `match &value` shares it, and `match &mut value` borrows it exclusively. Bare
`match value` is accepted only when the value is Copy. Arms are newline-separated expressions,
guards precede `=>`, and nested nominal patterns use `field`, `field: localName`, and `..`.

```silk
let code = match move event {
  Token { kind, .. } => kind
  End {} => 0
}
```

Arms run in source order. Guarded arms do not contribute to exhaustiveness, `_` is the universal
pattern, and the result type is the normalized union of reachable arm result types.

The accepted comparison is preserved as a throwaway primary source under
[`prototype-syntax`](../prototype-syntax/README.md) and runs with
`pnpm prototype:bootstrap-syntax`. It compares the imperative and fully piped forms above against
the corresponding real Effect patterns and records the ownership or lowering consequence of each
example. The isolated capture is branch `julia/prototype-bootstrap-syntax-20260804` at commit
`db1db1518e28032233fbbed4f8bf8f5ee98cc3c6`; the branch contains only the throwaway prototype and
its run script. This answer supersedes issue 03's original direct-execution assumption for
effectful functions and issue 02's blanket deferral of owned environments, narrowly for typed effect
values.

## Amendment — 2026-08-05

The data-slice planning session fixed the concrete import, nominal struct construction, and
mode-aware match spellings above. File-derived module identities and semantic normalization remain
owned by issues 04 and 02 respectively.

## Amendment — 2026-08-06: bootstrap type generics

The bootstrap generic spelling is now concrete. Structs and functions declare ordered type
parameters with angles, applied types use the same angles in type position, and a call may supply a
complete specialization between its qualified callee and argument list.

```silk
struct Box<T> { value: T }
fn identity<T>(value: T) -> T { return move value }

let inferred = identity(value)
let explicit = identity<Token>(value)
let qualified = Tree.parse<Token>(source)
```

Expression angles are specialization syntax only when they form a complete type-argument list and
are immediately followed by the call postfix. Thus `left < right`, `left <= right`, and the
reserved primary template start remain separate grammar cases. A call either supplies every type
argument explicitly or supplies none and infers all of them from its value arguments; partial
explicit argument lists and expected-result inference are not accepted.

## Amendment — 2026-08-07: Effect and owned allocation

The Effect and allocation review renamed the language abstraction from Flow to Effect, made
`effect {}` the primitive lazy imperative boundary and `effect fn` its function sugar, and removed
named scope wrappers from bootstrap. Allocation results are self-contained affine owners; arena-
backed escaping values and general provider-dependent validity remain deferred. The checked-in
prototype reflects this current direction, while its cited isolated branch remains the historical
experiment that led to it.
