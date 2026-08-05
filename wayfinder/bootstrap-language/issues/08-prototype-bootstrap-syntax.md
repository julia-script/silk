# Prototype the bootstrap language syntax

Type: prototype
Status: resolved
Blocked by: 01, 02, 03, 04

## Question

What concrete syntax makes the settled bootstrap semantics direct, constrained, readable, and easy
for humans and AI tools to produce—especially function contracts, service provision, allocation,
lifetimes, structs, runtime type unions, generics, matching, modules, and unsafe boundaries?
Ownership and borrowing syntax must make access modes and lexical boundaries intuitive without
assuming familiarity with Rust's notation. Prototype both qualified calls and first-argument
pipeline insertion for actor-module functions without introducing import-dependent method lookup.

## Answer

The accepted direction is a keyword-light language with two function forms. An ordinary `fn`
executes directly and is pure. Calling a `flow fn` packages its supplied inputs into a lazy typed
flow value without entering its body. The `run` keyword evaluates exactly one flow layer, including
when it appears inside another flow body, and `return` is always explicit. A single-statement body
may omit braces, but it may not omit `return`.

```silk
pub flow fn compile(request: own Request) -> Artifact
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

Actor operations remain qualified and data-first. Every eligible multi-argument actor operation has
compiler-supported dual calling forms: a complete call supplies the first argument normally, while
a partial call in a pipeline receives the piped value as argument one. No placeholder is required
when later arguments are already present.

```silk
let direct = Math.sum(2, 3)
let piped = 2 |> Math.sum(3)
let disabled = flag |> Bool.not
```

This is static elaboration of the same actor operation, not import-dependent method lookup. Actor
names therefore remain visible in both forms. Language behavior should prefer ordinary pipeable
actor operations such as `Flow.flatMap`, `FileSystem.provide`, and `Scope.scoped` over additional
keywords.

Flow nesting is valid and never flattened implicitly. Returning a flow preserves the nested layer;
`run` evaluates one layer; `Flow.flatten` removes one layer explicitly; and `Flow.flatMap` composes
with the same one-layer flattening rule. The bootstrap Flow actor also needs the ordinary small
composition family: `map`, `flatMap`, `flatten`, `tap`, and typed failure handlers. Their data-first
forms are all pipeable through the same dual-call rule.

Provision is specialization of a flow value rather than a new lexical block syntax.
`Capability.provide(provider, @Role)` captures an existing implementation and removes that
capability-role entry from the flow's requirement row. Because the open function is itself a value,
callers can branch and specialize it before supplying affine inputs.

```silk
let withScratch = Compiler.compile
  |> Allocator.provide(&mut scratch, @Scratch)

let fsCompilation = withScratch
  |> FileSystem.provide(&fileSystem)

let memoryCompilation = withScratch
  |> FileSystem.provide(&virtualFileSystem)

let diskArtifact = run fsCompilation(move diskRequest)
return run memoryCompilation(move memoryRequest)
```

A borrowed provider constrains the specialized flow's lifetime. A moved provider is owned by the
specialized flow and is cleaned up when that flow ends. Shared, exclusive, and consuming access are
checked exactly like other captures. `Capability.provideWith(acquisitionFlow, @Role)` is the
separate operation for acquiring a fresh provider on every execution. Its acquisition requirements
and failures compose mechanically with the target flow, it brackets the target, and successful
providers are cleaned up infallibly in reverse acquisition order after success, typed failure,
defect, or interruption. Acquisition is never implicitly memoized.

`Scope.scoped('name)` likewise creates a fresh named scope for every execution of the wrapped flow.
Providers and scopes are ordinary wrappers, so their order determines acquisition, handler, and
cleanup order. A value borrowing a provider or allocation tied to such a per-run scope cannot escape
the wrapper. It must be consumed there, copied or promoted into an enclosing scope, or returned from
a flow using a longer-lived captured provider.

Flow reuse is derived from captured ownership rather than represented by separate reusable and
single-shot effect types. A flow whose body only views captured state can run repeatedly. One that
edits captured state requires exclusive run access. Once an execution takes a captured owned value,
the closed flow cannot run again. Diagnostics should identify the consumed capture, even though the
failed operation is a second `run`.

```silk
let reusable = inspect(&payload)
let first = run reusable
let second = run reusable

let once = consume(move payload)
let digest = run once
let again = run once // error: captured payload was taken
```

Recursive flow construction is lazy, but laziness alone does not guarantee bounded native stack
usage during execution. Compiler-proven tail-recursive flow calls lower directly to loops. Every
other recursive cycle must cross the ordinary pipeable `Flow.suspend` operation or compilation
fails. A suspended recursive edge lowers through a trampoline and explicit continuation frames.
This makes stack safety a checked property without imposing interpreter overhead on every flow.

```silk
pub flow fn depth(node: &Node) -> U32 {
  if node.isLeaf {
    return 1
  }

  return run (
    depth(&node.child)
      |> Flow.suspend
      |> Flow.map(Math.add(1))
  )
}
```

Statically known non-recursive compositions elaborate directly to MIR and need no generic runtime
Flow objects, vtables, or per-combinator allocation. Tail recursion becomes a loop. A stored or
dynamically selected flow may require a compiler-shaped environment, and suspended non-tail
recursion necessarily requires memory for unfinished continuations, but the compiler may
monomorphize and stack- or scope-allocate those structures. The target is zero abstraction overhead
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
effectful functions and issue 02's blanket deferral of owned environments, narrowly for typed flow
values.

## Amendment — 2026-08-05

The data-slice planning session fixed the concrete import, nominal struct construction, and
mode-aware match spellings above. File-derived module identities and semantic normalization remain
owned by issues 04 and 02 respectively.
