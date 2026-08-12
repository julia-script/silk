# Silk language reference

This reference states the rules of the Silk language as the current compiler implements them. It
covers the lexical form, the declarations, the type system, the memory and ownership rules, and the
effect system. The [tutorial](./tutorial.md) is the gentler introduction; this document is the one
that answers "is that allowed?".

The specifications under `openspec/specs/` are the normative source. Every Silk block on this page
is compiled by `packages/compiler/test/DocumentationExamples.test.ts`.

## 1. Lexical form

### 1.1 Keywords

The keyword vocabulary is closed. There are 28:

```
pub struct service interface effect fn run fail drop unsafe impl for
return import as let const mut once move match if else while
break continue true false
```

Keyword recognition applies to a complete identifier only, so `letter`, `iffy`, and `matcher` are
ordinary identifiers.

There is no `enum`, no `type` alias, no `trait` (it is `interface`), no `loop`, no `async`, and no
visibility beyond `pub`. `for` appears only in `impl Capability for Target`.

### 1.2 Identifiers

An identifier starts with an ASCII letter or `_` and continues with ASCII letters, digits, and `_`.
Non-ASCII identifiers are not supported. `_` alone is an identifier that gains its
universal-pattern meaning only in pattern position.

### 1.3 Comments

| Form | Meaning |
| --- | --- |
| `// text` | ordinary comment |
| `/// text` | documentation attached to the following declaration |
| `//! text` | documentation of the containing module |

Consecutive `///` lines attach to the declaration that follows, at every level: function, struct,
field, parameter, and impl operation. A blank line or an intervening `//` breaks the attachment.

### 1.4 Integer and float literals

Integer literals accept the base prefixes `0x`, `0b`, and `0o` (upper case also accepted), and
bare digits are base ten. The digit separator `_` must sit between two digits of the same run, so
`1_`, `1__0`, and `0x_ff` are each rejected.

Float literals carry a decimal point, an exponent, or both, and accept the same separator.

There is no negative literal: a leading `-` is a separate prefix operator.

```silk
pub fn main() -> i32 {
  let decimal = 1_000_000
  let hex = 0xff_ff
  let binary = 0b1010_0000
  let octal = 0o777
  let ratio = 1_000.5
  let scaled = 1.25e3
  let negative = 0 - 42
  return decimal - decimal + negative - negative
}
```

### 1.5 Text and byte literals

Four introductions, and the modifier must touch the delimiter:

```
"text"        """text"""        b"bytes"        b"""bytes"""
```

The escapes are `\n`, `\r`, `\t`, `\0`, `\"`, `\\`, `\xNN` with exactly two hex digits, and
`\u{...}` for a Unicode scalar. Any other escape is an error, and a backslash may not continue a
physical line.

### 1.6 Operators and precedence

Higher binds tighter.

| Precedence | Operators | Associativity |
| --- | --- | --- |
| postfix | `.field`, `[index]`, `(args)`, `<T>` | left |
| 60 | prefix `-` `!` `~` | — |
| 50 | `*` `/` `%` | left |
| 40 | `+` `-` | left |
| 37 | `&` | left |
| 35 | `^` | left |
| 33 | `\|` | left |
| 30 | `<` `<=` `>` `>=` | **none** |
| 20 | `==` `!=` | **none** |
| 10 | `\|>` | left |

Comparison operators are non-associative, so an ungrouped chain such as `1 < 2 < 3` is rejected.
The bitwise operators occupy three separate levels rather than one.

Operators are not overloadable. Each lowers to a compiler-known operation chosen by operand type.

`|>` is the pipeline operator: it inserts its left operand as the *leading* argument of the
callable on its right.

```silk
pub fn main() -> i32 {
  let piped = 2 |> i32.add(3)
  let masked = 0b1100 & 0b1010
  let shifted = (1 + 2) * 3
  let compared = 1 < 2
  if compared { return piped + masked + shifted }
  return 0
}
```

A `<` immediately followed by a tag identifier or `>` at the start of a primary expression is
reserved for future template syntax and is a parse error.

## 2. Declarations

There are exactly seven top-level declaration forms: `import`, `const`, `struct`, `service`,
`interface`, `impl`, and `fn` (including `effect fn`). Visibility is binary — `pub`, or private to
the module. Each module has one flat top-level namespace, and a name collision inside it is
rejected.

### 2.1 Imports

A module's identity comes from its file path; there is no `module` declaration. An import names a
dotted path and binds either the namespace, selected members, or both.

```
import compiler.Syntax
import compiler.Syntax as Tree
import compiler.Syntax { Node, parse, encode as encodeSyntax }
import compiler.Syntax as Tree { Node, parse }
```

A redundant alias (`as` naming the same spelling) is rejected, and one module may name another in
at most one import declaration. Imports are unconditional and top-level only.

### 2.2 Constants

A constant requires a type annotation and a bare literal initializer of a scalar type. There is no
inference, no aggregate constant, and no computed initializer. A constant lowers to an immediate,
so it has no address and cannot be borrowed, assigned, or moved.

```silk
pub const limit: i32 = 2
const asciiSpace: u8 = 32
const ratio: f64 = 1.5
const enabled: bool = true

pub fn main() -> i32 {
  if enabled { return limit }
  return 0
}
```

### 2.3 Functions

```
[pub] fn name[<TypeParams>](params) [-> ReturnType] { body }
[pub] effect fn name[<TypeParams>](params) -> Type [! FailureRow] [? RequirementRow] { body }
```

An omitted return type means `()`. Parameters are `name: Type` and a trailing comma is allowed.
Every function body must end in a `return`; a body that falls off its closing brace is a `PAR0004`
error with a recovered return.

### 2.4 Structs

Fields are listed one per line with no separators, and each may be `pub` independently. Structs are
generic over type parameters and may be empty.

```silk
pub struct Pair {
  pub left: i32
  right: bool
}

pub struct Box<T> {
  pub value: T
}

pub struct Marker {}

pub fn main() -> i32 {
  let pair = Pair { left: 1, right: true }
  let boxed = Box<i32> { value: 2 }
  return pair.left + boxed.value
}
```

### 2.5 Interfaces and impls

An `interface` is a static conformance contract resolved at specialization time. It creates no
runtime dispatch, no provider slot, and no requirement row.

An `impl` maps operation names to *existing* functions; it does not contain method bodies. Both the
interface and the service conformance paths reject a hook body outright, with the message
"implementations use operation mappings, not a hook body". `Drop` is the exception: its conformance
carries a hook body rather than an operation mapping.

```
pub interface Integer<T> {
  fn add(left: T, right: T) -> T
}

impl Integer<i32> for i32 { add: Intrinsic.i32Add }
impl Integer<u8> for u8 { add: Intrinsic.u8Add }
```

Note the current limitation: an interface conformance may only name an `Intrinsic.*` operation whose
signature matches the contract exactly. Mapping an ordinary user-defined function is rejected with
`SEM0083`, so interfaces today serve to abstract over the compiler's own primitive operations rather
than to let user types opt into a user-declared contract. `silk.numeric` uses this to give one
generic function a body that works for every integer width:

```silk
import silk.numeric { add }

pub fn main() -> i32 {
  let summed = add<i32>(2, 3)
  let counted = add<u8>(1, 2)
  if counted == 3 { return summed }
  return 0
}
```

The `T: Integer` bound on `add` is what makes `left + right` legal inside its body: a generic body
may use only the operations its bounds promise.

### 2.6 Services

A `service` declares runtime capability contracts — operation signatures only, with no fields, no
bodies, and no default provider. Using one places an entry in a requirement row, which a caller
must later satisfy with `provide`. This is the deliberate contrast with `interface`, which is
resolved statically and never appears in a row.

### 2.7 Bindings

`let` binds immutably and `let mut` binds mutably. There is no `var`, and locals carry no type
annotation — a local's type is always inferred. Assignment is a statement, never an expression.

## 3. Type system

### 3.1 Primitive types

The primitive spellings are lowercase and closed:

`bool`, `i8`, `i16`, `i32`, `i64`, `isize`, `u8`, `u16`, `u32`, `u64`, `usize`, `f32`, `f64`.

`Bool` and `I32` are not aliases; they resolve as unknown user types. `()` is the unit type and its
only value. `never` is the uninhabited type and joins into any other.

### 3.2 Type constructors

| Form | Meaning |
| --- | --- |
| `[T; N]` | fixed array; the length is part of the type |
| `&[T]` / `&mut [T]` | shared / exclusive slice |
| `T \| U` | structural union |
| `Name<A, B>` | generic application |
| `fn(A) -> B` | shared reusable callable |
| `mut fn(A) -> B` | exclusive reusable callable |
| `once fn(A) -> B` | consuming callable |
| `&T` / `&mut T` | shared / exclusive access |
| `Effect<A ! E ? R>` | effect with its three channels |
| `Row<!E>` | a failure row reified as ordinary data |

Callable substitution runs one way: `fn` may be used where `mut fn` is expected, and `mut fn` where
`once fn` is expected, never the reverse.

### 3.3 Nominal structs, structural unions

Structs are **nominal**: two structurally identical declarations are different types. Unions are
**structural**: an unordered, duplicate-free set of nominal members. Normalization erases order,
nesting, and duplicates, so `A | B`, `B | A`, and `A | (B | A)` are the same type, and a
single-member union collapses to that member. `never | A` is `A`.

Union members must be nominal — a scalar, array, or borrow is rejected as a direct member.

A nominal value converts into an expected union at a declared position (return type, parameter,
field, array element, assignment destination), and a union widens into a superset union. This never
rewrites the source expression's own inferred type, and narrowing back requires `match`.

### 3.4 Inference

Inference is local and deliberately weak.

- An integer literal keeps its exact magnitude until a context types it; unconstrained, it is
  `i32`. An unconstrained float literal is `f64`.
- **There is no implicit numeric conversion, in either direction.** `i32 + i64` is an error, not a
  widening, and an `i32` value passed to a `usize` parameter is an error, not a conversion.
- Arithmetic is homogeneous and traps on overflow, invalid division, and invalid shift counts.
  Named recoverable forms such as `u8.checkedAdd` return `Option<T>`; wrapping and saturating forms
  return `T`.
- A generic call infers only from its supplied arguments, never from the expected return type, so
  a function whose parameter list does not mention `T` needs an explicit type argument.
- An array literal infers its element type and written length; an empty literal needs context.

### 3.5 Generics

Type parameters are declaration-local. Parameters come in three kinds, distinguished by sigil:
plain value types, `!E` failure rows, and `?R` requirement rows. Using a row parameter where a
value type belongs is a kind mismatch.

A generic body is checked once against its canonical parameters, so specialization can never enable
an operation the declaration did not promise. Specialization is monomorphic and finite: there are
no runtime dictionaries or type descriptors, and polymorphic recursion that changes its type
arguments is rejected.

```silk
pub struct Holder<T> {
  pub value: T
}

fn identity<T>(value: T) -> T {
  return move value
}

fn store<T>(value: T) -> Holder<T> {
  return Holder<T> { value: move value }
}

pub fn main() -> i32 {
  let held = store<i32>(7)
  return identity<i32>(held.value)
}
```

Bounded generics take an interface bound, written `T: Bound`.

## 4. Memory and ownership

### 4.1 Affine values, explicit moves

Silk is affine: a value of a move-only type is used at most once. There are **no named lifetimes and
no region syntax**; borrow safety comes from lexical call scoping instead.

Every binding has one of two ownership categories, and the category decides whether `move` is
required:

| Category | Types | Consuming use |
| --- | --- | --- |
| **Copyable** | every scalar and `bool`, `string`, `never`, `OutOfMemory`, shared slices `&[T]`, shared references `&T`, shared effects, shared callables, and fixed arrays whose element type is copyable | copies; the binding stays usable |
| **Move-only** | everything else — nominal structs, exclusive slices `&mut [T]`, exclusive references `&mut T`, exclusive or `once` effects and callables, and fixed arrays of move-only elements | requires an explicit `move`; omitting it is `OWN0003` |

Two rules follow, and they are easy to conflate:

- **`move` is required only for move-only types.** Passing a copyable binding into a consuming
  position simply copies it. `OWN0003` is raised only for a move-only binding, so an `i32` may be
  passed to a by-value parameter as many times as you like.
- **Writing `move` always consumes the binding, in both categories.** An explicit `move` on a
  copyable binding still ends its life, and a later use is `OWN0001`.

```silk
pub struct Ticket {
  pub id: i32
}

fn consume(ticket: Ticket) -> i32 {
  return ticket.id
}

fn double(value: i32) -> i32 {
  return value * 2
}

pub fn main() -> i32 {
  let ticket = Ticket { id: 3 }
  let identifier = consume(move ticket)

  // `count` is copyable, so a consuming position copies it and it stays usable.
  let count = 2
  let first = double(count)
  let second = double(count)

  return identifier + first + second - 8
}
```

Using a binding after it is moved is `OWN0001`, reported at the later use and pointing back at the
move. A move inside a conditional arm counts conservatively for every use after that conditional;
the analysis is not path-sensitive.

Partial moves are rejected — `move value.field` is a violation, because nothing could restore a
valid whole value afterwards. Use a place replacement to swap a field's contents instead, which
leaves the place initialized throughout.

### 4.2 Borrows

Borrows are written `&` and `&mut` and exist **only as call arguments and parameters**. A standalone
borrow binding such as `let view = &values` is rejected, as is borrowing a temporary or storing a
slice inside an owned value.

Borrowing through a **nominal field** is supported and is a first-class part of the model: `&x.field`
and `&mut x.field` project through resolved struct fields rooted in a stable local, pattern binding,
or borrowed parameter, and the borrow retains that field path rather than copying the projected
value. Exclusive projection through a parameter requires an exclusive reference.

```silk
pub struct Inner {
  pub value: i32
}

pub struct Outer {
  pub inner: Inner
}

fn readInner(inner: &Inner) -> i32 {
  return inner.value
}

fn bumpInner(inner: &mut Inner) {
  inner.value = inner.value + 1
  return
}

pub fn main() -> i32 {
  let mut outer = Outer { inner: Inner { value: 1 } }
  let bumped = bumpInner(&mut outer.inner)
  return readInner(&outer.inner)
}
```

The narrower prohibition is on **array subplaces**: a slice must be formed over a complete array
root, so `&values[1]` and a borrow of part of an array are unsupported.

Within one call, two shared borrows of the same root are fine; a shared and an exclusive borrow, or
two exclusive borrows, are rejected. A shared parameter reborrows only as shared; an exclusive
parameter reborrows as either, and the parent's access is suspended for the nested call and restored
on return. Strengthening a shared borrow to exclusive is refused.

A borrow of an array must cover the complete array through a live binding, and `&mut` requires a
mutable root. There is no implicit array-to-slice decay.

### 4.3 Cleanup

Each function carries a cleanup plan recording every structured exit with its releases ordered
last-acquired-first-released. A binding already consumed by a move is not released again, and an
arm-local binding is released at its arm boundary. Every `return` is its own exit.

`drop value` consumes a value early. Dropping a borrowed owner is rejected. A `Drop` hook may
declare neither a failure row nor a capability requirement.

### 4.4 Allocation

There is no ambient heap. Allocation is a capability obtained through the `Allocator` service, and
it can fail with a typed `OutOfMemory` value rather than aborting. Roles let one computation require
more than one allocator.

Raw storage primitives require an explicit `unsafe { ... }` boundary; the same operations outside one
are a missing-unsafe diagnostic. The collection types are ordinary Silk source written on those
primitives — the compiler contains no collection-shaped allocation.

## 5. Effect system

Silk's effect system is not algebraic effects with handlers: there is no `handle`, no `resume`, and
no continuation capture. An effect is a **typed, lazy computation value** with three channels.

### 5.1 The three channels

`Effect<A ! E ? R>`

- `A` — the success type.
- `! E` — the **failure row**: a normalized, duplicate-free set of nominal types.
- `? R` — the **requirement row**: access-qualified capability requirements, each optionally
  carrying a role.

Rows are optional and independent, and multi-member rows are written with `|`.

### 5.2 Constructing and running

An `effect fn` and an `effect { ... }` block both construct a computation **without entering the
body**. Invoking an `effect fn` therefore performs no work; `run` executes exactly one layer.

`run` extends through the complete following expression, including pipeline branches across line
breaks, and stops at a comma, a closing delimiter, or a statement boundary. Parenthesize to end it
early.

`fail value` stops the current computation and has success type `never`. A copyable payload is
copied; an affine payload needs `fail move value`. A failure payload must be a detached owned value
holding no borrow.

```silk
pub struct Rejected {
  pub code: i32
}

impl Report for Rejected {}

effect fn validate(value: i32) -> i32 ! Rejected {
  if value < 0 {
    fail Rejected { code: value }
  }
  return value
}

effect fn twice(value: i32) -> i32 ! Rejected {
  let checked = run validate(value)
  return checked * 2
}

pub effect fn main() -> () ! Rejected {
  let result = run twice(21)
  return ()
}
```

### 5.3 Propagation

Failure rows propagate by union through composition, and requirement rows propagate by union and are
removed one entry at a time by provision. Propagation is entirely type-directed: an undeclared
propagation is a compile error rather than a runtime surprise.

### 5.4 Handling is library code

The compiler owns exactly two primitives at this seam: one that runs a layer and reifies its outcome
as `Result<A, Row<!E>>` data, and one that removes a single capability-role entry.

Everything else — `map`, `mapError`, `flatMap`, `tap`, `catch`, `catchAll`, `retry`, `provide`,
`provideMut`, `provideWith` — is ordinary Silk source in `effects.silk`. The compiler must not infer
their meaning from a name or an origin, so a user-defined equivalent gets identical treatment with
no registration.

Recovery is therefore just reify, `match`, and re-raise or return. Because a failure row reifies to
`Row<!E>` — ordinary value data projected to a structural union — a handler can match on it like any
other value.

`Effect.catch` as it ships today is an unconditional alias for `Effect.catchAll`: its body is
`return run catchAll(move self, move onFailure)`, and its doc comment is identical. Both take a
handler over the whole reified row `Row<!E>`, discard the entire failure row `!E`, and replace it
with the handler's own `!F`. There is no residual row and no member selector — the `E` in
`catch<A, !E, !F, ?R, ?S>` is the protected row, not a chosen member, so a selective
`Effect.catch<E>(handler)` cannot be written.

Selective recovery is therefore done by hand: reify with `Effect.result`, `match` the row, and
re-raise the members you do not handle. The specification does describe a member-selective `catch`;
the standard library does not yet implement one.

### 5.5 Provision

`provide`, `provideMut`, and `provideWith` each remove exactly one capability-role entry and preserve
every remaining one. Provision accepts any provider with a valid conformance; no compiler phase
consults a closed list of service names. `provideWith` acquires inside its own body, so each
execution — including a retry — gets a fresh provider, and it reifies the inner computation to a
result first so that a typed failure arrives as data before the provider is released.

### 5.6 Entry points

An entry point takes one of two forms:

```
pub fn main() -> i32              // exit status is the returned value
pub effect fn main() -> () ! E    // requirement row must be empty
```

Every nominal member of an effectful entry's failure row needs exactly one `impl Report for X {}`
marker conformance, or the entry is unavailable before lowering. Success is status 0; an unhandled
failure becomes a normalized non-zero status after its payload is released through the cleanup plan.
No typed failure or requirement row crosses the machine entry ABI.

## 6. Control flow

### 6.1 `if`

`if` is a **statement**, not an expression. The condition takes no parentheses, the braces are
mandatory, and `else` takes either a block or a chained `if`.

```silk
fn classify(value: i32) -> i32 {
  if value < 0 {
    return 0
  } else if value < 10 {
    return 1
  } else {
    return 2
  }
  return 3
}

pub fn main() -> i32 {
  return classify(5)
}
```

The trailing `return 3` is required even though every arm above it returns. The rule is syntactic:
a body must end in a `return` statement, and the compiler does not exempt a chain whose arms are
exhaustive.

### 6.2 `while`

`while` is the only loop, with bare `break` and `continue`. There is no `for`, no `loop`, no
`do`/`while`, no labeled break, and no iterator or range. The condition must be exactly `bool`.

```silk
pub fn main() -> i32 {
  let mut total = 0
  let mut index = 0
  while index < 10 {
    index = index + 1
    if index == 3 { continue }
    if index > 7 { break }
    total = total + index
  }
  return total
}
```

### 6.3 `match`

`match` **is** an expression and is valid anywhere an expression is. Its access mode is part of the
syntax:

| Form | Requirement | Bindings |
| --- | --- | --- |
| `match value` | scrutinee must be copyable | copies |
| `match move value` | consumes one owned value | owned |
| `match &value` | — | shared, arm-local |
| `match &mut value` | mutable live place | exclusive, arm-local |

A bare `match` on a non-copyable scrutinee is rejected: the mode must be explicit.

Arms are separated by line breaks and need no commas. Each arm is `pattern [if guard] => expression`.
Patterns destructure fields by name, bind a whole member with `Member name`, acknowledge omitted
fields with `..`, and match anything with `_`. A pattern without `..` must name every field exactly
once. Bindings are flat, arm-local, non-shadowing, and precisely narrowed.

Exhaustiveness works by canonical union subtraction in source order. An unguarded nominal arm removes
its member; a **guarded arm does not**, since its guard may fail. `_` covers the remainder and makes
any later arm unreachable. Duplicate, unreachable, and incomplete matches are all rejected, naming
the exact members and spans.

```silk
pub struct Idle {}

pub struct Running {
  pub progress: i32
}

pub struct Done {
  pub code: i32
}

fn status(state: Idle | Running | Done) -> i32 {
  return match move state {
    Running { progress } if progress > 50 => 2
    Running { progress } => 1
    Done { code } => code
    Idle nothing => 0
  }
}

pub fn main() -> i32 {
  let active = Running { progress: 80 }
  return status(move active)
}
```

### 6.4 Expression statements

A complete expression is a statement wherever a statement may begin — no semicolon and no
terminator. Its type must be compatible with `()` or `never`; a non-unit, non-diverging result is
`SEM0087`, telling you to bind it, return it, or consume it explicitly. Values are never discarded
by accident.

Because assignment is a statement rather than an expression, a leading writable place followed by
`=` is an assignment, and any other leading expression forms an expression statement.

### 6.5 `unsafe`

`unsafe { ... }` is a block boundary that authorizes raw storage operations. It contains ordinary
qualified calls and introduces no scope syntax of its own.

## 7. What Silk does not have

Stated plainly, because each one is a deliberate choice rather than an omission:

no `enum`; no type aliases; no `for`/`loop` loops, iterators, or ranges; no named lifetimes or
regions; no implicit numeric conversion or integer widening; no operator overloading; no `if` as an
expression; no implicit moves; no implicit array-to-slice decay; no standalone borrow bindings; no
ambient allocator; no visibility beyond `pub`; no macros; no `async`; and no non-ASCII identifiers.

Concurrency, networking, a package registry, broad FFI, and self-hosting are future work.

## See also

- [Tutorial](./tutorial.md) — from `silk init` to a running program.
- [Standard library](./stdlib.md) — every module and public declaration.
- [Diagnostic index](./diagnostics.md) — every error code, including those cited above.
