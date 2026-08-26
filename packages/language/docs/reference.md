# Silk language reference

This reference states the rules of the Silk language as the current compiler implements them. It
covers the lexical form, the declarations, the type system, the memory and ownership rules, and the
effect system. The [tutorial](./tutorial.md) is the gentler introduction; this document is the one
that answers "is that allowed?".

The specifications under `openspec/specs/` are the normative source. Every Silk block on this page
is compiled by `packages/compiler/test/DocumentationExamples.test.ts`.

## 1. Lexical form

### 1.1 Keywords

The keyword vocabulary is closed. There are 29:

```
pub struct enum service interface effect fn run fail drop unsafe impl for
return import as let const mut once move match if else while
break continue true false
```

Keyword recognition applies to a complete identifier only, so `letter`, `iffy`, and `matcher` are
ordinary identifiers.

There is no `type` alias, no `trait` (it is `interface`), no `loop`, no `async`, and no visibility
beyond `pub`. `for` appears only in `impl Capability for Target`.

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

### 1.6 Character literals

A character literal is delimited by `'` and holds exactly one Unicode scalar. Its type is `char`,
and no integer literal ever takes that type.

```
'a'        ' '        '\t'        '\u{2603}'        '\''        'é'
```

The escapes are the text escapes, plus `\'` for the delimiter. The rule is a scalar rule and never
a byte rule: `'é'` is two UTF-8 bytes and one character. An empty body, a body of more than one
scalar, and a body that runs to the line ending are each one lexical error.

```silk
const asciiSpace: char = ' '
const asciiTab: char = '\t'
const snowman: char = '\u{2603}'

pub fn isSpace(value: char) -> bool {
  return value == asciiSpace
}

pub fn main() -> i32 {
  if isSpace(' ') {
    if asciiTab < snowman { return 0 }
  }
  return 1
}
```

### 1.7 Operators and precedence

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
| 18 | `&&` | left |
| 16 | `\|\|` | left |
| 10 | `\|>` | left |

Comparison operators are non-associative, so an ungrouped chain such as `1 < 2 < 3` is rejected.
The bitwise operators occupy three separate levels rather than one.

Operators are not overloadable. Each lowers to a compiler-known operation chosen by operand type —
except `&&` and `||`, which lower to a conditional instead, because a call would evaluate both
operands. See [1.8](#18-short-circuit-operators).

`|>` is the pipeline operator: it inserts its left operand as the *leading* argument of the
callable on its right.

```silk
import silk.i32 as i32

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

### 1.8 Short-circuit operators

`&&` and `||` take `bool` operands and give `bool`. There is no truthiness: no other type is
accepted on either side. `&&` does not evaluate its right operand when its left operand is
`false`, and `||` does not evaluate its right operand when its left operand is `true`. This is a
guarantee about what runs, not an optimization — a right operand that would trap does not trap on
the path that skips it, which is what makes a bounds check like the one below correct:

```silk
pub fn inRange(values: &[i32], index: usize) -> bool {
  return index < values.length && values[index] > 0
}

pub fn main() -> i32 {
  let values = [1, 0, 3]
  if inRange(&values, 4000) { return 1 }
  return 0
}
```

The right operand must be a **pure** expression. An effect site (`run`) or a `move` anywhere
inside it is rejected, so no effect is conditionally performed and no value is conditionally
consumed. The left operand carries no such restriction, because it always evaluates. To perform an
effect conditionally, use a statement-level `if`, which already carries the ownership rules for a
value produced on one path only.

## 2. Declarations

There are exactly eight top-level declaration forms: `import`, `const`, `struct`, `enum`, `service`,
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

An unchanged alias, an exact duplicate, and separate compatible imports of one module are valid.
Only bindings that claim the same local spelling for different declarations collide. The language
service may warn about redundant forms and offer to consolidate them. Imports are unconditional and
top-level only, and standard-library actors require them just like project actors.

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

An initializer may also name one **target fact** instead of a literal. A bound on a pointer-width
integer has no literal spelling — `wasm32-unknown-unknown` words its pointers at 32 bits and every
native triple at 64 — so the compiler selects the value once the target is chosen. The vocabulary is
closed, and each fact carries its own type:

| Fact                  | Type    | Value                                    |
| --------------------- | ------- | ---------------------------------------- |
| `Target.usizeMax`     | `usize` | The largest `usize` at the pointer width |
| `Target.isizeMax`     | `isize` | The largest `isize` at the pointer width |
| `Target.isizeMin`     | `isize` | The smallest `isize` at the pointer width |
| `Target.pointerBits`  | `u32`   | The target pointer width in bits         |

```silk
pub const MAX: usize = Target.usizeMax
pub const BITS: u32 = Target.pointerBits
```

This is the only non-literal initializer. `Target` is recognized in this position alone, and an
expression over a target fact — `Target.pointerBits + 1` — is rejected like any other computed
initializer.

### 2.3 Functions

```
[pub] fn name[<TypeParams>](params) [-> ReturnType] { body }
[pub] effect fn name[<TypeParams>](params) -> Type [! FailureType] [? RequirementRow] { body }
```

An omitted return type means `()`. Parameters are `name: Type` and a trailing comma is allowed.
Every reachable path of a non-unit body must end in a compatible `return` or another terminal
operation. An incompatible return reports `SEM0129`; reachable non-unit fallthrough reports
`SEM0130`. A trailing `return` is unnecessary when all branches are already terminal.

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

### 2.5 Scalar enums

A scalar enum declares a closed, nominal set of payload-free values. Its representation defaults
exactly to `u8`; the compiler does not infer a wider type. An explicit representation may be `u8`,
`u16`, `u32`, `u64`, `i8`, `i16`, `i32`, or `i64`. Choose one explicitly when the discriminants do
not fit the default or when an ABI requires a particular width.

```silk
enum Direction {
  North,
  East,
  South,
  West,
}

enum(i16) Status {
  Unknown = -1,
  Ready = 10,
  Running,
}

fn statusCode(value: Status) -> i16 {
  return Status.value(value)
}

fn classify(value: Status) -> i32 {
  return match value {
    Status.Unknown => 0
    Status.Ready => 41
    Status.Running => 42
  }
}

pub fn main() -> i32 {
  let status = Status.Running
  let code = statusCode(status)
  drop code
  if status == Status.Running { return classify(status) }
  return 0
}
```

Members are constructed only by qualified names such as `Status.Ready`; there is no call,
payload, or allocation. Every declaration implicitly contributes `EnumName.value(value)`, which
returns the exact representation type. There is no built-in conversion from an integer to an enum.

The first implicit discriminant is `0`. Each later implicit discriminant is the previous value plus
one, including after an explicit signed decimal discriminant, so `Status.Running` above is `11`.
Every value is checked against the declared representation. Negative values are invalid for unsigned
representations, duplicate values are rejected, and explicit or implicit overflow is an error. An
enum that outgrows the default `u8` must be widened explicitly.

Enum equality and inequality require two values of the same declaration. Even enums with identical
members and representations are different nominal types. Enums have no direct ordering; compare
`EnumName.value(left)` and `EnumName.value(right)` when numeric ordering is intended.

A match uses qualified member patterns and must cover every member, unless `_` covers the remainder.
Scalar enums are not structural unions: an enum is one nominal, payload-free type with a chosen public
integer representation, while `A | B` is a structural choice among payload-bearing nominal types and
keeps its runtime tag private.

### 2.6 Interfaces and impls

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

An operation whose name an operator spells is reached through that operator. Every other operation
is reached by qualifying through the bound's own name, `Bound.operation(args)`, the same shape a
service operation takes:

```silk
pub interface Mixer {
  fn mix(left: Self, right: Self) -> Self
}

impl Mixer for i32 { mix: Intrinsic.i32WrappingAdd }
impl Mixer for u8 { mix: Intrinsic.u8SaturatingAdd }

pub fn blend<T: Mixer>(left: T, right: T) -> T { return Mixer.mix(move left, move right) }
```

One body, two specializations, two instructions: `blend<i32>` wraps and `blend<u8>` saturates,
because each reads the operation its own witness maps. The call is still specialization-time only —
no dispatch, no provider slot, no row.

Inside a body bounded by an interface, that name selects the bound's operation even when the
interface's module also declares a public function of the same name; the module function stays
reachable through its module everywhere else. A declaration that bounds two of its type parameters
by one interface leaves the receiver naming neither, and the call is reported with `SEM0097`.

### 2.7 Services

A `service` declares runtime capability contracts — operation signatures only, with no fields, no
bodies, and no default provider. Using one places an entry in a requirement row, which a caller
must later satisfy with `provide`. This is the deliberate contrast with `interface`, which is
resolved statically and never appears in a row.

### 2.8 Bindings

`let` binds immutably and `let mut` binds mutably. There is no `var`, and locals carry no type
annotation — a local's type is always inferred. Assignment is a statement, never an expression.

## 3. Type system

### 3.1 Primitive types

The primitive spellings are lowercase and closed:

`bool`, `char`, `i8`, `i16`, `i32`, `i64`, `isize`, `u8`, `u16`, `u32`, `u64`, `usize`, `f32`,
`f64`.

`char` is one Unicode scalar value. It is not an integer: it has equality and ordering and no
arithmetic, and a conversion to or from an integer has to be written out in source.

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

Type parameters are declaration-local. Plain parameters such as `T` and `E` are ordinary value
types. Requirement rows use the distinct `?R` kind. `!` labels an Effect's failure channel; it does
not declare a generic kind. Using `?R` where an ordinary value type belongs is a kind mismatch.

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

#### Which type arguments may be omitted

A call writes **all**, **some**, or **none** of a callable's type arguments. What it writes is a
prefix matched to the declared parameters in order; every parameter past the prefix is inferred from
the value arguments exactly as it is when nothing was written. A declaration with one parameter
inference cannot reach therefore costs one annotation rather than a full list.

```silk
fn pick<A, B>(left: A, right: B) -> A {
  return move left
}

fn phantom<A, B>(value: A) -> A {
  return move value
}

pub fn main() -> i32 {
  let inferred = pick(40, true)
  let prefix = pick<i32>(1, true)
  let complete = phantom<i32, bool>(1)
  return inferred + prefix + complete
}
```

Three rules bound a prefix:

- Writing **more** type arguments than the callable declares is `SEM0051`. Writing fewer is a
  prefix, not an arity error.
- A parameter the prefix does not write and no value argument determines is `SEM0099`, which names
  that parameter rather than the whole list.
- A written type argument the value arguments contradict is `SEM0100`, reported at the type argument
  that was written rather than at the call.

The list is positional, so a parameter can be omitted only when every parameter after it is omitted
too: `pick<i32>` writes `A`, and there is no way to write `B` alone. Declare the parameters
inference cannot reach — a phantom parameter, one that appears only in the return type, or one that
appears only in a failure or requirement row — ahead of the parameters the arguments determine.

A callable section, which supplies every argument but the first, takes a prefix the same way. Its
own leading parameter stays open there, because the section is still waiting for that argument.

## 4. Memory and ownership

### 4.1 Affine values, explicit moves

Silk is affine: a value of a move-only type is used at most once. There are **no named lifetimes and
no region syntax**; borrow safety comes from lexical call scoping instead.

Every binding has one of two ownership categories, and the category decides whether `move` is
required:

| Category | Types | Consuming use |
| --- | --- | --- |
| **Copyable** | every scalar and `bool`, `string`, `never`, `OutOfMemoryError`, shared slices `&[T]`, shared references `&T`, shared effects, shared callables, and fixed arrays whose element type is copyable | copies; the binding stays usable |
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

Cleanup descends through hooks by ordinary calls, so releasing a deeply nested owned value is a deep
recursion and is bounded by the machine stack like any other. A type that can form an unbounded
chain needs an explicit iterative teardown — see
[Recursion and the machine stack](./recursion.md#6-cleanup-has-the-same-limit-and-fewer-ways-out).

### 4.4 Allocation

There is no ambient heap. Allocation is a capability obtained through the `Allocator` service, and
it can fail with a typed `OutOfMemoryError` value rather than aborting. Roles let one computation require
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
- `! E` — the **failure type**: an ordinary detached owned type or structural union; `never` is
  empty.
- `? R` — the **requirement row**: access-qualified capability requirements, each optionally
  carrying a role.

The labeled channels are optional and independent, and multi-member unions are written with `|`.

Failure types use ordinary structural-union normalization, containment, and difference. Requirement
rows have their own finite algebra: they are keyed by capability and role and store either shared
or exclusive access. Requirement union joins a
shared/exclusive collision to the stronger exclusive entry, while membership, subset, intersection,
and difference compare the stored access exactly. Provider compatibility is separate: an exclusive
provider can satisfy a shared demand, but selection returns the original shared member before
subtraction.

`Without<E, S>` subtracts ordinary union alternatives. `Without<R, S>` also operates on requirement
rows when both operands are requirement-row values. Both forms are forward-computed after their
operands are independently known; neither infers an unknown operand backwards from an expected
remainder. Difference is total:

- `Without<First | Second | Third, First | Third>` is `Second`.
- `Without<First | Second, Other>` is still `First | Second` because an absent member is a no-op.
- `Without<&mut Logger, &Logger>` is still `&mut Logger` because shared and exclusive stored access
  do not match.
- `Without<&mut Clock | &mut Logger, &mut Logger>` is `&mut Clock`.

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

Failure types propagate by ordinary union through composition, and requirement rows propagate by union and are
removed one entry at a time by provision. Propagation is entirely type-directed: an undeclared
propagation is a compile error rather than a runtime surprise.

### 5.4 Handling is library code

The compiler owns three sealed primitives at this seam: one that runs a layer and reifies its
outcome as `Result<A, E>` data, one that binds a single capability-role entry, and one that
dispatches a selected failure type. Their contracts use ordinary failure types plus the same checked
requirement-row constraints as ordinary source declarations.

Everything else — `map`, `mapError`, `flatMap`, `flatten`, `tap`, `catchAll`, `retry`, `ensuring`,
`zip`, `zip3`, `provide`, `provideMut`, `provideEffect`, and the singleton `catch` wrapper — is
ordinary Silk source in `effect.silk`. The compiler must not infer their meaning from a name or an
origin, so a user-defined equivalent gets identical treatment with no registration.

Recovery is therefore just reify, `match`, and re-raise or return. `E` is already ordinary value
data, so a handler can match on it like any other value.

Selective and whole-row recovery have distinct names and contracts:

```
Effect.catchAll(protected, handler)    // whole failure-type recovery
Effect.catch<S>(protected, handler)    // selective: recovers S, propagates the rest
```

`Effect.catchAll` takes a handler over ordinary `E`, discards `E` in full, and replaces it with the
handler's own `F`. `Effect.catch<S>` places the selected type first, checks `S in E`, gives the
handler a value of `S`, and computes `Without<E, S> | F`. `S` may itself be a union. Omitting `<S>`
infers it from the handler input.

#### Why singleton catch has a sealed dispatch primitive

Type algebra can state the generic result, but ordinary Silk pattern matching still has no residual
binder that can split one runtime union value into “selected payload” versus “the same payload under
the residual union.” `Intrinsic.catchFailure` is the minimal target-neutral dispatch operation. It
consumes already-proved membership and concrete specialized types; it does not infer `S`, filter a
union, or reconstruct the result type.

#### What ships today

`Effect.catch<S>` is executable on the evaluator, WebAssembly, and native targets. The protected
Effect and handler are formed in ordinary call-evaluation order, then the protected Effect runs once.
Success bypasses the handler. A failure belonging to `S` invokes the handler exactly once; every
other failure keeps its payload and remains in the residual union. All targets share that specialized
MIR behavior and cleanup order. Invalid selectors (`never` or any type containing an alternative
absent from `E`) are rejected during semantic analysis before lowering.

`Effect.ensuring` is the cleanup counterpart: it runs a finalizer after the protected Effect
completes either way, then hands on the original success value or the original typed failure
unchanged. It is built the same way — reify, run the finalizer, then return or re-raise — and that
shape is what fixes the order. The protected Effect's frame has already exited and its locals are
already cleaned when the finalizer starts, so the finalizer exits last, in reverse acquisition order
against the cleanup it wraps.

The finalizer is typed `Effect<() ! never ? S>`. It cannot fail, so there is no second outcome to
reconcile with the one being preserved, and the failure type of the wrapped Effect is untouched. A
release that can fail is recovered into that contract first — `Effect.catchAll(release(), ignore)` —
which leaves the decision about what a failed release means with the caller. A trap is not an
outcome: it bypasses the finalizer exactly as it bypasses `catch` and every `Drop` hook.

`Effect.zip` collects instead of transforming: it runs two Effects in order and returns a `Pair`
holding both success values, with both failure types and both requirement rows unioned. `Effect.zip3`
does the same for three operands and returns a `Triple`. Sequencing is the body's own statement
order — `let first = run self` then `let second = run other` — so a typed failure from the first
operand propagates out before the second `run` is reached. The second Effect is never executed, and
because it is an owned local of the frame the failure leaves, the propagation exit releases it like
any other local. Neither combinator reifies, because neither has anything to do after a failure.

Arity is fixed, and that is a property of the language rather than a simplification. A
collection-taking `all` would need `Vector<Effect<...>>`, which cannot be lowered: Effect values are
compiler-private with no target layout, and they survive being passed and returned only because
hidden-identity specialization erases them at each statically known use. A `RawBuffer` element's
identity is a runtime value, so there is nothing to erase — such a program passes semantic analysis
today and then fails MIR verification with `MissingTypeLayout`. Distinct parameters keep every
operand statically known. A caller combining more than three Effects nests: `zip(zip3(a, b, c), d)`.

### 5.5 Provision

`provide`, `provideMut`, and `provideEffect` each remove exactly one capability-role entry and preserve
every remaining one. Provision accepts any provider with a valid conformance; no compiler phase
consults a closed list of service names. `provideEffect` acquires inside its own body, so each
execution — including a retry — gets a fresh provider, and it reifies the inner computation to a
result first so that a typed failure arrives as data before the provider is released.

Provider modes are fixed and source-visible: `provide` selects through a shared borrow,
`provideMut` through an exclusive borrow, and `bindRequirementOwned` by taking ownership. Shared
providers select only shared demands; exclusive and owned providers may satisfy shared or exclusive
demands. In every case selection returns the exact stored member, including its role and access,
before `Without` removes it. If one provider matches several entries, the call is ambiguous unless
the complete selected row is supplied as the first generic argument. A partially applied provider
section retains that selection obligation until a statically visible Effect application completes
it.

Here `Logger.stdoutProvider()` returns a `StdoutLogger`, which conforms to `Logger`. The explicit
selected row removes `&mut Logger` while preserving `&mut Clock`, independently of canonical row
order:

```silk
import silk.effect as Effect
import silk.logger { Logger, LogError }

service Clock {
  effect fn tick() -> i32 ? &mut Clock
}

effect fn read() -> i32 ! LogError ? &mut Clock | &mut Logger {
  run Effect.log("Reading clock")
  return run Clock.tick()
}

effect fn withLogger() -> i32 ! LogError ? &mut Clock {
  let mut logger = Logger.stdoutProvider()
  return run Effect.provideMut<Logger>(read(), &mut logger)
}
```

### 5.6 Entry points

An entry point takes one of two forms:

```
pub fn main() -> i32              // exit status is the returned value
pub effect fn main() -> () ! E    // requirement row must be empty
```

Success is status 0. An unhandled typed failure becomes a normalized runtime error with status 1
after its payload is released through the cleanup plan. No marker interface is required merely to
use an ordinary value as an error. No typed failure or requirement row crosses the machine entry
ABI.

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

`while` is also the answer to depth. An ordinary recursive call costs a machine stack frame and Silk
promises no bound on how many are available, so a traversal whose depth comes from input is written
as a loop — see [Recursion and the machine stack](./recursion.md).

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

The scrutinee must be a nominal type or a union of nominal members, in every mode. `match` is not a
general switch — matching a scalar such as an `i32`, or a borrow of one, is `SEM0041` ("Cannot match
non-nominal type"). Use `if` / `else if` to branch on a scalar.

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
- [Recursion and the machine stack](./recursion.md) — the bound on ordinary recursion, and how it
  fails on each engine.
- [Standard library](./stdlib/) — every module and public declaration.
- [Diagnostic index](./diagnostics.md) — every error code, including those cited above.
