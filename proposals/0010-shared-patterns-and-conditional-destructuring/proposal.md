# SLP-0010: Shared patterns and conditional destructuring

SLP: 0010
Status: Draft
Revision: 3
Author: Julia Ortiz
Created: 2026-08-19
Updated: 2026-08-19
Discussion: —
Review record: —
Depends on: SLP-0003, SLP-0005
Split from: —
Split into: —
Supersedes: —
Superseded by: —
Revisit when: —
Resolution: —
OpenSpec handoff: —

## Summary

Silk should have one small pattern language shared by exhaustive `match`, irrefutable local
destructuring, and conditional `if let`. Patterns inspect nominal values and structural-union
members without calling user code; the matched expression's ordinary `move`, `&`, or `&mut`
access determines what every binding receives. This first direction deliberately excludes
destructuring assignment, parameter patterns, literal/range/or patterns, `while let`, and pattern
chains so that pattern syntax does not become a second expression language.

The author confirmed PATT-001–019 as the intended stabilization direction. Their identifiers remain
proposal-local until the direction is promoted and handed to OpenSpec.

## Problem and evidence

Silk already has recursive nominal patterns, whole-member bindings, omission with `..`, guards,
and exhaustive structural-union matching. Those patterns are accepted only in `match` arms.
Programmers cannot destructure an ordinary nominal value directly, and a simple conditional test
must be written as a complete `match` even when only one member is interesting.

The language reference explicitly records pattern-conditioned `if` as desired but undefined. The
current parser confirms the discontinuity: `parsePattern` is reached from match-arm parsing, while
binding statements require one identifier and boolean `if` requires one expression.

The desired feature is not a new kind of value or a general runtime reflection API. It is one
consistent way to test and bind existing nominal structure at the source positions where that is
useful.

## Driving examples: current and desired

### Case: destructure a known nominal value

#### Intent

Take ownership of a `Point` and give its fields useful local names without writing a one-arm match.

#### Current Silk

```silk
struct Point {
  x: i32
  y: i32
}

fn sum(point: Point) -> i32 {
  return match move point {
    Point { x, y } => x + y
  }
}
```

The complete match is semantically sufficient but makes unconditional destructuring look
conditional.

#### Desired Silk

```silk
struct Point {
  x: i32
  y: i32
}

fn sum(point: Point) -> i32 {
  let Point { x, y } = move point
  return x + y
}
```

#### Observable result

`point` is moved exactly once. `x` and `y` become ordinary locals, and `sum(Point { x: 2, y: 3 })`
returns `5`.

#### Boundary case

```silk
struct Token { kind: i32 }
struct End {}

fn invalid(event: Token | End) -> i32 {
  let Token { kind } = move event
  return kind
}
```

The binding is invalid because `Token { kind }` is refutable for `Token | End` and a `let` binding
has no mismatch path.

### Case: inspect one union member conditionally

#### Intent

Read one member of a union and use a fallback without spelling an exhaustive match.

#### Current Silk

```silk
struct Token { kind: i32 }
struct End {}

fn kindOrZero(event: Token | End) -> i32 {
  return match &event {
    Token { kind } => kind
    _ => 0
  }
}
```

#### Desired Silk

The syntax in this Draft is proposed rather than accepted:

```silk
struct Token { kind: i32 }
struct End {}

fn kindOrZero(event: Token | End) -> i32 {
  if let Token { kind } = &event {
    return kind
  }
  return 0
}
```

#### Observable result

The scrutinee is evaluated once. The binding `kind` exists only in the selected body. A `Token`
returns its kind, an `End` falls through, and the borrowed `event` remains usable afterward.

#### Boundary case

```silk
fn invalid(event: Token | End) -> i32 {
  if let Token { kind } = &event {
    return kind
  }
  return kind
}
```

The final `kind` is unknown because conditional pattern bindings do not escape the selected body.

### Case: consume one member conditionally

#### Intent

Consume an owned union, handling one member and accepting ordinary cleanup for the other.

#### Current Silk

```silk
fn consume(event: Token | End) -> i32 {
  return match move event {
    Token token => token.kind
    End {} => 0
  }
}
```

#### Desired Silk

```silk
fn consume(event: Token | End) -> i32 {
  if let Token token = move event {
    return token.kind
  }
  return 0
}
```

#### Observable result

`event` is unavailable after the test on both paths. A selected `Token` becomes the arm-local owner
`token`; an unmatched `End` is cleaned before execution continues after the conditional.

#### Boundary case

```silk
fn invalid(event: Token | End) -> Token | End {
  if let Token token = move event {
    return move token
  }
  return move event
}
```

The final return is invalid: mismatch does not undo the move or restore the original union owner.

## Goals and non-goals

### Goals

- Give `match`, unconditional destructuring, and conditional destructuring one programmer model.
- Preserve exact structural-union, ownership, borrowing, cleanup, visibility, and scope rules.
- Make refutability explicit: `let` requires proof of a match; `if let` provides a mismatch path.
- Keep patterns declarative and compiler-checkable without user-defined matching hooks.
- Provide local diagnostics for invalid members, fields, bindings, access, and refutability.

### Non-goals

- Destructuring assignment or partial replacement of existing places.
- Destructuring function parameters or callable parameters.
- `while let`, `let ... else`, chained conditional patterns, or value-producing `if`.
- Literal, range, alternative/or, negated, active, extractor, or user-defined patterns.
- Pattern-based overload selection, conversion, interface dispatch, or runtime reflection.
- Revising the already-confirmed coverage and result-joining rules of exhaustive `match`.

## Current language model

Patterns currently occur only in `match` arms:

- `_` covers every remaining member;
- `Type { fields }` selects and destructures one nominal member;
- `Type binding` selects and binds one complete nominal member;
- fields may use shorthand, `field: local`, nested nominal patterns, and `..`;
- an optional guard may inspect provisional bindings;
- the `match` scrutinee selects bare Copy, `move`, `&`, or `&mut` access;
- match coverage is exhaustive over normalized structural-union members.

A binding statement accepts `let` plus optional `mut`, one local name, and an initializer. An `if`
statement accepts only a boolean condition. Neither source position reuses the existing pattern
grammar.

## Proposed language model

### PATT-001 — One contextual pattern grammar

Silk has one pattern grammar. The same nominal destructuring and whole-member forms mean the same
thing in `match`, `let`, and `if let`. Each context may restrict which patterns are valid without
changing the meaning of a pattern it accepts.

### PATT-002 — The matched expression chooses access

The expression on the right of `=` is evaluated exactly once. Ordinary expression syntax chooses
access: a Copy value may be used bare, `move value` consumes an owner, `&value` creates shared
bindings, and `&mut value` creates exclusive bindings. Patterns do not contain a second move or
borrow language.

### PATT-003 — Nominal patterns select canonical nominal identity

`Type { ... }` and `Type binding` match the canonical nominal type, including a structural-union
member with that identity. Spelling aliases or imports differently does not create a distinct
pattern case. Patterns do not expose or compare numeric runtime tags.

The whole-value `Type binding` form also selects an exact non-nominal structural-union member such
as `i32`, `[i32; 2]`, a view, callable, or Effect. Field destructuring remains nominal-only.

### PATT-004 — Field destructuring is recursive and explicit

A nominal field pattern may use its field name as the local name, rename it with `field: local`, or
apply another nominal pattern recursively. Without `..`, every visible field must be named exactly
once. `..` explicitly acknowledges omitted fields, including inaccessible fields; omitted owned
state retains ordinary branch-local cleanup.

### PATT-005 — Pattern bindings are fresh lexical locals

Every binding introduced by one pattern is a flat local declaration scoped to the successful body
or match arm. Bindings cannot collide with one another or shadow another declaration visible in
that scope. Their types and ownership modes are derived from the selected field or whole payload.

### PATT-006 — `let` accepts only irrefutable destructuring

`let Pattern = expression` is valid only when static type information proves that the pattern must
match. An exact nominal value may be destructured; a pattern selecting one member of a multi-member
union is refutable and therefore invalid. Silk does not insert a trap or hidden fallback.

A standalone wildcard binding such as `let _ = operation()` is not an alternative spelling of
`drop`. Intentional result discard continues to use the accepted explicit `drop` statement.

### PATT-007 — `if let` makes one refutable test

`if let Pattern = expression { body }` executes `body` only when the pattern matches. An optional
`else` executes on mismatch and introduces none of the pattern bindings. `else if let` is ordinary
nested conditional syntax. Initial stabilization does not add pattern chains or a pattern guard to
this header; a nested boolean `if` expresses an additional condition.

### PATT-008 — Consuming tests consume on every outcome

When the right expression uses `move`, the source owner is consumed before testing. On success, the
selected payload or fields become bindings and omitted state remains local cleanup. On mismatch,
the unmatched payload is cleaned before the mismatch path continues. Pattern failure does not roll
back a move or reconstruct the original union.

### PATT-009 — Borrowed tests preserve the owner

When the right expression uses `&` or `&mut`, successful bindings are match-local loans and end with
the selected body. On mismatch no field loan is introduced. The original owner remains available
after the complete conditional, subject to the ordinary lifetime of the scrutinee loan.

### PATT-010 — Redundant conditional patterns remain valid

An `if let` whose pattern is statically irrefutable is legal and always selects its first body. The
compiler does not reject harmless redundancy. The language service may warn and offer conversion
to an unconditional destructuring `let`.

### PATT-011 — Patterns execute no user code

Matching performs only compiler-defined canonical union-membership tests and nominal field
projections. It invokes no interface operation, equality overload, conversion, constructor,
accessor, or extractor. Literal, range, or user-defined matching requires a separate proposal.

### PATT-012 — Pattern errors identify the failed proof

Diagnostics attach to the smallest invalid pattern or access expression and distinguish at least:

- refutable pattern in an unconditional `let`;
- member absent from the scrutinee type;
- missing, duplicate, unknown, or inaccessible field;
- duplicate or conflicting binding;
- invalid move, shared borrow, exclusive borrow, or escaping loan;
- pattern form unavailable in the current context; and
- use of a conditional binding outside its body.

Diagnostics should name the scrutinee type, the relevant member or field set, and the required
repair. Stable codes remain an OpenSpec realization question where no existing code applies.

### PATT-013 — Existing exhaustive `match` semantics remain authoritative

This direction reuses rather than weakens `match`: source-order arms, guards, universal coverage,
exhaustiveness, reachability, local narrowing, access modes, and result joins keep their confirmed
rules. `if let P = value { A } else { B }` may lower through the same pattern decision machinery as
a two-arm match without making `if` a value-producing expression.

### PATT-014 — The first pattern surface stays deliberately small

Destructuring assignment, parameter patterns, `while let`, `let ... else`, aliases that bind both a
whole value and its fields, and additional pattern operators remain invalid until separately
designed. Their absence does not reserve implementation-specific behavior.

### PATT-015 — Whole-value type patterns select any exact member

`Type binding` may select and bind one exact canonical member of a structural union, including
non-nominal ordinary value types. Scalar, array, view, callable, Effect, and other non-nominal
patterns bind one complete value; they expose no invented fields, elements, captures, channels, or
representation details.

### PATT-016 — Non-nominal coverage uses exact identity

Coverage, guards, reachability, and exhaustiveness treat a non-nominal type pattern exactly like a
nominal member pattern. Compatibility, equal layout, conversion, or a shared interface does not
merge distinct member identities.

### PATT-017 — One type pattern selects one normalized member

The selected type must normalize to exactly one member. A union type is not an implicit or-pattern
or subset selector; separate arms express separate normalized members.

### PATT-018 — Non-nominal bindings preserve ordinary access

Whole-value bindings inherit the matched expression's bare Copy, `move`, `&`, or `&mut` access and
introduce no allocation, boxing, erasure, or conversion.

### PATT-019 — Generic selectors require static distinctness

A type-parameter selector is valid only when declared constraints prove that it remains one
distinct scrutinee member for every admitted specialization. Pattern validity is checked once in
the generic body rather than changing at individual calls.

## Worked language experience

### Nested unconditional destructuring

```silk
struct Span {
  start: i32
  end: i32
}

struct Token {
  kind: i32
  span: Span
}

fn start(token: Token) -> i32 {
  let Token { span: Span { start: offset, .. }, .. } = move token
  return offset
}
```

The nested pattern is irrefutable because the initializer is exactly `Token`. Omitted owned fields
are cleaned exactly once; `offset` is the only introduced name.

### Private fields may be omitted, not inspected

```silk
// In a module that imports public type Secret:
let Secret { publicId, .. } = move secret
```

`..` allows a consumer to acknowledge fields it cannot name. Attempting to bind a private field is
the ordinary visibility error. The implementation still cleans omitted private owned state through
the defining type's cleanup contract.

### Exclusive conditional access

```silk
if let Token { kind } = &mut event {
  kind = kind + 1
}
```

The selected binding is an exclusive view governed by the same rules as an exclusive match. The
exclusive loan covers the complete conditional and prevents competing access until it ends.

### Additional boolean condition stays explicit

```silk
if let Token { kind } = &event {
  if kind > 0 {
    return kind
  }
}
```

This first model avoids defining evaluation, binding, and failure behavior for mixed pattern and
boolean condition chains.

## Semantic sketch

1. Evaluate the initializer or conditional scrutinee exactly once under its written access mode.
2. Resolve every nominal pattern path and field name to canonical identities.
3. Determine whether the context requires an irrefutable pattern and prove refutability from the
   normalized scrutinee type.
4. Plan member testing, projections, binding modes, and cleanup using the same machinery as
   exhaustive match.
5. On success, introduce all bindings simultaneously in the selected lexical body.
6. On mismatch, introduce no bindings. Preserve a borrowed owner or complete cleanup of a consumed
   payload before continuing.
7. End pattern-local loans and clean non-transferred bindings at the end of the selected body.

## Compiler–standard library boundary

### Compiler necessity

Ordinary Silk functions cannot inspect structural-union membership, introduce lexically scoped
bindings conditionally, prove irrefutability, or split affine cleanup obligations without already
having compiler-supported pattern semantics.

### Smallest target-neutral primitive

This proposal needs no source-callable intrinsic. The smallest compiler capability is contextual
pattern parsing and elaboration that reuses the existing canonical member-test, projection,
ownership, cleanup, and control-flow machinery for `match`, `let`, and `if let`.

### Standard-library construction

No standard-library actor is compiler-known or required. Libraries may expose predicates or
ordinary functions for domain-specific tests, but those functions do not become patterns.

### Privilege audit

Lowering `if let` to the existing two-arm match decision model and unconditional destructuring to
proven projections avoids a second runtime protocol. A library-only encoding cannot introduce the
required bindings or preserve affine branch cleanup, while user-defined extractor privilege would
be strictly larger than the driving cases require.

## Whole-language interaction map

| Surface | Disposition | Analysis |
| --- | --- | --- |
| Syntax and names | Affected | Add patterns after `let` and the contextual `if let` header; all bindings are ordinary lexical names. |
| Types and abstraction | Affected | Irrefutability and member selection use exact nominal identity and normalized structural unions. |
| Execution contracts | Affected | Scrutinees evaluate once; patterns themselves are pure compiler-defined tests and projections. |
| Ownership and resources | Affected | Written access controls binding modes; success, mismatch, omission, and scope exit preserve exact cleanup. |
| Runtime and targets | Affected | Reuse target-neutral match decisions; no reflection API or new runtime service. |
| Compiler | Affected | Parser, semantic facts, HIR/MIR control flow, ownership, evaluator, backends, formatter, and diagnostics reuse one pattern model. |
| Standard library | Not affected — no library protocol | Domain predicates remain ordinary functions. |
| Tooling and diagnostics | Affected | Completion, rename, navigation, formatting, diagnostics, and optional redundancy actions must understand contextual bindings. |
| Learning and use | Affected | Teach one pattern grammar, one access rule, and the irrefutable/refutable context distinction. |

## Scope cohesion

The thesis is one shared pattern language across three closely related binding contexts. Existing
match semantics provide the pattern model; `let` and `if let` differ only in whether they require or
branch on a successful proof. More expressive pattern operators solve independent matching use
cases and are excluded. Destructuring assignment and parameters introduce distinct place-update or
API-contract questions and are excluded.

## Complexity and subtraction budget

The proposal adds two source positions but no new runtime abstraction, standard-library protocol,
or matching operation. It subtracts the conceptual rule that patterns exist only inside exhaustive
match. The largest cost is ownership-correct mismatch cleanup for consuming `if let`; that behavior
must reuse match rather than create an independent lowering path.

## Surface displacement

- The existing future note under `IF-001` becomes a defined conditional form if accepted.
- The binding-statement reference expands from one name to an irrefutable pattern.
- Pattern rules currently housed under match should move to or be shared with a dedicated pattern
  reference rather than duplicated.
- `drop` remains the sole intentional-result-discard statement; wildcard `let` does not displace it.

## Drawbacks and risks

- `if let` is convenience over `match`, so it adds syntax that is not strictly necessary.
- Refutable versus irrefutable is a new term that needs small examples and strong diagnostics.
- Consuming `if let` may surprise programmers who expect a failed test to restore the owner.
- Allowing `..` in unconditional destructuring can hide newly added public fields; this is already
  the meaning of `..` in match and should remain explicit rather than accidental.
- Reusing one grammar may tempt later contexts to accept every pattern indiscriminately; contextual
  validity must remain explicit.

## Alternatives and prior art

### Status quo

Use exhaustive `match` for every destructuring or conditional membership test. This is smaller in
grammar but keeps ordinary destructuring verbose and makes the same pattern mean useful things in
only one syntactic island.

### Smaller primitive or library solution

Add only struct field projection and library predicates such as `Token.is(event)`. A predicate
cannot safely return conditionally scoped borrowed fields or divide affine cleanup without either
runtime boxes or a larger first-class view protocol. It also duplicates union membership knowledge
outside the language.

### Strongest competing language model

Make `match` the only pattern construct and add no `if let`; optionally permit only irrefutable
`let` destructuring. This keeps conditional control flow singular but forces a universal arm and a
result/statement adaptation for the common one-member case. It remains the strongest subtractive
alternative if `if let` proves to add more learning cost than convenience.

## Falsifiers and acceptance blockers

- Consuming conditional mismatch cannot reuse match cleanup without target-specific observable
  differences.
- `let Pattern = value` cannot be diagnosed as irrefutable before executable lowering.
- Shared grammar produces materially different binding or visibility semantics across contexts.
- The accepted explicit-discard model cannot coexist cleanly with pattern omission.
- A simpler spelling than `if let Pattern = expression` materially improves parsing or the
  programmer model without adding ambiguity.

## Open realization questions

- Exact grammar recovery anchors and stable diagnostic codes.
- Whether `let mut Pattern = expression` makes every introduced binding mutable or should remain
  unavailable until per-binding mutability is designed.
- Whether borrowed conditional loans may end before the complete statement under ordinary
  non-lexical lifetime analysis; the observable rule only requires safe access.
- The canonical formatter layout for long nested patterns and `if let` headers.
- Which existing match semantic facts can be generalized directly instead of duplicated.

## Future directions

- `while let` for repeated refutable matching.
- `let ... else` for a required diverging mismatch path.
- Destructuring function parameters.
- Destructuring assignment with atomic replacement and cleanup.
- Literal, range, or-pattern, and alias/as patterns.
- User-authored extractors only if realistic programs justify a larger effect and ownership model.

## OpenSpec realization map

If accepted, one OpenSpec change should define shared pattern syntax and semantic facts, contextual
irrefutability, `let` and `if let` control flow, ownership and cleanup, evaluator/backend parity,
diagnostics, formatter/LSP behavior, and reference updates. It must preserve rather than restate or
weaken the existing exhaustive-match contract.

## Revision and decision record

| Revision | Date | Change or decision |
| --- | --- | --- |
| 1 | 2026-08-19 | Created the first complete review batch: shared nominal patterns in match, irrefutable let, and conditional if-let; ordinary access controls ownership; broader pattern and binding surfaces remain deferred. |
| 2 | 2026-08-19 | Author confirmed PATT-001–014 together: one contextual pattern grammar, irrefutable let, refutable if-let, ordinary access and cleanup, no wildcard discard escape, and a deliberately small initial surface. |
| 3 | 2026-08-19 | Author confirmed the non-nominal completion: exact whole-value type patterns cover any single canonical union member, preserve ordinary access, expose no invented structure, and require generic selectors to be statically distinct for every specialization. |
