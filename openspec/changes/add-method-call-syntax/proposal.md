## Why

`add-inherent-impl-members` gives Silk declared type members but leaves invocation data-first only:
`Option.map(move value, f)` and `move value |> Option.map(f)` work, `value.map(f)` does not. Today
that spelling parses as a call of a callable stored in field `map`. Receiver syntax is the last
missing presentation of one member, and a generic body bounded by an interface still needs a
value-side call form for the bound's operations. This change adds `receiver.member(args)` as a
third spelling of the same statically selected target, with no new callable model and no dynamic
dispatch.

## What Changes

- Resolve `receiver.member(args)` where the receiver's static type is a nominal declaration: first
  an existing field of that name (callable-field application is unchanged), then an inherent
  receiver method of the owner. Where the receiver's static type is a generic parameter, resolve
  `member` as the unique receiver operation among that parameter's declared bounds, through the
  existing bound-operation call surface.
- Analyze the receiver once against parameter zero and the written arguments against parameters
  one onward, then record the same static call the explicit form records. `value.M(args)`,
  `Type.M(value, args)`, and `value |> Type.M(args)` name one member.
- Adapt receiver ownership from the declared parameter zero: `self: &Self` takes a shared loan of a
  receiver place, `self: &mut Self` an exclusive loan (rejected on a non-`mut` binding), `self:
Self` consumes a place or an rvalue. No auto-dereference, no other coercion; `Type.M(...)` and
  `Type.M(&value, ...)` and `Type.M(move value, ...)` keep every explicit form.
- Reject `value.member` when `member` is an associated function without a receiver, and reject a
  receiver method named outside callee position: bound method values are not part of V1.
- Defer interface-backed members on concrete receivers (`document.print()` through a
  Printable-for-Document witness). Bare `Interface.op(&value)` has no witness path for a concrete
  provider today, so that exposure needs its own provider-selection design and its own change.
- Teach completion after `value.` to list fields and receiver methods, hover on a called member to
  show the receiver-bound contract, and definition/rename to converge on the member.

## Capabilities

### New Capabilities

- `bootstrap-method-calls`: receiver-syntax resolution, receiver adaptation, precedence and
  ambiguity rules, and the equivalence of the three call forms.

### Modified Capabilities

- `bootstrap-type-generics`: a generic receiver's methods come only from its declared bounds and
  resolve to the same bound operation the explicit `Bound.op(value)` form resolves to.
- `language-server-completion`: value-qualified completion lists receiver methods beside fields.
- `language-server-hover`: a called member hovers with its receiver-bound contract.
- `language-server-navigation`: receiver-syntax occurrences share the member's identity.

## Impact

Expression and call analysis (`ExpressionAnalysis.ts`, `CallResolution.ts`), synthesized
receiver borrow and move facts, bound-operation candidate collection over one parameter's bounds,
presentation and editor intelligence (`Completion.ts`, hover, navigation), and the prescriptive
reference pages. No parser change: `value.member(args)` already parses. No HIR, MIR, or backend
change: a method call lowers as the same call the explicit form lowers to. Depends on
`add-inherent-impl-members`.
