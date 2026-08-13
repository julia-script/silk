## Context

A constant initializer is read once, in `DeclarationIndex`, into a `ConstantLiteralFact` that
retains the source token. `Elaboration` turns that fact into a typed immediate, and a reference to
the constant lowers to the same HIR literal the equivalent inline spelling would produce. The chain
is deliberately target-independent: elaboration ranges every integer at 64 bits, and the selected
target does not appear until `Layout.catalog`.

That is why the pointer-width bounds could not ship with #38 and it is also what constrains the fix.
The value has to be chosen after target selection, but the declaration has to be accepted, typed,
and navigable before it.

## Goals / Non-Goals

- **Goal.** Let a declaration name a pointer-width fact, and have exactly one phase turn that name
  into a number.
- **Goal.** Keep the one-initializer shape. `SEM0086` is load-bearing and stays as it is.
- **Non-goal.** Evaluating anything. No arithmetic, no calls, no folding.
- **Non-goal.** A new syntax node. The form has to parse with the grammar as it stands.

## Decisions

### The form is `Target.<fact>`, recognized on syntax alone

`Target.usizeMax` already parses, as a `FieldProjectionExpression` over an `IdentifierExpression`.
`DeclarationIndex` matches that shape against the spelling `Target` and a known member, and produces
a `TargetConstant` literal fact. Nothing is resolved through name resolution: `Target` names no
declaration and continues to name none.

The alternatives were a new grammar production and reusing the `Intrinsic` root.

- A grammar production costs a token or node kind plus the parser, the formatter, the syntax
  correspondence, and the editor surfaces — a large amount of machinery for four names.
- `Intrinsic` is a closed catalog of *operations* with arities and availability contracts. A bound
  is not an operation, and putting it there would drag the intrinsic-boundary specification into a
  change that has nothing to do with it. `Target` also says the true thing: the value comes from the
  target.

Because a field projection has never been an accepted constant initializer, the form can only turn
a rejected program into an accepted one. It cannot change what an accepted program means.

### The vocabulary is closed and carries its own type

Four facts: `usizeMax`, `isizeMax`, `isizeMin`, `pointerBits`. Each maps to one declared type —
`usize`, `isize`, `isize`, `u32` — and to one value per pointer width, read from `Scalar.range` and
`Scalar.bits`, the same table `Backend` and `BootstrapEvaluation` enforce their checked arithmetic
against. A bound cannot drift from the range the checked path actually applies, because it is that
range.

Declaring a selector at any other type is rejected. Without that, `pub const MAX: isize =
Target.usizeMax` would be a plausible typo that produces a wrong bound with no diagnostic.

`usize.MIN` is deliberately *not* a selector. Zero is zero at every pointer width, and routing it
through the mechanism would suggest the mechanism is about pointer-width types rather than about
pointer-width *values*.

### Lowering selects; nothing before it does

`Lower.lowerProgram` receives the `Layout.Plan`, so it holds the target, and it is the last phase
before the three engines diverge — evaluation, WebAssembly, and native LLVM all consume the MIR it
emits. Selecting there gives all three the same number without any of them knowing a selection
happened, and without a fourth copy of the pointer-width rule.

Before lowering, elaboration records the fact's *widest* value together with its selector. A value
has to be present for the declaration to type-check and for tooling to treat the reference as an
ordinary immediate, and 64 bits is the width elaboration already ranges every integer at. It is a
placeholder, and two things keep it from leaking:

- HIR prints the selector rather than the number, so no dump shows a value the target contradicts.
- The target-aware `usize` range check in `Layout.catalog` ranges a selector at the selected target,
  not at the recorded width. Without this the 64-bit `usize.MAX` would be reported `LAY0001` on
  every `wasm32-unknown-unknown` compilation, because that check reads every declared `usize`
  constant whether or not the program mentions it.

The acceptance test asserts the *absence* of the other width's value in the lowered MIR, not only
the presence of the right one, because a placeholder that stops being replaced is the failure this
design has to rule out.

## Risks / Trade-offs

- **The recorded value is width-specific before selection.** Hover on a `wasm32` project reports the
  constant's type, which is correct, but the underlying fact carries the 64-bit number. The
  mitigation is that no surface prints it: HIR prints the selector and the module surface records
  the selector. Making elaboration target-aware would be the real fix and is a much larger change to
  the snapshot model.
- **`Target` is a bare identifier.** A user module could export a type or value named `Target`. In
  constant-initializer position the projection form is claimed by this feature; everywhere else the
  spelling is untouched. The cost is bounded because the position it is claimed in currently accepts
  nothing.
