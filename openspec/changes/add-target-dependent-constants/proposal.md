## Why

`usize` and `isize` are the only integers with no `MAX`, `MIN`, or `BITS`. #38 shipped those three
in the eight fixed-width modules as single explicitly typed literals and had to leave the
pointer-width pair out, because a bound on a pointer-width integer has no literal spelling:
`wasm32-unknown-unknown` words its pointers at 32 bits and the three native triples at 64, so
`18446744073709551615` is the largest `usize` on one target and out of range on the other. The
constant contract admits one literal, and there is no one literal to write.

Nothing here needs evaluating. The compiler already holds both bounds — `Scalar.range` is the table
the checked intrinsics enforce, and it is already parameterized by pointer width. What is missing is
a way for a declaration to *name* the fact instead of spelling a number, and a defined point at
which the naming resolves to one value.

The alternative considered and rejected on #109 was a general compile-time-evaluated initializer,
relaxing the one-literal rule so `usize.MAX` could be written as an expression. That is the
irreversible direction — once initializers accept expressions, source depends on it immediately —
and it answers a much larger question ("which expressions? which calls? loops?") than this one.

## What Changes

- Add one accepted constant initializer form beside the literals: `Target.<fact>`, where `<fact>`
  comes from a closed compiler-owned vocabulary of pointer-width facts. It is recognized on syntax
  alone, in constant-initializer position only. A field projection was never an accepted
  initializer, so no program that analyzes today changes meaning.
- Define the closed vocabulary as exactly the facts the bounds need: `Target.usizeMax`,
  `Target.isizeMax`, `Target.isizeMin`, and `Target.pointerBits`. Each names one type as well as one
  value, so a selector declared at the wrong type is rejected rather than silently mis-bounded.
- Resolve the selection during lowering, which is the first phase holding the selected target and
  the last one shared by evaluation, WebAssembly, and native LLVM. All three engines therefore
  observe one value without any of them selecting it.
- Add `MAX`, `MIN`, and `BITS` to `silk/usize` and `silk/isize`. `usize.MIN` stays an ordinary
  literal `0`, because an unsigned floor is zero at every pointer width.

## Non-goals

- **No general constant evaluation.** `SEM0086` still admits exactly one term, and an expression
  initializer — including `Target.pointerBits + 1` — is still rejected.
- **No `NAN`.** A NaN is a spelling problem, not a target problem, and #109 records that it blocks
  nothing: a NaN is obtainable at runtime from `zero / zero`. It stays a separate follow-up.
- **No new target facts beyond the four.** The vocabulary is closed and each entry exists because a
  declared bound needs it.
