## 1. The target fact vocabulary

- [x] 1.1 Add a compiler-owned table naming the closed vocabulary — `usizeMax`, `isizeMax`,
      `isizeMin`, `pointerBits` — each mapping to one declared type and to one value per pointer
      width, read from the `Scalar` range table the checked intrinsics enforce.
- [x] 1.2 Recognize `Target.<fact>` in constant-initializer position on syntax alone, as a new
      constant literal fact, without a grammar change and without resolving `Target` as a name.
- [x] 1.3 Reject a member outside the vocabulary with a detail naming the accepted facts, and
      reject a fact declared at any type other than the one it carries.

## 2. Selection

- [x] 2.1 Record the fact's widest value plus its selector during elaboration, so the declaration
      types and navigates before a target exists.
- [x] 2.2 Select the value in lowering from the layout plan's target, which every engine reads
      through the MIR it produces.
- [x] 2.3 Range a target fact at the selected target in the target-aware `usize` check, so a
      pointer-width bound is not reported out of range on a 32-bit target.
- [x] 2.4 Print the selector rather than the unselected value in HIR text and in the module
      surface, both of which are target-independent artifacts.

## 3. Standard library

- [x] 3.1 Add `MAX`, `MIN`, and `BITS` to `silk/usize`, with `MIN` an ordinary `0` literal.
- [x] 3.2 Add `MAX`, `MIN`, and `BITS` to `silk/isize`.
- [x] 3.3 Regenerate the compiler-shipped standard-library source table.

## 4. Acceptance

- [x] 4.1 Assert each bound is declared as the target fact and each fixed bound as a literal.
- [x] 4.2 Assert each selector's value equals the scalar range at that pointer width, and that the
      two widths disagree — the premise that no single literal serves.
- [x] 4.3 Assert the lowered MIR carries the selected target's value and not the other width's, on
      both a 32-bit and a 64-bit target.
- [x] 4.4 Assert an unknown fact, a mistyped fact, and a computed initializer are all rejected, and
      that `Target` outside an initializer stays unresolvable.
- [x] 4.5 Assert realizing a program reports no diagnostic on either width, which covers the
      target-aware `usize` range check over the new declarations.
- [x] 4.6 Assert the bounds against the identities that define them — `MAX` is all ones, one past it
      wraps and refuses the checked step, `isize.MAX` complements `isize.MIN`, `BITS - 1` shifts
      `MAX` down to one — and answer the selected pointer width, on evaluation and WebAssembly at
      32 bits and on evaluation and native LLVM at 64.

## 5. Documentation

- [x] 5.1 Regenerate the standard-library documentation page.
