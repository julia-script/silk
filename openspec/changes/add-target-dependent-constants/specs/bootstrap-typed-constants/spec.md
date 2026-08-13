## MODIFIED Requirements

### Requirement: Constants have explicit primitive contracts

A top-level constant SHALL declare a name, one concrete primitive type, and one initializer.
Accepted types SHALL include `bool`, every supported integer primitive including `usize`, every
supported floating primitive, and `string`. The initializer MUST be either a literal of the declared
kind that fits the declared type for the selected target, or one target fact named as
`Target.<fact>`. A `string` constant SHALL accept an escaped or a raw text literal in either
delimiter width and SHALL reject a byte-string literal. Type inference, aggregate constants,
computed initializers, and effectful initialization SHALL remain unavailable. When an initializer is
neither a literal of the declared kind nor a known target fact, the reported detail SHALL name that
restriction rather than the set of accepted types.

#### Scenario: Declare representative scalar constants

- **WHEN** a module declares boolean, `u8`, `i32`, `usize`, `f32`, and `f64` constants with fitting literals
- **THEN** every declaration records its exact primitive type and canonical literal value

#### Scenario: Declare a string constant from either literal form

- **WHEN** a module declares `string` constants initialized by an escaped literal and by a raw literal that spell the same content
- **THEN** both declarations record the type `string` and the identical decoded bytes their literals produce

#### Scenario: Reject a mismatched or overflowing initializer

- **WHEN** a constant's literal has the wrong scalar kind or exceeds the declared primitive range
- **THEN** semantic analysis reports the declaration-local mismatch and exposes no usable value

#### Scenario: Report a non-literal initializer as such

- **WHEN** a constant's initializer is an expression rather than a literal or a target fact
- **THEN** semantic analysis reports that the initializer must be one literal

## ADDED Requirements

### Requirement: A constant may name one target fact instead of spelling a literal

A bound on a pointer-width integer has no literal spelling, because `wasm32-unknown-unknown` words
its pointers at 32 bits and every native triple at 64. A constant initializer SHALL therefore accept
`Target.<fact>` naming one member of a closed, compiler-owned vocabulary of pointer-width facts. The
vocabulary SHALL be exactly `Target.usizeMax`, `Target.isizeMax`, `Target.isizeMin`, and
`Target.pointerBits`.

Each fact SHALL determine one declared type — `usize`, `isize`, `isize`, and `u32` respectively —
and a declaration at any other type SHALL be rejected. A member spelling outside the vocabulary
SHALL be rejected with a detail naming the vocabulary.

The form SHALL be recognized on syntax alone and only in constant-initializer position. `Target`
SHALL remain unresolvable as an ordinary name, so the spelling outside an initializer keeps whatever
meaning it has today. This form SHALL NOT admit any other expression: an initializer that applies an
operator to a target fact SHALL be rejected exactly as any other computed initializer is.

Each fact's value SHALL be the one the compiler's scalar table already gives for that pointer width
— the same table the checked arithmetic intrinsics enforce their bounds against — so a named bound
cannot differ from the bound the checked path applies.

#### Scenario: Declare a pointer-width bound

- **WHEN** a module declares `MAX` at type `usize` initialized by `Target.usizeMax`
- **THEN** the declaration is accepted and records the target fact rather than a number

#### Scenario: Reject an unknown target fact

- **WHEN** a constant initializer names a member below `Target` that is not in the vocabulary
- **THEN** semantic analysis reports an invalid constant whose detail names the accepted facts

#### Scenario: Reject a target fact at the wrong type

- **WHEN** a constant declared `isize` is initialized by `Target.usizeMax`, or one declared `i32` by `Target.pointerBits`
- **THEN** semantic analysis reports an invalid constant naming the type the fact carries

#### Scenario: Reject a computed target fact

- **WHEN** a constant initializer applies an operator to a target fact
- **THEN** semantic analysis rejects it as a non-literal initializer

### Requirement: A target fact resolves to one value at the selected target

A constant that names a target fact SHALL resolve to exactly one value once a target is selected,
and every execution engine SHALL observe that same value. The selection SHALL happen in lowering, so
that evaluation, direct WebAssembly, and native LLVM read one already-selected value rather than
each applying the pointer-width rule themselves. No value belonging to a pointer width other than
the selected target's SHALL appear in the lowered program.

Target-aware `usize` range checking SHALL range a target fact at the selected target's value. A
pointer-width bound therefore SHALL NOT be reported out of range on any target, whether or not the
program under analysis mentions it.

Presentation of a target fact SHALL name the fact rather than a value, because the module surface
and the HIR text are target-independent artifacts.

#### Scenario: Select the same bound on every engine

- **WHEN** a program comparing `usize.MAX`, `usize.BITS`, `isize.MAX`, and `isize.MIN` against the identities that define them runs on evaluation, WebAssembly, and native LLVM
- **THEN** every engine agrees, and the observed pointer width is the selected target's

#### Scenario: Observe a different bound on a different width

- **WHEN** the same program is compiled for `wasm32-unknown-unknown` and for a native triple
- **THEN** the 32-bit compilation observes the 32-bit bounds and the 64-bit compilation the 64-bit bounds, with neither width's values present in the other's lowered program

#### Scenario: Accept a pointer-width bound on a narrow target

- **WHEN** any program is analyzed for `wasm32-unknown-unknown` with the pointer-width bounds declared in the standard library
- **THEN** no target-range diagnostic is reported for those declarations
