# bootstrap-integer-scalars Specification

## Purpose

Define Silk's complete bootstrap integer vocabulary, ergonomic unit and bottom forms, exact literals, conversions, operation modes, and cross-engine behavior for real width-conscious programs.
## Requirements
### Requirement: Integer primitive spellings are lowercase and closed

Silk SHALL recognize exactly `bool`, `u8`, `u16`, `u32`, `u64`, `usize`, `i8`, `i16`, `i32`, `i64`, and `isize` as the integer and Boolean primitive spellings. Uppercase spellings such as `Bool`, `I32`, and `Usize` MUST NOT remain aliases.

#### Scenario: Resolve every integer primitive

- **WHEN** declarations use every canonical lowercase integer spelling
- **THEN** each resolves to one distinct compiler-known type

#### Scenario: Reject a removed spelling

- **WHEN** source uses `I32` without a user declaration of that name
- **THEN** it is unresolved rather than mapped to `i32`

### Requirement: Unit and bottom use ergonomic source forms

`()` SHALL be the unit type and sole value. A function without a written result SHALL return `()`, and bare `return` SHALL return it. `never` SHALL be uninhabited bottom and SHALL contribute no runtime payload.

#### Scenario: Return unit

- **WHEN** a function omits its result and reaches its end
- **THEN** it completes with `()` and no value lane

#### Scenario: Join non-returning control flow

- **WHEN** a branch of type `never` appears where `i32` is required
- **THEN** the join accepts it without constructing a bottom value

### Requirement: Integer literals retain exact contextual magnitude

Integer literals SHALL retain exact magnitude until typed. An immediate integer context, including a concrete ordinary-call parameter, pipeline-inserted parameter, or known homogeneous operator operand, SHALL select a type only when representable; an unconstrained integer SHALL default to `i32`. An enclosing operator-result context MUST NOT suppress the operand context. Range rejection MUST occur before MIR lowering without JavaScript-number rounding. Contextual literal selection MUST NOT convert an already-typed integer expression or otherwise introduce implicit numeric conversion.

#### Scenario: Type a byte magnitude

- **WHEN** `255` appears where `u8` is required
- **THEN** it receives `u8` with exact value 255

#### Scenario: Contextualize an ordinary call literal

- **WHEN** an exact integer literal is passed directly to a concrete integer parameter of an ordinary function
- **THEN** the literal receives the parameter type when its value is representable

#### Scenario: Contextualize a pipeline literal

- **WHEN** an exact integer literal is inserted as a concrete integer parameter by the pipeline operator
- **THEN** the literal receives the parameter type when its value is representable

#### Scenario: Contextualize a comparison operand literal

- **WHEN** `return byte == 13` compares a known `u8` operand with an exact literal inside a `bool` result context
- **THEN** the literal receives `u8` from the homogeneous comparison operands rather than `bool` or the unconstrained `i32` default

#### Scenario: Reject a wide out-of-range magnitude

- **WHEN** a literal exceeds its selected integer range
- **THEN** analysis reports the range before MIR lowering

#### Scenario: Reject an already-typed mismatch

- **WHEN** an integer expression with an established type is passed to a different integer parameter type without an explicit conversion
- **THEN** analysis rejects the call rather than converting the expression

### Requirement: Integer operations are homogeneous and explicit

Ordinary arithmetic SHALL accept one identical integer type and trap on overflow, invalid
division/remainder, or invalid shift counts. Comparisons SHALL return `bool`. Every integer SHALL
expose bitwise operations, shifts, and rotates. Named recoverable checked operations SHALL remain
ordinary standard-library wrappers returning direct nominal `Option<T>` values; their sealed scalar
primitives SHALL report only the low-level present-or-absent outcome through carrier-neutral inputs
and MUST NOT construct or recognize `Option` by spelling. Wrapping and saturating variants SHALL
return `T`. No numeric conversion SHALL be implicit.

#### Scenario: Trap ordinary byte overflow

- **WHEN** `u8.add(255, 1)` executes
- **THEN** evaluation, native, and WebAssembly trap at the same operation

#### Scenario: Recover checked overflow

- **WHEN** `u8.checkedAdd(255, 1)` executes
- **THEN** it returns `Option<u8>.None`, while representable addition returns `Option<u8>.Some`

#### Scenario: Reject mixed arithmetic

- **WHEN** an expression combines `i32` and `i64` without conversion
- **THEN** analysis rejects it without choosing a wider type

#### Scenario: Rename a checked wrapper and carrier

- **WHEN** ordinary source calls the same checked scalar primitive with equivalent present and absent constructors for another nominal union
- **THEN** the primitive reports the same arithmetic outcome without compiler registration of either carrier or variant spelling

### Requirement: Every admitted integer operation has engine parity

HIR, MIR, layout, evaluator, native LLVM, and direct WebAssembly SHALL support every integer type and operation accepted by analysis. Unsupported target behavior MUST be rejected before artifact commitment.

#### Scenario: Compare integer engines

- **WHEN** an accepted integer fixture completes, returns `Option`, or traps
- **THEN** evaluator, native, and WebAssembly agree on its outcome and provenance

### Requirement: Concrete integer primitives use the Intrinsic namespace

Each admitted integer type and concrete primitive operation SHALL have one unambiguous
type-specific member of `Intrinsic`. Its name and contract SHALL identify the concrete input and
result types without overload resolution or runtime type inspection. Existing arithmetic,
comparison, bitwise, shift, rotate, checked, wrapping, saturating, negate, and conversion semantics
MUST remain unchanged.

#### Scenario: Call one concrete primitive

- **WHEN** source calls `Intrinsic.i32Add` with two `i32` values
- **THEN** it produces the same `i32` value or overflow trap on every execution engine

#### Scenario: Reject a mismatched concrete type

- **WHEN** `Intrinsic.i32Add` receives a value of another integer type
- **THEN** analysis rejects the call without conversion or generic inference

### Requirement: Generic integer APIs are source-defined

The standard library SHALL define ordinary numeric interfaces and actor-module functions that can
operate over supported integer types. Primitive integer types SHALL have canonical standard-library
conformances mapping interface operations to their concrete intrinsics. Generic calls SHALL
monomorphize to the selected concrete operation without a runtime union, type tag, service slot, or
numeric registry.

#### Scenario: Specialize generic addition

- **WHEN** a generic `add<T: Integer>` call is instantiated with `i32`
- **THEN** specialization selects the canonical `i32` conformance and lowers to `Intrinsic.i32Add`

#### Scenario: Reject a non-integer type

- **WHEN** a type without the Integer conformance is passed to the generic addition API
- **THEN** conformance analysis rejects the call before MIR lowering

### Requirement: Unicode scalar conversion is explicit and checked

The concrete scalar surface SHALL expose one checked `u32`-to-`char` conversion returning
`Option<char>` and one total explicit `char`-to-`u32` conversion. The checked conversion SHALL
accept exactly `0...0xd7ff` and `0xe000...0x10ffff`; it SHALL return `None` for surrogates and larger
integers without truncating or trapping. Both operations SHALL be available through ordinary
standard-library source backed by the smallest concrete Intrinsic primitives.

#### Scenario: Convert the maximum scalar

- **WHEN** checked conversion receives `0x10ffff`
- **THEN** it returns `Some<char>` containing that scalar on every engine

#### Scenario: Reject the surrogate hole

- **WHEN** checked conversion receives a value from `0xd800` through `0xdfff`
- **THEN** it returns `None` on every engine

#### Scenario: Recover the scalar integer

- **WHEN** explicit conversion receives any valid `char`
- **THEN** it returns the exact corresponding `u32`

### Requirement: Signed remainder overflow semantics are identical across executors

Signed integer `%` with operands `MIN` and `-1` SHALL trap on every executor (interpreter, wasm, native), consistent with the existing rule that ordinary arithmetic traps on invalid division/remainder. The checked remainder of `MIN` and `-1` SHALL return `None` on every executor, and no executor SHALL evaluate it through an operation whose result is undefined for those operands.

#### Scenario: Ordinary remainder of MIN by -1 traps everywhere
- **WHEN** a program evaluates `i32::MIN % -1` (or the equivalent for any signed width) on any executor
- **THEN** execution traps, and the same program traps identically on the interpreter, the wasm backend, and the native backend

#### Scenario: Checked remainder of MIN by -1 is None everywhere
- **WHEN** a program evaluates the checked remainder of `i32::MIN` and `-1` on any executor
- **THEN** the result is `None`, identically on the interpreter, the wasm backend, and the native backend

### Requirement: Rotate counts wrap modulo lane width on every executor

Rotate-left and rotate-right SHALL interpret the count modulo the operand's bit width using an unsigned (Euclidean) reduction, so negative and out-of-range counts wrap instead of degenerating, identically on every executor.

#### Scenario: Rotate by a negative count wraps
- **WHEN** a program evaluates `rotate_left(x, -1)` on an odd `i32` value on any executor
- **THEN** the result equals `rotate_left(x, 31)` — the low bit wraps into bit 31 — identically on the interpreter, the wasm backend, and the native backend
