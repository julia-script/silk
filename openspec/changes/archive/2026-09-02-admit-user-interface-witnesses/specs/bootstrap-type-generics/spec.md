## ADDED Requirements

### Requirement: A bounded parameter specializes at a user type and reaches its witness

Specialization SHALL admit a user-defined nominal type as the argument for a parameter bounded by an
interface whenever that type's conformance covers the bound's whole contract, on the same terms as a
built-in scalar. An operator on a bound-typed operand SHALL reach the operation the argument's
conformance maps: the sealed operation when the mapping names an intrinsic, and an ordinary static
call to the provider's own function when it names one, with each operand passed by shared borrow.
The redirected call SHALL be reachable from instance discovery even though no ordinary call names
it, SHALL remain fully static — no requirement row, no provider slot, and no runtime dispatch — and
SHALL NOT consume either operand.

An operand read that serves only such a call SHALL be permitted to observe a non-`Copy` place: a
place read whose value is never accessed as an owner and is borrowed shared claims nothing, so it
can be neither moved, dropped, nor written through.

#### Scenario: Specialize a two-operation bound at a user struct

- **WHEN** a user struct maps both operations of an interface declaring `add` and `lessThan`, and a generic bounded by it is specialized at that struct
- **THEN** the specialization is admitted and each operator reaches the struct's own mapped function

#### Scenario: Keep a scalar argument on its compiler-known operation

- **WHEN** the same generic is specialized at `i32`, whose conformance maps the operation to an intrinsic
- **THEN** the operator lowers to the compiler-known operation with no call to source

#### Scenario: Order a move-only element type

- **WHEN** a bound-typed operand is an element type that owns an allocation and is therefore never `Copy`
- **THEN** the comparison borrows the element rather than moving it, and the element is neither duplicated nor released twice

#### Scenario: Reject an incomplete witness at the specialization

- **WHEN** a user type's conformance maps only some of the bound's operations and a call specializes at it
- **THEN** the call is rejected naming each operation the type does not implement
