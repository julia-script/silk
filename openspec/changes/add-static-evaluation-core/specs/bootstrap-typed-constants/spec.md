## MODIFIED Requirements

### Requirement: Constants have explicit primitive contracts

A top-level constant SHALL declare a name, one concrete primitive type, and one statically evaluable
initializer. Accepted types SHALL include `bool`, every supported integer primitive including
`usize`, every supported floating primitive, and `string`. The initializer MUST produce exactly the
declared type for the selected target without a runtime parameter, ordinary function call, Effect,
service requirement, borrow, unsafe operation, or observable allocation. A `string` constant SHALL
accept an escaped or raw text literal in either delimiter width and SHALL reject a byte-string
literal. Type inference and aggregate constants SHALL remain unavailable.

Constant initialization SHALL use the ordinary static evaluator. Target-dependent constant values
SHALL resolve through an imported ordinary standard-library target declaration over the sealed
static target-profile intrinsic; the compiler MUST NOT recognize `Target` or any member spelling as
a special initializer form. A failed static evaluation, wrong result type, or selected-target range
failure SHALL expose no usable constant value and SHALL retain the initializer's static diagnostic.

Declaration indexing and target-neutral module surfaces SHALL retain a constant's explicit declared
type, initializer body template, and source provenance without publishing a target-selected value.
After the concrete target has been selected, the target realization SHALL evaluate that template and
publish exactly one canonical selected constant value for residual HIR, cross-module references, and
semantic inspection. No runtime backend SHALL select or replace the value again.

#### Scenario: Declare representative scalar constants

- **WHEN** a module declares boolean, `u8`, `i32`, `usize`, `f32`, and `f64` constants with fitting literals
- **THEN** every declaration records its exact primitive type and canonical static value

#### Scenario: Declare a string constant from either literal form

- **WHEN** a module declares `string` constants initialized by escaped and raw text literals that decode to the same content
- **THEN** both declarations record the type `string` and the identical canonical static text value

#### Scenario: Compute one primitive constant statically

- **WHEN** a constant initializer calls a static function with literals and the function returns the declared primitive type
- **THEN** the declaration records the computed value with no runtime initializer, storage, call, or cleanup

#### Scenario: Derive a target-width constant through ordinary source

- **WHEN** the standard library initializes a `usize` constant from its ordinary imported target fact on a 32-bit and a 64-bit compilation
- **THEN** each compilation records its selected value through the static evaluator and no syntax-only target selector or backend-specific selection remains

#### Scenario: Keep declaration surfaces target neutral

- **WHEN** a public constant initializer depends on the selected target
- **THEN** its declaration and module surface retain the declared type, initializer template, and provenance while each concrete target realization publishes its own selected canonical value

#### Scenario: Reject a mismatched or overflowing initializer

- **WHEN** an initializer produces the wrong primitive type or exceeds the declared type on the selected target
- **THEN** semantic analysis reports the declaration-local mismatch and exposes no usable constant value

#### Scenario: Report a non-literal initializer as such

- **WHEN** a non-literal initializer calls an ordinary function, requires an Effect, or otherwise depends on runtime work
- **THEN** semantic analysis reports the initializer-local static-phase violation and exposes no usable constant value

#### Scenario: Keep aggregates and inference unavailable

- **WHEN** a constant omits its declared type or its initializer produces a struct, array, union, or other aggregate
- **THEN** analysis rejects the declaration without publishing a partially evaluated constant
