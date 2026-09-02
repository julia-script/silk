## Purpose

Define the transparent `type` declaration: a module-level name for an existing type that is erased
at resolution and never creates a distinct canonical type.

## ADDED Requirements

### Requirement: A type alias names an existing type transparently

`[pub] type Name = <type>` SHALL declare one module-level name for the canonical type its target
spells. The alias SHALL NOT create a new canonical type, nominal identity, representation, or
conversion. Every type position that names the alias SHALL resolve to the target's canonical type,
and two aliases with equal targets SHALL be the same type. Canonical type identity, keys, encoded
spellings, module surfaces, and diagnostics SHALL present the erased target and SHALL NOT retain
the alias name. An alias SHALL accept any type admissible at the position where the alias is later
used; admissibility SHALL be judged on the erased target at each use, not at the declaration.

#### Scenario: Two scalar aliases are interchangeable

- **WHEN** a module declares `type Meters = u32` and `type Kilograms = u32` and passes a `Kilograms` value to a `Meters` parameter
- **THEN** analysis accepts the call and both parameters have the canonical type `u32`

#### Scenario: Alias a generic application

- **WHEN** a module declares `struct Point<T> { x: T y: T }` and `type PointF32 = Point<f32>` and a field is typed `PointF32`
- **THEN** the field has the canonical nominal type `Point<f32>` and constructs and projects exactly as that spelling would

#### Scenario: Alias a structural union

- **WHEN** a module declares `type Shape = Circle | Square` and a function returns `Shape`
- **THEN** the function's canonical result is the normalized union `Circle | Square`, and a `Circle` value injects into it at the return boundary

#### Scenario: Diagnostics present the erased target

- **WHEN** a `Shape` parameter receives a `Triangle` argument
- **THEN** the mismatch diagnostic names `Circle | Square` and does not name `Shape`

### Requirement: Aliases resolve through aliases and reject cycles

An alias target MAY name another alias, and resolution SHALL erase every alias in the chain to one
canonical type independent of declaration order. A chain that reaches its own declaration SHALL be
rejected: each alias on the cycle SHALL report one declaration-local cyclic-alias diagnostic that
relates the other declarations on the cycle, and every alias on the cycle SHALL publish no available
type. Aliases outside the cycle SHALL remain available.

#### Scenario: Resolve an alias declared after its use

- **WHEN** `type Outer = Inner` precedes `type Inner = i32` in one module
- **THEN** both names resolve to `i32`

#### Scenario: Reject a two-alias cycle

- **WHEN** a module declares `type A = B` and `type B = A`
- **THEN** analysis reports one cyclic-alias diagnostic at each declaration, relates the other, and publishes neither type

#### Scenario: A self-referential alias through a union is still a cycle

- **WHEN** a module declares `type Tree = Leaf | Tree`
- **THEN** analysis reports the cyclic-alias diagnostic and publishes no type for `Tree`

### Requirement: Alias visibility and exposure follow declaration rules

A `pub` alias SHALL be resolvable from other modules through selected and namespace-qualified
imports exactly as a public nominal type is, and a private alias SHALL be inaccessible outside its
module while retaining the inaccessible candidate. A `pub` alias whose erased target exposes a
private nominal type SHALL be rejected with the ordinary exposure diagnostic at the alias
declaration, and dependents SHALL observe no available type. An alias name SHALL occupy the module's
flat top-level namespace and SHALL collide with any other declaration of the same name.

#### Scenario: Import a public alias

- **WHEN** module `net` declares `pub type FetchError = HttpError | JsonError` and another module imports `net { FetchError }` into a failure row
- **THEN** the importing module resolves the identical canonical union that `net` resolves

#### Scenario: Refuse a private alias

- **WHEN** a module qualifies a private alias from another module
- **THEN** lookup retains the inaccessible identity and reports one visibility diagnostic without resolving a usable type

#### Scenario: Reject exposure of a private type

- **WHEN** a module declares `struct Hidden {}` and `pub type Leaked = Hidden`
- **THEN** analysis reports the exposure diagnostic at the alias declaration and publishes no type for `Leaked`

#### Scenario: Reject a name collision

- **WHEN** a module declares `struct Token {}` and `type Token = i32`
- **THEN** analysis reports the later declaration as a collision with the first

### Requirement: Aliases declare no type parameters

A type alias declaration SHALL NOT accept a type-parameter list. A declaration such as
`type Pair<T> = Point<T>` SHALL be rejected with one diagnostic naming the restriction at the
parameter list, and the alias SHALL publish no available type. A type-parameter list on the alias
target, such as `Point<f32>`, SHALL remain accepted.

#### Scenario: Reject a parameterized alias

- **WHEN** a module declares `type Pair<T> = Point<T>`
- **THEN** analysis reports the parameter restriction at `<T>` and publishes no type for `Pair`

#### Scenario: Accept an applied target

- **WHEN** a module declares `type Pair = Point<i32>`
- **THEN** the alias resolves to `Point<i32>`
