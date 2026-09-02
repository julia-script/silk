## MODIFIED Requirements

### Requirement: Associated members order owner binders before local binders

The canonical generic sequence of an associated member SHALL be the owner's binders that the
member mentions (all of them when the member names `Self`), in declaration order, followed by the
member's own binders. An owner binder the member never mentions is not part of its sequence, so a
member such as `Fiber.cancel(canceller: CompletionCanceller)` is callable without naming an
instantiation. A bare qualifier `Owner.member<Args>` SHALL bind an explicit prefix over that
complete sequence and infer the rest from supplied arguments exactly as an ordinary generic call
does. An applied qualifier `Owner<Args>.member<Locals>` SHALL fix the owner binders from the
applied arguments and bind the explicit locals over the member's own binders only. Owner binders
fixed by the qualifier and by an explicit prefix or by argument evidence SHALL agree, and a
disagreement SHALL be reported at the responsible argument.

#### Scenario: Supply owner and local binders through a bare qualifier

- **WHEN** source calls `Option.map<i32, i64>(move value, widen)` against `impl<T> Option<T> { fn map<U>(self: Self, transform: once fn(T) -> U) -> Option<U> }`
- **THEN** `T = i32` and `U = i64` bind as one explicit prefix and the call type-checks

#### Scenario: Infer every binder from arguments

- **WHEN** source calls `Option.map(move value, widen)` with `value: Option<i32>` and `widen: fn(i32) -> i64`
- **THEN** `T` and `U` are inferred without explicit arguments

#### Scenario: Call a member that mentions no owner binder

- **WHEN** source calls `Fiber.cancel(move canceller)` against `impl<A, E> Fiber<A, E> { pub fn cancel(canceller: CompletionCanceller) -> () }`
- **THEN** the call type-checks with no type arguments and `Fiber.cancel<i32, Problem>(...)` is rejected as too many arguments

#### Scenario: Pre-bind owner binders through an applied qualifier

- **WHEN** source calls `Option<i32>.map<i64>(move value, widen)`
- **THEN** `T` is fixed to `i32` by the qualifier, `U` binds to `i64` from the local list, and a local list longer than one argument is rejected

#### Scenario: Reject disagreeing owner evidence

- **WHEN** source calls `Option<i32>.map(move value, widen)` with `value: Option<u8>`
- **THEN** the call reports the disagreement at the applied owner argument rather than silently preferring either binding
