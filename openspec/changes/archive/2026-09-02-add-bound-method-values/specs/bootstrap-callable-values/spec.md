## ADDED Requirements

### Requirement: Bound method values are receiver sections

`value.member` naming an inherent receiver method SHALL construct a section whose single capture is
parameter zero and whose remaining parameters are the member's parameters one onward, in declared
order. Every rule of "Sections capture every ownership mode" SHALL apply to that capture: a shared
loan constrains the callable's lifetime, an exclusive loan additionally requires exclusive
invocation, and a moved affine receiver makes the callable take-once and is dropped exactly once if
the callable is never invoked. Application SHALL order operands by parameter ordinal: captures at
their declared ordinal, supplied arguments filling the remaining ordinals in order. Bound method
values SHALL monomorphize, lower, and execute exactly as trailing sections of the same member do on
the evaluator and on every compiled backend.

#### Scenario: Apply a bound method

- **WHEN** `let plusForty = counter.add` is applied as `plusForty(2)` with `fn add(self: &Self, adjustment: i32) -> i32`
- **THEN** the invocation calls `Counter.add(&counter, 2)` on the evaluator, LLVM, and Wasm, with the supplied argument placed after the captured receiver

#### Scenario: Bind a receiver-only method

- **WHEN** `let reader = counter.read` is applied as `reader()` with `fn read(self: &Self) -> i32`
- **THEN** the application supplies the captured loan as the only operand and produces the same result as `counter.read()`

#### Scenario: Drop an uninvoked bound receiver

- **WHEN** `let taker = token.take` moves an affine `token` and `taker` leaves its region uninvoked
- **THEN** the callable environment drops the captured token exactly once
