## Context

First widening change after the realignment: `let` bindings and `move` per issues 01/08, riding
the unchanged spine. Every layer already models what this change needs (MIR `Drop`, cleanup
blocks, ownership fact table, binding trace events); the work is making the grammar and phases
produce real instances of them.

## Goals / Non-Goals

- Goals: statement sequences, `let`, `move`, inference from initializers, non-shadowing
  resolution, real liveness ranges, use-after-move enforcement, populated cleanup plans, lowered
  drops, end-to-end parity (interpreter and native), labs showing all of it.
- Non-goals: mutability (`mut`), borrows, named scopes, arithmetic, `Bool`/`if`, non-copyable
  types, block expressions, shadowing (rejected, not deferred — issue 04 pins non-shadowing).

## Decisions

### `move` of a copyable binding consumes it

Issue 01 pins affine consumption for non-copy values and leaves copyable moves unspecified.
Decision: an explicit `move` is a consuming use regardless of copyability — the binding's
liveness ends at the move, and any later use is an `OWN0001` violation. Rationale: the
programmer wrote an explicit transfer, honoring it makes ownership checking real in this change
(the only types are copyable), and it is the conservative choice — relaxing later accepts more
programs, the reverse breaks them. Ordinary bare-name reads of copyable bindings copy and never
consume. Rejected: `move`-as-no-op (keeps the checker vacuous until structs); rejecting `move`
on copyables (bans the only way to exercise the machinery this slice exists to prove).

### Cleanup plans record every ownership end, uniformly

A release entry records where a binding's ownership ends at an exit, whether or not the type
carries cleanup behavior; lowering emits one generated `Drop` per release. For `I32` these drops
are semantically no-ops (the interpreter records them, the backend ignores them — both already
do). Rationale: one shape for the plan and lowering now and when cleanup-bearing types arrive;
the visible artifact is the point of this slice. Rejected: releasing only cleanup-bearing types
— plans and drops would stay empty until structs, failing the change's purpose.

### Ownership diagnostics get their own phase

New `'ownership'` phase (rank 4, after `'semantic'`) and code `OWN0001` (use after move) with
reason data carrying the consuming move's span. A violated function keeps its published facts
(timeline stays inspectable) but gets a `Violation` verdict arm carrying the diagnostic
identity; lowering treats violations like unavailable bodies — generated trap. Rejected:
reporting through the semantic phase (phases never lie about origin; the driver report and merge
order treat ownership as its own phase already).

### Statements in HIR and MIR

HIR bodies become `ReadonlyArray<Statement>`: `Bind{binding, initializer}` and
`Return{expression}`; expressions gain `BindingReference{binding}` and `Move{binding}` with a
function-local binding identity (statement ordinal). Elaboration walks statements in order,
carrying a scope map of parameters and completed bindings; inference is initializer type, never
forward. MIR lowering assigns each binding one typed local after the parameter locals; drops
lower from the plan in release order before each exit terminator. New codes: `SEM0008`
(rebinding a name, related span = original), `OWN0001`. `SEM0006` prose generalizes from
"unknown parameter" to unknown value name; code and shape unchanged.

## Risks / Trade-offs

- Dead drops for copyable types cost MIR/trace noise → they are generated-marked and cheap;
  ponytail ceiling noted in lowering.
- The `Violation` verdict is a third arm next to `Satisfied`/`Unavailable` → keeps
  "unavailable ≠ wrong" honest; consumers already switch on tags.
- Grammar recovery around `let` inside blocks is new territory → bounded by the same
  statement-boundary anchors (`let`, `return`, `}`) the function boundary already uses.

## Migration Plan

Purely additive grammar; every existing program parses and means the same. Encoders gain nodes —
existing goldens stay byte-identical except where fixtures deliberately grow.

## Open Questions

None — both proposal-level questions (copyable moves, empty plans) are decided above.
