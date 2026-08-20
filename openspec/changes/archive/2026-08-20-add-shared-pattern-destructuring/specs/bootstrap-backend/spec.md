## ADDED Requirements

### Requirement: Backends privately realize verified statement-pattern dispatch

Native LLVM and direct WebAssembly emission SHALL realize every verified expression-match and
statement-pattern selection from the compiler-owned MIR member and layout plan. Both backends SHALL
preserve source-ordered selection, retained statement bindings, branch-local borrowed bindings,
move-on-both-outcomes, active-payload cleanup, and structured joins without introducing a distinct
pattern ABI or independently choosing tags.

#### Scenario: Emit shared statement patterns

- **WHEN** one program uses recursive let destructuring and both matching and mismatching if-let selections
- **THEN** native, WebAssembly, and evaluation agree on results, binding visibility, and active-payload cleanup
