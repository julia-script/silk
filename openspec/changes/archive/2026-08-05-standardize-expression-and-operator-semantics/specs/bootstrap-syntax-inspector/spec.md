## ADDED Requirements

### Requirement: Inspect operator and pipeline expressions end to end

The facade-only labs SHALL expose operator tokens, concrete precedence and grouping, semantic
operation and pipeline facts, canonical HIR calls, MIR operations, interpreter outcomes, native and
WebAssembly artifacts, exact source provenance, and phase-owned diagnostics without reconstructing
operator semantics. Presets SHALL cover every precedence level, associativity, grouping, prefix
negation and boolean negation, scalar equality, arithmetic and comparison operators, pipeline
insertion and chaining, imported targets, damaged syntax, mistyped operands, and arithmetic traps.
All state SHALL remain browser-local and every graphical relationship SHALL have an accessible text
equivalent.

#### Scenario: Inspect precedence and canonical lowering

- **WHEN** a developer selects `1 + 2 * 3`
- **THEN** the syntax view shows the nested precedence structure while HIR and MIR show canonical `Multiply` feeding `Add`

#### Scenario: Inspect a pipeline mapping

- **WHEN** a developer selects `2 |> I32.add(3)`
- **THEN** the semantic view links the left expression to parameter zero, the explicit argument to parameter one, and both to one canonical builtin call

#### Scenario: Inspect a damaged operator

- **WHEN** a preset omits an operator operand or grouping parenthesis
- **THEN** the lab retains the missing syntax, parser diagnostic, unavailable dependent fact, and all unrelated facts

#### Scenario: Inspect operator execution parity

- **WHEN** a valid or trapping operator preset is evaluated and emitted
- **THEN** the lab presents the same result or trap provenance beside the shared MIR and target-aware native and WebAssembly artifacts
