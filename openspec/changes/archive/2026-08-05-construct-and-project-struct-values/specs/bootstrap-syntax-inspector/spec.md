## ADDED Requirements

### Requirement: Inspect struct values in the unified workbench

The unified `/labs` workbench SHALL present struct literal syntax, source-to-canonical field
mappings, projection chains, typed aggregate HIR, whole-value ownership and cleanup, runtime layout
and calling shapes, aggregate MIR, evaluation events, and native and WebAssembly artifacts through
facade queries. Every graphical aggregate relationship SHALL have an accessible text equivalent and
all source, pane, selection, and evaluation state SHALL remain browser-local.

#### Scenario: Inspect reordered construction end to end

- **WHEN** a preset constructs a struct with reordered fields and projects one field
- **THEN** the workbench shows source order beside canonical field order and links the projected value through HIR, MIR, evaluation, and emission

#### Scenario: Inspect a nested aggregate call

- **WHEN** a public factory returns a nested struct through an internal call
- **THEN** the workbench links canonical types, field paths, calling-shape lanes, symbols, and the final projected result across panes

#### Scenario: Inspect a whole-value move

- **WHEN** a preset moves a struct binding and then attempts to reuse it
- **THEN** ownership and diagnostics panes show the transfer, source liveness end, cleanup owner, and exact use-after-move cause

#### Scenario: Inspect invalid construction and projection

- **WHEN** presets contain external raw construction, missing or duplicate fields, a mistyped initializer, an unknown projection, a private projection, or a partial move
- **THEN** each retained syntax and semantic fact remains visible beside its phase-owned cause without a fabricated successful aggregate path

### Requirement: Struct-value presets cover the complete slice

Browser-local presets SHALL cover empty, scalar, reordered, nested, cross-module factory, chained
projection, whole move, use after move, external literal refusal, missing, duplicate, unknown,
mistyped, private, and partial-move cases on native and WebAssembly targets where applicable.

#### Scenario: Reload after aggregate inspection

- **WHEN** the workbench reloads after editing, selecting, evaluating, or emitting an aggregate preset
- **THEN** it returns to its canonical browser-local state without persisting source or derived compiler facts
