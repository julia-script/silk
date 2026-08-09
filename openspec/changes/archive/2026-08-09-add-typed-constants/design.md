## Context

Silk's top-level namespace currently contains function and struct facts, while expression names
resolve either local values or callable declarations. The existing scalar-literal machinery already
owns exact integer parsing, contextual type selection, float canonicalization, target-aware `usize`
validation, HIR/MIR literals, and immediate backend emission. The design should reuse that path and
must not create a second runtime-global model merely to name literals.

## Goals / Non-Goals

**Goals:**

- Add one source-retained declaration kind that is visible to modules and editor tooling.
- Reuse existing scalar validation and immediate lowering so constants remain pay-for-use names.
- Keep declaration collection order-independent and all published artifacts deterministic.
- Exercise the feature in the lexer and VM rather than accepting syntax-only evidence.

**Non-Goals:**

- General compile-time evaluation, constant functions, or arithmetic in initializers.
- Aggregate, string, callable, Effect, reference, or affine constant values.
- Addressable globals, static mutable state, initialization ordering, linkage sections, or ABI symbols.
- Inferred declaration types or style rules requiring uppercase names.

## Decisions

### Use `const name: type = literal` as a top-level declaration

The explicit type is part of the contract, while the initializer is deliberately limited to one
boolean, signed/unsigned integer, or floating literal. `pub` uses the same visibility meaning as
other declarations. This gives the pressure corpus useful names immediately and keeps validation
finite. Inferring from the literal was rejected because exact integer literals intentionally defer
their type today; general constant expressions were rejected because they would require a
compile-time evaluator and dependency-cycle model before the examples need either.

### Store a typed literal header in the declaration index

`ConstantFact` joins the existing member union and flat namespace. It retains declared-type and
initializer syntax plus a resolved scalar value when available. Collection establishes identity;
header resolution validates the declared primitive and literal using existing scalar rules. This
keeps forward references and import cycles order-independent. Treating constants as hidden
zero-argument functions was rejected because it would make callable and Effect facts dishonest and
would obscure navigation and diagnostics.

### Substitute a constant use during elaboration

Name resolution records a canonical occurrence binding, then elaboration emits the existing typed
HIR literal shape. MIR and both backends therefore need no constant-specific instruction, symbol,
storage, or cleanup path. Semantic facts and presentation retain the constant declaration and use
even though executable IR contains only the immediate. Emitting globals was rejected because it
would add addresses, initialization, linkage, and loads that the requested feature does not need.

### Keep source tooling declaration-aware

The formatter gains a constant branch; declaration presentation, hover, definitions, and
occurrences use the same indexed identity as functions and structs. Inspector artifacts show the
fact and the resulting ordinary literal HIR/MIR. This preserves the user's ability to go to the
named source even though runtime lowering is intentionally erased.

### Prove both local semantics and module boundaries

Focused tests cover lossless syntax and recovery, duplicate/collision behavior, literal range and
kind failures, local/selected/qualified resolution, visibility, editor navigation, all three
engines, and fresh-process determinism. The lexer and VM then replace only representative repeated
literals; their established oracles and allocation sweeps remain the regression contract.

## Risks / Trade-offs

- [The literal-only surface may soon feel narrow] → Keep its syntax and fact model extensible, but
  require a pressure example before adding constant expressions or aggregates.
- [Inlining erases the declaration from executable IR] → Preserve canonical occurrences and
  declaration facts in semantic/tooling artifacts; runtime debuggability of constants is not
  promised in this slice.
- [`usize` validity depends on the target] → Validate through the existing target-aware scalar path
  and never cache one target's accepted value as a universal runtime global.
- [Adding a third member kind widens many exhaustive switches] → Let strict TypeScript expose every
  affected phase and keep function-only APIs explicitly filtered rather than weakening the union.

## Migration Plan

Add the token, CST, declaration fact, resolution, elaboration, formatting, and tooling paths behind
ordinary compiler tests, then update representative pressure source. The project is unreleased, so
no compatibility bridge is required. Rollback is the change commit plus the pressure-source edits;
no persisted data or generated ABI requires migration.
