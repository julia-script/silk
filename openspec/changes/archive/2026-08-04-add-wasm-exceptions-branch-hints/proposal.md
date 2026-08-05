# Add exception handling and branch hinting to `@silk-effect/wasm`

## Why

Exception handling with `exnref` is the standardized (Wasm 3.0) way to represent unwinding, and
it is the last remaining Chrome-baseline feature apart from GC. Branch hinting is a small,
standardized custom-section feature that is emitter-coupled — its section records instruction
byte offsets — so it lands together with the exception work that already touches the encoder's
instruction bookkeeping.

## What Changes

- **Tags**: a new index space for exception tags. A tag references an interned function type
  whose results MUST be empty. Tags can be defined, imported, and exported, carry optional
  names (name-section subsection 11), and follow the imports-first index numbering of every
  other space.
- **`exnref`**: a new reference type usable in blocks, locals, globals, selects, and drops.
- **Instructions**: `throw` (tag), `throw_ref`, and `try_table` — a structured block carrying an
  immediate list of catch clauses (`catch`, `catch_ref`, `catch_all`, `catch_all_ref`), each
  targeting an enclosing label by relative depth with the label types the clause implies.
- **Branch hinting**: an optional `likely`/`unlikely` hint on `br_if` and `if` instructions,
  emitted as the `metadata.code.branch_hint` custom section (function index + instruction byte
  offset + hint byte) and as `(@metadata.code.branch_hint "…")` annotations in text.
- **Permanently excluded**: the legacy `try`/`catch`/`catch_all`/`delegate` instructions.
- Parity: oracle features gain `exceptions` and branch-hint handling; new fixtures and negative
  corpus entries; existing fixtures remain byte-identical.

## Capabilities

### New Capabilities

None — all changes extend existing capabilities.

### Modified Capabilities

- `wasm-module-declarations`: tag declarations, imports, and exports join the entity kinds.
- `wasm-function-bodies`: `throw`/`throw_ref`/`try_table` typing (including catch-clause label
  arities) and branch-hint placement validation.
- `wasm-output`: tag section and import/export descriptors, `try_table` encoding, `exnref`
  encoding, tag names subsection, and the branch-hint custom section with byte-offset tracking;
  text renderings including catch clauses and hint annotations.
- `wasm-builder-parity`: oracle feature list gains exceptions; fixture inventory gains
  exception and branch-hint modules; negative corpus covers the new rules.

## Impact

- `packages/wasm` only: new `Tag` actor module (new subpath export), `ValType`, `Instr`,
  instruction table, validator, both emitters, name section, fixtures, and scripts.
- The pinned `wasm-tools 1.255.0` oracle supports exceptions and branch-hint annotations; no
  dependency changes.
- Public API grows a `Tag` module and new `Instr` constructors; `ValType.RefType` gains a
  variant (allowed breaking change while unreleased).
