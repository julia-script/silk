# Design — add-wasm-exceptions-branch-hints

## Context

Third change in the `@silk-lang/wasm` series (see the archived core and bulk-instruction
changes). The extension points this change relies on are established: `ValType` is a tagged
union (`exnref` slots in as a `RefType` variant), section framing is generic (tag section is id
13), all instruction dispatchers carry `satisfies never` exhaustiveness guards, and byte
stability of existing fixtures is the regression gate. See proposal.md for scope.

## Goals / Non-Goals

**Goals:**

- Tags as a fifth importable/exportable entity kind following every existing handle pattern
  (owner-checked, imports-first numbering, optional unique names).
- `try_table` as one more structured `Instr` variant — nested body plus an immediate list of
  catch-clause values.
- Branch hints as data on `BrIf`/`If` variants, with byte offsets computed only inside the
  binary encoder.

**Non-Goals:**

- No legacy exception handling (`try`/`catch`/`delegate`) — permanently excluded.
- No JS-exception interop concerns; tags and payloads are the consumer's contract.
- No hint kinds beyond `likely`/`unlikely`, and no hints on instructions the proposal does not
  cover.

## Decisions

### 1. Tags reuse the entity pattern wholesale

`Tag.make(builder, type, options)` + `Import.tag` + `Export.tag`, a `tags` entry array in
module state with `importSource`, `IndexSpace` resolution identical to funcs, and name-section
subsection 11. The only tag-specific rule — the referenced function type must have empty
results — is checked at declaration. Rationale: nothing about tags warrants novelty; the fifth
copy of a proven pattern is the safest code in the package.

### 2. `try_table` is a structured variant with catch clauses as data

```
{ _tag: 'TryTable', blockType, catches: ReadonlyArray<Catch>, body: ReadonlyArray<Instr> }
Catch = { _tag: 'Catch' | 'CatchRef', tag: Tag, depth: number }
      | { _tag: 'CatchAll' | 'CatchAllRef', depth: number }
```

Validation pushes the block frame exactly like `block`, then checks each clause's target label
against the types the clause delivers (tag params, plus `exnref` for the `_ref` forms).
Encoding is the `0x1F` opcode, block type, clause vector, body, `end`. Alternative — modeling
catches as trailing pseudo-instructions like the legacy proposal — rejected; the standardized
design is immediate data and maps directly onto our nested-data convention.

### 3. `exnref` joins `RefType`

One new variant (`{ _tag: 'ExnRef' }`, binary `0x69`, text `exnref`). Existing generic code
(select typing, ref.null, locals, globals, drops) picks it up through `ValType.isRefType`.
`ref.null exn` heap type follows the same keyword table as `func`/`extern`.

### 4. Hints live on the instruction; offsets live in the encoder

`Instr.brIf(depth, { hint: 'likely' })` and `Instr.ifElse(..., { hint })` store an optional
field. The binary encoder already walks bodies in order; it records the byte offset (relative
to the function body start, per the branch-hinting spec) of each hinted instruction as it
writes it, then emits one `metadata.code.branch_hint` custom section immediately before the
code section — which requires encoding code section bodies before the section that precedes
them; the encoder buffers per-function body bytes first, then writes sections in order.
Rationale: offsets are an emission artifact and must never appear in the data model.
Alternative — a post-pass re-walking the encoded bytes — rejected as a second source of truth
for instruction sizes.

### 5. Text uses standard annotations

Hinted instructions render as `(@metadata.code.branch_hint "\00")`/`"\01"` annotations
preceding the instruction, which the pinned `wasm-tools` parses back into the custom section.
The round-trip oracle check arbitrates exact placement and encoding, as it did for the name
section and data count.

## Risks / Trade-offs

- [Buffering code bodies to place the hint section before the code section reorders encoder
  internals] → bodies were already sized via a nested writer; the change is lifting that buffer
  one level, verified by byte-stable existing fixtures.
- [wasm-tools annotation syntax details (quoting, placement on folded/unfolded forms) may
  differ from first guess] → same empirical loop as prior changes: the round-trip check
  arbitrates; text forms adjust to what parses to our bytes.
- [Catch-clause typing has subtle interplay with branch label types] → clauses reuse the
  existing `labelTypes` machinery; negative corpus gets one case per clause kind.

## Open Questions

- Whether `Instr.tryTable` takes catches before or after the body in its parameter order is a
  pure API-ergonomics call, decidable at implementation time.
