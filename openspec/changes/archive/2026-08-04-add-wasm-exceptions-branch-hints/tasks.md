# Tasks — add-wasm-exceptions-branch-hints

## 1. Tags and exnref

- [x] 1.1 Add `exnref` to `ValType` (`RefType` variant, binary byte, text keyword, heap-type
      text) and thread it through generic reference handling
- [x] 1.2 Add the `Tag` actor module (declare with empty-result check, name, handle kind),
      `Import.tag`, `Export.tag`, module-state entries, and index-space resolution
- [x] 1.3 Add the `./Tag` subpath export and barrel entry; declaration and rejection tests

## 2. Exception instructions

- [x] 2.1 Add `Throw`, `ThrowRef`, and `TryTable` (block type, catch-clause data, nested body)
      variants with constructors and table opcodes
- [x] 2.2 Validate throw against tag parameters, throw_ref against `exnref`, and each catch
      clause's target label against the types the clause delivers; unit tests per clause kind
- [x] 2.3 Encode the tag section (id 13), tag import/export descriptors, `try_table`/`throw`/
      `throw_ref`, and tag names (subsection 11); render the text forms

## 3. Branch hints

- [x] 3.1 Add optional `hint` to `BrIf` and `If` constructors and variants
- [x] 3.2 Track hinted-instruction byte offsets during code encoding and emit the
      `metadata.code.branch_hint` custom section before the code section only when hints exist
- [x] 3.3 Render `(@metadata.code.branch_hint …)` annotations in text; round-trip verified

## 4. Parity and release

- [x] 4.1 Extend oracle features with exceptions; new `exceptions` and `branch-hints` fixtures;
      exhaustive fixture picks up new table rows; byte-stability of existing fixtures
- [x] 4.2 Negative corpus: tag with results, catch arity mismatch, throw_ref on non-exnref,
      throw with missing operands
- [x] 4.3 Update README baseline and UPSTREAM feature list; JSDoc on new public surface;
      changeset
