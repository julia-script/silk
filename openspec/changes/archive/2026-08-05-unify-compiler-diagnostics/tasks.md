## 1. Diagnostic concept module

- [x] 1.1 Create `packages/compiler/src/Diagnostic.ts`: the unified `Diagnostic` model — stable code, severity, message, primary span, structured per-code reason data, optional labeled related spans/notes, optional machine-applicable edits (modeled, unpopulated), originating phase literal union, optional semantic entity, optional causal diagnostic identity
- [x] 1.2 Implement deterministic diagnostic identity (phase, code, primary span, ordinal among equals) with an equality/ordering helper
- [x] 1.3 Implement the pure cross-phase merge/sort function ((module identity placeholder), primary span, code, tie-breaker) with unit tests proving stability across repeated runs
- [x] 1.4 Export the module from `packages/compiler/src/index.ts`

## 2. Migrate the lexer

- [x] 2.1 Re-shape lexical diagnostics onto `Diagnostic` with phase `lexical`, preserving existing codes, messages, and spans; delete `LexicalDiagnostic.ts`
- [x] 2.2 Update lexer tests/fixtures; verify within-result ordering (span, code) is unchanged

## 3. Migrate the parser

- [x] 3.1 Re-shape parser diagnostics onto `Diagnostic` with phase `parser`, preserving codes/messages/spans; delete `ParseDiagnostic.ts`
- [x] 3.2 Update parser tests/fixtures; verify the repeat-malformed-parsing determinism scenario still passes

## 4. Migrate semantic analysis

- [x] 4.1 Re-shape semantic diagnostics onto `Diagnostic` with phase `semantic`, preserving codes (incl. `SEM0003`), reason data, and spans; surface duplicate-name `originalSpan` reasons as labeled related spans; delete `SemanticDiagnostic.ts`
- [x] 4.2 Extend existing `Unavailable`/`Absent` sentinel variants with the optional originating diagnostic identity; wire causes where the sentinel already knows its origin (unresolved references, unavailable call contracts)
- [x] 4.3 Add the cause-suppression check: dependent facts with a caused sentinel emit no duplicate diagnostic for the same origin; cover the unavailability-links-to-its-cause scenario
- [x] 4.4 Update semantic tests/fixtures, including the three existing determinism/duplicate scenarios

## 5. Inspector

- [x] 5.1 Replace the existing per-phase `DiagnosticList` sections (syntax-inspector.tsx) with the unified panel: merged driver-order stream, each entry labeled with phase, code, severity, primary span
- [x] 5.2 Implement causal-chain reveal: selecting a caused diagnostic surfaces its originating diagnostic and span
- [x] 5.3 Update inspector tests for the panel and the cross-phase fixture (lexical + parser + semantic mistakes in one source)

## 6. Verification

- [x] 6.1 Run the full compiler and inspector test suites; confirm no phase writes diagnostic text to any output stream
- [x] 6.2 `openspec validate unify-compiler-diagnostics --type change --strict` passes
