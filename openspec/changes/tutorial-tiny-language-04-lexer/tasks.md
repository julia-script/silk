## 1. Lexer implementation

- [x] 1.1 Add immutable `Token` data with string-literal kinds, lexemes, and UTF-16 start/end offsets.
- [x] 1.2 Implement `Lexer.tokenize` as a named Effect operation for whitespace, decimal integers, identifiers/keywords, punctuation, operators, and EOF.
- [x] 1.3 Add a typed lexical diagnostic and guarantee that every scan branch advances or fails.

## 2. Tests and lesson

- [x] 2.1 Add exact token tests for the first Tiny function, keyword classification, whitespace, EOF, and invalid input.
- [x] 2.2 Create Lesson 4 with the guided scanning loop, expected token output, verification checkpoint, and cursor/span recovery walkthrough.
- [x] 2.3 Link Lesson 4 from the series index and neighboring lessons.

## 3. Verification

- [x] 3.1 Run the consumer example typecheck and lexer tests against the packed package.
- [x] 3.2 Run Biome on the new example and documentation files.
- [x] 3.3 Run `pnpm check` and `pnpm release:candidate`, reporting any pre-existing failure separately.
