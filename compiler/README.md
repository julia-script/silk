# Self-hosted Silk frontend

This directory contains the self-hosted lexer and parser. The current executable reads one Silk
file and prints its flat AST and syntax diagnostics. It does not yet perform name resolution,
type checking, lowering, or code generation on that input. The TypeScript bootstrap compiler
still builds this executable.

## Inspect a source file

From the repository root:

```sh
pnpm exec silk run --manifest-path compiler/silk.toml -- compiler/src/main.silk
```

Pass one normalized relative path beneath the working directory. Absolute paths and `.`/`..`
components are rejected. File, allocation, host-input, and writer failures propagate from `main`.

The dump starts with `root #<id>`, then lists nodes in array order. Each node reports its kind and
half-open byte span. Indented lines contain child node IDs, token IDs with their kinds and spans,
or missing-token placeholders. The final section lists syntax diagnostics. Lexical diagnostics
are printed alongside invalid tokens. IDs belong to this tree only.

After a successful build, running the executable directly avoids rebuilding it. For example, on
Apple Silicon with the debug LLVM target:

```sh
compiler/build/llvm/aarch64-apple-darwin/debug/silk-compiler compiler/src/main.silk
```

## Representation and recovery

`SyntaxTree` owns the source `Bytes`, the complete token vector (including trivia), a flat node
vector, the root ID, and syntax diagnostics. A `NodeId` is exactly an index into that node vector.
Children are appended before parents; the source-file root is last. Nodes contain only their
kind, span, and direct `Element` vector. No child node or token payload is copied into a parent.

The parser reads significant tokens but retains trivia in the original token vector. Source bytes
allow contextual identifiers such as `where`, `with`, and `operator` to remain ordinary identifiers
elsewhere. `Parser.parse` takes ownership of both the bytes and their corresponding token vector;
those inputs must describe the same file.

Missing tokens produce zero-width placeholders and diagnostics. Unexpected source is retained in
error nodes. List and block loops enforce cursor progress, and recursive rules have a stack-safety
bound with iterative recovery. A malformed child does not invalidate valid siblings. Syntax
problems are result values, not effect failures; allocation failure is still an effect failure.
The AST inspection executable intentionally prints the recovered result instead of stopping at
its first syntax diagnostic. A future compilation driver can stop on that diagnostic.

CST construction remains postponed. The token vector retains source information, but there is no
second concrete-syntax tree to maintain.

## Grammar modules

| Module                                         | Responsibility                                                                                                                              |
| ---------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- |
| `src/lexer/`                                   | Pull-based byte scanner, tokens, spans, and lexical diagnostics                                                                             |
| `parser/ParseState.silk`                       | Ownership-threaded cursor, node construction, contextual spelling, and recovery primitives                                                  |
| `parser/Parser.silk`                           | Source-file loop and final tree assembly                                                                                                    |
| `parser/Import.silk`                           | Module paths, aliases, selected members, and public imports                                                                                 |
| `parser/Declaration.silk`                      | Nominal declarations, functions, constants, parameters, services/interfaces, implementations, native headers, and static declaration groups |
| `parser/Type.silk`                             | Type paths, generics, lifetimes, references, arrays, pointers, callable types, effect rows, and constraints                                 |
| `parser/Expression.silk`                       | Literals, constructors, calls, projections, prefix/infix precedence, pipelines, effects, anonymous callables, and matches                   |
| `parser/Pattern.silk`                          | Nominal/applied patterns, whole-value bindings, field shorthand/rest, enum/integer cases, and wildcards                                     |
| `parser/Statement.silk`                        | Bindings, assignments, control flow, transfers, static loops, unsafe blocks, and block recovery                                             |
| `parser/Property.silk`                         | Sealed function/module property syntax                                                                                                      |
| `parser/Grammar.silk`, `parser/Lookahead.silk` | Shared boundaries, precedence, and non-consuming ambiguity checks                                                                           |
| `parser/SyntaxTree.silk`                       | Tree ownership and flat AST printing                                                                                                        |

Grammar rules are ordinary Silk functions. They consume `State` and return it with either an
unfinished element list or a completed node ID. Replacement values are evaluated before assigning
them back to a local owner, as required by Silk's ownership rules.

## Verification

Check the self-hosted program with the bootstrap compiler:

```sh
pnpm exec silk check --manifest-path compiler/silk.toml
```

After building the executable, run the parser corpus with its path:

```sh
node compiler/scripts/test-parser.mjs compiler/build/llvm/aarch64-apple-darwin/debug/silk-compiler
```

The harness reuses one built executable. It compares significant AST structure with the bootstrap
parser across grammar fixtures, the self-hosted sources, and the standard library. It also checks
postorder IDs, source spans, reachability, unique token ownership, and preservation of following
declarations after malformed syntax. Extra file paths after the executable select a smaller corpus.
The JavaScript harness imports the bootstrap package's built `dist` modules.

`fixtures/parser/` contains syntax-only programs: names need not resolve and operations need not
typecheck. `recovery.silk` deliberately contains syntax errors. The other files directly under
`fixtures/` exercise lexical categories and are not necessarily complete Silk programs.

Bootstrap issues encountered while developing this frontend are recorded in [BUGS.md](BUGS.md).
