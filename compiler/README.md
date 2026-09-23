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

## Lowered representation

`hir/Hir.silk` defines the target of lowering: a `Module` is one postorder node arena where a
`HirId` is exactly an index and children precede parents. A node owns no heap vector — single
children are `HirId`, optional children are `HirRef`, and lists are `Range` runs into one shared
child vector. Every byte string is interned through `hir/Intern.silk`, so a module holds no source
bytes and no `SyntaxTree`; `spans` and `nodeCauses` run parallel to `nodes` and keep coordinates
only. Every tagged record of the bootstrap `AuthoredHir` has a counterpart variant; the module doc
comment lists each deliberate difference.

`Hir.write` dumps a module as indented text and `Hir.render` produces the same bytes in an owned
buffer, so a test can compare a lowered module against expected text. `Hir.verify` reports the
arena invariants a module violates rather than asserting them, which replaces the bootstrap's
structured-clone publication step.

`hir/Draft.silk` owns the module while it is being built. Every node enters the arena through
`Draft.append`, which pushes one `nodes`, one `spans`, and one `nodeCauses` entry together, so the
three parallel vectors cannot drift. Child lists and recovery-cause runs are collected on scratch
vectors and copied into the module's shared storage when they close. Lexical binders use one flat
vector with a frame-start stack, so opening and closing a scope truncates rather than allocates.
The syntax accessors, the recovery-cause selection, and the name, path, and literal lowerings port
the bootstrap `AuthoredLowering` helpers of the same names; the literal decoders port
`internal/IntegerLiteral`, `internal/DurationLiteral`, `LiteralForm`, `StaticText`, and
`internal/Escape`. A magnitude that no `u64` can hold lowers to an invalid expression with an
overflow cause rather than widening the vocabulary.

`hir/LowerType.silk` lowers every type operand, generic binder list, row, `where` constraint, and
callable contract. `LowerType.lowerType` is the one type entry point: it unwraps grouping
parentheses in place, so a parenthesized type leaves no node, and retains a lifetime, a bare
requirement, or a damaged region in a type position as an invalid type. `LowerType.rowOperand`
selects between a type and a written requirement wherever a row admits both.
`LowerType.callableContract` reads the contract pieces directly off a callable header, because the
grammar attaches them there rather than to a node of their own. Property clauses are not lowered
yet; their values are expressions, so `ForeignFunctionType` carries an empty `properties` range
until the expression lowering lands.

CST construction remains postponed. The token vector retains source information, but there is no
second concrete-syntax tree to maintain.

Recursive rules share a budget of 32 active grammar operations. This counts parser operations,
not just parentheses: nested blocks, types, patterns, and expressions share the same budget.
Siblings restore their parent's depth. Exceeding the budget reports `NestingLimit` and skips the
damaged branch without recursive recovery. The budget is an implementation resource limit, not
a Silk grammar restriction or a limit on file size. The previous value of 256 exceeded the
available stack in the bootstrap-generated debug executable before recovery could run.

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
| `hir/Intern.silk`                              | Deduplicated byte-string storage: `Symbol` identities and the `StringTable` that produces them                                              |
| `hir/Hir.silk`                                 | The flat HIR vocabulary, the `Module` arena, its debug dump writer, and its arena verifier                                                  |
| `hir/Draft.silk`                               | The in-progress module a lowering builds: arena appends, interning, binder frames, syntax accessors, and the name, path, and literal lowerings |
| `hir/LowerType.silk`                           | Types, generic parameters, rows, `where` constraints, and callable contracts                                                                |

Grammar rules are ordinary Silk functions. They consume `State` and return it with either an
unfinished element list or a completed node ID. Replacement values are evaluated before assigning
them back to a local owner, as required by Silk's ownership rules.

## Verification

Check the self-hosted program with the bootstrap compiler:

```sh
pnpm exec silk check --manifest-path compiler/silk.toml
```

Run the source-written parser tests with the default project root:

```sh
pnpm exec silk test --manifest-path compiler/silk.toml
```

These tests parse source strings, including malformed syntax, and assert self-hosted parser
behavior. `src/main.silk` imports them for test discovery; normal builds do not execute tests.
They are not invoked by CI. The JavaScript harness below remains the
TypeScript-versus-self-hosted comparison.

After building the executable, run the parser corpus with its path:

```sh
node compiler/scripts/test-parser.mjs compiler/build/llvm/aarch64-apple-darwin/debug/silk-compiler
```

The harness reuses one built executable. It compares significant AST structure with the bootstrap
parser across grammar fixtures, the self-hosted sources, and the standard library. It also checks
postorder IDs, source spans, reachability, unique token ownership, and preservation of following
declarations after malformed syntax. Extra file paths after the executable select a smaller corpus.
The JavaScript harness imports the bootstrap package's built `dist` modules.

The default run also executes the behavioral cases in `scripts/parser-cases.mjs`, adapted from
the bootstrap parser tests. These compare acceptance and require specific valid constructs to
survive damage in both parsers. They do not compare diagnostic wording, diagnostic counts, error
tree shapes, or the two implementations' numerical resource budgets. Every native tree must
still satisfy the flat-tree invariants, and every diagnostic must address a valid source span.
The existing valid-file corpus retains its significant-tree comparison as a separate regression
check; a representation change can update that check without changing the behavioral cases.

Run only the behavioral cases, or select individual cases by name:

```sh
node compiler/scripts/test-parser.mjs compiler/build/llvm/aarch64-apple-darwin/debug/silk-compiler --cases
node compiler/scripts/test-parser.mjs compiler/build/llvm/aarch64-apple-darwin/debug/silk-compiler --cases missing-parameter-comma overdeep-call
```

Each case uses a temporary source file beneath `compiler/fixtures/`, which the harness removes
after success or failure. The native executable is built once, not once per test. The nesting
cases exercise both accepted inputs and diagnostic recovery, including depth-budget reuse by
sibling expressions and a following declaration after unclosed delimiters.

`fixtures/parser/` contains syntax-only programs: names need not resolve and operations need not
typecheck. `recovery.silk` deliberately contains syntax errors. The other files directly under
`fixtures/` exercise lexical categories and are not necessarily complete Silk programs.

Bootstrap issues encountered while developing this frontend are recorded in [BUGS.md](BUGS.md).
