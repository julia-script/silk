# Tutorial Blueprint: Build a Tiny Compiled Language with TypeScript, Effect, and LLVM

## Tutorial promise

After this tutorial, the learner can design, implement, compile, and extend a small expression
language that turns source text into LLVM IR and a native executable.

## Learner and context

- Audience: TypeScript developers comfortable with Effect who are curious about compiler
  construction.
- Starting capability: Can work with discriminated unions, modules, generators, and `Effect.gen`.
- Prior compiler or LLVM knowledge: None.
- Motivation: See the complete path from human-readable source code to a running native program.
- Assumed context: The package is presented through its public npm API even while pre-release
  validation uses a packed local build.

## Learning outcome

- Target capability: Build a lexer, precedence-aware parser, AST, resolver, and LLVM lowering
  pipeline for a small language.
- Guided completion evidence:
  - Compile a multi-function source program into readable `.ll`.
  - Compile that IR with Clang.
  - Run the executable and observe exit code `20`.
- Independent transfer evidence:
  - Add `%` to the token set, parser precedence table, AST behavior, and LLVM lowering.
  - Compile and run a program that uses it without copying a finished implementation.

## Central program

```text
fn abs(x) = if x < 0 then -x else x

fn score(x, y) = abs(x - y) * 3 + 2

fn main() = score(4, 10)
```

The executable returns `20`. The program exercises multiple definitions, parameters, calls,
comparison, expression-valued control flow, unary negation, and arithmetic precedence.

## Language contract

The working language name is Tiny. Renaming it is cosmetic.

```ebnf
program        = functionDefinition* EOF ;
functionDefinition
               = "fn" IDENTIFIER "(" parameters? ")" "=" expression ;
parameters     = IDENTIFIER ("," IDENTIFIER)* ;

expression     = ifExpression | comparison ;
ifExpression   = "if" expression "then" expression "else" expression ;
comparison     = addition (("<" | ">") addition)* ;
addition       = multiplication (("+" | "-") multiplication)* ;
multiplication = unary (("*" | "/") unary)* ;
unary          = "-" unary | primary ;
primary        = INTEGER
               | IDENTIFIER
               | IDENTIFIER "(" arguments? ")"
               | "(" expression ")" ;
arguments      = expression ("," expression)* ;
```

Semantic rules:

- All values, parameters, and function results are signed `i32`.
- Every function returns an expression; there is no explicit `return` syntax.
- `main` must exist and take no parameters.
- Arithmetic uses signed integer operations.
- Comparisons produce language-level `0` or `1` as `i32`.
- An `if` condition treats zero as false and nonzero as true.
- Function names and parameter names must be unique in their respective scopes.
- Calls must resolve to declared functions and use the correct arity.
- Function signatures are declared before bodies are lowered, enabling forward calls and
  recursion.
- Whitespace separates tokens; newlines have no special meaning.
- Nested expressions and nested conditionals are supported.

## LLVM teaching strategy

LLVM must be introduced as a toolchain and an intermediate language before learners are asked to
generate it. The tutorial uses two layers of explanation.

### Early mental model

Lesson 2 explains:

- LLVM is reusable compiler infrastructure, not a source language and not the Tiny compiler.
- Tiny owns source syntax, lexing, parsing, AST design, name resolution, and semantic decisions.
- `@silk-effect/llvm` constructs and validates LLVM modules and serializes them as IR or bitcode.
- Clang consumes the generated IR and performs target-specific compilation and linking.
- Textual LLVM IR, LLVM bitcode, object files, executables, and JIT execution are different
  artifacts.

### Concepts introduced immediately before use

- Before lowering `main`: modules, functions, signatures, basic blocks, instructions, types, and
  terminators.
- Before arithmetic lowering: typed SSA values, instruction results, and the rule that an SSA
  value is assigned once.
- Before conditional lowering: control-flow graphs, predecessors, dominance at a beginner level,
  and PHI nodes.

The SSA explanation contrasts mutable-looking source with versioned values:

```text
x = 1
x = x + 2
```

```llvm
%x1 = add i32 0, 1
%x2 = add i32 %x1, 2
```

The teaching message is that LLVM values are assigned once. Blocks and branches express control
flow, and a PHI node selects the value associated with the predecessor that reached a merge block.

## Scope

### Included

- A compiler-pipeline orientation and explicit LLVM mental model.
- Tokenization with source spans.
- Handwritten recursive-descent parsing with precedence climbing.
- Immutable AST data modeled with discriminated unions.
- Typed lexical, parsing, and resolution diagnostics.
- User-defined functions, parameters, calls, forward references, and recursion.
- Arithmetic, comparison, unary negation, and expression-valued conditionals.
- Two-pass function lowering.
- LLVM constants, typed SSA values, instructions, calls, blocks, branches, and PHI nodes.
- Textual LLVM IR generation.
- Native compilation at the application boundary with Clang.
- A brief LLVM bitcode variation.
- An optional compile-only browser playground.

### Explicitly excluded

- JIT execution and optimization passes.
- Mutable variables, assignment, local bindings, and loops.
- User-defined operators.
- Type annotations or type inference.
- Floating-point values.
- External function declarations, a standard library, or printing.
- Garbage collection.
- Server-side playground compilation.
- Object-code generation or linking inside `@silk-effect/llvm`.

### Deferred to explanation or reference

- A deeper history or survey of LLVM.
- Formal dominance algorithms and advanced SSA construction.
- Full grammar, token, operator, AST, and diagnostic reference tables.
- Exhaustive LLVM API options.
- Optimization, ABI, target-triple, and linker reference material.

## Prerequisites and setup

- Node.js 22.13 or newer.
- pnpm 11.
- TypeScript, Effect, Vitest, and `@effect/vitest`.
- `@silk-effect/llvm` through public package subpaths.
- LLVM/Clang 22 for the documented native checkpoint.
- A macOS or Linux POSIX shell for the primary command path, with a compact PowerShell equivalent
  for exit-code verification.

Consumer-facing installation:

```sh
pnpm add effect @silk-effect/llvm
pnpm add -D typescript vitest @effect/vitest
```

Until publication, validation installs a packed local artifact while retaining the same public
imports. The starter state is a fresh TypeScript project with a supplied application-edge CLI,
test configuration, and empty actor modules for the compiler stages.

Setup verification prints tool versions, imports `Builder` and `IrText` from public subpaths, and
renders an empty LLVM module. Estimated completion time is two to three hours.

## Proposed project structure

```text
tiny-language/
├── examples/
│   ├── answer.tiny
│   ├── factorial.tiny
│   └── score.tiny
├── src/
│   ├── Diagnostic.ts
│   ├── Token.ts
│   ├── Lexer.ts
│   ├── Expression.ts
│   ├── Program.ts
│   ├── Parser.ts
│   ├── Compiler.ts
│   └── Cli.ts
└── test/
    ├── Lexer.test.ts
    ├── Parser.test.ts
    └── Compiler.test.ts
```

The code follows the repository actor-module conventions. Public fallible operations use
`Effect.fn`, AST modules contain immutable data, Effect-returning tests use `@effect/vitest`, and
`Effect.runPromise` appears only at the CLI edge.

## Thirteen-lesson learning arc

### Lesson 1: Meet Tiny and follow the compilation pipeline

- Milestone: Identify the artifact produced by every compiler stage.
- Action: Read `score.tiny`, preview its AST, LLVM IR, and native result, and follow a pipeline
  diagram from source to executable.
- Essential concept: Tiny source is not LLVM IR; the frontend must translate it.
- Expected result: The learner can name what the lexer, parser, lowering layer, LLVM package, and
  Clang each own.
- Verification: Match representative source, token, AST, IR, object, and executable artifacts to
  their stages.
- Likely misconception: LLVM parses or understands Tiny syntax.
- Recovery: Trace one literal from source characters through to `ret i32`.
- Scaffolding: Complete orientation and finished-state preview.

### Lesson 2: Understand LLVM's role and read basic IR

- Milestone: Read a minimal LLVM function and explain why it is well formed.
- Action: Annotate a small module containing `define i32 @main()`, an `entry` block, and
  `ret i32 42`.
- Essential concepts: Module, function, signature, type, basic block, instruction, terminator,
  textual IR, bitcode, object, executable, frontend, and backend.
- Expected result: The learner can distinguish Tiny, `@silk-effect/llvm`, and Clang responsibilities.
- Verification: Label each part of the minimal IR and order the output artifact pipeline.
- Likely misconceptions: LLVM is a virtual machine; `.ll` is assembly for a physical CPU; bitcode
  is a runnable browser bytecode.
- Recovery: Return to the toolchain diagram and identify which component consumes each artifact.
- Scaffolding: Fully guided reading; no LLVM generation yet.

### Lesson 3: Create the consumer project and render a module

- Milestone: Use the package successfully through its public API.
- Action: Install dependencies, create a builder, and render an empty module.
- Essential concept: One builder owns one LLVM module; serialization is Effectful and typed.
- Expected result: Module headers appear in the terminal.
- Verification: Typecheck and run the smoke program.
- Likely failures: Pre-release package resolution, internal imports, or missing Clang.
- Recovery: Install the packed artifact, keep public subpaths, and separate rendering from the
  later native checkpoint.
- Scaffolding: Complete code supplied.

### Lesson 4: Tokenize Tiny source

- Milestone: Convert source characters into a structured token stream.
- Action: Define token kinds and implement `Lexer.tokenize` with start/end spans.
- Essential concept: Lexing removes character-level concerns from the parser.
- Expected result: `fn main() = 1 + 2 * 3` produces the expected tokens and EOF.
- Verification: Exact token assertion.
- Likely failures: Keywords emitted as identifiers, cursor stalls, or incorrect EOF spans.
- Recovery: Trace cursor advancement over an annotated short source.
- Scaffolding: Token model supplied; learner completes the scanning loop.

### Lesson 5: Build an AST and resolve arithmetic precedence

- Milestone: Represent calculation order independently of formatting.
- Action: Define expression nodes and implement precedence climbing for arithmetic.
- Essential concept: `1 + 2 * 3` becomes `1 + (2 * 3)` before LLVM is involved.
- Expected result: AST snapshots show correct precedence, parentheses, unary binding, and left
  associativity.
- Verification: Parse `1 + 2 * 3`, `(1 + 2) * 3`, `10 - 3 - 2`, and `-2 * 3`.
- Likely failures: Reversed precedence, right-associative subtraction, or unconsumed operators.
- Recovery: Walk the token cursor and minimum-precedence value for one example.
- Scaffolding: One operator pair demonstrated; learner completes the table.

### Lesson 6: Parse functions, calls, and conditionals

- Milestone: Parse a complete Tiny program into `Program` data.
- Action: Add function definitions, parameters, argument lists, calls, and `if/then/else`.
- Essential concept: Recursive grammar rules become focused parser operations.
- Expected result: `score.tiny` parses into three definitions with the intended nested expressions.
- Verification: Snapshot the full AST and reject trailing or incomplete syntax.
- Likely failures: Confusing a name with a zero-argument call, failing to consume `else`, or silently
  accepting duplicate parameters.
- Recovery: Report the current token, expected token, and source span from the owning production.
- Scaffolding: Function parsing demonstrated; calls and conditionals completed with guidance.

### Lesson 7: Lower and run `fn main() = 42`

- Milestone: Produce and execute the first native program.
- Action: Create `i32`, declare `main`, build an entry block, return an integer constant, render
  `.ll`, compile with Clang, and run it.
- Essential concepts: Typed signatures, block ownership, instructions, and required terminators.
- Expected result: IR contains `define i32 @main()` and `ret i32 42`; execution returns `42`.
- Verification: Inspect the IR and native exit status.
- Likely failures: Wrong `main` signature, unterminated block, or cross-builder handle reuse.
- Recovery: Compare the builder/type/function/body lifecycle with the minimal IR from Lesson 2.
- Scaffolding: Complete literal lowering supplied.

### Lesson 8: Lower arithmetic and learn SSA

- Milestone: Recursively lower arithmetic, unary negation, and comparisons into typed SSA values.
- Action: Map Tiny operators to `add`, `sub`, `mul`, `sdiv`, `icmp`, `zext`, and
  `FunctionBody.negate`.
- Essential concepts: Instruction results are typed values; SSA values are assigned exactly once;
  the AST determines evaluation order.
- Expected result: `fn main() = 1 + 2 * 3` emits multiplication before addition and returns `7`.
- Verification: Connect every LLVM temporary to one AST node and confirm comparison results become
  language-level `i32` zero or one.
- Likely failures: Unsigned operations, returning raw `i1`, or treating SSA names as mutable
  variables.
- Recovery: Draw the AST-to-SSA-value mapping and inspect every operand type.
- Scaffolding: Mapping table supplied; recursive dispatch is partially guided.

### Lesson 9: Resolve functions and lower calls

- Milestone: Support multiple functions, forward references, and recursion.
- Action: Declare every signature in pass one, then build bodies in pass two; map parameters with
  `Value.argument`; resolve calls with `FunctionBody.callDirect`.
- Essential concept: A symbol and signature can be known before its body exists.
- Expected result: `score` calls `abs` regardless of definition order, and factorial can call itself.
- Verification: Reverse definition order and compile the optional factorial example.
- Likely failures: Declaring functions while compiling bodies, mixing parameter/function scopes,
  wrong arity, or assuming a direct call always returns a value.
- Recovery: Keep distinct environments and translate impossible void results into a typed compiler
  error rather than using a non-null assertion.
- Scaffolding: Two-pass structure supplied; resolution cases completed by the learner.

### Lesson 10: Lower `if` with control flow and PHI nodes

- Milestone: Convert a conditional expression into blocks and one resulting SSA value.
- Action: Compare the `i32` condition with zero, create true/false/merge blocks, terminate both
  branches, and seal an `i32` PHI node.
- Essential concepts: Control-flow graph, predecessor, reachable definition, beginner-level
  dominance, merge block, and PHI selection.
- Expected result: `abs` forms a control-flow diamond whose PHI selects `-x` or `x` based on the
  predecessor that ran.
- Verification: Match generated blocks and PHI incoming values to a diagram.
- Likely failures: Passing `i32` directly to `conditionalBranch`, missing terminators or incoming
  edges, and duplicate names in nested conditionals.
- Recovery: Track the insertion point and predecessors; use a per-body block-name counter; rely on
  transactional body validation.
- Scaffolding: Diagram and lowering pseudocode supplied; implementation partially guided.

### Lesson 11: Compile and run the complete language

- Milestone: Operate the complete frontend as one compiler.
- Action: Connect lexer, parser, validation, lowering, and rendering in `Compiler.compile`; emit
  `score.ll`; compile and run it.
- Essential concept: Each compiler phase has a precise input, output, and typed failure channel.
- Expected result: Parser and compiler tests pass, Clang accepts the IR, and the executable returns
  `20`.
- Verification: Inspect `abs`, `score`, and `main`, then capture the native exit status.
- Likely failures: Debug output contaminates IR, errors use stdout, or the shell hides the previous
  exit code.
- Recovery: Reserve stdout for IR, diagnostics for stderr, and capture status immediately.
- Scaffolding: Mostly removed; the learner connects completed modules.

### Lesson 12: Diagnose failures and understand bitcode

- Milestone: Distinguish language diagnostics from LLVM validation failures and choose an output
  representation.
- Action: Trigger lexical, parse, resolution, arity, and unterminated-block failures; then replace
  `IrText.render` with `Bitcode.encode` in a short variation.
- Essential concepts: Phase-owned diagnostics, transactional body construction, textual IR versus
  bitcode, and application-owned persistence/tool invocation.
- Expected result: Every failure names its phase and span or LLVM operation; bitcode begins with the
  expected LLVM magic bytes.
- Verification: One test per failure category and one deterministic bitcode check.
- Likely misconception: Every compiler failure is an LLVM failure or bitcode is directly runnable.
- Recovery: Identify the last successful phase and the consumer of each output representation.
- Scaffolding: Diagnostic shapes supplied; contextual messages completed by the learner.

### Lesson 13: Extend Tiny with `%` and consolidate learning

- Milestone: Extend the whole pipeline with reduced guidance.
- Action: Add `%` to tokens, lexing, multiplication-level precedence, AST lowering, and tests using
  signed LLVM `srem`.
- Essential concept: A well-separated compiler lets one language feature travel through explicit
  stages without unrelated edits.
- Expected result: Generated IR contains `srem`, precedence remains correct, and a program using `%`
  executes successfully.
- Verification: Parse and compile `10 + 7 % 4 * 2`; run `isOdd(7)`; explain every changed stage.
- Likely failures: Giving `%` the wrong precedence or choosing unsigned remainder.
- Recovery: Compare the operator's path with `*` and signed `/`.
- Scaffolding: Success criteria and tests supplied; implementation withheld.

## Playground plan

The playground is optional progressive enhancement. The written tutorial remains complete without
it.

- It runs the same lexer, parser, and compiler core in the browser.
- It does not invoke Clang, execute native code, or contact a server.
- It presents editable source plus tokens, AST, diagnostics, and LLVM IR.
- It preserves successful earlier-stage output when a later stage fails.
- It marks diagnostic spans and offers copyable LLVM IR.
- All static inputs, outputs, diagrams, and exercises remain available in the article.
- Playground hosting remains unresolved because the repository currently contains Markdown
  documentation rather than a documentation application.

## Supporting assets

- A consumer-shaped completed example tested against a packed local package.
- Starter scaffold and chapter checkpoints.
- `score.tiny`, `factorial.tiny`, invalid-source fixtures, and transfer-task fixtures.
- Expected token, AST, IR, diagnostic, and bitcode snapshots.
- A compiler pipeline/tool-ownership diagram.
- An annotated minimal LLVM function.
- An AST precedence tree and AST-to-SSA mapping.
- A conditional control-flow/PHI diagram.
- A compact operator-to-LLVM table.
- A browser playground adapter that depends only on the compiler core.

## Writing brief

- Voice: Curious, direct, and collaborative; assume programming competence but no compiler
  vocabulary.
- Pacing: Introduce one phase at a time and produce a visible artifact every 15–25 minutes.
- Explanation discipline: Give the LLVM mental model early, then teach SSA, control flow, dominance,
  and PHI only immediately before their use. Avoid an extended theory detour.
- Terminology:
  - Use LLVM IR or LLVM assembly for `.ll`.
  - Use LLVM bitcode, not bytecode, for `.bc`.
  - Distinguish parsing, resolution, lowering, native compilation, and linking.
- Code conventions:
  - One actor module per concept.
  - Public fallible functions use `Effect.fn` with precise errors.
  - Tests use `@effect/vitest` for Effect-returning code.
  - No class-per-entity AST, non-null assertions, convenience casts, or package internals.
  - Node filesystem and process handling remain at the supplied application boundary.
- Accessibility:
  - Every diagram has an adjacent prose description.
  - Meaning never depends on syntax color alone.
  - Outputs are available as text.
  - The playground is keyboard-operable.

## Validation plan

- Typecheck and test the completed consumer example against a packed local package.
- Assert representative token streams and AST shapes.
- Render and snapshot representative LLVM IR.
- Compile the output with pinned Clang 22, execute it, and assert status `20`.
- Compile factorial to validate self-reference and two-pass declarations.
- Complete `%` in a hidden validation fixture.
- Compare browser and local IR output byte-for-byte for the same source.
- Pilot with two or three target learners and observe whether they can:
  - explain which component owns each compiler stage;
  - read a minimal LLVM function before generating one;
  - predict precedence ASTs;
  - connect AST nodes to SSA values;
  - explain why a PHI node is needed;
  - locate the phase that owns an error; and
  - add `%` without editing unrelated modules.

Revision is required if multiple learners block at the same checkpoint, confuse LLVM with the Tiny
frontend, copy IR without connecting it to the AST, or need implementation-equivalent hints for
the transfer task.

## Acceptance criteria

- [ ] The learner can explain what LLVM, `@silk-effect/llvm`, the Tiny frontend, and Clang each do.
- [ ] The learner can read a minimal LLVM function before generating IR.
- [ ] The learner reaches a native executable returning `20`.
- [ ] Every lesson has an observable checkpoint and recovery path.
- [ ] Precedence is demonstrated through AST shape rather than only final output.
- [ ] SSA is introduced with an AST-to-value example before arithmetic lowering.
- [ ] Control flow, predecessor relationships, and PHI selection are explained before conditional
      lowering.
- [ ] The compiler supports multiple functions, calls, forward references, and recursion.
- [ ] Comparison and condition truthiness semantics are explicit.
- [ ] The `%` exercise tests transfer through the complete pipeline.
- [ ] The written path is complete without the playground.
- [ ] Consumer code uses only public package APIs.
- [ ] The packed-package setup is validated before publication.

## Decisions and assumptions

Confirmed decisions:

- Audience has TypeScript and Effect experience but no compiler or LLVM experience.
- The tutorial uses a handwritten lexer and precedence parser.
- Tiny supports integer arithmetic, calls, comparisons, and conditional expressions.
- Lowering uses two function passes and supports forward calls and recursion.
- Textual LLVM IR is primary; bitcode is a short secondary path.
- Native execution uses Clang without JIT.
- The playground is optional, browser-only, compile-only, and serverless.
- `%` is the independent transfer exercise.
- The experience is chaptered and targets two to three hours.
- LLVM receives a dedicated mental-model lesson; SSA and PHI are reinforced at their point of use.

Assumptions:

- Tiny uses signed `i32` exclusively.
- Comparisons normalize to `i32` zero or one.
- Primary commands target a POSIX shell.
- Pre-release testing uses a packed local package.

Intentionally unresolved:

- Final language name.
- Playground hosting framework and page integration.
- Exact published package version and final npm installation command.
