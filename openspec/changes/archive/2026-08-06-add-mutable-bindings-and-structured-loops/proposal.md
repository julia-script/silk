## Why

Fixed-size arrays can now represent compiler-shaped data, but Silk still cannot update that data or
repeat work over it. Adding explicit mutation and structured loops is the smallest next slice that
makes the bootstrap language capable of expressing real bounded algorithms while exposing the
ownership, cleanup, and control-shape invariants the compiler will need for self-hosting.

The current MIR contract also discards structured control into a general CFG and asks structured
backends to recover it. Loops are the point where that choice becomes costly. Compiler-published
representations must instead remain DAG-shaped, with repetition represented by an explicit
structured loop node, so every backend receives the same recoverable structure and performs only
the target-specific conversion it actually needs.

## What Changes

- Add mutable local bindings, assignment to writable bindings and field/index places, and complete
  replacement semantics that keep safe values initialized and clean replaced owned values exactly
  once.
- Add statement-form `while`, `break`, and `continue` with strict `Bool` conditions, lexical loop
  targets, deterministic evaluation order, and cleanup on every iteration and loop exit.
- Extend semantic facts, HIR, ownership, cleanup, evaluation, diagnostics, and the analysis facade
  with explicit mutability, writes, loop regions, control transfers, and provenance.
- **BREAKING** Replace MIR's general cyclic basic-block CFG contract with a canonical structured
  control DAG. Repetition is represented only by an explicit loop region; arbitrary back-edges are
  invalid. Syntax trees remain the tree subset of this rule, and semantic/HIR/MIR references expose
  acyclic ownership and control relationships through canonical identities.
- Make each backend consume the compiler-owned control DAG directly: LLVM linearizes it into blocks
  and back-edges, while WebAssembly maps it into structured `block`, `loop`, and `if` constructs.
  Backends MUST NOT recover source structure from a flattened graph.
- Add evaluator/native/WebAssembly parity and fresh-process determinism coverage for mutation,
  checked array updates, zero-iteration and multi-iteration loops, nested loops, early exits, traps,
  and cleanup.
- Extend the unified `/labs` workbench with coordinated mutation, control-DAG, ownership, MIR,
  evaluation, and backend views rather than a standalone loop inspector.
- Amend the Wayfinder compiler-pipeline decision and project roadmap to record the canonical DAG
  boundary and supersede their current general-CFG/structure-recovery wording.

## Capabilities

### New Capabilities

- `bootstrap-mutable-loops`: Defines mutable owners, safe assignment and replacement, structured
  `while` loops, lexical `break`/`continue`, and their observable cleanup and execution semantics.

### Modified Capabilities

- `bootstrap-syntax`: Adds lossless mutable-binding, assignment, `while`, `break`, and `continue`
  syntax with bounded recovery.
- `bootstrap-semantic-facts`: Adds authoritative mutability, writable-place, assignment, loop-target,
  condition, and control-transfer facts and diagnostics.
- `bootstrap-hir`: Adds typed writes and structured loop/control-transfer regions without cyclic HIR.
- `bootstrap-ownership`: Extends liveness and cleanup across repeated paths, replacement, `break`,
  `continue`, and `return` while enforcing exclusive mutation.
- `bootstrap-mir`: Replaces the general CFG contract with a verified structured control DAG and adds
  write, loop, and lexical transfer operations.
- `bootstrap-evaluation`: Executes mutation and structured repetition from the control DAG with
  deterministic traps, replacement, and traces.
- `bootstrap-backend`: Requires LLVM and WebAssembly to lower the same compiler-owned control DAG
  into their target control forms without reconstructing discarded structure.
- `bootstrap-compiler-driver`: Extends differential and determinism gates with mutable-loop programs
  and control-DAG artifacts.
- `bootstrap-analysis-facade`: Exposes immutable mutation, loop-region, control-edge, cleanup, trace,
  and emission queries from the authoritative snapshot.
- `bootstrap-syntax-inspector`: Adds coordinated mutable-loop and control-DAG inspection to `/labs`.

## Impact

This change affects the compiler lexer/parser and syntax model; semantic elaboration and diagnostics;
HIR, ownership, cleanup, MIR lowering and verification; the interpreter; native LLVM and direct
WebAssembly backends; driver corpora; analysis facade; unified labs; deterministic encoders and
goldens; Wayfinder issue 06; and the project roadmap. MIR constructors and backend consumers change
incompatibly because cyclic block graphs are no longer valid compiler interchange data. No new
runtime dependency, iterator protocol, `for` syntax, range type, labeled loop, expression-valued
loop, or backend-specific control IR is introduced.
