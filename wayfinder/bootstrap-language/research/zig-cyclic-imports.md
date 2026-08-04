# Zig cyclic imports and declaration dependencies

Research target: Zig 0.15.2. Sources are the versioned language reference and the official compiler repository at the 0.15.2 release commit.

## Findings

- Zig explicitly permits cycles in its **module dependency graph**. A Zig module is not one source file: it is a collection of source files with one root source file. The language reference says dependency loops between modules are allowed. [`Compilation Model`](https://ziglang.org/documentation/0.15.2/#Compilation-Model)
- Zig does not separately phrase the rule as “arbitrary source-file import cycles,” but its official tests intentionally construct both `foo -> bar -> foo` and a module importing itself. They assert that following the cycle returns the same module identity. `@import` likewise says a target file is added only if it is not already in the compilation. [`dep_mutually_recursive`](https://github.com/ziglang/zig/tree/e4cbd752c8c05f131051f8c873cff7823177d7d3/test/standalone/dep_mutually_recursive), [`dep_recursive`](https://github.com/ziglang/zig/tree/e4cbd752c8c05f131051f8c873cff7823177d7d3/test/standalone/dep_recursive), [`@import`](https://ziglang.org/documentation/0.15.2/#import)
- The standard compilation is further concrete evidence. `std.zig` imports both `start.zig` and `root`; `start.zig` imports both `root` and `std.zig`; ordinary roots commonly import `std`. Zig therefore relies on reciprocal file references in normal compilation. [`std.zig`](https://github.com/ziglang/zig/blob/e4cbd752c8c05f131051f8c873cff7823177d7d3/lib/std/std.zig#L110-L180), [`start.zig`](https://github.com/ziglang/zig/blob/e4cbd752c8c05f131051f8c873cff7823177d7d3/lib/std/start.zig#L1-L4), [`Hello World`](https://ziglang.org/documentation/0.15.2/#Hello-World)
- Import-graph cycles and semantic dependency cycles are different. An analyzed import discovers a file, but an ordinary named declaration is analyzed only when referenced; declarations are order-independent and references may cross files. This permits useful mutual recursion without requiring a topological order over files. [`File and Declaration Discovery`](https://ziglang.org/documentation/0.15.2/#File-and-Declaration-Discovery)
- Mutually recursive functions are accepted. Zig's own behavior suite has `foo` call `bar` and `bar` call `foo`. Recursive type graphs are accepted when an indirection makes layout finite, as in `A -> B -> *A`. [`call.zig`](https://github.com/ziglang/zig/blob/e4cbd752c8c05f131051f8c873cff7823177d7d3/test/behavior/call.zig#L306-L340), [`struct.zig`](https://github.com/ziglang/zig/blob/e4cbd752c8c05f131051f8c873cff7823177d7d3/test/behavior/struct.zig#L1397-L1411)
- Cycles that require a value, type, or layout to be completed from itself are rejected. The compiler test suite expects `dependency loop detected` for mutually dependent top-level `@TypeOf` declarations, and rejects the by-value layout cycle `A -> B -> C -> A`. [`top_level_decl_dependency_loop.zig`](https://github.com/ziglang/zig/blob/e4cbd752c8c05f131051f8c873cff7823177d7d3/test/cases/compile_errors/top_level_decl_dependency_loop.zig), [`indirect_struct_loop.zig`](https://github.com/ziglang/zig/blob/e4cbd752c8c05f131051f8c873cff7823177d7d3/test/cases/compile_errors/indirect_struct_loop.zig)
- Zig avoids runtime global-initialization ordering as a separate cycle problem. Container-level variables have static lifetime, are order-independent and lazily analyzed, and their initializer is implicitly evaluated at compile time. A `const` remains compile-time-known; a mutable `var` becomes runtime-known storage after that compile-time initialization. The compile-error suite rejects an initializer that calls an external runtime function because the initializer must be compile-time-known. [`Container Level Variables`](https://ziglang.org/documentation/0.15.2/#Container-Level-Variables), [`global_variable_initializer_must_be_constant_expression.zig`](https://github.com/ziglang/zig/blob/e4cbd752c8c05f131051f8c873cff7823177d7d3/test/cases/compile_errors/global_variable_initializer_must_be_constant_expression.zig)

## Recommendation for the bootstrap language

Allow cyclic imports, but do not treat that as permission for every semantic cycle:

1. Intern each canonical module once and collect every top-level declaration before resolving bodies.
2. Resolve declarations on demand with states such as `unseen`, `resolving`, and `resolved`.
3. Permit re-entry through references that do not require an immediate completed value or layout: function calls, nominal type names, and pointer-like indirections.
4. Diagnose a cycle only when resolution requires its own unfinished result, and report the declaration path rather than rejecting the enclosing import cycle.
5. Keep bootstrap globals compile-time initialized and side-effect-free; reject cyclic constant evaluation. Do not introduce runtime module initializers or an initialization order.

Good — cyclic imports with resolvable declarations:

```silk
// syntax/Expression.silk
import syntax/Statement as Statement
pub struct Expression { continuation: &Statement.Statement }

// syntax/Statement.silk
import syntax/Expression as Expression
pub struct Statement { value: &Expression.Expression }
```

Bad — a layout requires itself by value:

```silk
// syntax/Expression.silk
import syntax/Statement as Statement
pub struct Expression { continuation: Statement.Statement }

// syntax/Statement.silk
import syntax/Expression as Expression
pub struct Statement { value: Expression.Expression }
// Error: type-layout dependency cycle:
// Expression -> Statement -> Expression
```

Bad — compile-time initialization requires its own unfinished value:

```silk
// config/A.silk
import config/B as B
pub const a: U32 = B.b + 1

// config/B.silk
import config/A as A
pub const b: U32 = A.a + 1
// Error: constant-evaluation dependency cycle: A.a -> B.b -> A.a
```
