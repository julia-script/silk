# Program entry

A Silk executable starts from one public function named `main` in the root module. A private
function named `main` is an ordinary module-local function, not an executable entry point.

## ENTRY-001 — `main` must be public

**Status:** Candidate

The executable entry must be declared with `pub`. The two supported shapes are a zero-argument
ordinary `main` returning `i32`, or a zero-argument effect `main` succeeding with `()` and carrying
no unresolved requirements.

```silk
pub effect fn main() -> () {
}
```

An empty effect entry succeeds with `()`.

**Boundary:** Removing `pub` makes the function private and leaves the executable without a usable
entry.

```silk,ignore
effect fn main() -> () {
}
```

**Diagnostics:** A private root `main` must produce an entry diagnostic that identifies the missing
`pub` visibility at the declaration. No stable diagnostic code is currently assigned.

**Current compiler:** The compiler currently reports:

```text
No entry point: `main` must declare a resolved return type
```

The return type is resolved. Entry discovery groups private visibility together with unresolved
typing under one `UntypedEntry` reason, so the message hides the actionable requirement. The source
diagnostic should say that `main` must be public, or that the root module has no public entry.

**Evidence:** [entry-instance requirements](../../openspec/specs/bootstrap-instances/spec.md),
[entry selection](../../packages/compiler/src/Instances.ts),
[CLI entry messages](../../packages/compiler-cli/src/Report.ts).

## Pending rules

Later passes will cover entry failure reporting, requirement closure, exit statuses, and the exact
diagnostic assigned to each invalid entry shape.
