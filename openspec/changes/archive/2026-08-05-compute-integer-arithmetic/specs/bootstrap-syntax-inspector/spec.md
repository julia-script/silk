## ADDED Requirements

### Requirement: Inspect arithmetic through the labs

The labs SHALL present the arithmetic surface over the facade: the syntax and HIR views show
qualified callees, signed literals, and builtin-call operations; the MIR lab shows binary
operations with their operators and provenance; the LLVM IR lab shows the checked expansion
(overflow intrinsics and trap branches); and the evaluation surface shows arithmetic results and
arithmetic trap outcomes with their provenance.

#### Scenario: Inspect a built-in call end to end

- **WHEN** a developer enters `pub fn main() -> I32 { return I32.add(40, 2) }`
- **THEN** the HIR view shows a builtin `Add` call, the MIR lab shows one `Add` binary operation, and the LLVM IR lab's text contains the overflow intrinsic

#### Scenario: Inspect an arithmetic trap

- **WHEN** a developer evaluates a program dividing by zero
- **THEN** the evaluation surface reports the blocked arithmetic trap with its provenance rather than a value
