## ADDED Requirements

### Requirement: One sealed primitive exposes the selected target only to static evaluation

The sealed `Intrinsic` namespace SHALL expose one safe, zero-argument, static-only target-profile
operation returning a closed primitive profile code for the four canonical bootstrap targets. The
operation SHALL read the compilation target selected by the compiler, SHALL be available to static
evaluation only, and MUST NOT lower to HIR runtime operations, MIR, evaluator instructions, host
imports, native symbols, or WebAssembly instructions. Its profile codes SHALL be deterministic and
documented so ordinary standard-library source can map them to nominal target enums and derive all
public target facts.

No compiler phase MAY recognize the spelling of the standard-library target module, its enums,
facts, or wrappers. The intrinsic MUST NOT expose backend objects, host detection, arbitrary target
strings, layout offsets, feature probes, or a runtime target query.

#### Scenario: Build the public target API in ordinary source

- **WHEN** the standard-library target module calls the static target-profile intrinsic and maps its result to a nominal architecture enum
- **THEN** a user target check resolves through ordinary imports, calls, enum equality, and static evaluation without compiler-known library spelling

#### Scenario: Reject the target query at runtime

- **WHEN** an ordinary runtime expression calls the target-profile intrinsic outside static evaluation
- **THEN** analysis reports that the intrinsic is static-only and no runtime intrinsic inventory entry is created

#### Scenario: Audit the minimal target seam

- **WHEN** the intrinsic catalog and generated standard-library source are inspected
- **THEN** exactly one static target-profile primitive exists and target policy, enums, pointer-width facts, and presentation remain ordinary source
