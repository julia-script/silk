## Why

`add-method-call-syntax` made `value.member(args)` the third spelling of one member but deferred
the bare spelling: `let mapper = option.map` is rejected with `SEM0199` so that receiver capture
semantics would not appear by accident. That leaves one asymmetry in the callable story: every
named function and `Owner.member` is a first-class value, while the value-side spelling exists for
calls only. This change closes it (Linear JUL-92) without a new callable model: a bound method
value is an ordinary section whose captured parameter is parameter zero.

## What Changes

- Resolve `value.member` outside callee position, when `value` has a nominal type with no field
  `member` and its owner declares inherent receiver method `member`, to a callable section that
  captures the receiver per the declared parameter zero: `&Self` takes a shared loan of the place,
  `&mut Self` an exclusive loan, `Self` moves the place or consumes an rvalue. The remaining
  parameters are one onward, so a method with only a receiver binds to a zero-parameter callable.
- Generalize section construction and callable operand assembly from "captures are a trailing
  suffix" to "captures sit at their parameter ordinal and supplied arguments fill the remaining
  ordinals in order". The HIR, MIR, layout, and ownership representations already carry per-capture
  parameter ordinals; only construction and the evaluator/backend application paths assumed a
  suffix.
- **BREAKING** Retire `SEM0199`. `value.zero` for an associated function without a receiver keeps
  reporting `SEM0198`. A borrowed-receiver method bound to an rvalue (`Counter { value: 1 }.read`
  as a value) is rejected with the ordinary borrow-operand diagnostic, because a section may not
  hold a loan of a temporary.
- Hover on the bound member presents the receiver-bound contract, as a called member already does;
  completion after `value.` is unchanged.
- Bound operations on a generic receiver (`value.print` with `T: Printable`) are not bound here:
  `Bound.op` is not a first-class value today, so a witness-carrying callable target is its own
  change. The projection keeps its existing diagnostic.
- A member type parameter the receiver does not fix (`map<U>`) stays open until application,
  exactly as a trailing section leaves it. Sections with an open binder are accepted by analysis
  and fail at execution today (`apply(41)` then `f(addOne)` reports a missing instance); that
  gap predates this change and is not closed here.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-method-calls`: "Associated functions are not value members and members are not
  values" becomes "Associated functions are not value members; receiver methods bind their
  receiver".
- `bootstrap-callable-values`: sections may capture a leading parameter; a bound method value is
  the receiver section of an inherent member.
- `language-server-hover`: a bound member hovers with its receiver-bound contract.

## Impact

`ExpressionAnalysis.ts` (`analyzeProjection`), `CallResolution.ts` (`finishCallableSection`,
`analyzeSectionContract`, `sectionCallableType`, `executableSites`), `Diagnostic.ts` (retired
code), `Mir.ts` (one shared operand-ordering rule), `MirNormalization.ts`,
`BootstrapEvaluation.ts`, `WasmBackend.ts`, `NativeCallOperation.ts`,
`NativeExecutionOperation.ts`, the reference page on functions and callables, and the generated
diagnostic index. No parser change: `value.member` already parses. No layout or ownership change:
both already key off `parameterOrdinal`.
