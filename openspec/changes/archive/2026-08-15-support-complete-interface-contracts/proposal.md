## Why

Current user-interface witnesses cover narrow pure operations and blanket-adapt value operands to
shared borrows. Static decoders and other ordinary abstractions need literal ownership plus complete
success, failure, requirement, and access contracts.

## What Changes

- Allow interface declarations, applications, conformances, and witness targets to bind and
  specialize failure-row and requirement-row parameters.
- Interpret interface operand ownership literally: `&T` borrows, `&mut T` borrows exclusively, and
  `T` transfers ownership.
- Admit witnesses with smaller rows or weaker required access through explicit subsumption while
  keeping the instantiated interface contract source-observable at generic call sites.
- Infer generic type, row, and representation arguments for mapped witness functions and include
  them in concrete witness instance keys.
- **BREAKING**: remove blanket source-witness value-to-borrow adaptation; migrate ordinary `Order`
  and `HashKey` interfaces, confining any legacy adapter to sealed intrinsic lowering.

## Capabilities

### New Capabilities

- `bootstrap-complete-interface-contracts`: Literal operands, complete rows, access/row variance,
  generic witness inference, HIR questions, and static lowering.

### Modified Capabilities

- `bootstrap-instances`: Witness instance discovery includes inferred type, row, and representation
  arguments and mapped generic target instances.

## Impact

Depends on `admit-conditional-generic-conformances`; representation-bearing witnesses also depend
on `introduce-representation-parameters`. Affects interface analysis, conformance mapping, HIR,
instance discovery, ownership, row checking, `Order`/`HashKey` source, diagnostics, and lowering.
