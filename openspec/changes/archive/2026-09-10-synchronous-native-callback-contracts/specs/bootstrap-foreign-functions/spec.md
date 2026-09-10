## MODIFIED Requirements

### Requirement: C callbacks are exact noncapturing export addresses

Silk SHALL define `extern "C" fn(P...) -> R` as a nonnull C function-pointer type with its exact ABI and normalized forbidden-unwind access contract. An optional `with Intrinsic.foreign(...)` clause SHALL express admitted behavior; unnamed type parameters SHALL use decimal ordinal strings in parameter sets. A named, nongeneric, synchronous `export "C" fn` item SHALL contextually convert only when both its value signature and declared behavioral contract match. Stronger exported assertions SHALL require an unsafe export. Ordinary functions, effect or suspending functions, generic functions, and capturing callables SHALL NOT convert.

#### Scenario: Pass an exported comparator to qsort

- **WHEN** an exact synchronous exported comparator is passed to a foreign qsort declaration with an explicit callback invocation promise
- **THEN** native execution passes the comparator's C-callable address and C invokes the Silk thunk

#### Scenario: Reject a nonexported callback

- **WHEN** an ordinary private function is used where a C function pointer is required
- **THEN** semantic analysis reports that only an exact exported function is addressable

#### Scenario: Reject a suspending or capturing callback

- **WHEN** an effect/suspending function or capturing anonymous callable is used as a C callback
- **THEN** semantic analysis reports the unsupported callback form at the conversion site

#### Scenario: Reject a stronger expected contract

- **WHEN** a conservatively declared export is passed where argument-local read-only access is required
- **THEN** analysis rejects the mismatch rather than assigning the expected promises to the export
