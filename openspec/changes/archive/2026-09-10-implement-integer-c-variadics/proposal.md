## Why

Selected native `open` and `openat` calls require genuine C variadic declarations. A fixed signature
cannot express their calling convention, especially Darwin ARM64's separate unnamed-argument stack
rules. JUL-140 supplies the bounded integer tail needed by the ordinary-source filesystem consumer.

## What Changes

- Preserve a final `...` marker and fixed parameter boundary from syntax through semantic identity,
  C ABI manifests, per-call planning, MIR verification and LLVM variadic function types.
- Admit integer tails with C integer promotions; reject unsupported tail categories and variadic
  definitions. Zero-tail calls retain the same variadic declaration.
- Preserve conservative foreign contracts and fatal unwind enforcement with per-call guard frames.
- Verify real C `va_arg` receivers and direct `open`/`openat` calls on the three pinned native targets,
  in debug and optimized modes. Reject LTO until admitted independently.
- **BREAKING**: ABI records include variadic status; every producer, decoder, consumer and golden
  changes together. No fixed-signature casts, generated C adapters, or spelling-based classification.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-foreign-functions`: true integer C variadic declarations, promotions, call identity,
  conservative contract preservation, unsupported forms, and required native conformance.

## Impact

Parser and syntax tooling; declaration/call analysis; CAbi and ABI manifests; instance discovery,
MIR and native LLVM lowering/guards; diagnostics and inspection; selected native fixtures and CI;
prescriptive foreign boundary documentation. No variadic definitions, va_list API, floating-point,
pointer or aggregate tails, general indirect variadic calls, filesystem provider migration, or fcntl
protocol is introduced. Fixed pointer parameters remain supported.
