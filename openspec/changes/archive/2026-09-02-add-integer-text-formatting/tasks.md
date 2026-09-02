## 1. The Formatting Module

- [x] 1.1 Add the canonical `silk/format` module holding the whole engine: rendering and reading
      written once over `u64` and once over `i64`, with the typed reading failure alongside.
      **Written twice rather than once and generically**, measured, not preferred: an interface may
      carry only operations an operator spells, so `checkedMultiply` has no call surface through a
      bound. Widening is lossless in both directions, so the two implementations are the whole truth
      about every integer type.
- [x] 1.2 Render most-significant digit first, computing the leading power of ten before emitting
      anything. `String.append` only appends, so a least-significant-first walk would need to
      prepend.
- [x] 1.3 Take a signed value's digits from its negative form, so the smallest signed value needs no
      special case: negating it would overflow, while negating one of its digits never does.
- [x] 1.4 Accumulate a signed reading negatively for the same reason, so text naming the smallest
      value reads like any other text.
- [x] 1.5 Name the accumulator's bound with the constants #38 shipped — reject before the overflow,
      not after it.
- [x] 1.6 Narrow to the target type with the existing checked conversion rather than a comparison
      against that type's own `MAX`, so `usize` and `isize` read exactly like the eight fixed-width
      types and need no target-dependent constant (#109).
- [x] 1.7 Register the module and its failure aliases in the standard-library manifest and
      regenerate the compiler-shipped source table.

## 2. The Integer Modules

- [x] 2.1 Add `toText` to all ten integer modules, widening to `u64` or `i64` and delegating.
- [x] 2.2 Add `parse` to all ten, returning the completed outcome carrying the typed failure.
- [x] 2.3 Reach both through the ambient namespace rather than an import. **Revised during
      implementation**, measured: importing `silk.string`, `silk.result` and `silk.format` into each
      integer module adds a resolved import edge and grows the closure the same way, but it also
      changes the module-closure facts a program publishes. The ambient form keeps the import facts
      untouched and resolves identically.

## 3. Owned Text

- [x] 3.1 Add `String.appendOwned`, consuming the appended String.
- [x] 3.2 Stop `String.append` copying the receiving string into fresh storage before growing.
      **Not in the issue**, but the operation the issue exists to enable: composing a short message
      allocated once per piece and copied the whole message each time. The byte append underneath is
      already atomic on failure, so the guarantee in the doc comment is unchanged. Measured on the
      lexer pressure program: 30 allocations before, 11 after.

## 4. Wasm Lane Widths

- [x] 4.1 Record every physical local's declared value type in the function layout, parameters
      included, so a transfer can see both ends.
- [x] 4.2 Bridge a transfer between lanes of different value types: normalize to the integer of its
      own width, adjust the width, reinterpret into the target. Widen unsigned so narrowing back
      yields exactly the bits that went in, whatever the member's own signedness.
- [x] 4.3 Use the bridge for the match destructure, both union conversions, and the effect failure
      pack, and spell a missing lane's zero in that lane's own value type.
- [x] 4.4 Turn the reclaim path's silent skip of a widened lane into a refusal to emit. It reads
      slots directly and has no local of the member's own type to bridge into; releasing nothing
      would leak the member's blocks without a word.
- [x] 4.5 Pin the round trip in both directions as a backend acceptance test, on the evaluator and
      the Wasm backend, in the shape that motivated it.

## 5. The Lexer Example

- [x] 5.1 Render each lexical diagnostic as a message carrying the line and column its byte offset
      resolves to, replacing the diagnostic half of the `i32` fingerprint.
- [x] 5.2 Fold the rendered message's bytes into the value the harness checks, and mirror the
      rendering in the harness, so a wrong line number fails rather than differs.
- [x] 5.3 Keep the token half of the fingerprint and the trace observations unchanged, so the
      corpus oracle and the three-engine parity tests still compare what they compared.
- [x] 5.4 **The message is composed and verified, not printed.** The harness asserts the Wasm module
      imports nothing, so the program has no stream to print to on that engine. Comparing the
      rendered bytes is the strongest form the acceptance criterion can take across all three
      engines.

## 6. Acceptance

- [x] 6.1 Render and read every fixed-width type at both bounds, in one program, on the evaluator,
      the Wasm backend, and the native backend, answering with the index of the first disagreement.
- [x] 6.2 Round trip `usize` and `isize` at values every supported pointer width holds, since they
      have no bounds to name.
- [x] 6.3 Pin every reading failure: empty text, letters, a trailing letter, an interior space, a
      leading `+`, a sign alone, a sign followed by a letter, a value above every type, and a
      negative for an unsigned type.
- [x] 6.4 Compose the issue's own example message and assert it byte for byte, with allocation
      acquires equal to releases.
- [x] 6.5 Pin that a rendering whose allocator refuses hands back the ordinary allocation failure
      and acquires nothing.
- [x] 6.6 Pin the pair's presence on every integer module the catalog knows, so a later integer type
      cannot ship without it, and pin the absence of the float pair so the deferral does not read as
      an oversight.
- [x] 6.7 Run the full compiler suite and report every test whose expectation changed.

## 7. Specification

- [x] 7.1 Add the `bootstrap-silk-stdlib` requirements for rendering, reading, the typed failure,
      and the owned-text append.
- [x] 7.2 Add the `bootstrap-backend` requirement for a union member narrower than its payload slot.
