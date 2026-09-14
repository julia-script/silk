## Context

See `proposal.md` for motivation and `specs/base64-codec/spec.md` for the public behavior. The standard library already supports ordinary `Result`, `Option`, slices, target-width `usize.MAX`, and checked arithmetic without allocation. The private certificate PEM decoder cannot be reused because it deliberately combines Base64 with whitespace, framing, certificate limits, and certificate-specific diagnostics.

The codec must expose deterministic failures while keeping every destination byte unchanged on invalid input or insufficient capacity. It must also prove sizing on both admitted pointer widths without ever constructing an enormous slice.

## Goals / Non-Goals

**Goals:**

- Keep sizing, validation, and byte transformation in one ordinary target-neutral actor module.
- Make validation reusable between `decodedLength` and `decodeInto` without weakening `decodedLength` into a shape-only check.
- Establish mutation-free preflight phases so atomic failure follows directly from control flow.
- Keep boundary sizing callable with arbitrary `usize` values for focused scalar and cross-width evidence.

**Non-Goals:**

- Allocating convenience functions, strings, writers, streams, MIME line folding, URL-safe or unpadded profiles.
- Replacing the private certificate PEM decoder or its diagnostics.
- Adding a compiler intrinsic or recognizing `Base64` declarations by spelling.
- Adding HTTP authentication, WebSocket handshake, or replay policy.

## Decisions

### Use one record-shaped error with an enumerated reason

`Base64Error` always has the same four public fields. Small private constructors populate only fields meaningful to the reason: validation offsets, or capacity required/available, or no optional details for size overflow. This gives consumers one stable inspection shape and makes absence explicit.

Alternative considered: a payload-carrying union per reason. That would encode field applicability more tightly, but it would not match the triaged consumer contract and would make generic protocol error reporting more cumbersome.

### Compute encoded size from quotient and remainder

Sizing divides the input by three first. It compares the quotient with `usize.MAX / 4`, then performs the safe multiplication and adds one final four-byte quantum only after checking room. No `input + 2` ceiling shortcut or unchecked multiplication occurs. Boundary acceptance can call this function directly with target-width maximum values.

Alternative considered: checked-add and checked-multiply combinators. They are correct, but quotient/remainder expresses the precise last-success boundary directly and avoids intermediate option choreography.

### Make decoded sizing the single strict preflight validator

`decodedLength` first checks the global length rule and then walks the encoding in four-byte quanta. All non-final quanta require four alphabet bytes. The final quantum admits exactly the three RFC shapes, checks unused bits on the last significant sextet, and computes the exact result. `decodeInto` calls this validator before capacity checking and performs a separate emission pass only after both succeed.

Alternative considered: decode to temporary storage while validating. That would require allocation or scratch memory, undermine caller-buffer atomicity, and duplicate capacity policy.

### Separate alphabet mapping for encode and validate/decode

Encoding maps a known sextet to its standard alphabet byte. Decoding maps an input byte to an optional sextet, while `=` is handled explicitly as syntax rather than a pseudo-sextet. This keeps URL-safe bytes and all whitespace outside the alphabet and allows misplaced padding to be distinguished from invalid bytes.

Alternative considered: a 256-byte lookup table. A table is viable but adds a large constant and sentinel policy for a small branch structure; the arithmetic ranges are clearer in ordinary Silk and remain constant-space.

### Write complete quanta only after preflight

Encoding and decoding use simple index loops over complete three/four-byte groups, followed by one explicit tail case. Neither function clears or fills the output. Consequently only the successful initialized prefix changes, and suffix preservation requires no restoration path.

Alternative considered: one branch-heavy loop that mixes validation and emission. It could save a pass for decoding but would partially mutate output before detecting a later error, requiring rollback storage or weakening atomicity.

### Keep acceptance evidence in one Base64-specific source program

One reusable Silk acceptance program will table-check independent RFC vectors, binary alphabet cases, tail shapes, malformed classes, precedence, destination atomicity, suffix preservation, and scalar pointer-width boundaries. Focused analysis and code generation will realize it for both admitted pointer widths, LLVM-to-Wasm will execute it in the feature test, and the shared native corpus will reuse its public-import form as the behavioral truth. An ownership-only analysis case will attempt an overlapping borrow and assert the existing ownership diagnostic. Static evaluation is deliberately absent because the four public codec operations are ordinary runtime functions; no compile-time-only Base64 behavior is claimed.

Alternative considered: one compiler execution per vector or error. That would repeat parsing, analysis, lowering, and process startup while adding no distinct signal.

## Risks / Trade-offs

- **[Risk] Validation precedence becomes ambiguous around missing final padding** → Treat `=` as syntax, document missing required pad positions at `input.length`, and pin representative fixtures in focused acceptance evidence.
- **[Risk] A successful write accidentally touches spare capacity** → Never clear or initialize the output; assert sentinel suffix bytes for every tail shape.
- **[Risk] Boundary tests try to allocate near-`usize.MAX` storage** → Test `encodedLength` directly with scalar values under wasm32 and 64-bit target semantics.
- **[Risk] Separate validation and decoding passes cost an extra scan** → Accept at most two linear passes to guarantee failure atomicity without allocation; protocol inputs remain explicitly bounded by their slice lengths.

## Migration Plan

Add the module, generated catalog entry, focused acceptance evidence, and reference together. There is no public predecessor to migrate or compatibility path to preserve. If the module must be removed before stabilization, deleting its catalog entry, source, tests, docs, and capability artifacts fully rolls back the addition.
