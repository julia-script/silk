## Context

See `proposal.md` for motivation. `@silklang/llvm` and the Tiny-language tutorial exist, but no
package owns Silk source text or tokenization. The accepted compiler architecture requires later
syntax artifacts to retain exact source bytes, trivia, stable identities, and byte spans, so the
first implementation should establish only that substrate.

## Goals / Non-Goals

**Goals:**

- Introduce the compiler package without committing to parser, analysis, or code-generation APIs.
- Make arbitrary source bytes immutable and lossless.
- Produce a total, deterministic tokenization result for the first permanent lexical subset.
- Give the future parser exact trivia, invalid-token, and end-of-file spans to consume.

**Non-Goals:**

- Parsing, syntax nodes, ASTs, parser recovery, declarations, or module loading.
- Name resolution, types, function contracts, ownership, HIR, MIR, LLVM, or native execution.
- Filesystem access, command-line behavior, text rendering, or line/column calculation.
- Strings, characters, floating-point literals, non-decimal numeric syntax, block comments,
  Unicode identifiers, or the complete future operator vocabulary.

## Decisions

### Create a narrow `@silklang/compiler` package

Create `packages/compiler` with public `SourceFile`, `SourceSpan`, `Token`, `LexicalDiagnostic`, and
`Lexer` actors. The explicit package barrel re-exports those actors as namespaces, and each public
actor receives a package subpath. No `Compiler`, `Analysis`, syntax-tree, HIR, MIR, backend, or
toolchain actor is introduced in this change.

The source and lexer operations are synchronous and total for their public inputs. Lexically
invalid bytes are ordinary result data, not an Effect failure. Callers cannot construct unchecked
spans; applying an owner-mismatched span to a source returns no slice, while spans produced by the
lexer are valid by construction.

Alternative considered: scaffold the complete eventual compiler module tree. Empty future modules
would make speculative boundaries look established before the code provides evidence for them.

### Represent source as owned bytes, not a JavaScript string

`SourceFile.make` accepts a copyable logical identity and a `Uint8Array`, makes an immutable copy,
and never decodes or normalizes it. `SourceSpan` is an opaque owner-qualified half-open byte range.
Public accessors expose identity and offsets; only validated constructors or package operations can
create a span.

Tokens store a kind and a span rather than duplicating a lexeme. Callers recover exact token bytes
by slicing the owning source file. This keeps token identity tied to the source snapshot and avoids
string-decoding assumptions before the language defines where text validity is required.

Alternative considered: accept a JavaScript string and store UTF-16 offsets. That cannot preserve
malformed UTF-8 and would make diagnostics, native file offsets, and later source tooling disagree.

### Make the token stream lossless by construction

`Lexer.lex` returns the source file, an ordered readonly token collection, and an ordered readonly
diagnostic collection. Whitespace, line comments, invalid regions, and end-of-file are token kinds,
not side tables. Every loop iteration consumes a non-empty byte range except the single final EOF
token, making contiguous full-source coverage a directly testable invariant.

The initial scanner recognizes only ASCII forms required by the next parser slice:

- whitespace bytes space, tab, carriage return, and line feed;
- `//` through but not including the next carriage return or line feed;
- identifier start `[A-Za-z_]` and continuation `[A-Za-z0-9_]`;
- complete identifier reclassification for `pub`, `fn`, and `return`;
- decimal digit runs;
- `(`, `)`, `{`, `}`, and the longest-match `->` token; and
- maximal contiguous invalid regions between supported token starts.

`I32` is therefore an identifier in this change; type meaning belongs to a later semantic change.
The lexer does not decide whether a token sequence forms a valid program.

Alternative considered: discard trivia and invalid bytes. A later lossless tree would then need a
second scanner or heuristic reconstruction, creating exactly the duplicate frontend the Wayfinder
decisions reject.

### Keep lexical diagnostics small and stable

The initial diagnostic vocabulary contains one stable invalid-byte-region code with a source-owned
primary span and deterministic message. Each maximal invalid region produces one invalid token and
one diagnostic. Diagnostics are sorted by span and code even though the single scanner naturally
discovers them in that order.

The lexer never throws or returns an Effect for source content. Unexpected implementation throws
remain defects during pure use and are not converted into fake source diagnostics.

Alternative considered: stop at the first invalid byte. That would make the first frontend
capability fail-fast and provide poor input to parser recovery work immediately following it.

### Isolate the per-byte scanner as the only imperative inner loop

The scan is one index-based pass over the owned byte array. It keeps local offsets, emits immutable
tokens and diagnostics at token boundaries, and exposes no mutable scanner state. A focused
benchmark records why this per-byte loop is the documented performance-critical exception allowed
by the repository's Effect architecture rules.

Alternative considered: model each byte transition as an Effect. Token classification has no
external boundary or recoverable operational failure, and adding Effect machinery per byte would
obscure the lexical state machine without improving its error model.

## Risks / Trade-offs

- [The lexical subset is mistaken for the complete language grammar] → Name it explicitly as the
  kernel vocabulary and list every deferred token family in the package documentation.
- [ASCII-only identifiers constrain later language design accidentally] → Treat ASCII as an
  accepted permanent subset, not a rejection promise for future Unicode identifier support.
- [Invalid UTF-8 produces noisy recovery] → Coalesce adjacent bytes that cannot start a supported
  token into one invalid region while preserving every byte and exact span.
- [Opaque spans make tests cumbersome] → Provide total readonly accessors and equality while
  keeping arbitrary unchecked construction private.

## Migration Plan

Add the compiler package and source/lexer exports without modifying existing LLVM or tutorial
packages. Later changes extend the token vocabulary and consume the token stream through new parser
actors. Because no compiler consumer exists yet, rollback removes only the new package, fixtures,
workspace wiring, and capability specs.
