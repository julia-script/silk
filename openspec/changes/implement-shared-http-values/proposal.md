## Why

Silk's HTTP parser, clients, and servers need one byte-preserving vocabulary for HTTP/1.x values
before they can safely share method, header, target, and authority semantics. JUL-193 adds that pure
foundation now that generic RFC 3986 URI support exists, without coupling values to transport,
framing, or connection state.

## What Changes

- Add ordinary Silk actors for HTTP versions, open method tokens, status codes, validated headers,
  request/response heads, request-target forms, and HTTP authorities.
- Add ordered borrowed header views and explicit owned copies that preserve duplicate fields,
  original name spelling, raw value octets, and lifetime safety.
- Add explicit finite limits, overflow-safe size accounting, typed semantic failures, deterministic
  allocation failure behavior, and mutation-atomic field formatting into caller storage.
- Add request-target construction from generic URIs, HTTP/1.1 Host validation, effective-authority
  selection, and bounded metadata token iterators that retain unknown values.
- Register the public modules and generated surfaces, document ownership and strictness choices,
  and prove behavior with structured analysis and the shared native acceptance
  corpus.

## Capabilities

### New Capabilities

- `http-values`: Shared validated HTTP/1.0 and HTTP/1.1 values, headers, request targets, ownership,
  limits, metadata views, and authority selection.

### Modified Capabilities

None.

## Impact

Changes affect `packages/compiler/stdlib/silk`, `packages/compiler/stdlib/manifest.json`, generated
standard-library source and API surfaces, compiler semantic/ownership fixtures, the shared
cross-target acceptance corpus, and `apps/docs/content/reference`. No HTTP-specific compiler
intrinsic, OS/link dependency, socket requirement, parser, body framing, content decoding,
connection reuse, proxy policy, or complete client/server exchange is introduced.
