## Context

See proposal.md for the motivation. Existing String, Bytes, RawBuffer, slices, and allocator Effects provide
the required storage. There is no URI actor or active URI change at the admission baseline.

## Goals / Non-Goals

**Goals:** One parser, immutable owned component ranges, typed syntax errors, and strict resolution.

**Non-Goals:** The exclusions in the spec; compiler intrinsics or backend-specific URI paths.

## Decisions

- `silk.uri_reference` owns `UriReference`, parser, component ranges, host kind, and syntax errors.
  `silk.uri` owns `Uri` with one `UriReference` field, enforces a scheme, and performs resolution.
  This shares one parser without a public unchecked constructor or duplicate storage.
- UriReference stores its ASCII serialization in one owned RawBuffer with a length. RawBuffer views
  support bounded reborrows without adding public substring APIs to several storage actors.
- Public parse operations return `Result<Owner, ParseError>` inside an allocation Effect. Validation
  is synchronous and precedes copying. Component accessors return `Option<string>` for optional
  components and `string` for path. `format` returns the full borrowed serialization.
- `Uri` means the RFC `URI` production (scheme required, fragment permitted), not the more restricted
  `absolute-URI` production. This keeps resolution results representable; documentation states the distinction.
- `silk.uri_percent.UriPercent` owns component-aware byte encoding and decoding through inherent operations. Byte decoding uses explicit
  `String.fromUtf8` when callers need text. No implicit unescaping appears in parsing or resolution.
- Use linear scans of ASCII bytes, index ranges, and explicit IPv6 group/compression validation.
  Avoid regular expressions, networking parsers, numeric port interpretation, and normalization.
- Resolution composes validated components, performs literal dot removal on path bytes, then parses
  the result through the same parser. Temporary owned buffers use ordinary destruction on every exit.
- Put all RFC resolution vectors and distinct parser/encoding boundaries in one shared native
  acceptance program. Existing manifest tests cover registration. Add ownership analysis only where
  existing generic lifetime evidence does not cover the public accessors.

## Risks / Trade-offs

- IPv6 compression and embedded IPv4 have many boundaries → test distinct valid and invalid group counts.
- Small parser errors can change delimiter meaning → retain all escape spelling and test empty delimiters.
- A large runtime case can add compilation cost → share one base and one program, measure focused runtime.
- Revalidation of resolved output adds a scan → keeps construction safe and avoids unchecked public APIs.
