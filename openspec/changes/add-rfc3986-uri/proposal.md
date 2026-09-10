## Why

Protocol clients need one reusable URI parser and reference resolver before transport and identity
policy exist. JUL-164 supplies RFC 3986 syntax without changing the input serialization.

## What Changes

- Add ordinary Silk `Uri` and `UriReference` actors with owned backing text and borrowed component accessors.
- Validate generic authority and host grammar, preserve empty components, and report typed syntax failures.
- Add byte percent coding and strict RFC 3986 reference resolution.
- Register the modules and generate their public documentation and source surfaces.
- Prove conformance through shared runtime acceptance fixtures and focused analysis checks.

## Capabilities

### New Capabilities

- `rfc3986-uri`: Lossless URI/reference syntax, owned representations, percent coding, and resolution.

### Modified Capabilities

None.

## Impact

Changes affect `packages/compiler/stdlib`, its generated surfaces, compiler acceptance fixtures,
and the runtime standard-library reference. No compiler privilege, networking dependency, WHATWG
URL behavior, DNS, IDNA, or TLS identity policy is introduced.
