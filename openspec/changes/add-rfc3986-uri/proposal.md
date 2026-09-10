## Why

Protocol clients need one reusable URI parser and reference resolver before transport and identity
policy exist. JUL-164 supplies RFC 3986 syntax without changing the input serialization.

## What Changes

- Add ordinary Silk `Uri` and `UriReference` actors with allocation-free borrowed parsing, explicit owned copies, and ownership-taking constructors.
- Validate generic authority and host grammar, preserve empty components, and report typed syntax failures.
- Add reusable-buffer and writer percent coding, in-place byte decoding, and strict resolution directly in reusable output storage.
- Add raw-versus-encoded components, construction/modification from components, selective serialization, and whole-path/unreserved encoding contexts.
- Add the minimal general shared-slice view primitive and ordinary storage operations needed to avoid copies; expose no compiler-known URI policy.
- Register the modules and generate their public documentation and source surfaces.
- Prove conformance through shared runtime acceptance fixtures and focused analysis checks.

## Capabilities

### New Capabilities

- `rfc3986-uri`: Lossless URI/reference syntax, borrowed and owned representations, component construction, percent coding, and resolution.

### Modified Capabilities

None.

## Impact

Changes affect `packages/compiler/stdlib`, its generated surfaces, compiler acceptance fixtures,
and the runtime standard-library reference. Compiler changes are limited to a target-neutral bounds-checked shared-slice view preserving its input lifetime. No URI-specific compiler privilege, networking dependency, WHATWG URL behavior, DNS, IDNA, or TLS identity policy is introduced.
