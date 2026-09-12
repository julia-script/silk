## Why

TLS peers and trust bundles supply untrusted certificate bytes. A shared, bounded decoding contract must preserve the material later signature, path and identity consumers need without accidentally making trust decisions during parsing.

## What Changes

- Specify owned `Certificate` and `CertificateBundle` actors with borrowed DER/PEM inputs, atomic results and typed failures.
- Fix a strict PEM profile, schema-aware DER rules, retained information and finite resource budgets.
- Pin authoritative fixtures and deterministic mutations with expected results.
- Decompose parser implementation into a separately estimated follow-up to [JUL-167](https://linear.app/juliaortiz/issue/JUL-167).

This change delivers planning artifacts only. No parser, public module, generated API reference or runtime support ships here.

## Capabilities

### New Capabilities

- `bounded-certificate-decoding`: Portable owned certificate decoding with bounded work and lossless signed material.

### Modified Capabilities

None. Existing owned-byte/allocation requirements are reused unchanged.

## Impact

Future implementation belongs in ordinary Silk standard-library source, its manifest, generated documentation and existing compiler acceptance infrastructure. No compiler-recognized certificate actor, external crypto provider or public generic ASN.1 API is introduced. Consumers are JUL-166 (algorithm policy), JUL-168 (path validation), JUL-169 (identity) and JUL-170 (trust acquisition).

## Non-goals

Signature verification, certificate-path construction/validation, SAN interpretation and identity matching, root acquisition, network access, PEM encoding, private keys, CRLs and general-purpose ASN.1 tooling.
