# Authenticated TLS client fixtures

These files are immutable, offline interoperability evidence for `silk/tls_client`.

- `generator/` pins rustls v0.23.35 at commit `7768cd2b44049e040685d48318d13bfa7f7d32a8`
  and the ring crypto provider. The checked-in Cargo lockfile is part of the pin.
- `keys/` contains visibly test-only private keys and certificates. Never use them outside tests.
- `captures/` contains recorded peer flights for the three admitted cipher suites, both key
  exchange groups, both certificate-signature profiles, optional client-certificate request,
  selected and absent ALPN, the forced P-256 HelloRetryRequest path, an exact wrong-name SNI, and
  an IP reference with no SNI extension. Default tests only read committed bytes, verify their
  certificate chains and ClientHello identity fields, and check their SHA-256 values; they require
  neither Rust nor network access.
- `manifest.json` records each capture's exact generator command/configuration, named intermediate
  transcript digests, peer profile,
  fixed Silk random script, fixture provenance, and every immutable file hash. The final
  application record is generated locally from rustls's logged server traffic secret at sequence
  zero; it is not emitted by `ServerConnection`.

rustls uses OS entropy for ephemeral key agreement. Running the opt-in generator checks the same
semantic TLS outcomes but intentionally creates different peer bytes. It does not promise
byte-identical regeneration. The committed captures themselves remain byte-for-byte checked.

The RFC 8448 component vectors are retained separately from full authentication because their
historical RSA-1024 credential is outside Silk's production certificate profile.
