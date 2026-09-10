## Why

A trusted certificate chain does not establish that its leaf identifies the intended HTTPS
origin. Silk needs one explicit name-matching contract before certificate, trust and TLS work
can compose without accepting names supplied by DNS resolution or the peer.

## What Changes

- Specify caller-owned origin inputs, borrowed decoded SAN inputs, exact Silk operation signatures,
  DNS-ID/IP-ID matching, typed failures, deterministic malformed-entry handling and resource limits.
- Pin an adversarial fixture matrix and compare the policy with RFC 9525, RFC 9110 and webpki.
- Decompose runtime implementation into a separately estimated follow-up.

This is the design-only deliverable for [JUL-169](https://linear.app/juliaortiz/issue/JUL-169).
No runtime API, certificate parser, trust validator or TLS integration is delivered here.

## Capabilities

### New Capabilities

- `https-service-identity`: HTTPS origin reference selection and bounded DNS-ID/IP-ID verification.

### Modified Capabilities

None. Existing cryptographic hashes and generic URI syntax do not own service identity policy.

## Impact

Only this OpenSpec change is edited. The future ordinary-source `silk.https_identity` actor will
consume explicit inputs without requiring a URI parser, certificate decoder or transport provider.
Generic URI syntax remains with JUL-164, certificate decoding with JUL-167, certification-path
policy and name constraints with JUL-168, and TLS composition with JUL-171. No new intrinsic is
needed. Published language references and generated stdlib inventories remain descriptions of
implemented capabilities.

At the refreshed base `991386ae75fe3037e70da1cde9dc71d91dbc3e67`, JUL-182 implements JUL-167's
certificate envelope decoder. It exposes raw extension values and retains duplicates; it does
not decode SAN GeneralNames. The proposed matcher still consumes explicit decoded inputs. A
separate identity-consumer adapter must bridge the raw extension API before certificate/TLS
integration; neither this design nor the pure matcher follow-up claims to deliver that adapter.
