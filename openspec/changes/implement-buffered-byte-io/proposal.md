## Why

Incremental protocols need bounded lookahead and short-write retention without duplicating cursor,
ownership, deadline, and terminal-failure rules in every parser. A shared buffered byte layer lets
HTTP preserve bytes read past a delimiter, stream finite bodies, and avoid retrying uncertain
transport prefixes.

## What Changes

- Add fixed-capacity buffered input and output actors over exact-prefix `ByteDuplex` operations.
- Add a scoped duplex session that exclusively retains one concrete transport and closes it after
  success, typed failure, or structured cancellation/interruption without implicitly flushing
  pending output; fatal traps follow the language rule that bypasses finalizers and `Drop`.
- Add a compile-time `BufferedContext` adapter witness so contextual acquisition can consume affine
  or once-callable state by value while lending an independently higher-ranked session, without a
  runtime service or requirement-row member.
- Add input-only and output-only adapters that preserve the distinct `StandardInput` partial-read
  and `Writer` all-or-error contracts.
- Add bounded and exact transfer operations that consume source bytes only after the destination
  reports accepting them.
- Add buffered directional shutdown that drains pending output before delegating the retained
  provider's canonical write shutdown without giving up the readable direction.
- Add typed buffering failures, exact reported progress, sticky terminal state, absolute-deadline
  forwarding, ownership evidence, and public reference documentation.

## Capabilities

### New Capabilities

- `buffered-byte-io`: Fixed-capacity buffered input, output, scoped duplex sessions, adapters,
  bounded transfer, and directional-shutdown semantics.

### Modified Capabilities

- `scoped-byte-duplex-tls-connection`: Extend delivery coverage from the underlying exact-prefix
  transport to the ordinary-source buffering layer that exclusively leases it.

## Impact

The standard library gains `silk.buffered_input`, `silk.buffered_output`,
`silk.buffered_duplex`, and `silk.buffered_transfer` actors. The standard-library manifest,
generated embedding/catalogs, focused compiler evidence, shared runtime corpus, and reference docs
must register and cover the new public surface; no existing `ByteDuplex`, `StandardInput`, `Writer`,
filesystem, socket, or general Stream contract changes.
