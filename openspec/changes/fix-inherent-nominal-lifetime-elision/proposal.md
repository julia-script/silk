## Why

After JUL-116, a borrowed holder's elided inherent constructor is accepted as a declaration but cannot be called: its impl head misses nominal lifetime replay and the member is unpublished. This breaks the ordinary `SliceStream.make(&values)` API independently of JUL-117 or borrowed Effect outcomes.

## What Changes

- Replay inherent owner lifetime elision after nominal lifetime arity is available, before publishing members and closing `Self`.
- Retain the whole-family rule over completed generic parameters and preserve used owner lifetime binders in member contracts.
- Preserve the distinction between an omitted nominal result lifetime and `Self`, which names the impl's fixed applied owner.
- Add frontend-only declaration, invocation, explicit/elided and `Self` regressions.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-declaration-index`: clarify inherent-head nominal lifetime elision and closure of `Self` before associated-member publication.

## Impact

Declaration collection/completion and semantic tests; reference clarification. No runtime representation, backend, borrowed Effect outcome or exclusive-storage change.
