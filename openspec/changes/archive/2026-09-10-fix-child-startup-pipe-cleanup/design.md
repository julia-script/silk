## Context

See proposal.md. The generated adapter already owns three pipes before fork and closes all six
endpoints after a fork failure, but its combined pipe/fcntl failure branch closes only four.

## Goals / Non-Goals

Preserve successful startup handoff and the existing process error ABI. Do not redesign spawning
or introduce source-owned providers.

## Decisions

Split acquisition and configuration into separate branches. After successful acquisition, the
configuration failure branch owns both notice endpoints and closes them alongside the four earlier
endpoints. Capture errno first. Use one close attempt per endpoint, matching the existing selected
close semantics; blind retries could close a reused descriptor.

Use one compiled generated-boundary fixture with scripted pipe/fcntl/close/fork and successful
parent-side read/poll/wait behavior. Invoke the exported operation; never start a real child in the
fixture. Assert returned numeric fields and the exact per-descriptor close record.

## Risks / Trade-offs

Unacquired notice slots are uninitialized. Keeping the acquisition-failure branch separate ensures
it never reads them; the fixture distinguishes four acquired endpoints from six.
