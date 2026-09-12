## Why

The captured scoped-resource callback needed by JUL-188 cannot satisfy the newly introduced
bracket's promise that its returned Effect remains valid for the entire resource-borrow lifetime.
A TLS-free reproduction confirms that the callback also retains an independent captured lifetime;
accepting it by reversing an outlives check would be unsound.

## What Changes

- Add finite lifetime intersections, spelled `'scope & 'env` in an Effect environment annotation,
  to express validity shared by an invocation borrow and retained captures.
- Elaborate the retained environment of an anonymous Effect-producing callable as a dependent
  intersection where its retained lifetime dependencies are known, rather than leaving an
  unrelated synthesized environment binder in its result.
- Compare these contracts inside the existing single outer higher-ranked binder, preserving
  symbolic success, failure, requirement rows, exact representations, and placeholder rejection.
- **BREAKING:** update both use and release contracts of `Effect.useReleaseNonParking` and its
  sealed intrinsic to return an Effect valid for the intersection of the resource borrow and
  callable environment. Update affected callers and generated documentation together.
- Keep lexical anonymous constraints available. The independently verified four-line fix is
  already committed as `81eb4667`; it is a prerequisite, not the lifetime solution.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-lifetimes`: finite intersection environments, canonical substitution, bounded
  outlives proofs, and runtime erasure.
- `bootstrap-callable-values`: dependent returned Effect environments under one outer lifetime
  binder, without allowing captured or invocation borrows to escape.

## Impact

Touches lifetime representation and consumers, Effect environment parsing/elaboration,
quantified callable compatibility/inference, semantic surface serialization, intrinsic bracket
contracts, ordinary-source Effect helpers, their generated embedding/docs, and focused compiler
regressions. It may expose further ownership or representation issues in the preserved generic
provider caller; that caller has not been validated with this proposed capability.

No new dependency or runtime dispatch mechanism is proposed. Implementation must prove that the
existing resource-finalizer lowering preserves loan ordering and cancellation behavior under the
new static contract.

## Non-goals

TLS implementation, authentication changes, provider discovery, ambient transport fallback,
manual cleanup, nested/unconstrained higher-rank inference, general row redesign, compiler
performance work, and changes to the original JUL-188 worktree, stashes, branch, or PR.

## Decision required

Approve or reject explicit finite intersection environments and the corresponding anonymous
callable elaboration before implementation. This is a material language-contract extension,
not authorization to silently accept the old bracket signature. See `design.md` and
`investigation.md` for the proposed proof rules, limitations, and observed evidence.
