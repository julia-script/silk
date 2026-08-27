# Releasing

Releases use Changesets and the `publish.yml` GitHub Actions workflow.

1. Add a changeset with `pnpm changeset`.
2. Merge the Changesets version pull request.
3. The release workflow validates, packages, and publishes `@silklang/llvm`.

Before the first release, configure `publish.yml` as the npm trusted publisher for the package.
The workflow rejects pnpm's `Skipped OIDC` fallback and verifies trusted-publisher provenance in the
npm registry after publication.
