## 1. Compiler Semantic Target Queries

- [x] 1.1 Add failing `@effect/vitest` coverage for local, parameter, imported, qualified, field, callable, unavailable, damaged, nested, and half-open position queries through `Analysis`
- [x] 1.2 Add the immutable semantic-target actor with tagged target identities, explicit recovery outcomes, origin spans, and declaration-name locations
- [x] 1.3 Collect deterministic per-module semantic target entries from existing recovered analysis facts and resolve their declaration locations without spelling-based lookup
- [x] 1.4 Store the semantic target index in `Analysis.Snapshot` and expose the position and declaration-location queries through the `Analysis` facade
- [x] 1.5 Verify repeated snapshots, source-order tie breaking, cross-module locations, and recovery isolation in compiler tests

## 2. Project Identity and Session Scheduling

- [x] 2.1 Add tests for project isolation, same-named modules across projects, and stable non-colliding identities for standalone and virtual documents
- [x] 2.2 Replace the shared `untitled` fallback with stable workspace and module identities while preserving project-relative canonical module names
- [x] 2.3 Add an internal project-session actor for synchronized document versions, immutable project revisions, committed sessions, pending work, and exact-version waiters
- [x] 2.4 Add controlled-clock tests proving debounce coalescing, one active worker per project, parallel independent projects, changed-root priority, and latest-pending execution
- [x] 2.5 Implement the latest-wins project worker that freezes overlays, analyzes open roots sequentially, and atomically commits only the current project revision
- [x] 2.6 Add tests proving stale runs cannot commit or publish, superseded exact-version waiters return no session, and close removes overlays and resolves waiters

## 3. Server Session Integration

- [x] 3.1 Replace the per-URI session and generation refresh path with project-session open, change, close, and atomic diagnostic publication flows
- [x] 3.2 Include analyzed document versions in published diagnostics and retain sibling-overlay refresh behavior under project-scoped scheduling
- [x] 3.3 Route hover, symbols, formatting, and new semantic handlers through exact-version session acquisition so positions never use mismatched text and facts
- [x] 3.4 Add explicit shutdown handling that closes project sessions, settles pending callbacks, releases registrations, and disposes the managed runtime
- [x] 3.5 Extend stdio tests for rapid edits, stale diagnostic suppression, versioned publication, coherent requests during refresh, document close, and multi-project isolation

## 4. Filesystem Invalidation

- [x] 4.1 Capture watched-file client capabilities during initialization and register `.silk` and `silk.toml` project watchers through the standard LSP mechanism
- [x] 4.2 Map source-file notifications to containing project invalidations while preserving open-buffer precedence and ignoring unrelated projects
- [x] 4.3 Rediscover open document membership after manifest notifications and migrate documents between affected project sessions without stale commits
- [x] 4.4 Add integration tests for closed dependency changes, synchronized files also changed on disk, manifest-driven source-root changes, and unrelated project events
- [x] 4.5 Add a VS Code client watcher fallback only if dynamic registration is unavailable, and document external-change limitations for clients that report no file events

## 5. Go-to-Definition

- [x] 5.1 Add `Document` tests for `LocationLink` conversion across local bindings, parameters, declarations, imports, qualified calls, fields, callable values, and unavailable targets
- [x] 5.2 Implement definition conversion from UTF-16 position to the `Analysis` semantic target and from exact snapshot sources to origin, target, and selection ranges
- [x] 5.3 Advertise `definitionProvider`, add the exact-version definition handler, and return no result for superseded, absent, ambiguous, inaccessible, conflicting, or unavailable targets
- [x] 5.4 Extend real stdio tests for capability negotiation, Unicode positions, shadowing, open unsaved target modules, and closed cross-file target modules
- [x] 5.5 Update the LSP package documentation to describe go-to-definition, project scheduling, watched-file behavior, and unsupported-client limitations

## 6. Verification and Release Validation

- [x] 6.1 Run focused compiler and LSP tests and resolve all semantic query, scheduler, protocol, and lifecycle failures
- [x] 6.2 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test` in repository verification order
- [ ] 6.3 Run `pnpm check` and report any failure with whether it predates this change
- [ ] 6.4 Run `pnpm release:candidate` because published compiler and LSP package contents change
