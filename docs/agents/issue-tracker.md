# Issue tracker: Local Markdown

General issues and specs live under `.scratch/<feature>/`. Wayfinder maps use the visible
`wayfinder/<effort>/` root so later sessions can discover and resume them reliably.

## Wayfinding operations

- A map is `wayfinder/<effort>/map.md` and has `Status: active` or `Status: complete` near the top.
- Child tickets are `wayfinder/<effort>/issues/<NN>-<slug>.md` with `Type:` and `Status:` lines.
- When a map path is supplied, read it exactly.
- When no map is supplied, run
  `find wayfinder -mindepth 2 -maxdepth 2 -type f -name map.md -print`, inspect each map status,
  and load the sole active map. If several maps are active, present their titles and paths for the
  user to choose. If none are active, load the sole completed map when it is the only map; present
  completed titles and paths when several exist; report that no map exists only when discovery is
  empty.
- never infer the tracker from the Git remote and never rely on a scan that omits hidden paths.
- A ticket is on the frontier when it is unresolved, unclaimed, and every ticket listed in its
  `Blocked by:` line is resolved. The first ticket by number wins.
- Claim by setting the child ticket to `Status: claimed` before work.
- Resolve by appending `## Answer`, setting `Status: resolved`, and adding a gist and link to the
  map's decision index.
- Complete a map when no open child remains and `Not yet specified` is empty: append `## Result`
  and set the map to `Status: complete`.
