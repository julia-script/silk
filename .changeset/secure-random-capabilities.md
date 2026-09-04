---
'@silklang/compiler': major
---

Rename the deterministic `Random` service to `InsecureRandom`, introduce a provider-driven secure
`Random` exact-fill service and stable shared `InsecureSeed`, plus a reachability-selected
GNU/Linux and macOS `OsRandom` provider.
