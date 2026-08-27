---
'@silklang/platform-webcontainer': major
---

Add an Effect-native WebContainer platform package with a scoped runtime, standard virtual
filesystem layer, WebContainer-native processes, and typed event streams. The filesystem layer
supports native `watch` (Node-style notifications mapped to typed `WatchEvent`s via
directory-listing probes), and `stat`/`exists`/`access` answer from directory listings without
reading file contents — file sizes are a documented zero approximation.
