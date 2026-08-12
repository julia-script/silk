# @silk-effect/platform-webcontainer

## 1.0.0

### Major Changes

- 8fd5458: Add an Effect-native WebContainer platform package with a scoped runtime, standard virtual
  filesystem layer, WebContainer-native processes, and typed event streams. The filesystem layer
  supports native `watch` (Node-style notifications mapped to typed `WatchEvent`s via
  directory-listing probes), and `stat`/`exists`/`access` answer from directory listings without
  reading file contents — file sizes are a documented zero approximation.
