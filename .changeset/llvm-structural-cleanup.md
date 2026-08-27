---
'@silklang/llvm': patch
---

Restructure the builder's internal state: collapse the parallel description/handle/key arrays into
interning tables (one Table per collection, plus dedicated GlobalTable and MetadataTable), deduplicate
handle resolution behind resolveActor, and inline the memory-access alignment encoder into MemoryAccess.
No public API change.
