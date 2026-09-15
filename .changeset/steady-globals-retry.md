---
'@silklang/llvm': patch
---

Keep function and variable declaration state unchanged when a foreign prefix, prologue, or retained
debug-expression handle is rejected, so callers can retry the same global name.
