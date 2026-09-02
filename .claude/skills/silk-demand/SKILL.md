---
name: silk-demand
description: Capture a direct request from Julia as one deduplicated Silk issue in Linear without prematurely prioritizing or implementing it. Use only when Julia explicitly invokes this skill or asks to record a request.
---

# Claude entrypoint for Silk demand

Read `../../../.codex/skills/silk-demand/SKILL.md` completely and follow it as the canonical skill.
Resolve every relative path in that file from its `.codex/skills/silk-demand/` directory, not from
this wrapper. When the canonical workflow requires subagents or a visible plan, use Claude Code's
corresponding agent and task-tracking facilities while preserving the same coordinator boundaries.
