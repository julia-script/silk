---
name: silk-work
description: Claim and implement one triaged Silk issue, verify it, run independent reviews, and finish with a draft PR. Use only when Julia explicitly invokes this skill, names an issue to implement, or asks to work the next queued issue.
---

# Claude entrypoint for Silk work

Read `../../../.codex/skills/silk-work/SKILL.md` completely and follow it as the canonical skill.
Resolve every relative path in that file from its `.codex/skills/silk-work/` directory, not from this
wrapper. When the canonical workflow requires subagents or a visible plan, use Claude Code's
corresponding agent and task-tracking facilities while preserving the same coordinator boundaries.
