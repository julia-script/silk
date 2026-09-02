---
name: silk-demand
description: Capture a direct request from Julia as one deduplicated Silk issue in Linear without prematurely prioritizing or implementing it. Use only when Julia explicitly invokes this skill or asks to record a request.
---

# Capture direct demand

Read `../../silk-manager/WORKFLOW.md` and `../../silk-manager/LINEAR.md` completely before acting.

Julia's current statement is sufficient evidence that the request was made. A link is useful but
not required. Preserve the requester, requested outcome, context, deadline, and wording when Julia
provides them. Do not invent missing detail or require a person's name when a role is sufficient.

## Procedure

1. Fetch the canonical Linear project by ID from `LINEAR.md` and verify its team. Never resolve or
   recreate it by display name. If it cannot be resolved, stop before writing.
2. Search canonical-project issues in every state for the same requested outcome and affected
   surface.
3. When one issue clearly matches, append the demand as a dated comment. Preserve its status,
   priority, estimate, and specification. A Canceled issue may return to Backlog only when the new
   demand directly changes the value judgment that caused cancellation; technical invalidation
   remains canceled until triage.
4. When several issues plausibly match, make no write and ask Julia to choose among their links.
5. When no issue matches, create one Backlog issue with no priority or estimate. Use the issue shape
   in `WORKFLOW.md`, `Origin: direct demand`, the faithful request as evidence, draft acceptance,
   and a lightweight repository baseline. Preserve Julia's stated reason in `## Why this matters`.
   If no reason was supplied, say that the request is direct demand and list the missing impact as
   a triage question instead of inventing one. Use `Feature`, `Bug`, or `Improvement` only when the
   request clearly fits; otherwise omit the label.
6. Read the issue back and report its identifier, title, link, requester, and captured outcome.

This skill records demand. It does not decide that the requested solution is correct, move the
issue to Todo, create an OpenSpec change, edit code, or create a PR.
