## Context

See proposal.md for JUL-74 motivation. Source-file printing currently supplies two hard lines for every declaration. Comment gaps separately preserve authored blank lines and trailing comments can consume the supplied prefix.

## Goals / Non-Goals

**Goals:** Apply spacing to declaration boundaries and their trivia while preserving syntax, order, and attachment.

**Non-Goals:** Import organization, semantic analysis, width-policy changes, or mass reformatting.

## Decisions

Classify neighboring top-level declaration kinds. Use one hard line between imports and two otherwise. Annotate only gaps at import-related boundaries with the selected separation so comment rendering can collapse interior whitespace and place boundary whitespace after trailing comments. Reuse ordinary comment rendering elsewhere; changing all comment gaps would broaden this task unnecessarily.

Keep existing width-aware import member formatting. Consecutive declaration layout does not force long member lists onto one physical line.

Use table-driven syntax-only tests for three imports, duplicate and public imports, both boundaries, trailing and standalone comments, module documentation, and final newlines. Every case checks exact bytes, reparsing, syntax/comment preservation, and unchanged byte-idempotent second formatting.

## Risks / Trade-offs

- Trailing comments can consume the declaration prefix → explicitly test boundaries with trailing comments.
- Global comment policy changes could alter unrelated layout → restrict the new gap policy to import-related boundaries.
- Canonical fixture drift → update only artifacts required by checks.

## Migration Plan

Apply code and spec atomically, run focused and repository checks, then sync the delta and archive the completed change. No compatibility path is retained.
