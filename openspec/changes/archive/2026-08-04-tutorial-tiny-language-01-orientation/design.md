## Context

The package currently has one lowering-first tutorial and Markdown documentation. The new tutorial is a numbered, multi-page journey governed by `docs/TINY_LANGUAGE_TUTORIAL_BLUEPRINT.md`; this change establishes its entry point and shared navigation. See `proposal.md` for motivation.

## Goals / Non-Goals

**Goals:**

- Create a destination-first opening that orients a compiler novice without teaching implementation details.
- Establish stable page, asset, checkpoint, and navigation conventions for the remaining lesson changes.
- Make every artifact understandable without an interactive playground.

**Non-Goals:**

- Explain LLVM internals in depth; Lesson 2 owns that material.
- Create the compiler implementation or consumer scaffold.
- Select or implement a documentation-site framework.

## Decisions

### Publish the tutorial as numbered Markdown pages under `packages/llvm/docs/tutorials/tiny-language/`

Separate lesson pages match the Kaleidoscope-style progression and let each OpenSpec change own one page. A single very long page would create continual merge pressure and weaker progress landmarks.

### Use Lesson 1 as the tutorial index and destination preview

The opening page will contain the confirmed Tiny program, output contract, time estimate, prerequisites summary, and links to all lessons. A separate empty index would add navigation without instruction.

### Show one tool-ownership pipeline diagram plus text equivalent

The diagram will distinguish Tiny, `@silklang/llvm`, Clang, and the operating system. A purely chronological list would not make ownership boundaries as clear.

### Keep the playground strictly optional

Static source, token, AST, IR, and command examples remain authoritative. The opening may link a future playground but cannot require it.

## Risks / Trade-offs

- [Risk] Previewing every artifact overwhelms a beginner → Limit each preview to one representative fragment and defer vocabulary to its owning lesson.
- [Risk] Later lesson paths change while authored independently → Define numbered filenames and update the index as each lesson lands.
- [Risk] The opening promises an unverified result → Back it with the final lesson's native validation fixture before the series is declared complete.

## Migration Plan

Create the tutorial directory and Lesson 1 page without replacing the existing tiny-expression tutorial. Add the new series to the documentation index only when the minimum linked lesson set is available. Rollback removes the new page and index link without affecting runtime code.
