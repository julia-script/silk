# Build Tiny, a compiled language: Understand LLVM's role

**Lesson 2 of 13** · [Previous: Meet Tiny](./01-meet-tiny.md) ·
[Next: Create the consumer project and render a module](./03-consumer-setup.md)

In the previous lesson, we followed Tiny source through the compiler pipeline. Now we will learn
to read the smallest LLVM function our compiler can produce. By the end of this lesson, you will
be able to label its types, signature, basic block, instruction, and terminator—and say which tool
owns each artifact after it.

We are not writing compiler code yet. Lesson 3 will set up the project, and Lesson 7 will generate
this function with `@silklang/llvm`.

## Place LLVM in the pipeline

LLVM is reusable compiler infrastructure built around a typed intermediate representation, or IR.
It is not Tiny's parser, and it is not the operating system that runs the final program.

In this tutorial, the responsibilities divide this way:

1. The **Tiny frontend** turns Tiny source into an AST and decides what the program means.
2. **`@silklang/llvm`** helps the frontend construct a valid LLVM module and serialize it.
3. The **Clang driver** accepts the emitted IR, asks the LLVM backend to produce host machine code,
   and invokes the platform linker.
4. The **operating system** loads and runs the linked executable.

The frontend/backend boundary is LLVM IR. Tiny is one possible frontend; the machine-specific
compiler work happens after the IR exists.

## Read a complete LLVM function

Here is a complete module that returns `42` from `main`:

```llvm
; This module came from a source file named answer.tiny.
source_filename = "answer.tiny"

; `define` begins a function definition.
; `i32` is its return type, `@main` is its name, and `()` is its parameter list.
define i32 @main() {
entry:
  ; `ret` is an instruction and the block's terminator.
  ; Its `i32 42` operand matches the function's return type.
  ret i32 42
}
```

Read it from the outside inward:

1. The whole file is an LLVM **module**. A module contains functions and other declarations.
2. `define i32 @main()` is the function **signature**: the function is named `main`, takes no
   parameters, and returns an `i32` value.
3. `i32` is an LLVM **type** representing a 32-bit integer.
4. `entry:` starts a **basic block**, a named sequence of instructions with one entry and one
   terminating control-flow instruction.
5. `ret i32 42` is an **instruction** that returns the typed integer value `42`.
6. The same `ret` is the block's **terminator**. Every basic block must end with a terminator; no
   instruction can follow it in that block.

That numbered explanation is the text equivalent of the comments embedded in the IR. If syntax
highlighting is unavailable, the punctuation still carries the structure: `@` marks a global
name, braces contain the function body, and a trailing colon introduces a block.

Do not worry yet about names such as `%result`, assignment-once values, or merging control-flow
paths. We will introduce those ideas when arithmetic and conditionals make them necessary.

## Distinguish the output artifacts

These files are related, but they are not interchangeable:

| Artifact | Typical form | Produced here by | Consumed here by |
| --- | --- | --- | --- |
| Tiny source | `.tiny` text | You | Tiny lexer and parser |
| LLVM assembly | `.ll` text | `@silklang/llvm` | Humans, Clang, LLVM tools |
| LLVM bitcode | `.bc` bytes | `@silklang/llvm` | Clang and LLVM tools |
| Object file | `.o` or platform equivalent | Clang/LLVM backend | Platform linker |
| Native executable | Platform binary | Linker, driven by Clang | Operating system |

Textual IR and bitcode encode the same kind of LLVM module. Textual `.ll` is designed to be read;
bitcode is its compact binary representation. Neither is the host machine's object code.

In the final command, Clang acts as a driver for more than one step: it compiles the LLVM module
for the current target and invokes the linker to create the executable. `@silklang/llvm` stops
before both operations.

## Avoid three misleading shortcuts

**“LLVM is the virtual machine that runs the program.”** In this tutorial, LLVM supplies an IR and
native compilation infrastructure. The operating system runs the resulting executable.

**“A `.ll` file is CPU assembly.”** It is textual LLVM IR. It still uses portable LLVM types and
operations rather than the instruction set of one physical processor.

**“Bitcode is browser bytecode that we can execute directly.”** LLVM bitcode is a binary LLVM
module. It needs an LLVM-aware consumer such as Clang; the browser playground will compile Tiny to
IR for inspection, not execute bitcode.

When one of these ideas feels blurry, ask: *which tool consumes this artifact next?* The answer
usually restores the boundary.

## Checkpoint: label the IR

Use the module below without looking back at the annotations:

```llvm
source_filename = "answer.tiny"

define i32 @main() {
entry:
  ret i32 42
}
```

Identify:

1. the function name;
2. the function's return type;
3. the parameter list;
4. the basic-block name;
5. the instruction;
6. the terminator; and
7. the value returned to the operating system.

Then put these artifacts in production order: native executable, Tiny source, object file, LLVM
IR.

<details>
<summary>Check your answer</summary>

1. The function name is `main`.
2. Its return type is `i32`.
3. Its parameter list is empty: `()`.
4. The basic block is named `entry`.
5. The instruction is `ret i32 42`.
6. That same `ret` instruction is the block terminator.
7. The returned value is the `i32` integer `42`.

The artifact order is Tiny source → LLVM IR → object file → native executable.

</details>

You can now read the exact LLVM function we will construct later. Next, we will create a clean
consumer project and use `@silklang/llvm` to render its first empty module.

[Previous: Meet Tiny](./01-meet-tiny.md) ·
[Next: Create the consumer project and render a module](./03-consumer-setup.md)
