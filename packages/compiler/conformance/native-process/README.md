# Native process conformance

This suite proves that Silk's process provider works against a real, pinned operating-system
supply. It is admission evidence for supported targets, not an ordinary compiler unit test, so CI
runs the exhaustive suite on `main`, on a weekday schedule, and on demand—not on every pull
request.

Run it after building `@silklang/compiler` and configuring the `SILK_SUPPLY_*` variables used by
the platform-supply workflow:

```sh
# Normal behavior, debug and optimized
pnpm --filter @silklang/compiler test:native-process

# Normal, parent-fault, and (on GNU) child-fault behavior, debug and optimized
pnpm --filter @silklang/compiler test:native-process:full

# One inspectable lane
SILK_PROCESS_MODE=parent-fault \
SILK_PROCESS_OPTIMIZATION=speed \
pnpm --filter @silklang/compiler test:native-process
```

Vitest reports every mode/optimization pair separately. `SILK_PROCESS_MODE` accepts `normal`,
`parent-fault`, `child-fault`, or `all`; `SILK_PROCESS_OPTIMIZATION` accepts `none`, `speed`, or
`all`.

## What the modes prove

- `normal` launches real child processes and checks arguments, environment, working directory,
  stdin, stdout/stderr, signals, missing executables, exit 127, binary bytes, and large concurrent
  output.
- `parent-fault` replaces parent-side foreign calls and fails each relevant call/allocation point.
  It checks that descriptors and child handles are cleaned up and that cleanup failures do not
  replace the original error.
- `child-fault` is GNU-only. It forks real children, injects failures during descriptor setup,
  `chdir`, and `exec`, and checks the startup-notice protocol and child reaping.

Both optimization lanes compile Silk, compile an independent C receiver, link the native runtime,
inspect symbols and relocations, disassemble the object, and execute the result. The runner also
checks LLVM 22.1.8 and the pinned platform-header digests. Per-lane evidence is written under
`.scratch/native-process`.

## Why the fixtures are separate files

The Silk sources and C receivers are test inputs, not hidden test logic. Keeping them external is
intentional: the receiver must be compiled independently so it can catch ABI/layout mistakes in
the compiler output. `NativeProcessConformance.test.mjs` owns orchestration and assertions; the
`.silk` and `.c` files own the programs being compiled. Embedding them as large strings would make
the test look self-contained while making both sides harder to inspect.
